import Browser
import Html exposing (Html, Attribute, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing (..)
import Dict exposing (Dict)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }

-- VIEW

view : Model -> Html Msg
view model =
  div [ style "font-family" "Courier" ]
    [ textarea [ cols 80
               , rows 30
               , placeholder "Assembly to assemble"
               , value model.content
               , onInput Change
               ] []
    , div [] <| List.map (\s -> div [] [ text s ]) (assemble model.content)
    ]

-- Assemble
type Immediate
    = Label String
    | Value Int

immToString : Immediate -> String
immToString i =
    case i of
        Label l -> l
        Value v -> String.fromInt v

type Register
  = Reg Int

regToString : Register -> String
regToString r =
    case r of
        Reg x -> String.fromInt x

type alias RType =
    { name : String
    , rd   : Register
    , rs   : Register
    , rt   : Register
    }

type alias IType =
    { name  : String
    , rt    : Register
    , imm   : Immediate
    , rs    : Register
    }

makeITypeBranch : String -> Register -> Register -> Immediate -> IType
makeITypeBranch name rs rt imm = IType name rt imm rs

type alias JType =
    { name  : String
    , imm   : Immediate
    }

type Instruction
    = R RType
    | I IType
    | J JType
    | Noop
    
type AssemblyLine
    = AssemblyInstr Instruction
    | AssemblyLabel String

spaces : Parser ()
spaces =
    succeed ()
        |. chompIf (\c -> c == ' ')
        |. chompWhile (\c -> c == ' ')

register : Parser Register
register =
    succeed Reg
        |. symbol "$"
        |= int

label : Parser String
label =
    getChompedString <|
        succeed ()
            |. chompIf Char.isAlpha
            |. chompWhile (\c -> Char.isAlpha c)

target : Parser String
target =
    succeed identity
        |= label
        |. symbol ":"

immediate : Parser Immediate
immediate =
    oneOf
        [ succeed Value |= int
        , succeed Label |= label
        ]

makeOpParser : String -> Parser String
makeOpParser op =
    succeed (\_ -> op) |= keyword op

makeOperation : List String -> Parser String
makeOperation ops =
    oneOf <| List.map makeOpParser ops
        
roperation : Parser String
roperation = makeOperation [ "add"
                           , "sub"
                           , "and"
                           , "or"
                           , "nor"
                           , "slt"
                           ]

jtype : Parser JType
jtype =
    succeed JType
        |= makeOperation [ "jmp" ]
        |. spaces
        |= immediate

itype : Parser IType
itype =
    succeed IType
        |= makeOperation [ "lw", "sw" ]
        |. spaces
        |= register
        |. spaces
        |= immediate
        |. symbol "("
        |= register
        |. symbol ")"

itypeBranch : Parser IType
itypeBranch =
    succeed makeITypeBranch
        |= makeOperation [ "beq" ]
        |. spaces
        |= register
        |. spaces
        |= register
        |. spaces
        |= immediate

rtype : Parser RType
rtype =
    succeed RType
        |= roperation
        |. spaces
        |= register
        |. spaces
        |= register
        |. spaces
        |= register

instruction : Parser Instruction
instruction =
    oneOf
        [ succeed R |= rtype
        , succeed I |= itype
        , succeed I |= itypeBranch
        , succeed J |= jtype
        , succeed Noop |. keyword "nop"
        ]

assemblyline : Parser AssemblyLine
assemblyline =
    oneOf
        [ succeed AssemblyInstr |= instruction |. end
        , succeed AssemblyLabel |= target |. end
        ]

liftOk : Result (List DeadEnd) AssemblyLine ->
         Result (List DeadEnd) (List AssemblyLine) ->
         Result (List DeadEnd) (List AssemblyLine)
liftOk r1 rlist = 
    case r1 of
        Ok al1 -> case rlist of
                    Ok l -> Ok <| al1::l
                    Err e -> Err e
        Err e -> Err e
                 
program : String -> Maybe (List AssemblyLine)
program prog =
    let lines = String.split "\n" <| String.trim prog
        alines = List.map (run assemblyline) lines
        res = List.foldr liftOk (Ok []) alines
    in
        case res of
            Ok l -> Just l
            Err _ -> Nothing

instructionToString : Instruction -> String
instructionToString instr =
    case instr of
        R r -> r.name ++ " " ++ regToString r.rd ++ ", " ++ regToString r.rs ++
               ", " ++ regToString r.rt
        I i -> i.name ++ " " ++ regToString i.rt ++ " " ++ immToString i.imm ++
               "(" ++ regToString i.rs ++ ")"
        J j -> j.name ++ " " ++ immToString j.imm
        Noop -> "nop"

assemblyLineToString : AssemblyLine -> String
assemblyLineToString line =
    case line of
        AssemblyInstr instr -> instructionToString instr
        AssemblyLabel l -> l

labelLines : List AssemblyLine -> List (String, Instruction)
labelLines xs =
    case xs of
        [] -> []
        (AssemblyLabel l)::(AssemblyInstr i)::zs -> (l, i)::labelLines zs
        (AssemblyInstr i)::zs -> ("", i)::labelLines zs
        (AssemblyLabel l)::zs -> (l, Noop)::labelLines zs

buildLabelDict : List (String, Instruction) -> Dict String Int
buildLabelDict xs =
    let makePair = \i -> \(s, _) -> (s, i)
    in
        Dict.fromList <| List.indexedMap makePair xs

nameToOpcode : String -> String
nameToOpcode name =
    case name of
        "and" -> "000000" 
        "or" -> "000000"
        "add" -> "000000"
        "sub" -> "000000"
        "nor" -> "000000"
        "slt" -> "000000"
        "beq" -> "000100"
        "lw" -> "100011"
        "sw" -> "101011"
        "jmp" -> "000010"
        _ -> "Bad Instruction!"

nameToFunct : String -> String
nameToFunct name =
    case name of
        "and" -> "100100" 
        "or" -> "100101"
        "add" -> "100000"
        "sub" -> "100010"
        "nor" -> "100111"
        "slt" -> "101010"
        _ -> "Bad Instruction!"
            
translateRegister : Register -> String
translateRegister (Reg x) =
    if x < 32 then
        unsignedToBinaryString 5 x
    else
        "Bad register name!"
        
translateR : RType -> String
translateR r = nameToOpcode r.name ++
               translateRegister r.rs ++
               translateRegister r.rt ++
               translateRegister r.rd ++
               "00000" ++
               nameToFunct r.name

toBinaryString : Int -> Int -> String
toBinaryString length value =
    if value < 0 then
        unsignedToBinaryString length ((2^length) + value)
    else
        unsignedToBinaryString length value

unsignedToBinaryString : Int -> Int -> String
unsignedToBinaryString length value =
    case (length, value) of
        (0, _) -> ""
        (l, v) -> (toBinaryString (l - 1) (v // 2)) ++
                  (String.fromInt <| modBy 2 v)
                   
translateI : Int -> Dict String Int -> IType -> String
translateI pc labels i =
    let immBin = 
            case i.imm of
                Value v -> toBinaryString 16 v
                Label l -> case (Dict.get l labels) of
                               Just x -> toBinaryString 16 (x - (pc + 1))
                               Nothing -> "Bad target!"
    in
        nameToOpcode i.name ++
        translateRegister i.rs ++
        translateRegister i.rt ++
        immBin

translateJ : Dict String Int -> JType -> String
translateJ labels j =
    let immBin =
            case j.imm of
                Value v -> toBinaryString 26 v
                Label l -> case (Dict.get l labels) of
                               Just x -> toBinaryString 26 x
                               Nothing -> "Bad target!"
    in
        nameToOpcode j.name ++
        immBin

translateInstruction : Dict String Int -> Int -> Instruction -> String
translateInstruction labels pc instr =
    case instr of
        Noop -> "00000000000000000000000000000000"
        R r -> translateR r
        I i -> translateI pc labels i
        J j -> translateJ labels j

assemble : String -> List String
assemble prog =
    case program prog of
        Just l ->
            let
                labeledLines = labelLines l
                labels = buildLabelDict labeledLines
                (_, instrs) = List.unzip labeledLines
            in
                List.indexedMap (translateInstruction labels) instrs
        Nothing -> ["Error: Could not assemble program."]
