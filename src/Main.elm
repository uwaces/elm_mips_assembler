import Browser
import Html exposing (Html, Attribute, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing (..)


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
  div []
    [ div [] [text "$zero and $at work!!! -- Make the boo a Woo"]
    , textarea [ cols 80, rows 30, placeholder "Assembly to assemble", value model.content, onInput Change ] []
    , div [] [ text (assemble model.content) ]
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
        ]

assemblyline : Parser AssemblyLine
assemblyline =
    oneOf
        [ succeed AssemblyInstr |= instruction
        , succeed AssemblyLabel |= target
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
        R r -> r.name ++ " " ++ regToString r.rd ++ ", " ++ regToString r.rs ++ ", " ++ regToString r.rt
        I i -> i.name ++ " " ++ regToString i.rt ++ " " ++ immToString i.imm ++ "(" ++ regToString i.rs ++ ")"
        J j -> j.name ++ " " ++ immToString j.imm

assemblyLineToString : AssemblyLine -> String
assemblyLineToString line =
    case line of
        AssemblyInstr instr -> instructionToString instr
        AssemblyLabel l -> l

assemble : String -> String
assemble prog =
  case program prog of
      Just l -> String.join "\n" <| List.map assemblyLineToString l
      Nothing -> ":("

