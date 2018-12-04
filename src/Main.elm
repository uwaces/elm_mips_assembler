import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=))


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
    , input [ placeholder "Text to reverse", value model.content, onInput Change ] []
    , div [] [ text (assemble model.content) ]
    ]

-- Assemble

type Register
  = Zero
  | At
  | V0
  | V1
  | A0
  | A1
  | A2
  | A3
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | T8
  | T9
  | K0
  | K1
  | Gp
  | Sp
  | Fp
  | Ra

type alias RS = Register
type alias RT = Register
type alias RD = Register

--type Instruction
--  = I IOptCode RS RT RD ShiftAmount Function
--  | R ROptCode RS RT Imm
--  | J JOptCode Address

--type IOptCode
--  = Addi
--  | Subi

registerParser : Parser.Parser Register
registerParser =
  Parser.oneOf
    [
      Parser.succeed Zero
        |. Parser.symbol "$zero"
    , Parser.succeed At
        |. Parser.symbol "$at"
    ]

assemble : String -> String
assemble program =
  case Parser.run registerParser program of
    Ok r -> "WOO"
    Err _ -> "BOO"

