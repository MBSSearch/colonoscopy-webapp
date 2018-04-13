import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, decodeString, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Task

-- In the future we'll want only the list from JSON generation, and management
-- to be in Elm, the rest should be in a template file. Plain HTML? Pug?
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

-- Model

type Model
  = Empty
  | Loading
  | Loaded (Result String QuestionTree)

init : (Model, Cmd Msg)
init = (Empty, Cmd.none)

-- Update

type Msg
  = Load
  | Succeed QuestionTree
  | Fail String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Load ->
      Loading
      |> update (processQuestionTreeJSON (decodeString (questionTreeDecoder) json))

    Succeed tree -> (Loaded (Ok tree), Cmd.none)

    Fail error -> (Loaded (Err error), Cmd.none)

processQuestionTreeJSON : Result String QuestionTree -> Msg
processQuestionTreeJSON result =
  case result of
    Ok tree -> Succeed tree
    Err error -> Fail error

-- View

view : Model -> Html Msg
view model =
  case model of
    Empty ->
      button [onClick Load] [text "Load"]

    Loading ->
      p [] [text "Loading..."]

    Loaded (Ok tree) -> (toHTML tree)

    Loaded (Err error) ->
      div [] [
        h3 [] [text "Error"]
        , p [] [text error]
      ]

toHTML : QuestionTree -> Html Msg
toHTML tree =
  div []
  [ h1 [] [text "Colonoscopy"]
  , h2 [] [text tree.root.text]
  , ul [] (List.map (\s -> li [] [text s]) tree.root.answers)
  ]

-- Types

type alias Question =
  { text : String
  , answers: List String
  }

type alias QuestionTree =
  { root: Question }

questionDecoder : Decoder Question
questionDecoder =
  decode Question
    |> required "text" string
    |> required "answers" (list string)

questionTreeDecoder : Decoder QuestionTree
questionTreeDecoder =
  decode QuestionTree
    |> required "root" (questionDecoder)

json : String
json = """
{
  "root": {
    "text": "Question",
    "answers": [
      "Answer A",
      "Answer B",
      "Answer C"
    ]
  }
}
"""

