import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (get, send)
import Json.Decode exposing (Decoder, decodeString, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Result exposing (mapError)

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
  | Loaded (Result Http.Error QuestionTree)

init : (Model, Cmd Msg)
init = (Loading, getJSON)

getJSON : Cmd Msg
getJSON =
  let
      url = "https://s3-ap-southeast-2.amazonaws.com/static.mbssearch.com/colonoscopy_decision_tree_demo.json"

      request =
        Http.get url questionTreeDecoder
  in
      Http.send NewJSON request

-- Update

type Msg
  = Load
  | NewJSON (Result Http.Error QuestionTree)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Load -> (Loading, getJSON)

    NewJSON (Ok tree) -> (Loaded (Ok tree), Cmd.none)

    NewJSON (Err error) -> (Loaded (Err error), Cmd.none)

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
        , p [] [text (toString error)]
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
