import Html exposing (..)
import Json.Decode exposing (Decoder, decodeString, list, string)
import Json.Decode.Pipeline exposing (decode, required)

type alias Question =
  { text : String
  , answers: List String
  }

type alias QuestionTree =
  { root: Question }

oldTree : QuestionTree
oldTree =
  { root =
    { text = "Question"
    , answers = ["Answer A", "Answer B", "Answer C"]
    }
  }

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

questionDecoder : Decoder Question
questionDecoder =
  decode Question
    |> required "text" string
    |> required "answers" (list string)

questionTreeDecoder : Decoder QuestionTree
questionTreeDecoder =
  decode QuestionTree
    |> required "root" (questionDecoder)

tree = decodeString (questionTreeDecoder) json

treeToHTML : Result String QuestionTree -> Html msg
treeToHTML input =
  case input of
    Ok tree ->
      div []
      [ h1 [] [text "Colonoscopy"]
      , h2 [] [text tree.root.text]
      , ul [] (List.map (\s -> li [] [text s]) tree.root.answers)
      ]
    Err error ->
      div []
      [ h3 [] [text "Error"]
      , p [] [text error]
      ]

-- In the future we'll want only the list from JSON generation, and management
-- to be in Elm, the rest should be in a template file. Plain HTML? Pug?
main =
  treeToHTML tree
