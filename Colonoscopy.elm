import Html exposing (..)

type alias Question =
  { text : String
  , answers: List String
  }

type alias QuestionTree =
  { root: Question }

tree : QuestionTree
tree =
  { root =
    { text = "Question"
    , answers = ["Answer A", "Answer B", "Answer C"]
    }
  }

-- In the future we'll want only the list from JSON generation, and management
-- to be in Elm, the rest should be in a template file. Plain HTML? Pug?
main =
  div []
  [ h1 [] [text "Colonoscopy"]
  , h2 [] [text tree.root.text]
  , ul [] (List.map (\s -> li [] [text s]) tree.root.answers)
  ]
