import Html exposing (..)

-- In the future we'll want only the list from JSON generation, and management
-- to be in Elm, the rest should be in a template file. Plain HTML? Pug?
main =
  div [] [
    h1 [] [text "Colonoscopy"]
    , h2 [] [text "Question?"]
    , ul [] [
      li [] [text "Answer A"]
      , li [] [text "Answer B"]
      , li [] [text "Answer C"]
      ]
    ]
