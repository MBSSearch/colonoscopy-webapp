import Array exposing (..)
import Either exposing (Either)
import Either.Decode exposing (either)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (..)
import Http exposing (get, send)
import Json.Decode exposing (Decoder, array, decodeString, lazy, int, map, oneOf, string)
import Json.Decode.Pipeline exposing (decode, required)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

-- Model

type Model
  = Loading
  | Loaded (Result Http.Error LoadedModel)

type alias LoadedModel =
  { tree: QuestionTree
  , currentNode: Either ItemNumber Question
  , nodesHistory: Array (Either ItemNumber Question)
  }

pop : Array a -> Array a
pop array =
  slice 0 -1 array

getLast : Array a -> Maybe a
getLast array =
  Array.get (length array - 1) array

init : (Model, Cmd Msg)
init = (Loading, getJSON)

getJSON : Cmd Msg
getJSON =
  let
      url = "https://s3-ap-southeast-2.amazonaws.com/static.mbssearch.com/colonoscopy_decision_tree.json"

      request =
        Http.get url questionTreeDecoder
  in
      Http.send NewJSON request

-- Update

type Msg
  = Load
  | NewJSON (Result Http.Error QuestionTree)
  | SelectAnswer Int
  | GoBack

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Load ->
      (Loading, getJSON)

    NewJSON (Ok tree) ->
      (Loaded (Ok { tree = tree, currentNode = (Either.Right tree.root), nodesHistory = fromList [] }), Cmd.none)

    NewJSON (Err error) ->
      (Loaded (Err error), Cmd.none)

    SelectAnswer index ->
      case model of
        Loaded (Ok loadedModel) ->
          (Loaded (Ok (updateWithSelectedAnswerIndex index loadedModel)), Cmd.none)

        _ -> (model, Cmd.none)

    GoBack ->
      case model of
        Loaded (Ok loadedModel) ->
          case getLast loadedModel.nodesHistory of
            Just previousNode ->
              (Loaded (Ok { tree = loadedModel.tree, currentNode = previousNode, nodesHistory = pop loadedModel.nodesHistory }), Cmd.none)

            Nothing ->
              -- Previous history was empty. This is a case in which we should
              -- never end up.
              (model, Cmd.none)

        _ ->
          -- We should never end up in this state, a `GoBack` message should be
          -- sent only from the state describe above.
          (model, Cmd.none)

updateWithSelectedAnswerIndex : Int -> LoadedModel -> LoadedModel
updateWithSelectedAnswerIndex index model =
  case getAnswer index model.currentNode of
    -- The given index did not match any of the answer in the current node.
    -- This is a case in which we should never end up.
    Nothing ->
      model

    Just nextQuestionOrItem ->
      { tree = model.tree
      , currentNode = nextQuestionOrItem
      , nodesHistory = append model.nodesHistory (fromList [model.currentNode])
      }

getAnswer : Int -> Either ItemNumber Question -> Maybe (Either ItemNumber Question)
getAnswer index node =
  case node of
    -- If the node is an `ItemNumber` then we are at the end of the tree, and
    -- there's nothing left to select. This is a case in which we should never
    -- end up.
    Either.Left item ->
      Nothing

    Either.Right question ->
      Maybe.map (\a -> a.next) <| get question.answers index

-- View

view : Model -> Html Msg
view model =
  case model of
    Loading ->
      p [] [text "Loading..."]

    Loaded (Ok loadedModel) ->
      div []
        [ h1 [] [text "Colonoscopy"]
        , backButtonIfNeeded loadedModel.nodesHistory
        , toHTML loadedModel.currentNode
        ]

    Loaded (Err error) ->
      div [] [
        h3 [] [text "Error"]
        , p [] [text <| toString error]
      ]

backButtonIfNeeded : Array a -> Html Msg
backButtonIfNeeded array =
  case getLast array of
    Nothing -> span [] []

    Just _ ->
        a [href "#", onClick GoBack] [text "Back"]

toHTML : Either ItemNumber Question -> Html Msg
toHTML itemOrQuestion =
  case itemOrQuestion of
    Either.Left itemNumber ->
      h2 [] [text <| toString itemNumber.number]

    Either.Right question ->
      questionToHTML question

questionToHTML : Question -> Html Msg
questionToHTML question =
  div []
  [ h2 [] [text question.text]
  -- Look, I don't know how I feel about leaking CSS in here... Apparently
  -- that's what "components" are all about, still.
  , ul [class "list-group"] (toList <| indexedMap (\index answer -> li [class "list-group-item"] [a [href "#", onClick
  (SelectAnswer index)] [text answer.text]]) question.answers)
  ]

-- Types

{- The JSON describing the decision tree looks like this:

"root": {
  "text": "What did you find at Colonoscopy?",
  "answers": [
    {
      "text": "Polyps",
      "next": {
        "text": "What was the clinical indication?",
        "answers": [
          { "text": "Polyps previously or IBD", "next": { ... } },
          ...
        ]
      }
    },
    { "text": "Failed bowel prep", "next": { "item_number": 32231 } },
    ...
  ]
}

The "answers" key of a question can be either an item number or a question
itself. This makes the representation recursive. -}

{- `QuestionTree` is nothing but the container of the questions. I think the
JSON looks neater with a "root" key, but in this domain there doesn't seem to
be a need for this type, other than for mapping the JSON structure.

I'd like to remove it, but I don't yet know how to write a `Decoder` that takes
a JSON with nested objects and returns a decoded object one or more levels
deep. -}
type alias QuestionTree =
  { root: Question }

type alias Question =
  { text : String
  , answers: Answers
  }

-- The opaque type `Answers` allows us to _hide_ the recursion of nested
-- questions in a question's answer.
-- See https://github.com/elm-lang/elm-compiler/blob/0.18.0/hints/recursive-alias.md
type Answers = Answers (Array Answer)

-- Because `Answers` hides `Array Answer` from the rest of the code, in order to
-- `map` on the list we need implement our own function.
-- See this post for more info on implementing functions for opaque types:
-- https://medium.com/@ghivert/designing-api-in-elm-opaque-types-ce9d5f113033
map : (Answer -> a) -> Answers -> Array a
map f (Answers l) = Array.map f l

indexedMap : (Int -> Answer -> a) -> Answers -> Array a
indexedMap f (Answers l) = Array.indexedMap f l

get : Answers -> Int -> Maybe Answer
get (Answers a) index =
  Array.get index a

type alias Answer =
  { text : String
  {- Interesting note. After initially failing to compile the decoding with
  `Either` I went through the path of defining an `AnswerNext` union type for
  this property. I dropped the approach because it resulted in the runtime
  error described by these posts:

  - https://github.com/elm-lang/core/issues/686
  - https://github.com/elm-lang/core/issues/703
  - https://github.com/elm-lang/elm-compiler/issues/1591

  Evan reckons the issues is fixed in Elm 0.19. Would be interseting to try it out.

  Anyway I think the `Either` approach reads better and requires less work, I'd
  rather stick with it. -}
  , next : Either ItemNumber Question
  }

{- As for `QuestionTree` above, the type defined in this way is a bit
redundant. I'd like to remove it, but I don't yet know how to write a `Decoder`
that takes a JSON with nested objects and returns a decoded object one or more
levels deep.

It would have been better to have `ItemNumber` as type alias for `Int`. It
would have made it clear what the numeric value was, without having to drill
into a type with a single property.-}
type alias ItemNumber =
  { number: Int }

-- JSON Decoders

questionTreeDecoder : Decoder QuestionTree
questionTreeDecoder =
  decode QuestionTree
    |> required "root" question

{- Unlike `questionTreeDecoder`, this and all the other `Decoder`s for the
custom types are simply called like the type they decode. The rationale for
this is to keep them in line with the other `Decoder` functions exposed by
`Json.Decode`, and to make the definition simpler to read.

`questionTreeDecoder` has the `Decoder` prefix because it's used outside the
scope of just decoding. Making the fact that it is a `Decoder` explicit helps
with readbility.

Is this an OK practice? Is the inconsistency in naming worth the readbility
gain? -}
question : Decoder Question
question =
  decode Question
    |> required "text" string
    |> required "answers" (Json.Decode.map Answers (array <| lazy (\_ -> answer)))

answer : Decoder Answer
answer =
  decode Answer
    |> required "text" string
    |> required "next" (lazy (\_ -> either itemNumber question))

itemNumber : Decoder ItemNumber
itemNumber =
  decode ItemNumber
    |> required "item_number" int
