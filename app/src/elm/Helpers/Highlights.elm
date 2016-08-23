module Helpers.Highlights exposing (Highlights, empty, singleton, merge, jsonEncode, jsonDecode, toList)

import Date exposing (Date)
import Dict exposing (Dict, get, insert)
import Json.Encode as Encode
import Json.Decode as Json
import String exposing (join)

type alias Highlights = Dict String (List String)

-- PUBLIC API

empty : Highlights
empty =
  Dict.empty


singleton : Date -> List String -> Highlights
singleton date highlights =
  Dict.singleton (formatDate date) highlights


merge : Highlights -> Highlights -> Highlights
merge h1 h2 =
  Dict.foldr addHighlights h1 h2


jsonEncode : Highlights -> String
jsonEncode highlights =
  Dict.toList highlights
    |> List.map encodeTuple
    |> Encode.object
    |> Encode.encode 0


jsonDecode : Json.Decoder Highlights
jsonDecode =
  Json.dict (Json.list Json.string)


toList : Highlights -> List (String, List String)
toList highlights =
  Dict.toList highlights

-- PRIVATE API

addHighlights : String -> List String -> Highlights -> Highlights
addHighlights date highlights target =
  let
    newHighlights =
      case get date target of
        Just existingHighlights ->
          existingHighlights ++ highlights

        Nothing ->
          highlights

  in
    insert date newHighlights target

encodeTuple : (String, List String) -> (String, Encode.Value)
encodeTuple (k, v) =
  (k, Encode.list (List.map Encode.string v))

formatDate : Date -> String
formatDate date =
  let
    (year, month, day) = (Date.year date, Date.month date, Date.day date)
  in
    [(toString year), (toString month), (toString day)] |> join "-"