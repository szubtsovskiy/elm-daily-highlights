module Helpers.Highlights exposing (Highlights, empty, singleton, addAll, add, jsonEncode, jsonDecode, toList, fromList)

import Date exposing (Date)
import Dict exposing (Dict, get, insert)
import Json.Encode as Encode
import Json.Decode as Json
import List exposing (map)
import Result exposing (Result(Ok, Err))
import String exposing (join)

type alias Highlights = Dict String (List String)

-- PUBLIC API

empty : Highlights
empty =
  Dict.empty


singleton : Date -> List String -> Highlights
singleton date highlights =
  Dict.singleton (formatDate date) highlights


fromList : List (Date, List String) -> Highlights
fromList list =
  map (\(date, x) -> (formatDate date, x)) list
    |> Dict.fromList


add : Date -> List String -> Highlights -> Highlights
add date highlights target =
  addInternal (formatDate date) highlights target


addAll : Highlights -> Highlights -> Highlights
addAll highlights target =
  Dict.foldr addInternal target highlights


jsonEncode : Highlights -> String
jsonEncode highlights =
  Dict.toList highlights
    |> List.map encodeTuple
    |> Encode.object
    |> Encode.encode 0


jsonDecode : Json.Decoder Highlights
jsonDecode =
  Json.dict (Json.list Json.string)


toList : Highlights -> List (Date, List String)
toList highlights =
  Dict.toList highlights
   |> List.map parseDate
   |> List.filterMap hasDate

-- PRIVATE API

addInternal : String -> List String -> Highlights -> Highlights
addInternal date highlights target =
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


parseDate : (String, x) -> (Result String Date, x)
parseDate (date, x) =
  (Date.fromString date, x)


hasDate : (Result String Date, x) -> Maybe (Date, x)
hasDate (result, x) =
  case result of
    Ok date ->
      Just (date, x)

    Err err ->
      let
        _ = Debug.log "Not a date" err
      in
        Nothing