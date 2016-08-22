module Helpers.Api exposing (Highlights, Action, save, fetch, receive)

import Dict exposing (Dict)
import Helpers.LocalStorage as LocalStorage
import Json.Encode as Encode
import Json.Decode as Json
import Result exposing (Result(Ok, Err))
import Task exposing (Task, andThen)

-- MODEL

type alias Highlights = Dict String (List String)

-- PUBLIC API

type Action
  = SaveSucceed Highlights
  | SaveFail LocalStorage.Error
  | FetchSucceed (Maybe Highlights)
  | FetchFail LocalStorage.Error


receive : Action -> Result String Highlights
receive action =
  case action of
    SaveSucceed highlights ->
      Ok highlights

    SaveFail err ->
      Err (toString err)

    FetchSucceed maybeHighlights ->
      case maybeHighlights of
        Just highlights ->
          Ok highlights

        Nothing ->
          Ok Dict.empty

    FetchFail err ->
      Err (toString err)


save : String -> Cmd Action
save h =
  let
    highlights = Dict.singleton "ph" [h]
  in
    Task.perform SaveFail (always (SaveSucceed highlights)) (mergeHighlights "ph" h)


fetch : Cmd Action
fetch =
  Task.perform FetchFail FetchSucceed getHighlights


-- PRIVATE API

getHighlights : Task LocalStorage.Error (Maybe Highlights)
getHighlights =
  LocalStorage.getJson decodeHighlights "highlights"

mergeHighlights : String -> String -> Task LocalStorage.Error ()
mergeHighlights date h =
  getHighlights `andThen` \maybeHighlights ->
    let
      highlights =
        case maybeHighlights of
          Just highlights ->
            addHighlight date h highlights

          Nothing ->
            Dict.singleton date [h]
    in
      LocalStorage.set "highlights" (encodeHighlights highlights)


encodeHighlights : Highlights -> String
encodeHighlights highlights =
  Dict.toList highlights
    |> List.map encodeTuple
    |> Encode.object
    |> Encode.encode 0

encodeTuple : (String, List String) -> (String, Encode.Value)
encodeTuple (k, v) =
  (k, Encode.list (List.map Encode.string v))


decodeHighlights : Json.Decoder Highlights
decodeHighlights =
  Json.dict (Json.list Json.string)


addHighlight : String -> String -> Highlights -> Highlights
addHighlight date h highlights =
  case Dict.get date highlights of
    Just list ->
      Dict.insert date (list ++ [h]) highlights

    Nothing ->
      Dict.insert date [h] highlights


