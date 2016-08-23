module Helpers.Api exposing (Action, save, fetch, receive)

import Dict
import Helpers.Highlights as Highlights exposing (Highlights)
import Helpers.LocalStorage as LocalStorage
import Json.Encode as Encode
import Json.Decode as Json
import Result exposing (Result(Ok, Err))
import Task exposing (Task, andThen, succeed)

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
    Task.perform SaveFail SaveSucceed (mergeHighlights highlights)


fetch : Cmd Action
fetch =
  Task.perform FetchFail FetchSucceed getHighlights


-- PRIVATE API

getHighlights : Task LocalStorage.Error (Maybe Highlights)
getHighlights =
  LocalStorage.getJson decodeHighlights "highlights"

mergeHighlights : Highlights -> Task LocalStorage.Error Highlights
mergeHighlights highlights =
  getHighlights `andThen` \maybeHighlights ->
    let
      newHighlights =
        case maybeHighlights of
          Just storedHighlights ->
            Highlights.merge storedHighlights highlights

          Nothing ->
            highlights
    in
      LocalStorage.set "highlights" (encodeHighlights newHighlights) `andThen` \_ -> succeed highlights


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

