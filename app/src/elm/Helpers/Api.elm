module Helpers.Api exposing (Action, save, fetch, receive)

import Date exposing (Date)
import Helpers.Highlights as Highlights exposing (Highlights, jsonEncode, jsonDecode)
import Helpers.LocalStorage as LocalStorage
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
          Ok Highlights.empty

    FetchFail err ->
      Err (toString err)


save : String -> Cmd Action
save h =
  Task.perform SaveFail SaveSucceed (addTodayHighlight h)


fetch : Cmd Action
fetch =
  Task.perform FetchFail FetchSucceed getHighlights


-- PRIVATE API

getHighlights : Task LocalStorage.Error (Maybe Highlights)
getHighlights =
  LocalStorage.getJson jsonDecode "highlights"

addTodayHighlight : String -> Task LocalStorage.Error Highlights
addTodayHighlight h =
  Date.now `andThen` \today -> mergeHighlights (Highlights.singleton today [h])

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
      LocalStorage.set "highlights" (jsonEncode newHighlights) `andThen` \_ -> succeed highlights

