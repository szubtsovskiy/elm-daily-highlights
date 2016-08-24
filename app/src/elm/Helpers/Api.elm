module Helpers.Api exposing (Action, save, fetch, receive)

import Date exposing (Date)
import Helpers.Highlights as Highlights exposing (Highlights, jsonEncode, jsonDecode)
import Helpers.LocalStorage as LocalStorage
import List exposing (filter)
import Result exposing (Result(Ok, Err))
import Task exposing (Task, andThen, succeed)


-- PUBLIC API

type Action
  = SaveSucceed Highlights
  | SaveFail LocalStorage.Error
  | FetchSucceed Highlights
  | FetchFail LocalStorage.Error


receive : Action -> Result String Highlights
receive action =
  case action of
    SaveSucceed highlights ->
      Ok highlights

    SaveFail err ->
      Err (toString err)

    FetchSucceed highlights ->
      Ok highlights

    FetchFail err ->
      Err (toString err)


save : String -> Date -> Cmd Action
save h date =
  Task.perform SaveFail SaveSucceed (mergeHighlights (Highlights.singleton date [h]))


fetch : Date -> Date -> Cmd Action
fetch from to =
  Task.perform FetchFail FetchSucceed (getHighlights `andThen` \maybeHighlights -> extractHighlights from to maybeHighlights)


-- PRIVATE API

getHighlights : Task LocalStorage.Error (Maybe Highlights)
getHighlights =
  LocalStorage.getJson jsonDecode "highlights"


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


extractHighlights : Date -> Date -> Maybe Highlights -> Task x Highlights
extractHighlights from to maybeHighlights =
  case maybeHighlights of
    Just highlights ->
      let
        filteredHighlights =
          Highlights.toList highlights
            |> filter (belongsToPeriod from to)
            |> Highlights.fromList
      in
        succeed filteredHighlights

    Nothing ->
      succeed Highlights.empty


belongsToPeriod : Date -> Date -> (Date, List String) -> Bool
belongsToPeriod from to (date, x) =
  let
    fromMs = Date.toTime from
    toMs = Date.toTime to
    dateMs = Date.toTime date
  in
    fromMs <= dateMs && dateMs <= toMs