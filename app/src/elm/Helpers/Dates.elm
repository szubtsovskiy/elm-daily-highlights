module Helpers.Dates exposing (Unit(..), today, between, subtract, same, min)

import Date exposing (Date, toTime, fromTime)
import Task exposing (Task, andThen)
import Time exposing (hour, minute, second, millisecond)

-- PUBLIC API

type Unit
  = Day


today : Task x Date
today =
  Date.now `andThen` \now -> datestamp now
    |> fromTime
    |> Task.succeed


between : Date -> Date -> Date -> Bool
between from to date =
  let
    fromMs = Date.toTime from
    toMs = Date.toTime to
    dateMs = Date.toTime date
  in
    fromMs <= dateMs && dateMs <= toMs


subtract : Int -> Unit -> Date -> Date
subtract n unit date =
  case unit of
    Day ->
      (toTime date) - (toFloat n) * 24 * hour
        |> fromTime


same : Date -> Date -> Bool
same a b =
  (datestamp a) - (datestamp b) == 0


min : List Date -> Maybe Date
min dates =
  case List.minimum (List.map datestamp dates) of
    Just ms ->
      Just (fromTime ms)

    Nothing ->
      Nothing

-- PRIVATE API

datestamp : Date -> Float
datestamp date =
  let
    h = Date.hour date |> toFloat
    m = Date.minute date |> toFloat
    s = Date.second date |> toFloat
    ms = Date.millisecond date |> toFloat
  in
    (toTime date) - h * hour - m * minute - s * second - ms * millisecond
