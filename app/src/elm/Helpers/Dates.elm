module Helpers.Dates exposing (today, between)

import Date exposing (Date, toTime, fromTime)
import Task exposing (Task, andThen)
import Time exposing (hour, minute, second, millisecond)

-- PUBLIC API

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
