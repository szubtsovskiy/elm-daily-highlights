module Helpers.Dates exposing (today)

import Date exposing (Date, toTime, fromTime)
import Task exposing (Task, andThen)
import Time exposing (hour, minute, second, millisecond)

-- PUBLIC API

today : Task x Date
today =
  Date.now `andThen` \now -> datestamp now
    |> fromTime
    |> Task.succeed


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
