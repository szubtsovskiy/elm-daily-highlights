module Helpers.Highlights exposing (Highlights, merge, singleton)

import Date exposing (Date)
import Dict exposing (Dict, get, insert, foldr)
import String exposing (join)

type alias Highlights = Dict String (List String)

-- PUBLIC API

singleton : Date -> List String -> Highlights
singleton date highlights =
  Dict.singleton (formatDate date) highlights

merge : Highlights -> Highlights -> Highlights
merge h1 h2 =
  foldr addHighlights h1 h2

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

formatDate : Date -> String
formatDate date =
  let
    (year, month, day) = (Date.year date, Date.month date, Date.day date)
  in
    [(toString year), (toString month), (toString day)] |> join "-"