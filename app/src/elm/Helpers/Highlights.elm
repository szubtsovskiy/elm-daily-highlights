module Helpers.Highlights exposing (Highlights, merge)

import Dict exposing (Dict, get, insert, foldr)

type alias Highlights = Dict String (List String)

merge : Highlights -> Highlights -> Highlights
merge h1 h2 =
  foldr addHighlights h1 h2

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

