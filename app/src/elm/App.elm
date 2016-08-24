module App exposing (main)

import Date exposing (Date, year, month, day, hour, minute, second, millisecond, toTime)
import Helpers.Api as Api
import Helpers.Dates as Dates exposing (Unit(..))
import Helpers.Highlights as Highlights exposing (Highlights)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json
import List exposing (map, foldr)
import Result exposing (Result(Ok, Err))
import String exposing (join)
import Task

-- MAIN

main : Program Styles
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = (\x -> Sub.none)
    }

-- MODEL

type alias Styles =
  { container : String
  , input : String
  , loaderIconContainer : String
  , loaderIcon : String
  }

type alias Model =
  { highlights : Highlights
  , current : String
  , today : Maybe Date
  , styles : Styles
  }


-- TODO next: fix bug with always appending highlights to the list
-- TODO next: add infinite scroll backwards

-- UPDATE

type Action
  = NoOp
  | Init Date
  | ReceiveHighlights Api.Action
  | ReceiveToday Date
  | SetCurrent String
  | KeyDown Int

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      (model, Cmd.none)

    Init date ->
      (model, Cmd.map ReceiveHighlights (Api.fetch (Dates.subtract 3 Day date) date))

    ReceiveHighlights action ->
      case Api.receive action of
        Ok highlights ->
          { model | highlights = Highlights.merge model.highlights highlights
          } ! [Task.perform (always NoOp) ReceiveToday Dates.today]

        Err err ->
          let
            _ = Debug.log "Error: " err
          in
            (model, Cmd.none)

    ReceiveToday date ->
      ({model | today = Just date}, Cmd.none)

    SetCurrent value ->
      ({model | current = value}, Cmd.none)

    KeyDown code ->
      case code of
        13 ->
          case model.today of
            Just date ->
              ({model | current = ""}, Cmd.map ReceiveHighlights (Api.save model.current date))

            Nothing ->
              (model, Cmd.none)

        27 ->
          ({model | current = ""}, Cmd.none)

        _ ->
          (model, Cmd.none)


getToday : (Date -> Action) -> Cmd Action
getToday action =
  Task.perform (always NoOp) action Date.now

-- VIEW

view : Model -> Html Action
view model =
  let
    highlights = Highlights.toList model.highlights |> List.sortBy (\(date, _) -> toTime date)
    sections = map (section model.today) highlights
    styles = model.styles
  in
    div []
    [ div [ class styles.container ] sections
    , input [ type' "text", class styles.input, placeholder "Highlight", onInput SetCurrent, onKeyDown KeyDown, value model.current ] []
    ]


section : Maybe Date -> (Date, List String) -> Html Action
section maybeToday (date, highlights) =
  fieldset [] ((title maybeToday date) :: (map highlight highlights))


title : Maybe Date -> Date -> Html Action
title maybeToday date =
  let
    displayDate = case maybeToday of
      Just today ->
        if isToday date today then
          "Today"
        else if isYesterday date today then
          "Yesterday"
        else if isTwoDaysAgo date today then
          "Two days ago"
        else if isThreeDaysAgo date today then
          "Three days ago"
        else
          formatDate date

      Nothing ->
        formatDate date

  in
    legend [] [ text displayDate ]


highlight : String -> Html Action
highlight content =
  p [] [ text content ]


isToday : Date -> Date -> Bool
isToday date today =
   Dates.same today date


isYesterday : Date -> Date -> Bool
isYesterday date today =
  Dates.subtract 1 Day today
    |> Dates.same date

isTwoDaysAgo : Date -> Date -> Bool
isTwoDaysAgo date today =
  Dates.subtract 2 Day today
    |> Dates.same date


isThreeDaysAgo : Date -> Date -> Bool
isThreeDaysAgo date today =
  Dates.subtract 3 Day today
    |> Dates.same date


formatDate : Date -> String
formatDate date =
  let
    y = toString (year date)
    m = toString (month date)
    d = toString (day date)
  in
    [y, m, d] |> join "-"

--onScroll : (Int -> action) -> Attribute action
--onScroll tagger =
--  on "scroll" (JsonD.map tagger scrollTop)

onKeyDown : (Int -> action) -> Attribute action
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

--scrollTop : JsonD.Decoder Int
--scrollTop =
--  JsonD.at [ "target", "scrollTop" ] JsonD.int

-- INIT

init : Styles -> (Model, Cmd Action)
init styles =
  { highlights = Highlights.empty
  , current = ""
  , today = Nothing
  , styles = styles
  } ! [Task.perform (always NoOp) Init Dates.today]

