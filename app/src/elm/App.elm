module App exposing (main)

import Date exposing (Date)
import Helpers.Api as Api
import Helpers.Highlights as Highlights exposing (Highlights)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json
import List exposing (map, foldr)
import Result exposing (Result(Ok, Err))
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


-- TODO next: sort highlights by date
-- TODO next: display highlights for 3 days including today
-- TODO next: add function generating sample data for today.. -1 week
-- TODO next: add infinite scroll backwards

-- UPDATE

type Action
  = NoOp
  | ReceiveHighlights Api.Action
  | ReceiveToday Date
  | SetCurrent String
  | KeyDown Int

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      (model, Cmd.none)

    ReceiveHighlights action ->
      case Api.receive action of
        Ok highlights ->
          ({model | highlights = Highlights.merge model.highlights highlights}, getToday)

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
          ({model | current = ""}, Cmd.map ReceiveHighlights (Api.save model.current))
        27 ->
          ({model | current = ""}, Cmd.none)

        _ ->
          (model, Cmd.none)


getToday : Cmd Action
getToday =
  Task.perform (always NoOp) ReceiveToday Date.now

-- VIEW

view : Model -> Html Action
view model =
  let
    sections = map (section model.today) (Highlights.toList model.highlights)
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
        formatDate date

      Nothing ->
        formatDate date

  in
    legend [] [ text displayDate ]


highlight : String -> Html Action
highlight content =
  p [] [ text content ]

formatDate : Date -> String
formatDate date =
  toString date

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
  ({highlights = Highlights.empty, current = "", today = Nothing, styles = styles}, Cmd.map ReceiveHighlights Api.fetch)
