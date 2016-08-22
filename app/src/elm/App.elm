module App exposing (main)

import Dict exposing (Dict, values)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)
import Helpers.Api as Api
import Helpers.LocalStorage as LocalStorage
import Json.Decode as Json
import List exposing (map, foldr)
import Result exposing (Result(Ok, Err))

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
  { items : Dict String (List String)
  , current : String
  , styles : Styles
  }

-- TODO next: bind highlights to current date and store object {[date]: [highlights]}
-- TODO next: sort highlights by date
-- TODO next: display highlights for 3 days including today
-- TODO next: add infinite scroll backwards

-- UPDATE

type Action
  = NoOp
  | ReceiveHighlights Api.Action
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
          ({model | items = Dict.union highlights model.items}, Cmd.none)

        Err err ->
          let
            _ = Debug.log "Error: " err
          in
            (model, Cmd.none)

    SetCurrent value ->
      ({model | current = value}, Cmd.none)

    KeyDown code ->
      case code of
        13 ->
          let
            new = {model | current = ""}
            items = new.items
          in
            (new, Cmd.map ReceiveHighlights (Api.save model.current))
        27 ->
          ({model | current = ""}, Cmd.none)

        _ ->
          (model, Cmd.none)

-- VIEW

view : Model -> Html Action
view model =
  let
    paras = map para (foldr (++) [] (values model.items))
    styles = model.styles
  in
    div []
    [ div [ class styles.container ] paras
    , input [ type' "text", class styles.input, placeholder "Highlight", onInput SetCurrent, onKeyDown KeyDown, value model.current ] []
    ]


para : String -> Html Action
para content =
  p [] [text content]

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
  ({items = Dict.empty, current = "", styles = styles}, Cmd.map ReceiveHighlights Api.fetch)
