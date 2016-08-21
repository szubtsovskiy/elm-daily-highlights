module App exposing (main)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)
import LocalStorage
import Task
import Json.Encode as JsonE
import Json.Decode as JsonD
import List

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
  , loaderIconContainer : String
  , loaderIcon : String
  }

type alias Model =
  { items : List String
  , current : String
  , styles : Styles
  }

-- TODO next: bind highlights to current date and store object {[date]: [highlights]}
-- TODO next: display highlights for 3 days including today
-- TODO next: add infinite scroll backwards
-- TODO next: refactor out LocalStorage implementation into a module to be able to use another storing facility (maybe two modules for saving and reading data to be able to import Json.Decode/Encode as Json)

-- UPDATE

type Action = Error LocalStorage.Error | Init (Maybe (List String)) | Highlight String | Save | NoOp

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      (model, Cmd.none)

    Init maybeList ->
      let
        items =
          case maybeList of
            Just list ->
              list

            Nothing ->
              []
      in
        ({model | items = items}, Cmd.none)

    Error err ->
      let
        _ =
          Debug.log "Error: " (toString err)
      in
        (model, Cmd.none)

    Highlight value ->
      ({model | current = value}, Cmd.none)

    Save ->
      let
        new = {model | items = model.items ++ [model.current], current = ""}
        items = new.items
      in
        (new, Task.perform Error (always NoOp) (LocalStorage.set "highlights" (jsonEncode items)))

-- VIEW

view : Model -> Html Action
view model =
  let
    paras = List.map para model.items
    styles = model.styles
  in
    div []
    [ div [ class styles.container ] paras
    , input [ type' "text", placeholder "Highlight", onInput Highlight, value model.current ] []
    ]

para : String -> Html Action
para content =
  p [] [text content]

--onScroll : (Int -> action) -> Attribute action
--onScroll tagger =
--  on "scroll" (JsonD.map tagger scrollTop)

onKeyDown : (Int -> action) -> Attribute action
onKeyDown tagger =
  on "keydown" (JsonD.map tagger keyCode)

--scrollTop : JsonD.Decoder Int
--scrollTop =
--  JsonD.at [ "target", "scrollTop" ] JsonD.int

-- INIT

init : Styles -> (Model, Cmd Action)
init styles =
  ({items = [], current = "", styles = styles}, Task.perform Error Init (LocalStorage.getJson (JsonD.list JsonD.string) "highlights"))

-- INTERNAL API

jsonEncode : List String -> String
jsonEncode items =
  List.map JsonE.string items
    |> JsonE.list
    |> JsonE.encode 0