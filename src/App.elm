module App exposing (main)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import LocalStorage
import Task
import Json.Encode

main = Html.program {init = init, view = view, update = update, subscriptions = (\x -> Sub.none)}

type alias Model =
  { items : List String
  , current : String
  }

type Action = Error LocalStorage.Error | Highlight String | Save | NoOp

-- TODO next: take initial list from the local storage
-- TODO next: print list of stored values (replace (always NoOp) with (always Refresh) to display changed state)
-- TODO next: add Bootstrap styles (hint: look how examples/Main.elm uses html page in fredcy/localstorage)
-- TODO next: bind highlights to current date and store object {[date]: [highlights]}
-- TODO next: display highlights for 3 days including today
-- TODO next: add infinite scroll backwards
-- TODO next: refactor out LocalStorage implementation into a module to be able to use another storing facility

-- UPDATE

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      (model, Cmd.none)
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
  div []
  [ input [ type' "text", placeholder "Highlight", onInput Highlight, value model.current ] []
  , button [ type' "submit", onClick Save ] [ text "save" ]
  ]

-- INIT

init : (Model, Cmd Action)
init =
  ({items = [], current = ""}, Cmd.none)

-- INTERNAL API

jsonEncode : List String -> String
jsonEncode items =
  List.map Json.Encode.string items
    |> Json.Encode.list
    |> Json.Encode.encode 0