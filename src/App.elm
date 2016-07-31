module App exposing (main)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import LocalStorage
import Task

main = Html.program {init = init, view = view, update = update, subscriptions = (\x -> Sub.none)}

type alias Model = String

type Action = Error LocalStorage.Error | Highlight String | Save | NoOp

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
      (value, Cmd.none)
    Save ->
      ("", Task.perform Error (always NoOp) (LocalStorage.set "highlights" model))

view : Model -> Html Action
view model =
  div []
  [ input [ type' "text", placeholder "Highlight", onInput Highlight, value model ] []
  , button [ type' "submit", onClick Save ] [ text "save" ]
  ]

init : (Model, Cmd Action)
init =
  ("", Cmd.none)