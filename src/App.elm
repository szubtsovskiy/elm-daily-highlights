module App exposing (main)

import Html exposing (..)
import Html.App as Html
import LocalStorage

main = Html.beginnerProgram {model = model, view = view, update = update}

type alias Model = String

model : Model
model =
  "No"

type Action = NoOp

view : Model -> Html Action
view model =
  div []
  [ text "Hi there!"
  ]

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model