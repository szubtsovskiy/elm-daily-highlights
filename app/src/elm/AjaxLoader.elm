module AjaxLoader exposing (Styles, Model, Action, init, show, hide, view)
import Html exposing (..)
import Html.Attributes exposing (..)

-- MODEL

type alias Styles =
  { container : String
  , icon : String
  }

type alias Model =
  { visible : Bool
  , styles : Styles
  }

type Action
  = Show
  | Hide

-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    Show ->
      { model | visible = True }

    Hide ->
      { model | visible = False }


show : Model -> Model
show model =
  update Show model


hide : Model -> Model
hide model =
  update Hide model

-- VIEW

view : Model -> Html Action
view model =
  let
    styles = model.styles
    visible = model.visible
  in
    div [ class styles.container, style (containerStyles visible) ]
    [ span [ class styles.icon, style iconStyles ] [ text " " ]
    ]

containerStyles : Bool -> List (String, String)
containerStyles visible =
  [ ("display", if visible then "flex" else "none")
  ]

iconStyles : List (String, String)
iconStyles =
  [ ("display", "flex")
  , ("margin", "auto")
  ]

-- INIT

init : Bool -> Styles -> Model
init visible styles =
  Model visible styles