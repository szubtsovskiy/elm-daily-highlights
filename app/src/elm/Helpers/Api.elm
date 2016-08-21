module Helpers.Api exposing (Highlight, Action, save, fetch, receive)
import Helpers.LocalStorage as LocalStorage
import Json.Encode as JsonE
import Json.Decode as JsonD
import Task
import Result exposing (Result(Ok, Err))

-- MODEL

type alias Highlight =
  { text : String
  }

-- PUBLIC API

type Action
  = SaveSucceed
  | SaveFail LocalStorage.Error
  | FetchSucceed
  | FetchFail LocalStorage.Error
--  | Init (Maybe (List String))

receive : Action -> Result String (List Highlight)
receive action =
  case action of
    SaveSucceed ->
      Ok []

    SaveFail err ->
      Err (toString err)

    FetchSucceed ->
      Ok []

    FetchFail err ->
      Err (toString err)


save : Highlight -> Cmd Action
save h =
  Cmd.none

fetch : Cmd Action
fetch =
  Cmd.none

-- save: Task.perform Error (always NoOp) (LocalStorage.set "highlights" (jsonEncode items))
-- fetch: Task.perform Error Init (LocalStorage.getJson (JsonD.list JsonD.string) "highlights"

--    Init maybeList ->
--      let
--        items =
--          case maybeList of
--            Just list ->
--              list
--
--            Nothing ->
--              []
--      in
--        ({model | items = items}, Cmd.none)
--
--    Error err ->
--      let
--        _ =
--          Debug.log "Error: " (toString err)
--      in
--        (model, Cmd.none)

-- PRIVATE API

jsonEncode : List String -> String
jsonEncode items =
  List.map JsonE.string items
    |> JsonE.list
    |> JsonE.encode 0
