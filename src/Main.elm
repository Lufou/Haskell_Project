module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Program exposing (Inst(..), Program, ProgramError, parseProgram)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { content : String, prog : Maybe Program, errors : List ProgramError }


init : Model
init =
  { content = "", prog = Nothing, errors = [] }

-- UPDATE

type Msg = Change String | Draw
type alias Cursor = 
  { x : Float, y : Float, angle : Float }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    
    Draw ->
      let
          ( prog, errors ) =
            Program.parseProgram model.content
      in
          (
              { model
                | content = model.content
                , prog =
                  if List.isEmpty errors then
                    prog
                  else
                    Nothing
                , errors = errors
              }
          )
      --draw model.content { x = 250, y = 250, angle = 0 }

-- VIEW

view : Model -> Html Msg
view model =
  if List.isEmpty model.errors then
    div [ Html.Attributes.class "page" ]
      [ input [ placeholder "example: [Repeat 360 [Forward 1, Left 1]]", value model.content, onInput Change ] []
      , button [ onClick Draw ] [ Html.text "Draw"]
      , svg
        [ Svg.Attributes.width "500"
        , Svg.Attributes.height "500"
        , viewBox "0 0 500 500"
        ]
        []
      ]
  else
      div [ Html.Attributes.class "page" ]
      [ input [ placeholder "example: [Repeat 360 [Forward 1, Left 1]]", value model.content, onInput Change ] []
      , button [ onClick Draw ] [ Html.text "Draw"]
      , div [ Html.Attributes.class "error" ]
          [ Html.text "Wrong syntax!" ]
      ]