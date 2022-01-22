module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Lazy exposing (lazy)
import Svg exposing (Svg, svg, line)
import Svg.Attributes exposing (..)
import Dict
import Program exposing (Inst(..), Program, Proc, ProgramError)

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

changeAngle : Float -> Float
changeAngle a =
  if a < 0 then
    a + 360.0
  else if a >= 360 then
    a - 360.0
  else
    a
draw : Program -> Proc -> Cursor -> List (Svg msg)
draw prog proc cursor =
  case proc of
    [] ->
        []

    inst :: subProc ->
      case inst of
        Forward length ->
          let
              newCursor =
                  { cursor
                      | x = cursor.x + length * cos (degrees cursor.angle)
                      , y = cursor.y + length * sin (degrees cursor.angle)
                  }
          in
          line
            [ x1 (String.fromFloat cursor.x)
            , y1 (String.fromFloat cursor.y)
            , x2 (String.fromFloat newCursor.x)
            , y2 (String.fromFloat newCursor.y)
            , Svg.Attributes.style "stroke:blue;stroke-width:1;stroke-linecap:round"
            ]
            []

            :: draw prog subProc newCursor

        Left n ->
            draw prog subProc { cursor | angle = changeAngle (cursor.angle - n) }

        Right n ->
            draw prog subProc { cursor | angle = changeAngle (cursor.angle + n) }

        Repeat n toRepeat ->
            if n <= 1 then
                draw prog (toRepeat ++ subProc) cursor

            else
                draw prog (toRepeat ++  Repeat (n - 1) toRepeat  :: subProc) cursor
        
        Call procName ->
                    draw prog (Maybe.withDefault [] (Dict.get procName prog) ++ subProc) cursor


-- VIEW

view : Model -> Html Msg
view model =
  if List.isEmpty model.errors then
    div [ Html.Attributes.class "page" ]
      [ input [ placeholder "example: [Repeat 360 [Forward 1, Left 1]]", value model.content, onInput Change ] []
      , button [ onClick Draw ] [ Html.text "Draw"]
      , lazy drawingSpace model.prog
      ]
  else
      div [ Html.Attributes.class "page" ]
      [ input [ placeholder "example: [Repeat 360 [Forward 1, Left 1]]", value model.content, onInput Change ] []
      , button [ onClick Draw ] [ Html.text "Draw"]
      , div [ Html.Attributes.class "error" ]
          [ Html.text "Wrong syntax!" ]
      ]

drawingSpace : Maybe Program -> Html Msg
drawingSpace maybeprog =
   svg
      [ Svg.Attributes.width "500"
      , Svg.Attributes.height "500"
      , viewBox "0 0 500 500"
      ]
      (case maybeprog of
          Just prog ->
              draw prog (Maybe.withDefault [] (Dict.get "main" prog)) { x = 250, y = 250, angle = 0 }
          Nothing ->
              []
      )