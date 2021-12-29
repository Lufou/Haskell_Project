module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }

-- UPDATE

type Msg = Change String | Draw


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    
    Draw ->
      { model | content = "" }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "example: [Repeat 360 [Forward 1, Left 1]]", value model.content, onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    , button [ onClick Draw ] [ text "Draw"]
    ]