module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Char exposing (fromCode)
import Window as W
import Task
import Keyboard exposing (KeyCode, ups, downs)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { x : Int
    , y : Int
    , vx : Int
    , vy : Int
    , history : List Msg
    , windowSize : W.Size
    }


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | WindowSize W.Size


type alias KeyBindings =
    { moveUp : Char
    , moveDown : Char
    , moveLeft : Char
    , moveRight : Char
    }


keyBindings : KeyBindings
keyBindings =
    { moveUp = 'K'
    , moveDown = 'J'
    , moveLeft = 'H'
    , moveRight = 'L'
    }


initModel : Model
initModel =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , history = []
    , windowSize = { width = 400, height = 400 }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            updateMotion model ( key, True )
                ! []

        KeyUp key ->
            updateMotion model ( key, False )
                ! []

        WindowSize { width, height } ->
            { model | windowSize = W.Size width height }
                ! []


updateMotion : Model -> ( KeyCode, Bool ) -> Model
updateMotion model ( keycode, act ) =
    let
        vel =
            if act then
                5
            else
                0

        key =
            fromCode keycode
    in
        if key == keyBindings.moveUp then
            { model | vy = vel }
        else if key == keyBindings.moveDown then
            { model | vy = -vel }
        else if key == keyBindings.moveLeft then
            { model | vx = -vel }
        else if key == keyBindings.moveRight then
            { model | vx = vel }
        else
            model


(=>) : a -> a -> ( a, a )
(=>) =
    (,)


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style <|
            [ ("width" => "500px")
            , ("height" => "500px")
            ]
        ]
        [ roundedRect ]


roundedRect : Html.Html msg
roundedRect =
    svg
        [ width "120", height "120", viewBox "0 0 120 120", fill "blue" ]
        [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ ups KeyUp
        , downs KeyDown
        , W.resizes WindowSize
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Task.perform WindowSize W.size )
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }
