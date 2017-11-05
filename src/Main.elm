module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Char exposing (fromCode)
import Window as W exposing (Size)
import Task
import Keyboard exposing (KeyCode, ups, downs)
import Svg
import Svg.Attributes as SvgA


-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , vx : Int
    , vy : Int
    , history : List Msg
    , windowSize : Size
    }


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | WindowSize Size


type alias Position =
    { x : Int
    , y : Int
    }


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



-- UPDATE


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
            { model | windowSize = Size width height }
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



-- VIEW


(=>) : a -> a -> ( a, a )
(=>) =
    (,)


px : Int -> String
px x =
    toString x ++ "px"


view : Model -> Html Msg
view model =
    let
        pos =
            Position
                (model.windowSize.width // 2)
                (model.windowSize.height // 2)
    in
        Html.div
            [ Html.Attributes.style <|
                [ "width" => (model.windowSize.width |> toString) ++ "px"
                , "height" => (model.windowSize.height |> toString) ++ "px"
                , "display" => "block"
                , "background-color" => "#2d2d2d"
                  -- , "align-items" => "center"
                  -- , "justify-content" => "center"
                  -- , "display" => "flex"
                ]
            ]
            [ circle 30 pos ]


circle : Int -> Position -> Html msg
circle r pos =
    Html.div
        [ Html.Attributes.style <|
            [ "width" => px (r * 2)
            , "height" => px (r * 2)
            , "position" => "absolute"
            , "left" => px (pos.x - r)
            , "top" => px (pos.y - r)
            ]
        ]
        [ Svg.svg
            [ SvgA.viewBox "0 0 100 100"
            ]
            [ Svg.circle
                [ SvgA.cx "50"
                , SvgA.cy "50"
                , SvgA.r "50"
                , SvgA.fill "lightblue"
                ]
                []
              --, Svg.rect
              --   [ SvgA.cx (toString pos.x)
              --   , SvgA.cy (toString pos.y)
              --   , SvgA.width "100%"
              --   , SvgA.height "100%"
              --   , SvgA.rx "100"
              --   , SvgA.ry "100"
              --   , SvgA.fill "lightgreen"
              --   ]
              --   []
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ ups KeyUp
        , downs KeyDown
        , W.resizes WindowSize
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Task.perform WindowSize W.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
