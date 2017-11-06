module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import AnimationFrame
import Char exposing (toCode)
import Window as W exposing (Size)
import Task
import Time exposing (..)
import Keyboard exposing (KeyCode, ups, downs)
import Svg
import Svg.Attributes as SvgA


-- MODEL


type alias Model =
    { pos : Position
    , size : Int
    , vel : Vec2
    , history : List Record
    , time : Time
    , recordLength : Time
    , windowSize : Size
    }


type Msg
    = KeyMsg KeyEvent
    | WindowSize Size
    | Tick Time
    | Purge Time


type KeyEvent
    = KeyDown KeyCode
    | KeyUp KeyCode


type alias Record =
    { pos : Position
    , vel : Vec2
    , msg : Msg
    , time : Time
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Vec2 =
    { x : Int
    , y : Int
    }


type alias KeyBindings =
    { moveUp : List Int
    , moveDown : List Int
    , moveLeft : List Int
    , moveRight : List Int
    , rewind : List Int
    }


keyBindings : KeyBindings
keyBindings =
    { moveUp =
        [ toCode 'K'
          -- Arrow up
        , 38
        ]
    , moveDown =
        [ toCode 'J'
          -- Arrow down
        , 40
        ]
    , moveLeft =
        [ toCode 'H'
          -- Arrow left
        , 37
        ]
    , moveRight =
        [ toCode 'L'
          -- Arrow right
        , 39
        ]
    , rewind =
        [ -- Spacebar
          32
        ]
    }


initModel : Model
initModel =
    { pos = Position 0 0
    , size = 50
    , vel = { x = 0, y = 0 }
    , history = []
    , time = 0
    , recordLength = 5
    , windowSize = { width = 400, height = 400 }
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyEvent ->
            let
                ( rw, isActivate ) =
                    isRewind keyEvent
            in
                if rw then
                    if isActivate then
                        rewind model
                            ! []
                    else
                        model ! []
                else
                    (model |> recHist msg |> updateMotion keyEvent)
                        ! []

        WindowSize { width, height } ->
            { model | windowSize = Size width height }
                ! []

        Tick dt ->
            ({ model | time = model.time + dt } |> recHist msg |> updatePos dt)
                ! []

        Purge _ ->
            (model |> historyGC)
                ! []


historyGC : Model -> Model
historyGC model =
    let
        notOld x =
            model.time - x.time < model.recordLength * second
    in
        { model | history = List.filter notOld model.history }


rewind : Model -> Model
rewind model =
    let
        ( hist, hists ) =
            case model.history of
                [] ->
                    ( toRecord (Tick 0) model, [] )

                x :: xs ->
                    ( x, xs )
    in
        { model
            | pos = hist.pos
            , vel = hist.vel
            , time = hist.time
            , history = hists
        }


isAction : List Int -> Int -> Bool
isAction action key =
    List.any ((==) key) action


isRewind : KeyEvent -> ( Bool, Bool )
isRewind keyEvent =
    case keyEvent of
        KeyDown key ->
            ( isAction keyBindings.rewind key, True )

        KeyUp key ->
            ( isAction keyBindings.rewind key, False )


recHist : Msg -> Model -> Model
recHist msg model =
    { model | history = (toRecord msg model) :: model.history }


toRecord : Msg -> Model -> Record
toRecord msg model =
    { pos = model.pos
    , vel = model.vel
    , msg = msg
    , time = model.time
    }


updatePos : Float -> Model -> Model
updatePos dt model =
    let
        perSec x =
            toFloat x * dt |> round

        dvx =
            perSec model.vel.x

        dvy =
            perSec model.vel.y

        collisionRadius =
            (model.size // 2) + 5

        padded begin end x =
            clamp (begin + collisionRadius) (end - collisionRadius) x

        bound range x =
            padded 0 range x

        bounded x y =
            Position
                (bound model.windowSize.width x)
                (bound model.windowSize.height y)
    in
        { model
            | pos =
                bounded (model.pos.x + dvx) (model.pos.y + dvy)
        }


updateMotion : KeyEvent -> Model -> Model
updateMotion keyEvent model =
    let
        ( speed, keycode ) =
            case keyEvent of
                KeyDown key ->
                    ( 1, key )

                KeyUp key ->
                    ( -1, key )

        isAction x =
            List.any ((==) keycode) x

        addVel x y =
            { model
                | vel =
                    { x =
                        if model.vel.x /= x then
                            model.vel.x + x
                        else
                            model.vel.x
                    , y =
                        if model.vel.y /= y then
                            model.vel.y + y
                        else
                            model.vel.y
                    }
            }
    in
        if isAction keyBindings.moveUp then
            addVel 0 -speed
        else if isAction keyBindings.moveDown then
            addVel 0 speed
        else if isAction keyBindings.moveLeft then
            addVel -speed 0
        else if isAction keyBindings.moveRight then
            addVel speed 0
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
            Position model.pos.x model.pos.y
    in
        Html.div
            [ Html.Attributes.style <|
                [ "width" => ((model.windowSize.width |> toString) ++ "px")
                , "height" => ((model.windowSize.height |> toString) ++ "px")
                , "display" => "block"
                , "background-color" => "#2d2d2d"
                ]
            ]
            [ circle (model.size // 2) pos ]


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
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tickIfMoving =
            if model.vel /= Vec2 0 0 then
                [ AnimationFrame.diffs Tick ]
            else
                []
    in
        Sub.batch <|
            [ ups (\x -> KeyMsg (KeyUp x))
            , downs (\x -> KeyMsg (KeyDown x))
            , W.resizes WindowSize
            , every (second * model.recordLength) Purge
            ]
                ++ tickIfMoving



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Task.perform WindowSize W.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
