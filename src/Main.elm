module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import AnimationFrame
import Char exposing (toCode)
import Window exposing (Size)
import Task
import Time exposing (..)
import Keyboard exposing (KeyCode, ups, downs)
import Circle as C exposing (setColor, setSize)
import Utils exposing (..)
import Color exposing (..)


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Task.perform WindowSize Window.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    { circleModel : C.Model
    , history : List ( C.Model, Msg )
    , timeFlow : TimeFlow
    , recordedTime : Time
    , windowSize : Size
    }


initModel : Model
initModel =
    { circleModel = C.initModel
    , history = []
    , timeFlow = Normal
    , recordedTime = 3 * second
    , windowSize = { width = 400, height = 400 }
    }


type TimeFlow
    = Normal
    | Paused
    | Reversed


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


isAction : List Int -> Int -> Bool
isAction action key =
    List.any ((==) key) action



-- UPDATE


type Msg
    = KeyMsg KeyEvent
    | WindowSize Size
    | Tick Time
    | Purge Time


type KeyEvent
    = KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyEvent ->
            (model |> keyInput keyEvent)
                ! []

        WindowSize { width, height } ->
            { model | windowSize = Size width height }
                ! []

        Tick dt ->
            (model |> tick (timeFlowDelta dt model.timeFlow))
                ! []

        Purge _ ->
            (model |> historyGC)
                ! []



-- The delta based on timeflow


timeFlowDelta : Time -> TimeFlow -> Time
timeFlowDelta dt timeFlow =
    case timeFlow of
        Normal ->
            dt

        Paused ->
            0

        Reversed ->
            -dt



-- The negative difference between current time and the time of the previous model


timeDiffBack : List ( C.Model, Msg ) -> Time
timeDiffBack hist =
    let
        time a =
            (Tuple.first a).time
    in
        case hist of
            [] ->
                0

            x :: [] ->
                time x

            x :: y :: _ ->
                (time y) - (time x)



-- React to keyboard input


keyInput : KeyEvent -> Model -> Model
keyInput keyEvent model =
    let
        updateVelocity circ =
            { circ | vel = circ.vel |> motionInput keyEvent }

        toHistory circ =
            ( circ, (KeyMsg keyEvent) ) :: (model.history |> validHistory model.recordedTime)
    in
        case keyEvent of
            KeyDown key ->
                if isAction keyBindings.rewind key then
                    { model | timeFlow = Reversed }
                else
                    { model
                        | history = toHistory model.circleModel
                        , circleModel = updateVelocity model.circleModel
                        , timeFlow = Normal
                    }

            KeyUp key ->
                if isAction keyBindings.rewind key then
                    { model | timeFlow = Normal }
                else
                    { model
                        | history = toHistory model.circleModel
                        , circleModel = updateVelocity model.circleModel
                    }



-- Set next velocity based on keyboard input


motionInput : KeyEvent -> Vec2 -> Vec2
motionInput keyEvent vel =
    let
        ( speed, keycode ) =
            case keyEvent of
                KeyDown key ->
                    ( 1, key )

                KeyUp key ->
                    ( -1, key )

        addIfNotEqual x y =
            if x /= y then
                x + y
            else
                x

        vel_ x y =
            { x = addIfNotEqual vel.x x
            , y = addIfNotEqual vel.y y
            }

        is action =
            isAction action keycode
    in
        if is keyBindings.moveUp then
            vel_ 0 -speed
        else if is keyBindings.moveDown then
            vel_ 0 speed
        else if is keyBindings.moveLeft then
            vel_ -speed 0
        else if is keyBindings.moveRight then
            vel_ speed 0
        else
            vel



-- Update meta model in regards to time


tick : Time -> Model -> Model
tick dt model =
    let
        delta =
            timeFlowDelta dt model.timeFlow

        withAddedTime dt0 mdl =
            let
                timeAdd dt0 mdl =
                    { mdl | time = mdl.time + dt0 }
            in
                { mdl | circleModel = mdl.circleModel |> timeAdd dt0 }

        withTimeDirection mdl =
            case mdl.timeFlow of
                Normal ->
                    let
                        circ =
                            mdl.circleModel
                    in
                        { mdl
                            | circleModel = { circ | time = circ.time + delta }
                            , history = ( mdl.circleModel, (Tick delta) ) :: model.history
                        }

                Paused ->
                    mdl

                Reversed ->
                    mdl |> stepBack

        bounds =
            model.windowSize |> fromSize

        withNewPos mdl =
            { mdl
                | circleModel =
                    mdl.circleModel
                        |> C.updatePos delta bounds
            }
    in
        model |> withTimeDirection |> withNewPos



-- Replace current model with the previous on in history and pop history


stepBack : Model -> Model
stepBack model =
    let
        previousModel =
            List.head model.history
                |> Maybe.withDefault ( model.circleModel, Tick 0 )
                |> Tuple.first
    in
        -- I'm not sure how to fix velocity getting stuck other than to reset it here
        { model
            | circleModel = { previousModel | vel = { x = 0, y = 0 } }
            , history = Maybe.withDefault [] (List.tail model.history)
        }



-- When velocity is 0, pause time


pauseIfNotMoving : Model -> Model
pauseIfNotMoving model =
    { model
        | timeFlow =
            if model.circleModel.vel /= Vec2 0 0 then
                Normal
            else
                Paused
    }



-- Clean up the history from entries that are too old


historyGC : Model -> Model
historyGC model =
    { model | history = model.history |> validHistory model.recordedTime }


validHistory : Time -> List ( C.Model, Msg ) -> List ( C.Model, Msg )
validHistory maxTime history =
    let
        currentTime =
            List.head history |> Maybe.map (Tuple.first >> \x -> x.time) |> Maybe.withDefault 0

        notOld : C.Model -> Bool
        notOld x =
            currentTime - x.time < maxTime
    in
        List.filter (notOld << Tuple.first) history



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style <|
            [ "width" => ((model.windowSize.width |> toString) ++ "px")
            , "height" => ((model.windowSize.height |> toString) ++ "px")
            , "display" => "block"
            , "background-color" => "#2d2d2d"
            ]
        ]
        (nodes model)


nodes : Model -> List (Html msg)
nodes model =
    List.concat
        [ circleTrail model
        , [ C.view (model.circleModel |> setColor green) ]
        ]


circleTrail : Model -> List (Html msg)
circleTrail model =
    let
        toCircle ( model, _ ) =
            C.view (model |> setColor charcoal |> setSize (model.size // 2))
    in
        model.history |> validHistory model.recordedTime |> List.map toCircle



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tickIfEventful =
            if model.timeFlow /= Paused then
                [ AnimationFrame.diffs Tick ]
            else
                []
    in
        Sub.batch <|
            [ ups (\x -> KeyMsg (KeyUp x))
            , downs (\x -> KeyMsg (KeyDown x))
            , Window.resizes WindowSize
              -- , every (second / 5) Purge
            ]
                ++ tickIfEventful
