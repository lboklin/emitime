module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import AnimationFrame
import Char exposing (toCode)
import Window exposing (Size)
import Task
import Time exposing (..)
import Keyboard exposing (KeyCode, ups, downs)
import Circle exposing (setColor, setSize)
import Utils exposing (..)
import Color exposing (..)


-- MODEL


type alias Model =
    { circleModel : Circle.Model
    , history : List ( Circle.Model, Msg )
    , timeFlow : TimeFlow
    , recordedTime : Time
    , windowSize : Size
    }


initMetaModel : Model
initMetaModel =
    { circleModel = Circle.initModel
    , history = []
    , timeFlow = Normal
    , recordedTime = 3 * second
    , windowSize = { width = 400, height = 400 }
    }


type Msg
    = KeyMsg KeyEvent
    | WindowSize Size
    | Tick Time
    | Purge Time


type KeyEvent
    = KeyDown KeyCode
    | KeyUp KeyCode


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        processedMetaModel =
            case msg of
                KeyMsg keyEvent ->
                    model |> keyInput keyEvent

                WindowSize { width, height } ->
                    { model | windowSize = Size width height }

                Tick dt ->
                    let
                        delta =
                            case model.timeFlow of
                                Normal ->
                                    dt

                                Paused ->
                                    0

                                Reversed ->
                                    -dt
                    in
                        model |> tick delta

                Purge _ ->
                    model |> historyGC
    in
        ( processedMetaModel, Cmd.none )



-- React to keyboard input


keyInput : KeyEvent -> Model -> Model
keyInput keyEvent model =
    let
        msg =
            (KeyMsg keyEvent)

        updateVelocity mdl =
            { mdl | circleModel = (mdl.circleModel |> motionInput keyEvent) }
    in
        case keyEvent of
            KeyDown key ->
                if isAction keyBindings.rewind key then
                    model |> setTimeFlow Reversed
                else
                    model |> setTimeFlow Normal |> modelToHistory msg |> updateVelocity

            KeyUp key ->
                if isAction keyBindings.rewind key then
                    model |> setTimeFlow Paused
                else
                    model |> modelToHistory msg |> updateVelocity |> pauseIfNotMoving



-- Update meta model in regards to time


tick : Time -> Model -> Model
tick dt model =
    let
        withAddedTime dt0 mdl =
            let
                timeAdd dt0 mdl =
                    { mdl | time = mdl.time + dt0 }
            in
                { mdl | circleModel = mdl.circleModel |> timeAdd dt0 }

        withTimeDirection mdl =
            case mdl.timeFlow of
                Normal ->
                    mdl |> withAddedTime dt |> modelToHistory (Tick dt)

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
                        |> updatePos dt mdl.timeFlow bounds
            }
    in
        model |> withTimeDirection |> withNewPos



-- updatePos : Float -> TimeFlow -> Vec2 -> Circle.Model -> Circle.Model
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
pauseIfNotMoving mdl =
    if mdl.circleModel.vel /= Vec2 0 0 then
        mdl |> setTimeFlow Normal
    else
        mdl |> setTimeFlow Paused



-- When position hasn't changed since last iteration, pause time


pauseIfNotMoving_ : Model -> Model
pauseIfNotMoving_ model =
    let
        currentPos =
            model.circleModel.pos

        lastPos =
            case model.history of
                [] ->
                    model.circleModel.pos

                x :: _ ->
                    (Tuple.first x).pos
    in
        if currentPos /= lastPos then
            model |> setTimeFlow Normal
        else
            model |> setTimeFlow Paused



-- Push current model onto the history stack


modelToHistory : Msg -> Model -> Model
modelToHistory msg model =
    { model | history = ( model.circleModel, msg ) :: (validHistory model) }



-- Setter for the flow of time


setTimeFlow : TimeFlow -> Model -> Model
setTimeFlow tf model =
    { model | timeFlow = tf }



-- The negative difference between current time and the time of the previous model


timeDiffBack : List ( Circle.Model, Msg ) -> Time
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



-- Update current position based on velocity


updatePos : Float -> TimeFlow -> Vec2 -> Circle.Model -> Circle.Model
updatePos dt timeFlow bounds model =
    let
        perSecond x =
            let
                delta =
                    case timeFlow of
                        Normal ->
                            dt

                        Paused ->
                            0

                        Reversed ->
                            dt * -1
            in
                toFloat x * delta |> round

        newX =
            model.pos.x + perSecond model.vel.x

        newY =
            model.pos.y + perSecond model.vel.y

        radius =
            (model.size // 2) + 5
    in
        { model | pos = withinBounds bounds (Vec2 newX newY) radius }



-- Restrict position to within the edges of the window


withinBounds : Vec2 -> Vec2 -> Int -> Vec2
withinBounds border vec radius =
    let
        bound end x =
            clamp radius (end - radius) x

        x_ =
            bound border.x vec.x

        y_ =
            bound border.y vec.y
    in
        Position x_ y_



-- Set next velocity based on keyboard input


motionInput : KeyEvent -> Circle.Model -> Circle.Model
motionInput keyEvent model =
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

        newVel x y =
            { x = addIfNotEqual model.vel.x x
            , y = addIfNotEqual model.vel.y y
            }

        setVel x y =
            { model | vel = newVel x y }

        is action =
            isAction action keycode
    in
        if is keyBindings.moveUp then
            setVel 0 -speed
        else if is keyBindings.moveDown then
            setVel 0 speed
        else if is keyBindings.moveLeft then
            setVel -speed 0
        else if is keyBindings.moveRight then
            setVel speed 0
        else
            model



-- Clean up the history from entries that are too old


historyGC : Model -> Model
historyGC model =
    { model | history = validHistory model }


validHistory : Model -> List ( Circle.Model, Msg )
validHistory model =
    let
        notOld : Circle.Model -> Bool
        notOld x =
            model.circleModel.time - x.time < model.recordedTime
    in
        List.filter (notOld << Tuple.first) model.history



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
        , [ Circle.view (model.circleModel |> setColor green) ]
        ]


circleTrail : Model -> List (Html msg)
circleTrail model =
    let
        toCircle ( model, _ ) =
            Circle.view (model |> setColor charcoal |> setSize (model.size // 2))
    in
        model |> validHistory |> List.map toCircle



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



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initMetaModel, Task.perform WindowSize Window.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
