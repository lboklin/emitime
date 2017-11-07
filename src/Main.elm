module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import AnimationFrame
import Char exposing (toCode)
import Window exposing (Size)
import Task
import Time exposing (..)
import Keyboard exposing (KeyCode, ups, downs)
import Circle
import Utils exposing (..)
import Color exposing (..)


-- MODEL


type alias MetaModel =
    { model : Circle.Model
    , history : List ( Circle.Model, Msg )
    , timeFlow : TimeFlow
    , recordedTime : Time
    , windowSize : Size
    }


initMetaModel : MetaModel
initMetaModel =
    { model = Circle.initModel
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


update : Msg -> MetaModel -> ( MetaModel, Cmd Msg )
update msg metaModel =
    let
        processedMetaModel =
            case msg of
                KeyMsg keyEvent ->
                    metaModel |> withKeyInput keyEvent

                WindowSize { width, height } ->
                    { metaModel | windowSize = Size width height }

                Tick dt ->
                    let
                        delta =
                            case metaModel.timeFlow of
                                Normal ->
                                    dt

                                Paused ->
                                    0

                                Reversed ->
                                    -dt
                    in
                        metaModel |> withTime delta

                Purge _ ->
                    metaModel |> historyGC
    in
        ( processedMetaModel, Cmd.none )



-- React to keyboard input


withKeyInput : KeyEvent -> MetaModel -> MetaModel
withKeyInput keyEvent metaModel =
    let
        msg =
            (KeyMsg keyEvent)
    in
        case keyEvent of
            KeyDown key ->
                if isAction keyBindings.rewind key then
                    metaModel |> withTimeFlow Reversed
                else
                    metaModel |> withTimeFlow Normal |> modelToHistory msg |> withMotionInput keyEvent

            KeyUp key ->
                if isAction keyBindings.rewind key then
                    metaModel |> withTimeFlow Paused
                else
                    metaModel |> modelToHistory msg |> withMotionInput keyEvent |> pauseIfNotMoving



-- Update meta model in regards to time


withTime : Time -> MetaModel -> MetaModel
withTime dt metaModel =
    let
        withAddedTime dt0 mMdl =
            let
                timeAdd dt0 model =
                    { model | time = model.time + dt0 }
            in
                { mMdl | model = mMdl.model |> timeAdd dt0 }

        withTimeDirection mMdl =
            case mMdl.timeFlow of
                Normal ->
                    mMdl |> withAddedTime dt |> modelToHistory (Tick dt)

                Paused ->
                    mMdl

                Reversed ->
                    mMdl |> stepBack
    in
        metaModel |> withTimeDirection |> updatePos dt



-- Replace current model with the previous on in history and pop history


stepBack : MetaModel -> MetaModel
stepBack metaModel =
    let
        previousModel =
            List.head metaModel.history
                |> Maybe.withDefault ( metaModel.model, Tick 0 )
                |> Tuple.first
    in
        -- I'm not sure how to fix velocity getting stuck other than to reset it here
        { metaModel
            | model = { previousModel | vel = { x = 0, y = 0 } }
            , history = Maybe.withDefault [] (List.tail metaModel.history)
        }



-- When velocity is 0, pause time


pauseIfNotMoving : MetaModel -> MetaModel
pauseIfNotMoving mMdl =
    if mMdl.model.vel /= Vec2 0 0 then
        mMdl |> withTimeFlow Normal
    else
        mMdl |> withTimeFlow Paused



-- When position hasn't changed since last iteration, pause time


pauseIfNotMoving_ : MetaModel -> MetaModel
pauseIfNotMoving_ mMdl =
    let
        currentPos =
            mMdl.model.pos

        lastPos =
            case mMdl.history of
                [] ->
                    mMdl.model.pos

                x :: _ ->
                    (Tuple.first x).pos
    in
        if currentPos /= lastPos then
            mMdl |> withTimeFlow Normal
        else
            mMdl |> withTimeFlow Paused



-- Push current model onto the history stack


modelToHistory : Msg -> MetaModel -> MetaModel
modelToHistory msg metaModel =
    { metaModel | history = ( metaModel.model, msg ) :: (validHistory metaModel) }



-- Setter for the flow of time


withTimeFlow : TimeFlow -> MetaModel -> MetaModel
withTimeFlow tf metaModel =
    { metaModel | timeFlow = tf }



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


updatePos : Float -> MetaModel -> MetaModel
updatePos dt metaModel =
    let
        model =
            metaModel.model

        perSecond x =
            let
                delta =
                    case metaModel.timeFlow of
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
    in
        metaModel |> withinBounds newX newY 5



-- Restrict position to within the edges of the window


withinBounds : Int -> Int -> Int -> MetaModel -> MetaModel
withinBounds x y padding metaModel =
    let
        model =
            metaModel.model

        collisionRadius =
            (model.size // 2) + padding

        bound end x =
            clamp (collisionRadius) (end - collisionRadius) x

        newModel =
            { model
                | pos =
                    Position
                        (bound metaModel.windowSize.width x)
                        (bound metaModel.windowSize.height y)
            }
    in
        { metaModel | model = newModel }



-- Set next velocity based on keyboard input


withMotionInput : KeyEvent -> MetaModel -> MetaModel
withMotionInput keyEvent metaModel =
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

        model =
            metaModel.model

        newVel x y =
            { x = addIfNotEqual model.vel.x x
            , y = addIfNotEqual model.vel.y y
            }

        withModelVel x y =
            { metaModel | model = { model | vel = newVel x y } }

        is action =
            isAction action keycode
    in
        if is keyBindings.moveUp then
            withModelVel 0 -speed
        else if is keyBindings.moveDown then
            withModelVel 0 speed
        else if is keyBindings.moveLeft then
            withModelVel -speed 0
        else if is keyBindings.moveRight then
            withModelVel speed 0
        else
            metaModel



-- Clean up the history from entries that are too old


historyGC : MetaModel -> MetaModel
historyGC metaModel =
    { metaModel | history = validHistory metaModel }


validHistory : MetaModel -> List ( Circle.Model, Msg )
validHistory metaModel =
    let
        notOld : Circle.Model -> Bool
        notOld x =
            metaModel.model.time - x.time < metaModel.recordedTime
    in
        List.filter (notOld << Tuple.first) metaModel.history



-- VIEW


view : MetaModel -> Html Msg
view metaModel =
    Html.div
        [ Html.Attributes.style <|
            [ "width" => ((metaModel.windowSize.width |> toString) ++ "px")
            , "height" => ((metaModel.windowSize.height |> toString) ++ "px")
            , "display" => "block"
            , "background-color" => "#2d2d2d"
            ]
        ]
        (nodes metaModel)


withColor : Color -> Circle.Model -> Circle.Model
withColor color model =
    { model | color = color }


withSize : Int -> Circle.Model -> Circle.Model
withSize size model =
    { model | size = size }


nodes : MetaModel -> List (Html msg)
nodes metaModel =
    List.concat
        [ circleTrail metaModel
        , [ Circle.view (metaModel.model |> withColor green) ]
        ]


circleTrail : MetaModel -> List (Html msg)
circleTrail metaModel =
    let
        toCircle ( model, _ ) =
            Circle.view (model |> withColor charcoal |> withSize (model.size // 2))
    in
        metaModel |> validHistory |> List.map toCircle



-- SUBSCRIPTIONS


subscriptions : MetaModel -> Sub Msg
subscriptions metaModel =
    let
        tickIfEventful =
            if metaModel.timeFlow /= Paused then
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


main : Program Never MetaModel Msg
main =
    Html.program
        { init = ( initMetaModel, Task.perform WindowSize Window.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
