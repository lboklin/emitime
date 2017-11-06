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


type alias MetaModel =
    { model : Model
    , history : List ( Model, Msg )
    , rewinding : Bool
    , recordedTime : Time
    , windowSize : Size
    }


initMetaModel : MetaModel
initMetaModel =
    { model = initModel
    , history = []
    , rewinding = False
    , recordedTime = 10
    , windowSize = { width = 400, height = 400 }
    }


type alias Model =
    { pos : Position
    , size : Int
    , vel : Vec2
    , time : Time
    }


initModel : Model
initModel =
    { pos = Position 0 0
    , size = 50
    , vel = { x = 0, y = 0 }
    , time = 0
    }


type Msg
    = KeyMsg KeyEvent
    | WindowSize Size
    | Tick Time
    | Purge Time


type KeyEvent
    = KeyDown KeyCode
    | KeyUp KeyCode


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
                    metaModel |> withTime dt

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
                    { metaModel | rewinding = True }
                else
                    (metaModel |> modelToHistory msg |> withMotionInput keyEvent)

            KeyUp key ->
                if isAction keyBindings.rewind key then
                    { metaModel | rewinding = False }
                else
                    (metaModel |> modelToHistory msg |> withMotionInput keyEvent)



-- Update meta model in regards to time


withTime : Time -> MetaModel -> MetaModel
withTime dt metaModel =
    let
        msg =
            (Tick dt)

        addTime dt0 model =
            { model | time = model.time + dt0 }

        delta =
            if metaModel.rewinding then
                timeDiffBack metaModel.history
            else
                dt

        withTimeDirection mMdl =
            if mMdl.rewinding then
                mMdl |> stepBack
            else
                { mMdl | model = mMdl.model |> addTime delta } |> modelToHistory msg
    in
        metaModel |> withTimeDirection |> updatePos delta


modelToHistory : Msg -> MetaModel -> MetaModel
modelToHistory msg metaModel =
    { metaModel | history = ( metaModel.model, msg ) :: metaModel.history }



-- Replace current model with the previous on in history and pop history


stepBack : MetaModel -> MetaModel
stepBack metaModel =
    let
        ( previousModel, previousHistory ) =
            case metaModel.history of
                [] ->
                    ( (metaModel.model), [] )

                x :: xs ->
                    ( Tuple.first x, xs )
    in
        { metaModel
            | model = previousModel
            , history = previousHistory
        }



-- The negative difference between current time and the time of the previous model


timeDiffBack : List ( Model, Msg ) -> Time
timeDiffBack hist =
    case hist of
        [] ->
            0

        x :: xs ->
            let
                time a =
                    (Tuple.first a).time
            in
                case xs of
                    [] ->
                        time x

                    y :: ys ->
                        (time y) - (time x)



-- case hist of
--     [] ->
--         0
--     x :: _ ->
--         case Tuple.second x of
--             Tick dt ->
--                 dt
--             _ ->
--                 0
-- Update current position based on velocity


updatePos : Float -> MetaModel -> MetaModel
updatePos dt metaModel =
    let
        model =
            metaModel.model

        perSecond x =
            let
                delta =
                    if metaModel.rewinding then
                        dt * -1
                    else
                        dt
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
    let
        notOld : Model -> Bool
        notOld x =
            metaModel.model.time - x.time < metaModel.recordedTime * second

        gc list =
            List.filter (notOld << Tuple.first) list
    in
        { metaModel | history = gc metaModel.history }



-- VIEW


(=>) : a -> a -> ( a, a )
(=>) =
    (,)


px : Int -> String
px x =
    toString x ++ "px"


view : MetaModel -> Html Msg
view metaModel =
    let
        pos =
            Position metaModel.model.pos.x metaModel.model.pos.y
    in
        Html.div
            [ Html.Attributes.style <|
                [ "width" => ((metaModel.windowSize.width |> toString) ++ "px")
                , "height" => ((metaModel.windowSize.height |> toString) ++ "px")
                , "display" => "block"
                , "background-color" => "#2d2d2d"
                ]
            ]
            [ circle (metaModel.model.size // 2) pos ]


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


subscriptions : MetaModel -> Sub Msg
subscriptions metaModel =
    let
        tickIfMoving =
            if metaModel.model.vel /= Vec2 0 0 then
                [ AnimationFrame.diffs Tick ]
            else
                []
    in
        Sub.batch <|
            [ ups (\x -> KeyMsg (KeyUp x))
            , downs (\x -> KeyMsg (KeyDown x))
            , W.resizes WindowSize
            , every (second * metaModel.recordedTime) Purge
            ]
                ++ tickIfMoving



-- MAIN


main : Program Never MetaModel Msg
main =
    Html.program
        { init = ( initMetaModel, Task.perform WindowSize W.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
