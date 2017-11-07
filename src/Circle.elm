module Circle exposing (..)

import Time exposing (..)
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes as SvgA
import Color exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Utils exposing (..)
import Keyboard exposing (KeyCode)


-- MODEL


type alias Model =
    { pos : Position
    , size : Int
    , color : Color
    , vel : Vec2
    , time : Time
    }


initModel : Model
initModel =
    { pos = Position 200 200
    , size = 50
    , color = red
    , vel = { x = 0, y = 0 }
    , time = 0
    }


setColor : Color -> Model -> Model
setColor color model =
    { model | color = color }


setSize : Int -> Model -> Model
setSize size model =
    { model | size = size }



-- UPDATE
-- Update current position based on velocity


updatePos : Float -> Vec2 -> Model -> Model
updatePos dt bounds model =
    let
        perSecond x =
            toFloat x * dt |> round

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



-- VIEW


view : Model -> Html msg
view model =
    let
        r =
            model.size // 2
    in
        Html.div
            [ Html.Attributes.style <|
                [ "width" => px (r * 2)
                , "height" => px (r * 2)
                , "position" => "absolute"
                , "left" => px (model.pos.x - r)
                , "top" => px (model.pos.y - r)
                ]
            ]
            [ Svg.svg
                [ SvgA.viewBox "0 0 100 100"
                ]
                [ Svg.circle
                    [ SvgA.cx "50"
                    , SvgA.cy "50"
                    , SvgA.r "50"
                    , SvgA.fill <| colorToCssRgb model.color
                    ]
                    []
                ]
            ]
