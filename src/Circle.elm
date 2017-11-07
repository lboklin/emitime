module Circle exposing (..)

import Time exposing (..)
import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes as SvgA
import Color exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Utils exposing (..)


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
