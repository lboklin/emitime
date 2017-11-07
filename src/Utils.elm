module Utils exposing (..)

import Window exposing (Size)


type alias Position =
    { x : Int
    , y : Int
    }


type alias Vec2 =
    { x : Int
    , y : Int
    }


(=>) : a -> a -> ( a, a )
(=>) =
    (,)


px : Int -> String
px x =
    toString x ++ "px"


fromSize : Size -> Vec2
fromSize size =
    Vec2 size.width size.height
