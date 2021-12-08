module Texture exposing (Texture, get, height, init, set, width)

import Array exposing (Array)
import Color
import Misc
import Renderer exposing (Color)


type alias Texture =
    { data : Array Color
    , width : Int
    , height : Int
    }


init : Int -> Int -> Color -> Texture
init width_ height_ color =
    { data = Array.repeat (width_ * height_) color
    , width = width_
    , height = height_
    }


width t =
    t.width


height t =
    t.height


set : Int -> Int -> Color -> Texture -> Texture
set x y color texture =
    { texture
        | data =
            Array.set (x + width texture * y) color texture.data
    }


get : Float -> Float -> Texture -> Color
get xF yF texture =
    let
        x =
            floor (clamp 0 1 xF * toFloat (width texture))

        y =
            floor (clamp 0 1 yF * toFloat (height texture))

        index =
            x + width texture * y
    in
    Array.get index texture.data
        -- |> Misc.unwrap ("Texture.get " ++ Debug.toString ( x, y ))
        |> Maybe.withDefault Color.green
