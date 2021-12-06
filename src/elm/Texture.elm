module Texture exposing (Texture, get, height, init, set, width)

import Array exposing (Array)
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


get : Int -> Int -> Texture -> Color
get x y texture =
    let
        index =
            x + width texture * y

        maxIndex =
            Array.length texture.data
    in
    Array.get index texture.data
        |> Misc.unwrap ("Texture.get " ++ Debug.toString ( index, maxIndex ))
