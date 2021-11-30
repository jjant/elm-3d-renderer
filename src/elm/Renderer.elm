module Renderer exposing
    ( Buffer
    , Color
    , Entity
    , FragmentShader
    , Triangle
    , VertexShader
    , draw
    , init
    , setPixel
    )

import AltMath.Vector3 exposing (Vec3)
import Array exposing (Array)
import Html exposing (Html)


type alias Buffer =
    { width : Int
    , height : Int
    , data : Array Color
    }


init : { width : Int, height : Int, color : Color } -> Buffer
init { width, height, color } =
    { width = width
    , height = height
    , data = Array.repeat (width * height) color
    }


setPixel : Int -> Int -> Color -> Buffer -> Buffer
setPixel x y c buffer =
    { buffer
        | data =
            buffer
                |> computeIndex x y
                |> Maybe.map (\index -> Array.set index c buffer.data)
                |> Maybe.withDefault buffer.data
    }


computeIndex : Int -> Int -> Buffer -> Maybe Int
computeIndex x y { width, height } =
    if 0 <= x && x < width && 0 <= y && y < height then
        Just (x + y * width)

    else
        Nothing


type alias Color =
    Vec3


type alias Entity attributes =
    List ( attributes, attributes, attributes )


type alias Triangle attributes =
    ( attributes, attributes, attributes )


type alias VertexShader uniforms attributes varyings =
    uniforms -> attributes -> { position : Vec3, varyings : varyings }


type alias FragmentShader uniforms varyings =
    uniforms -> varyings -> { color : Vec3 }


draw :
    Entity attributes
    -> VertexShader uniforms attributes varyings
    -> FragmentShader uniforms varyings
    -> Html msg
draw entity fragmentShader pixelShader =
    Debug.todo "todo"
