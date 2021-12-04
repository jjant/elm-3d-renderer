module Renderer exposing
    ( Buffer
    , Color
    , Entity
    , FragmentShader
    , Triangle
    , VertexShader
    , draw
    , init
    , ndcToScreen
    , setPixel
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 exposing (Vec3)
import Array exposing (Array)
import Html exposing (Html)


ndcToScreen : Buffer -> Mat4
ndcToScreen buffer =
    Mat4.makeScale3 0.5 0.5 1
        |> Mat4.mul (Mat4.makeTranslate3 0.5 0.5 0)
        |> Mat4.mul (Mat4.makeScale3 (toFloat buffer.width) (toFloat buffer.height) 1)
        |> Mat4.mul (Mat4.makeScale3 1 -1 1)
        |> Mat4.mul (Mat4.makeTranslate3 0 (toFloat buffer.height) 0)


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
    List ( Triangle attributes, Color )


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
draw _ _ _ =
    Debug.todo "todo"
