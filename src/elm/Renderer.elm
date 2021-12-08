module Renderer exposing
    ( Buffer
    , Color
    , Entity
    , Impl
    , Mesh
    , PixelShader
    , Triangle
    , Vertex
    , VertexShader
    , emptyImpl
    , init
    , mapTriangle
    , ndcToScreen
    , render
    , setPixel
    , transformMesh
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 exposing (Vec3)
import Array exposing (Array)
import Html exposing (Html)
import Misc


{-| The core type that defines a "thing" that can be rendered.
-}
type alias Entity uniforms attributes varyings =
    { uniforms : uniforms
    , mesh : Mesh attributes
    , vertexShader : VertexShader uniforms attributes varyings
    , pixelShader : PixelShader uniforms varyings
    , impl : Impl varyings
    }


type alias VertexShader uniforms attributes varyings =
    uniforms -> attributes -> { position : Vec3, varyings : varyings }


type alias PixelShader uniforms varyings =
    uniforms -> varyings -> Color


type alias Vertex varyings =
    { position : Vec3, varyings : varyings }


type alias Mesh attributes =
    List (Triangle attributes)


type alias Triangle attributes =
    ( attributes, attributes, attributes )


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


type alias Impl varyings =
    { add : varyings -> varyings -> varyings
    , sub : varyings -> varyings -> varyings
    , interpolate : Float -> varyings -> varyings -> varyings
    , scale : Float -> varyings -> varyings
    }


emptyImpl : Impl {}
emptyImpl =
    { add = \_ _ -> {}
    , sub = \_ _ -> {}
    , interpolate = \_ _ _ -> {}
    , scale = \_ _ -> {}
    }


render : List (Html.Attribute msg) -> Entity uniforms attributes varyings -> Html msg
render atts entity =
    render atts entity



--------------


ndcToScreen : Buffer -> Mat4
ndcToScreen buffer =
    Mat4.makeScale3 0.5 0.5 1
        |> Mat4.mul (Mat4.makeTranslate3 0.5 0.5 0)
        |> Mat4.mul (Mat4.makeScale3 (toFloat buffer.width) (toFloat buffer.height) 1)
        |> Mat4.mul (Mat4.makeScale3 1 -1 1)
        |> Mat4.mul (Mat4.makeTranslate3 0 (toFloat buffer.height) 0)


transformMesh : Mat4 -> Mesh { r | position : Vec3 } -> Mesh { r | position : Vec3 }
transformMesh mat entity =
    entity
        |> List.map
            (\tri ->
                mapTriangle (\v -> { v | position = Mat4.transform mat v.position }) tri
            )


mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle =
    Misc.mapThree



-- entity :
--     VertexShader attributes uniforms varyings
--     -> PixelShader uniforms varyings
--     -> Mesh attributes
--     -> uniforms
--     -> Entity uniforms attributes varyings
-- entity =
--     Debug.todo ""
