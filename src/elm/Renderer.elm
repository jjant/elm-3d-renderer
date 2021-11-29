module Renderer exposing
    ( Color
    , Entity
    , FragmentShader
    , Triangle
    , VertexShader
    , draw
    )

import AltMath.Vector3 exposing (Vec3)
import Html exposing (Html)


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
