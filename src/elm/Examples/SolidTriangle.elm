module Examples.SolidTriangle exposing (entity)

import Color
import Examples.Cube exposing (Uniforms)
import Mat4
import Renderer exposing (Entity, Mesh, PixelShader, VertexShader)
import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (vec3)


type alias Uniforms =
    { time : Float }


type alias Attributes =
    Vec2


type alias Varyings =
    {}


entity : Uniforms -> Entity Uniforms Attributes Varyings
entity uniforms =
    { mesh = mesh
    , uniforms = uniforms
    , vertexShader = vertexShader
    , pixelShader = pixelShader
    , impl = Renderer.emptyImpl
    }


mesh : Mesh Attributes
mesh =
    let
        bottomLeft =
            vec2 -1 -1

        topMiddle =
            vec2 0 1

        bottomRight =
            vec2 1 -1
    in
    [ ( bottomLeft, topMiddle, bottomRight ) ]


vertexShader : VertexShader Uniforms Attributes Varyings
vertexShader { time } pos =
    { position =
        vec3 pos.x pos.y 1
            |> Mat4.transformPoint
                (--Mat4.mul
                 Mat4.rotate (2 * time) (vec3 0 0 1)
                 -- (Mat4.scale (vec3 0.5 0.5 0.5))
                )
    , varyings = {}
    }


pixelShader : PixelShader Uniforms Varyings
pixelShader uniforms varyings =
    Color.red
