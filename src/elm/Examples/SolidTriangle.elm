module Examples.SolidTriangle exposing (entity)

import AltMath.Matrix4 as Mat4
import AltMath.Vector2 exposing (Vec2, vec2)
import AltMath.Vector3 exposing (vec3)
import Color
import Examples.Cube exposing (Uniforms)
import Renderer exposing (Entity, Mesh, PixelShader, VertexShader)


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
            |> Mat4.transform
                (Mat4.mul (Mat4.makeRotate time (vec3 0 0 1))
                    (Mat4.makeScale3 0.5 0.5 0.5)
                )
    , varyings = {}
    }


pixelShader : PixelShader Uniforms Varyings
pixelShader uniforms varyings =
    Color.red
