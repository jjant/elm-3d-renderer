module Examples.Cube exposing
    ( Attributes
    , Uniforms
    , Varyings
    , entity
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Color exposing (..)
import Examples.ShaderToy exposing (Uniforms)
import Misc
import Renderer exposing (Color, Entity, Impl, Mesh, PixelShader, VertexShader)


type alias Attributes =
    { position : Vec3, color : Vec3 }


type alias Uniforms =
    { time : Float }


type alias Varyings =
    { color : Color
    }


entity : Uniforms -> Entity Uniforms Attributes Varyings
entity uniforms =
    { uniforms = uniforms
    , mesh = mesh
    , vertexShader = vertexShader
    , pixelShader = pixelShader
    , impl = impl
    }


mesh : Mesh Attributes
mesh =
    let
        frontFace =
            [ ( { position = vec3 -1 -1 -1, color = red }
              , { position = vec3 -1 1 -1, color = green }
              , { position = vec3 1 1 -1, color = blue }
              )
            , ( { position = vec3 -1 -1 -1, color = red }
              , { position = vec3 1 1 -1, color = blue }
              , { position = vec3 1 -1 -1, color = green }
              )
            ]

        topFace =
            Renderer.transformMesh (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) frontFace

        bottomFace =
            Renderer.transformMesh (Mat4.makeRotate (-pi / 2) (vec3 1 0 0)) frontFace

        rightFace =
            Renderer.transformMesh (Mat4.makeRotate (-pi / 2) (vec3 0 1 0)) frontFace

        leftFace =
            Renderer.transformMesh (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) frontFace

        backFace =
            Renderer.transformMesh (Mat4.makeRotate pi (vec3 0 1 0)) frontFace
    in
    []
        ++ topFace
        ++ rightFace
        ++ leftFace
        ++ bottomFace
        ++ backFace
        ++ frontFace


vertexShader : VertexShader Uniforms Attributes Varyings
vertexShader { time } { position, color } =
    { position =
        position
            |> Mat4.transform
                (Mat4.makeRotate time (Vec3.normalize (vec3 1 2 1))
                    |> Mat4.mul (Mat4.makeScale3 0.5 0.5 0.5)
                    |> Mat4.mul (Mat4.makeTranslate3 0 0 20)
                )
    , varyings = { color = color }
    }


pixelShader : PixelShader Uniforms Varyings
pixelShader _ { color } =
    color


impl : Impl Varyings
impl =
    { add = \v1 v2 -> { color = Vec3.add v1.color v2.color }
    , sub = \v1 v2 -> { color = Vec3.sub v1.color v2.color }
    , interpolate = \t v1 v2 -> { color = Misc.interpolate3 t v1.color v2.color }
    , scale = \s v -> { color = Vec3.scale s v.color }
    }
