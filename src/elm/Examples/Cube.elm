module Examples.Cube exposing
    ( Uniforms
    , Varyings
    , impl
    , mesh
    , pixelShader
    , uniforms
    )

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (vec3)
import Color exposing (..)
import Examples.ShaderToy exposing (Uniforms)
import Misc
import Raster
import Renderer exposing (Color, Entity, Impl, PixelShader, Vertex)


type alias Uniforms =
    {}


uniforms : Uniforms
uniforms =
    {}


type alias Varyings =
    { color : Color
    }


mesh : Entity (Vertex Varyings)
mesh =
    let
        frontFace =
            [ ( { position = vec3 -1 -1 -1, varyings = { color = red } }
              , { position = vec3 -1 1 -1, varyings = { color = green } }
              , { position = vec3 1 1 -1, varyings = { color = blue } }
              )
            , ( { position = vec3 -1 -1 -1, varyings = { color = red } }
              , { position = vec3 1 1 -1, varyings = { color = blue } }
              , { position = vec3 1 -1 -1, varyings = { color = green } }
              )
            ]

        topFace =
            transformEntity (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) frontFace

        bottomFace =
            transformEntity (Mat4.makeRotate (-pi / 2) (vec3 1 0 0)) frontFace

        rightFace =
            transformEntity (Mat4.makeRotate (-pi / 2) (vec3 0 1 0)) frontFace

        leftFace =
            transformEntity (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) frontFace

        backFace =
            transformEntity (Mat4.makeRotate pi (vec3 0 1 0)) frontFace
    in
    []
        ++ topFace
        ++ rightFace
        ++ leftFace
        ++ bottomFace
        ++ backFace
        ++ frontFace


pixelShader : PixelShader Uniforms Varyings
pixelShader _ { color } =
    color


transformEntity : Mat4 -> Entity (Vertex varyings) -> Entity (Vertex varyings)
transformEntity mat entity =
    entity
        |> List.map
            (\tri ->
                Raster.mapTriangle (\v -> { v | position = Mat4.transform mat v.position }) tri
            )


impl : Impl Varyings
impl =
    { add = \v1 v2 -> { color = Vec3.add v1.color v2.color }
    , sub = \v1 v2 -> { color = Vec3.sub v1.color v2.color }
    , interpolate = \t v1 v2 -> { color = Misc.interpolate3 t v1.color v2.color }
    , scale = \s v -> { color = Vec3.scale s v.color }
    }
