module Examples.ShaderToy exposing
    ( Attributes
    , Mat2
    , Uniforms
    , Varyings
    , vertexShader
    , abs3
    , div2
    , impl
    , max3
    , mesh
    , min3
    , mod
    , mod2f
    , pi2
    , pmod
    , rot
    , transform2
    , uniforms
    , xy
    )

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 exposing (Vec3, vec3)
import Misc
import Renderer exposing (Impl, Mesh, VertexShader)


type alias Uniforms =
    { iTime : Float
    , iResolution : Vec2
    }


type alias Attributes =
    {}


type alias Varyings =
    { fragCoord : Vec2 }


uniforms : { iTime : Float, iResolution : Vec2 } -> Uniforms
uniforms u =
    u


mesh : { width : Float, height : Float } -> Mesh Varyings
mesh { width, height } =
    [ ( { position = vec3 -1 -1 5, varyings = { fragCoord = vec2 0 0 } }
      , { position = vec3 -1 1 5, varyings = { fragCoord = vec2 0 height } }
      , { position = vec3 1 1 5, varyings = { fragCoord = vec2 width height } }
      )
    , ( { position = vec3 -1 -1 5, varyings = { fragCoord = vec2 0 0 } }
      , { position = vec3 1 1 5, varyings = { fragCoord = vec2 width height } }
      , { position = vec3 1 -1 5, varyings = { fragCoord = vec2 width 0 } }
      )
    ]


impl : Impl Varyings
impl =
    { add = \v1 v2 -> { fragCoord = Vec2.add v1.fragCoord v2.fragCoord }
    , sub = \v1 v2 -> { fragCoord = Vec2.sub v1.fragCoord v2.fragCoord }
    , interpolate = \t v1 v2 -> { fragCoord = Misc.interpolate2 t v1.fragCoord v2.fragCoord }
    , scale = \s v -> { fragCoord = Vec2.scale s v.fragCoord }
    }


vertexShader : VertexShader Uniforms Attributes Varyings
vertexShader _ _ =
    -- TODO: Fix after introducing vertex shaders
    { position = vec3 0 0 0, varyings = { fragCoord = vec2 0 0 } }



---- UTILITY FUNCTIONS USUALLY USED IN GLSL ----


div2 : Vec2 -> Vec2 -> Vec2
div2 v1 v2 =
    vec2 (v1.x / v2.x) (v1.y / v2.y)


pi2 : Float
pi2 =
    pi * 2


type alias Mat2 =
    { m11 : Float
    , m12 : Float
    , m21 : Float
    , m22 : Float
    }


transform2 : Mat2 -> Vec2 -> Vec2
transform2 { m11, m12, m21, m22 } v =
    vec2
        (m11 * v.x + m12 * v.y)
        (m21 * v.x + m22 * v.y)


rot : Float -> Mat2
rot a =
    let
        c =
            cos a

        s =
            sin a
    in
    Mat2 c s -s c


pmod : Vec2 -> Float -> Vec2
pmod p r =
    let
        a =
            atan2 p.x p.y + pi / r

        n =
            pi2 / r

        b =
            toFloat (floor (a / n)) * n
    in
    transform2 (rot -b) p


abs3 : Vec3 -> Vec3
abs3 v =
    vec3 (abs v.x) (abs v.y) (abs v.z)


max3 : Vec3 -> Vec3 -> Vec3
max3 v u =
    vec3 (max v.x u.x) (max v.y u.y) (max v.z u.z)


min3 : Vec3 -> Vec3 -> Vec3
min3 v u =
    vec3 (min v.x u.x) (min v.y u.y) (min v.z u.z)


mod : Float -> Float -> Float
mod x y =
    x - y * toFloat (floor (x / y))


mod2f : Vec2 -> Float -> Vec2
mod2f v y =
    vec2 (mod v.x y) (mod v.y y)


xy : Vec3 -> Vec2
xy v =
    vec2 v.x v.y
