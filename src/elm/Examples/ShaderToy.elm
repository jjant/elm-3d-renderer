module Examples.ShaderToy exposing (Uniforms, Varyings, pixelShader)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import AltMath.Vector4 exposing (vec4)
import Color
import Renderer exposing (PixelShader)
import Svg.Attributes exposing (transform)



-- https://www.shadertoy.com/view/XsXXDn
-- THIS IS: https://www.shadertoy.com/view/ttKGDt


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


box : Vec3 -> Vec3 -> Float
box p b =
    let
        d =
            Vec3.sub (abs3 p) b
    in
    min (max d.x (max d.y d.z)) 0.0 + Vec3.length (max3 d (vec3 0 0 0))


ifsBox : Float -> Vec3 -> Float
ifsBox iTime p =
    let
        newP =
            List.range 0 (5 - 1)
                |> List.foldl
                    (\_ pLoop ->
                        let
                            pPrime =
                                Vec3.sub (abs3 pLoop) (vec3 1 1 1)

                            xy1 =
                                transform2 (rot (iTime * 0.3)) (vec2 pPrime.x pPrime.y)

                            pPrimePrime =
                                vec3 xy1.x xy1.y pPrime.z

                            xz1 =
                                transform2 (rot (iTime * 0.1)) (vec2 pPrimePrime.x pPrimePrime.y)

                            pPrimePrimePrime =
                                vec3 xz1.x pPrimePrime.y xz1.y
                        in
                        pPrimePrimePrime
                    )
                    p

        xz2 =
            transform2 (rot iTime) (vec2 newP.x newP.y)

        newNewP =
            vec3 xz2.x newP.y xz2.y
    in
    box newNewP (vec3 0.4 0.8 0.3)


mod x y =
    x - y * toFloat (floor (x / y))


map : Float -> Vec3 -> Vec3 -> Float
map iTime p cPos =
    let
        p1 =
            vec3
                (mod (p.x - 5.0) 10.0 - 5.0)
                (mod (p.y - 5.0) 10.0 - 5.0)
                (mod p.z 16.0 - 8.0)

        p1xy =
            pmod (vec2 p1.x p1.y) 5.0

        newP =
            vec3 p1xy.x p1xy.y p1.z
    in
    ifsBox iTime p1


type alias Uniforms =
    { iTime : Float
    , iResolution : Vec2
    }


type alias Varyings =
    { fragCoord : Vec2 }


xy : Vec3 -> Vec2
xy v =
    vec2 v.x v.y


pixelShader : PixelShader Uniforms Varyings
pixelShader { iTime, iResolution } { fragCoord } =
    let
        p : Vec2
        p =
            Vec2.scale (1 / min iResolution.x iResolution.y)
                (Vec2.sub (Vec2.scale 2 fragCoord) iResolution)

        cPos : Vec3
        cPos =
            vec3 0.0 0.0 (-3.0 * iTime)

        cDir : Vec3
        cDir =
            Vec3.normalize (vec3 0.0 0.0 -1.0)

        cUp : Vec3
        cUp =
            vec3 (sin iTime) 1.0 0.0

        cSide : Vec3
        cSide =
            Vec3.cross cDir cUp

        ray : Vec3
        ray =
            Vec3.add (Vec3.add (Vec3.scale p.x cSide) (Vec3.scale p.y cUp)) cDir
                |> Vec3.normalize

        ( _, acc, acc2 ) =
            computeAccs iTime cPos ray

        col : Vec3
        col =
            vec3
                (acc * 0.01)
                (acc * 0.011 + acc2 * 0.002)
                (acc * 0.012 + acc2 * 0.005)
    in
    -- TODO: we don't do transparency yet
    -- vec4 col.x col.y col.z (1.0 - t * 0.03)
    Vec3.scale 255 col


computeAccs iTime cPos ray =
    List.range 0 (99 - 1)
        |> List.foldl
            (\i ( t, acc, acc2 ) ->
                let
                    pos =
                        Vec3.add cPos (Vec3.scale t ray)

                    dist =
                        max (abs (map iTime pos cPos)) 0.02

                    a_ =
                        e ^ (-dist * 3)

                    ( a, newAcc2 ) =
                        if mod (Vec3.length pos + 24.0 * iTime) 30.0 < 3.0 then
                            ( a_ * 2, acc2 + a_ * 2 )

                        else
                            ( a_, acc2 )

                    newAcc =
                        acc + a

                    newT =
                        t + dist * 0.5
                in
                ( newT, newAcc, newAcc2 )
            )
            ( 0, 0, 0 )



-- for (int i = 0 i < 99 i++) {
--     vec3 pos = cPos + ray * t
--     float dist = map(pos, cPos)
--     dist = max(abs(dist), 0.02)
--     float a = exp(-dist*3.0)
--     if (mod(length(pos)+24.0*iTime, 30.0) < 3.0) {
--         a *= 2.0
--         acc2 += a
--     }
--     acc += a
--     t += dist * 0.5
-- }
