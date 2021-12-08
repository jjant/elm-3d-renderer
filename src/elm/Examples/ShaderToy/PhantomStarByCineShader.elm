module Examples.ShaderToy.PhantomStarByCineShader exposing (entity)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Examples.ShaderToy as ShaderToy exposing (Uniforms, Attributes, Varyings, abs3, max3, mod, pmod, rot, transform2)
import Misc
import Renderer exposing (PixelShader)
import Renderer exposing (Entity)

entity : Uniforms -> Entity Uniforms Attributes Varyings
entity uniforms =
    { uniforms = uniforms
    , mesh = ShaderToy.mesh {width = uniforms.iResolution.x, height = uniforms.iResolution.y}
    , vertexShader = ShaderToy.vertexShader
    , pixelShader = pixelShader
    , impl = ShaderToy.impl
    }

-- Phantom Star for CineShader
-- https://www.shadertoy.com/view/ttKGDt


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


computeAccs : Float -> Vec3 -> Vec3 -> ( Float, Float, Float )
computeAccs iTime cPos ray =
    ( 0, 99 - 1 )
        |> Misc.for
            (\_ ( t, acc, acc2 ) ->
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


map : Float -> Vec3 -> Vec3 -> Float
map iTime p _ =
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
    ifsBox iTime newP


ifsBox : Float -> Vec3 -> Float
ifsBox iTime p =
    let
        newP =
            ( 0, 5 - 1 )
                |> Misc.for
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


box : Vec3 -> Vec3 -> Float
box p b =
    let
        d =
            Vec3.sub (abs3 p) b
    in
    min (max d.x (max d.y d.z)) 0.0 + Vec3.length (max3 d (vec3 0 0 0))
