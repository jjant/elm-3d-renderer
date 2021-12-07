module Examples.ShaderToy.CreationBySilexars exposing (pixelShader)

import AltMath.Vector2 as Vec2 exposing (vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Examples.ShaderToy as ShaderToy exposing (Uniforms, Varyings, div2)
import Misc
import Renderer exposing (PixelShader)



-- https://www.shadertoy.com/view/XsXXDn
-- If you intend to reuse this shader, please add credits to 'Danilo Guanabara'


setCoord : Int -> Float -> Vec3 -> Vec3
setCoord i coord v =
    case i of
        0 ->
            Vec3.setX coord v

        1 ->
            Vec3.setY coord v

        2 ->
            Vec3.setZ coord v

        _ ->
            Debug.todo ""


pixelShader : PixelShader Uniforms Varyings
pixelShader { iResolution, iTime } { fragCoord } =
    let
        ( c_, l_, _ ) =
            ( 0, 2 )
                |> Misc.for
                    (\i ( c, _, z ) ->
                        let
                            p =
                                div2 fragCoord iResolution

                            newP =
                                Vec2.sub p (vec2 0.5 0.5)

                            newNewP =
                                vec2 (newP.x * iResolution.x / iResolution.y) newP.y

                            newZ =
                                iTime + 0.07

                            newL =
                                Vec2.length newNewP

                            uv =
                                Vec2.add p (Vec2.scale (1 / newL * (sin z + 1.0) * abs (sin (newL * 9.0 - newZ - newZ))) newNewP)

                            c_i =
                                0.01 / Vec2.length (Vec2.sub (ShaderToy.mod2f uv 1.0) (vec2 0.5 0.5))

                            newC =
                                setCoord i c_i c
                        in
                        ( newC, newL, newZ )
                    )
                    ( vec3 0 0 0, 0, iTime )
    in
    c_
        -- |> Vec3.scale (1 / (iTime * l_))
        |> Vec3.scale 255
