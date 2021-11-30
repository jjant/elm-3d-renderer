module Raster exposing (mapTriangle, renderTriangle)

import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Color exposing (blue, green, red, white)
import Misc
import Renderer exposing (Buffer, Color, Triangle)
import Vec3i exposing (Vec3i, vec3i)


renderTriangle : Triangle { position : Vec3i, color : Color } -> Buffer -> Buffer
renderTriangle triangle buffer =
    let
        sortedTriangle =
            triangle
                |> sort3 (\attribute -> ( attribute.position.y, attribute.position.x ))

        ( v0, v1, v2 ) =
            mapTriangle .position sortedTriangle

        ( c0, c1, c2 ) =
            mapTriangle .color sortedTriangle

        slope_ =
            slope v2 v0
                |> Misc.unwrap "slope_"

        w2 =
            -- TODO: Compute Z properly
            vec3i (v0.x + round (slope_ * toFloat (v1.y - v0.y))) v1.y 0

        ( u, v, w ) =
            barycentric (Vec3i.toVec w2) (mapThree Vec3i.toVec ( v0, v1, v2 ))

        d2 =
            Vec3.scale u c0
                |> Vec3.add (Vec3.scale v c1)
                |> Vec3.add (Vec3.scale w c2)
    in
    buffer
        -- |> renderFlatBottomTriangle ( v0, c0 ) ( v1, c1 ) ( w2, d2 )
        |> renderFlatTopTriangle ( v1, c1 ) ( w2, d2 ) ( v2, c2 )
        |> Renderer.setPixel v0.x v0.y white
        |> Renderer.setPixel v1.x v1.y white
        |> Renderer.setPixel v2.x v2.y white


renderTrianglePoint : ( Int, Int ) -> ( Vec3i, Vec3 ) -> ( Vec3i, Vec3 ) -> ( Vec3i, Vec3 ) -> Buffer -> Buffer
renderTrianglePoint ( x, y ) ( v0, c0 ) ( v1, c1 ) ( v2, c2 ) buf =
    let
        ( u, v, w ) =
            -- TODO: Compute Z properly!!!!
            -- Very important
            -- Look something up about "perspective correction"
            barycentric
                (vec3 (toFloat x) (toFloat y) 0)
                (mapThree Vec3i.toVec ( v0, v1, v2 ))

        color =
            Vec3.scale u c0
                |> Vec3.add (Vec3.scale v c1)
                |> Vec3.add (Vec3.scale w c2)
    in
    buf
        |> Renderer.setPixel x y color


renderFlatTopTriangle : ( Vec3i, Vec3 ) -> ( { x : Int, y : Int, z : Int }, Vec3 ) -> ( { x : Int, y : Int, z : Int }, Vec3 ) -> Buffer -> Buffer
renderFlatTopTriangle ( v0, c0 ) ( v1, c1 ) ( v2, c2 ) buf =
    let
        _ =
            Debug.log "" ( v0, v1, v2 )
    in
    (List.range v1.y v2.y
        |> List.foldl
            (\y currentBuffer ->
                let
                    _ =
                        Debug.log "\ny" y
                in
                case ( slope v2 v0, slope v2 v1 ) of
                    ( Just slopeV2V0, Just slopeV2V1 ) ->
                        let
                            dy =
                                toFloat (y - v0.y)

                            startX =
                                (v0.x + round (dy * slopeV2V0))
                                    |> Debug.log "startX"

                            endX =
                                (v1.x + round (dy * slopeV2V1))
                                    |> Debug.log "endX"
                        in
                        List.range startX endX
                            |> List.foldl
                                (\x currentCurrentBuffer ->
                                    renderTrianglePoint
                                        ( x, y )
                                        ( v0, c0 )
                                        ( v1, c1 )
                                        ( v2, c2 )
                                        currentCurrentBuffer
                                )
                                currentBuffer

                    _ ->
                        currentBuffer
            )
            buf
    )
        |> Renderer.setPixel v0.x v0.y red
        |> Renderer.setPixel v1.x v1.y green
        |> Renderer.setPixel v2.x v2.y blue



-- renderFlatBottomTriangle : Vec3i -> Vec3i -> Vec3i -> Buffer -> Buffer


renderFlatBottomTriangle ( v0, c0 ) ( v1, c1 ) ( v2, c2 ) buf =
    List.range v0.y v1.y
        |> List.foldl
            (\y currentBuffer ->
                case ( slope v1 v0, slope v2 v0 ) of
                    ( Just slopeV1V0, Just slopeV2V0 ) ->
                        let
                            dy =
                                toFloat (y - v0.y)

                            startX =
                                v0.x + round (dy * slopeV1V0)

                            endX =
                                v0.x + round (dy * slopeV2V0)
                        in
                        List.range startX endX
                            |> List.foldl
                                (\x currentCurrentBuffer ->
                                    renderTrianglePoint
                                        ( x, y )
                                        ( v0, c0 )
                                        ( v1, c1 )
                                        ( v2, c2 )
                                        currentCurrentBuffer
                                )
                                currentBuffer

                    _ ->
                        currentBuffer
            )
            buf


mapThree : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapThree f ( a0, a1, a2 ) =
    ( f a0, f a1, f a2 )


mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle =
    mapThree


sort2 : (a -> comparable) -> ( a, a ) -> ( a, a )
sort2 f ( a, b ) =
    if f a <= f b then
        ( a, b )

    else
        ( b, a )


{-| Taken from <https://www.reddit.com/r/programminghorror/comments/fpu16c/nicest_way_to_sort_3_numbers/>
-}
sort3 : (a -> comparable) -> ( a, a, a ) -> ( a, a, a )
sort3 f (( a, b, c ) as triplet) =
    let
        ( fa, fb, fc ) =
            mapThree f triplet
    in
    if fa < fb then
        if fa < fc then
            if fb < fc then
                ( a, b, c )

            else
                ( a, c, b )

        else
            ( c, a, b )

    else if fa > fc then
        if fb > fc then
            ( c, b, a )

        else
            ( b, c, a )

    else
        ( b, a, c )


barycentric : Vec3 -> ( Vec3, Vec3, Vec3 ) -> ( Float, Float, Float )
barycentric p ( a, b, c ) =
    let
        v0 : Vec3
        v0 =
            Vec3.sub b a

        v1 : Vec3
        v1 =
            Vec3.sub c a

        v2 : Vec3
        v2 =
            Vec3.sub p a

        d00 =
            Vec3.dot v0 v0

        d01 =
            Vec3.dot v0 v1

        d11 =
            Vec3.dot v1 v1

        d20 =
            Vec3.dot v2 v0

        d21 =
            Vec3.dot v2 v1

        denom =
            d00 * d11 - d01 * d01

        v =
            (d11 * d20 - d01 * d21) / denom

        w =
            (d00 * d21 - d01 * d20) / denom

        u =
            1.0 - v - w
    in
    ( u, v, w )


slope : Vec3i -> Vec3i -> Maybe Float
slope v w =
    let
        -- TODO: I'm now calculating dx/dy, why is this fine?
        denom =
            v.y - w.y
    in
    if denom == 0 then
        Nothing

    else
        Just (toFloat (v.x - w.x) / toFloat denom)
