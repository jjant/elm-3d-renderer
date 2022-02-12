module Raster exposing
    ( renderLine
    , renderTriangle
    , renderTriangleLines
    )

{-| Coordinates in this module are pixel-coords following th standard convention:
The x-axis going right, y-axis going down, and the origin at the top left:

              |-----  width -----|
            (0,0)
       ---    o----> x
        |     |
        |     |
        |     v y
      height
        |
        |
        |
       ---

-}

import Color exposing (white)
import Misc
import Renderer exposing (Buffer, Color, Impl, PixelShader, Triangle, Vertex)
import Vec3 exposing (Vec3)



------- RASTERIZE TRIANGLES -------


renderTriangle :
    Impl varyings
    -> Triangle (Vertex varyings)
    -> uniforms
    -> PixelShader uniforms varyings
    -> Buffer
    -> Buffer
renderTriangle impl triangle uniforms pixelShader buffer =
    let
        sortedTriangle =
            triangle
                |> Misc.sort3 (\attribute -> attribute.position.y)

        ( v0, v1, v2 ) =
            sortedTriangle
    in
    if v0.position.y == v1.position.y then
        -- Natural flat top
        let
            ( actualV0, actualV1 ) =
                ( v0, v1 )
                    |> Misc.sort2 (\v -> v.position.x)
        in
        buffer
            |> renderFlatTopTriangleTex impl ( actualV0, actualV1, v2 ) uniforms pixelShader

    else if v1.position.y == v2.position.y then
        -- Natural flat bottom
        let
            ( actualV1, actualV2 ) =
                ( v1, v2 )
                    |> Misc.sort2 (\v -> v.position.x)
        in
        buffer
            |> renderFlatBottomTriangleTex impl ( v0, actualV1, actualV2 ) uniforms pixelShader

    else
        let
            alpha =
                (v1.position.y - v0.position.y) / (v2.position.y - v0.position.y)

            vi =
                interpolate impl alpha v0 v2
        in
        if v1.position.x < vi.position.x then
            buffer
                |> renderFlatBottomTriangleTex impl ( v0, v1, vi ) uniforms pixelShader
                |> renderFlatTopTriangleTex impl ( v1, vi, v2 ) uniforms pixelShader

        else
            buffer
                |> renderFlatBottomTriangleTex impl ( v0, vi, v1 ) uniforms pixelShader
                |> renderFlatTopTriangleTex impl ( vi, v1, v2 ) uniforms pixelShader


renderFlatTopTriangleTex :
    Impl varyings
    -> Triangle (Vertex varyings)
    -> uniforms
    -> PixelShader uniforms varyings
    -> Buffer
    -> Buffer
renderFlatTopTriangleTex impl ( v0, v1, v2 ) pixelShader buffer =
    let
        delta_y =
            v2.position.y - v0.position.y

        dv0 =
            scale impl (1 / delta_y) (sub impl v2 v0)

        dv1 =
            scale impl (1 / delta_y) (sub impl v2 v1)

        itEdge1 =
            v1
    in
    renderFlatTriangleTex impl ( v0, v1, v2 ) ( dv0, dv1 ) itEdge1 pixelShader buffer


renderFlatBottomTriangleTex :
    Impl varyings
    -> Triangle (Vertex varyings)
    -> uniforms
    -> PixelShader uniforms varyings
    -> Buffer
    -> Buffer
renderFlatBottomTriangleTex impl (( v0, v1, v2 ) as triangle) uniforms pixelShader buffer =
    let
        delta_y =
            v2.position.y - v0.position.y

        dv0 =
            scale impl (1 / delta_y) (sub impl v1 v0)

        dv1 =
            scale impl (1 / delta_y) (sub impl v2 v0)

        itEdge1 =
            v0
    in
    renderFlatTriangleTex impl triangle ( dv0, dv1 ) itEdge1 uniforms pixelShader buffer


renderFlatTriangleTex :
    Impl varyings
    -> Triangle (Vertex varyings)
    -> ( Vertex varyings, Vertex varyings )
    -> Vertex varyings
    -> uniforms
    -> PixelShader uniforms varyings
    -> Buffer
    -> Buffer
renderFlatTriangleTex impl ( v0, v1, v2 ) ( dv0, dv1 ) itEdge1_ uniforms pixelShader buffer =
    let
        yStart =
            ceiling (v0.position.y - 0.5)

        yEnd =
            ceiling (v2.position.y - 0.5)

        itEdge0 =
            add impl v0 (scale impl (toFloat yStart + 0.5 - v0.position.y) dv0)

        itEdge1 =
            add impl itEdge1_ (scale impl (toFloat yStart + 0.5 - v0.position.y) dv1)
    in
    ( yStart, yEnd - 1 )
        |> Misc.for
            (\y buf ->
                let
                    newItEdge0 =
                        add impl itEdge0 (scale impl (toFloat (y - yStart)) dv0)

                    newItEdge1 =
                        add impl itEdge1 (scale impl (toFloat (y - yStart)) dv1)

                    xStart =
                        ceiling (newItEdge0.position.x - 0.5)

                    xEnd =
                        ceiling (newItEdge1.position.x - 0.5)

                    dtcLine =
                        impl.scale
                            (1 / (newItEdge1.position.x - newItEdge0.position.x))
                            (impl.sub newItEdge1.varyings newItEdge0.varyings)

                    itcLine =
                        impl.add newItEdge0.varyings
                            (impl.scale (toFloat xStart + 0.5 - newItEdge0.position.x) dtcLine)
                in
                ( xStart, xEnd - 1 )
                    |> Misc.for
                        (\x newBuf ->
                            let
                                newDtcLine =
                                    impl.add itcLine (impl.scale (toFloat (x - xStart)) dtcLine)
                            in
                            newBuf
                                |> Renderer.setPixel x y (pixelShader uniforms newDtcLine)
                        )
                        buf
            )
            buffer



---- MISC ----
----- WIREFRAME DRAWING------


renderTriangleLines : Triangle (Vertex varyings) -> Color -> Buffer -> Buffer
renderTriangleLines triangle color buffer =
    let
        ( v0, v1, v2 ) =
            Misc.sort3 (\v -> v.position.x) triangle
    in
    buffer
        |> renderLine { start = v0.position, end = v1.position } color
        |> renderLine { start = v1.position, end = v2.position } color
        |> renderLine { start = v0.position, end = v2.position } color
        |> Renderer.setPixel (ceiling (v0.position.x - 0.5)) (ceiling (v0.position.y - 0.5)) white
        |> Renderer.setPixel (ceiling (v1.position.x - 0.5)) (ceiling (v1.position.y - 0.5)) white
        |> Renderer.setPixel (ceiling (v2.position.x - 0.5)) (ceiling (v2.position.y - 0.5)) white


renderLine : { start : Vec3, end : Vec3 } -> Color -> Buffer -> Buffer
renderLine { start, end } color buffer =
    let
        dx =
            end.x - start.x

        dy =
            end.y - start.y

        slope =
            dy / dx

        xStart =
            ceiling (start.x - 0.5)

        xEnd =
            -- The "scanrow" AFTER the last line drawn
            ceiling (end.x - 0.5)
    in
    if dx == 0 then
        renderVerticalLine (round start.x) (round start.y) (round end.y) color buffer

    else
        ( xStart, xEnd - 1 )
            |> Misc.for
                (\x buf ->
                    let
                        y =
                            start.y + (slope * (toFloat x + 0.5 - start.x))
                    in
                    Renderer.setPixel x (ceiling (y - 0.5)) color buf
                )
                buffer


renderVerticalLine : Int -> Int -> Int -> Color -> Buffer -> Buffer
renderVerticalLine x y0 y1 color buffer =
    List.range y0 y1
        |> List.foldl
            (\y buf ->
                Renderer.setPixel x y color buf
            )
            buffer


interpolate : Impl varyings -> Float -> Vertex varyings -> Vertex varyings -> Vertex varyings
interpolate impl t from to =
    { position = Misc.interpolate3 t from.position to.position
    , varyings = impl.interpolate t from.varyings to.varyings
    }


scale : Impl varyings -> Float -> Vertex varyings -> Vertex varyings
scale impl s v =
    { position = Vec3.scale s v.position
    , varyings = impl.scale s v.varyings
    }


add : Impl varyings -> Vertex varyings -> Vertex varyings -> Vertex varyings
add impl a b =
    { position = Vec3.add a.position b.position
    , varyings = impl.add a.varyings b.varyings
    }


sub : Impl varyings -> Vertex varyings -> Vertex varyings -> Vertex varyings
sub impl a b =
    { position = Vec3.sub a.position b.position
    , varyings = impl.sub a.varyings b.varyings
    }
