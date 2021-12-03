module Raster exposing
    ( mapTriangle
    , renderLine
    , renderTriangle
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

import AltMath.Vector3 as Vec3 exposing (Vec3)
import Renderer exposing (Buffer, Color, Triangle)


renderTriangle : Triangle { position : Vec3 } -> Color -> Buffer -> Buffer
renderTriangle triangle color buffer =
    let
        sortedTriangle =
            triangle
                |> sort3 (\attribute -> attribute.position.y)

        ( v0, v1, v2 ) =
            mapTriangle .position sortedTriangle
    in
    if v0.y == v1.y then
        -- Natural flat top
        let
            ( actualV0, actualV1 ) =
                sort2 .x ( v0, v1 )
        in
        buffer
            |> renderFlatTopTriangle actualV0 actualV1 v2 color

    else if v1.y == v2.y then
        -- Natural flat bottom
        let
            ( actualV1, actualV2 ) =
                sort2 .x ( v1, v2 )
        in
        buffer
            |> renderFlatBottomTriangle v0 actualV1 actualV2 color

    else
        let
            alpha =
                (v1.y - v0.y) / (v2.y - v0.y)

            vi =
                Vec3.add v0 (Vec3.scale alpha (Vec3.sub v2 v0))
        in
        if v1.x < vi.x then
            buffer
                |> renderFlatBottomTriangle v0 v1 vi color
                |> renderFlatTopTriangle v1 vi v2 color

        else
            buffer
                |> renderFlatBottomTriangle v0 vi v1 color
                |> renderFlatTopTriangle vi v1 v2 color


renderFlatTopTriangle : Vec3 -> Vec3 -> Vec3 -> Color -> Buffer -> Buffer
renderFlatTopTriangle v0 v1 v2 color buffer =
    let
        m0 =
            (v2.x - v0.x) / (v2.y - v0.y)

        m1 =
            (v2.x - v1.x) / (v2.y - v1.y)

        yStart =
            ceiling (v0.y - 0.5)

        yEnd =
            ceiling (v2.y - 0.5)
    in
    List.range yStart (yEnd - 1)
        |> List.foldl
            (\y currentBuffer ->
                let
                    px0 =
                        m0 * (toFloat y + 0.5 - v0.y) + v0.x

                    px1 =
                        m1 * (toFloat y + 0.5 - v1.y) + v1.x

                    xStart =
                        ceiling (px0 - 0.5)

                    xEnd =
                        ceiling (px1 - 0.5)
                in
                List.range xStart (xEnd - 1)
                    |> List.foldl
                        (\x currentCurrentBuffer ->
                            Renderer.setPixel x y color currentCurrentBuffer
                        )
                        currentBuffer
            )
            buffer


renderFlatBottomTriangle : Vec3 -> Vec3 -> Vec3 -> Color -> Buffer -> Buffer
renderFlatBottomTriangle v0 v1 v2 color buffer =
    let
        -- calculate slopes in screen space
        s1 =
            (v1.x - v0.x) / (v1.y - v0.y)

        s2 =
            (v2.x - v0.x) / (v2.y - v0.y)

        -- calculate start and end scanlines
        yStart =
            ceiling (v0.y - 0.5)

        yEnd =
            -- The scanline AFTER the last line drawn
            ceiling (v2.y - 0.5)
    in
    List.range yStart (yEnd - 1)
        |> List.foldl
            (\y currentBuffer ->
                let
                    -- caluclate start and end points
                    -- add 0.5 to y value because we're calculating based on pixel CENTERS
                    px0 =
                        v0.x + (s1 * (toFloat y + 0.5 - v0.y))

                    px1 =
                        v0.x + (s2 * (toFloat y + 0.5 - v0.y))

                    -- calculate start and end pixels
                    xStart =
                        ceiling (px0 - 0.5)

                    xEnd =
                        -- the pixel AFTER the last pixel drawn
                        ceiling (px1 - 0.5)
                in
                List.range xStart (xEnd - 1)
                    |> List.foldl
                        (\x currentCurrentBuffer ->
                            currentCurrentBuffer
                                |> Renderer.setPixel x y color
                        )
                        currentBuffer
            )
            buffer



---- MISC ----


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



----------


renderLine : { start : Vec3, end : Vec3 } -> Color -> Buffer -> Buffer
renderLine { start, end } color buffer =
    let
        dx =
            end.x - start.x
    in
    if dx == 0 then
        renderVerticalLine (round start.x) (round start.y) (round end.y) color buffer

    else
        buffer


renderVerticalLine x y0 y1 color buffer =
    List.range y0 y1
        |> List.foldl
            (\y buf ->
                Renderer.setPixel x y color buf
            )
            buffer
