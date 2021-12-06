module Raster exposing
    ( mapTriangle
    , renderLine
    , renderTriangle
    , renderTriangleLines
    , renderTriangleTex
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

import AltMath.Vector2 as Vec2 exposing (Vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3)
import Color exposing (white)
import Renderer exposing (Buffer, Color, Triangle)
import TexVec exposing (TexVec)
import Texture exposing (Texture)


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


renderTriangleLines : Triangle { position : Vec3 } -> Color -> Buffer -> Buffer
renderTriangleLines triangle color buffer =
    let
        ( v0, v1, v2 ) =
            sort3 (\v -> v.position.x) triangle
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
        List.range xStart (xEnd - 1)
            |> List.foldl
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



------- TEXTURE STUFF -------


renderTriangleTex : Triangle TexVec -> Texture -> Buffer -> Buffer
renderTriangleTex triangle texture buffer =
    let
        sortedTriangle =
            triangle
                |> sort3 (\attribute -> attribute.position.y)

        ( v0, v1, v2 ) =
            sortedTriangle
    in
    if v0.position.y == v1.position.y then
        -- Natural flat top
        let
            ( actualV0, actualV1 ) =
                ( v0, v1 )
                    |> sort2 (\v -> v.position.x)
        in
        buffer
            |> renderFlatTopTriangleTex actualV0 actualV1 v2 texture

    else if v1.position.y == v2.position.y then
        -- Natural flat bottom
        let
            ( actualV1, actualV2 ) =
                ( v1, v2 )
                    |> sort2 (\v -> v.position.x)
        in
        buffer
            |> renderFlatBottomTriangleTex v0 actualV1 actualV2 texture

    else
        let
            alpha =
                (v1.position.y - v0.position.y) / (v2.position.y - v0.position.y)

            vi =
                TexVec.interpolate
                    { t = alpha
                    , from = v0
                    , to = v2
                    }
        in
        if v1.position.x < vi.position.x then
            buffer
                |> renderFlatBottomTriangleTex v0 v1 vi texture
                |> renderFlatTopTriangleTex v1 vi v2 texture

        else
            buffer
                |> renderFlatBottomTriangleTex v0 vi v1 texture
                |> renderFlatTopTriangleTex vi v1 v2 texture


renderFlatTopTriangleTex : TexVec -> TexVec -> TexVec -> Texture -> Buffer -> Buffer
renderFlatTopTriangleTex v0 v1 v2 texture buffer =
    let
        m0 =
            (v2.position.x - v0.position.x) / (v2.position.y - v0.position.y)

        m1 =
            (v2.position.x - v1.position.x) / (v2.position.y - v1.position.y)

        yStart =
            ceiling (v0.position.y - 0.5)

        yEnd =
            ceiling (v2.position.y - 0.5)

        tcEdgeL =
            Vec2.add v0.tc
                (Vec2.scale (toFloat yStart + 0.5 - v1.position.y) tcEdgeStepL)

        tcEdgeR =
            Vec2.add v1.tc
                (Vec2.scale (toFloat yStart + 0.5 - v1.position.y) tcEdgeStepR)

        tcBottom =
            v2.tc

        tcEdgeStepL =
            Vec2.sub tcBottom v0.tc
                |> Vec2.scale (1 / (v2.position.y - v0.position.y))

        tcEdgeStepR =
            Vec2.sub tcBottom v1.tc
                |> Vec2.scale (1 / (v2.position.y - v1.position.y))

        texWidth =
            toFloat (Texture.width texture)

        texHeight =
            toFloat (Texture.height texture)

        texClampX =
            texWidth - 1

        texClampY =
            texHeight - 1
    in
    List.range yStart (yEnd - 1)
        |> List.foldl
            (\y currentBuffer ->
                let
                    newTcEdgeL =
                        Vec2.add tcEdgeL (Vec2.scale (toFloat (y - yStart)) tcEdgeStepL)

                    newTcEdgeR =
                        Vec2.add tcEdgeR (Vec2.scale (toFloat (y - yStart)) tcEdgeStepR)

                    px0 =
                        m0 * (toFloat y + 0.5 - v0.position.y) + v0.position.x

                    px1 =
                        m1 * (toFloat y + 0.5 - v1.position.y) + v1.position.x

                    xStart =
                        ceiling (px0 - 0.5)

                    xEnd =
                        ceiling (px1 - 0.5)

                    tcScanStep =
                        Vec2.scale (1 / (px1 - px0)) (Vec2.sub newTcEdgeR newTcEdgeL)

                    tc =
                        Vec2.add newTcEdgeL (Vec2.scale (toFloat xStart + 0.5 - px0) tcScanStep)
                in
                List.range xStart (xEnd - 1)
                    |> List.foldl
                        (\x currentCurrentBuffer ->
                            let
                                newTc =
                                    Vec2.add tc (Vec2.scale (toFloat (x - xStart)) tcScanStep)

                                color =
                                    Texture.get
                                        (round (min (newTc.x * texWidth) texClampX))
                                        (round (min (newTc.y * texHeight) texClampY))
                                        texture
                            in
                            Renderer.setPixel x y color currentCurrentBuffer
                        )
                        currentBuffer
            )
            buffer


renderFlatBottomTriangleTex : TexVec -> TexVec -> TexVec -> Texture -> Buffer -> Buffer
renderFlatBottomTriangleTex v0 v1 v2 texture buffer =
    let
        -- calculate slopes in screen space
        s1 =
            (v1.position.x - v0.position.x) / (v1.position.y - v0.position.y)

        s2 =
            (v2.position.x - v0.position.x) / (v2.position.y - v0.position.y)

        -- calculate start and end scanlines
        yStart =
            ceiling (v0.position.y - 0.5)

        yEnd =
            -- The scanline AFTER the last line drawn
            ceiling (v2.position.y - 0.5)

        tcBottomL =
            v1.tc

        tcBottomR =
            v2.tc

        tcEdgeStepL =
            Vec2.sub tcBottomL v0.tc
                |> Vec2.scale (1 / (v1.position.y - v0.position.y))

        tcEdgeStepR =
            Vec2.sub tcBottomR v0.tc
                |> Vec2.scale (1 / (v1.position.y - v0.position.y))

        -- Pre-stepped coords
        tcEdgeL =
            Vec2.add v0.tc
                (Vec2.scale (toFloat yStart + 0.5 - v0.position.y) tcEdgeStepL)

        tcEdgeR =
            Vec2.add v0.tc
                (Vec2.scale (toFloat yStart + 0.5 - v0.position.y) tcEdgeStepR)

        texWidth =
            toFloat (Texture.width texture)

        texHeight =
            toFloat (Texture.height texture)

        texClampX =
            texWidth - 1

        texClampY =
            texHeight - 1
    in
    List.range yStart (yEnd - 1)
        |> List.foldl
            (\y currentBuffer ->
                let
                    newTcEdgeL =
                        Vec2.add tcEdgeL (Vec2.scale (toFloat (y - yStart)) tcEdgeStepL)

                    newTcEdgeR =
                        Vec2.add tcEdgeR (Vec2.scale (toFloat (y - yStart)) tcEdgeStepR)

                    -- calculate start and end points
                    -- add 0.5 to y value because we're calculating based on pixel CENTERS
                    px0 =
                        v0.position.x + (s1 * (toFloat y + 0.5 - v0.position.y))

                    px1 =
                        v0.position.x + (s2 * (toFloat y + 0.5 - v0.position.y))

                    -- calculate start and end pixels
                    xStart =
                        ceiling (px0 - 0.5)

                    xEnd =
                        -- the pixel AFTER the last pixel drawn
                        ceiling (px1 - 0.5)

                    tcScanStep =
                        Vec2.scale (1 / (px1 - px0)) (Vec2.sub newTcEdgeR newTcEdgeL)

                    tc =
                        Vec2.add newTcEdgeL
                            (Vec2.scale (toFloat xStart + 0.5 - px0) tcScanStep)
                in
                List.range xStart (xEnd - 1)
                    |> List.foldl
                        (\x currentCurrentBuffer ->
                            let
                                newTc =
                                    Vec2.add tc (Vec2.scale (toFloat (x - xStart)) tcScanStep)

                                color =
                                    Texture.get
                                        (round (clamp 0 texClampX (newTc.x * texWidth)))
                                        (round (clamp 0 texClampY (newTc.y * texHeight)))
                                        texture
                            in
                            currentCurrentBuffer
                                |> Renderer.setPixel x y color
                        )
                        currentBuffer
            )
            buffer
