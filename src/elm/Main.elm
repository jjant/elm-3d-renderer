module Main exposing (main)

import AltMath.Matrix4 as Mat4
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Array exposing (..)
import Browser
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes as Html
import Renderer exposing (Color, Triangle)


someTriangle : Triangle { position : Vec3i, color : Color }
someTriangle =
    ( { position = vec3i 15 5 0, color = green }
    , { position = vec3i 10 40 0, color = red }
    , { position = vec3i 30 40 0, color = blue }
    )


someTriangle2 : Triangle { position : Vec3i, color : Color }
someTriangle2 =
    ( { position = vec3i 15 5 0, color = green }
    , { position = vec3i 5 15 0, color = red }
    , { position = vec3i 30 40 0, color = blue }
    )


view : Model -> Html Msg
view model =
    let
        angle =
            model.t

        rotation =
            Mat4.makeRotate angle (vec3 0 0 1)

        rotatedTriangle =
            mapTriangle
                (\att ->
                    { att
                        | position =
                            att.position
                                |> toVec
                                |> Mat4.transform rotation
                                |> fromVec
                    }
                )
                someTriangle
    in
    mainDiv
        [ renderBuffer (renderTriangle someTriangle initBuffer)
        , renderBuffer (renderTriangle someTriangle2 initBuffer)
        ]



---- API ----


vec3i : Int -> Int -> Int -> Vec3i
vec3i =
    Vec3i


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

        renderTopHalf buf =
            List.range v0.y v1.y
                |> List.foldl
                    (\y currentBuffer ->
                        let
                            ( startX_, endX_ ) =
                                computeXs y v0 v1 v2
                        in
                        List.range startX_ endX_
                            |> List.foldl
                                (\x currentCurrentBuffer ->
                                    renderTrianglePoint ( x, y ) currentCurrentBuffer
                                )
                                currentBuffer
                    )
                    buf

        renderBottomHalf buf =
            List.range v1.y v2.y
                |> List.foldl
                    (\y currentBuffer ->
                        let
                            dyV0 =
                                toFloat (y - v0.y)

                            dyV1 =
                                toFloat (y - v1.y)

                            ( startX_, endX_ ) =
                                computeXs y v1 v2 v0
                        in
                        List.range startX_ endX_
                            |> List.foldl
                                (\x currentCurrentBuffer ->
                                    renderTrianglePoint ( x, y ) currentCurrentBuffer
                                )
                                currentBuffer
                    )
                    buf

        renderTrianglePoint ( x, y ) buf =
            let
                ( u, v, w ) =
                    -- TODO: Compute Z properly!!!!
                    -- Very important
                    -- Look something up about "perspective correction"
                    barycentric (vec3 (toFloat x) (toFloat y) 0) (mapThree toVec ( v0, v1, v2 ))

                color =
                    Vec3.scale u c0
                        |> Vec3.add (Vec3.scale v c1)
                        |> Vec3.add (Vec3.scale w c2)
            in
            buf
                |> setPixel x y color
    in
    buffer
        |> renderTopHalf
        -- |> renderBottomHalf
        |> setPixel v0.x v0.y white
        |> setPixel v1.x v1.y white
        |> setPixel v2.x v2.y white


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


computeXs : Int -> Vec3i -> Vec3i -> Vec3i -> ( Int, Int )
computeXs y p0 p1 p2 =
    let
        dy_ =
            toFloat (y - p0.y)

        _ =
            Debug.log "\ny" y

        slopeP1P0 =
            slope p1 p0
                |> Debug.log "slopeP1P0"

        slopeP0P2 =
            slope p0 p2
                |> Debug.log "slopeP0P2"

        startX =
            slopeP1P0
                |> Maybe.map (\slope_ -> p0.x + round (slope_ * dy_))
                |> Maybe.withDefault (min p0.x p1.x)

        endX =
            slopeP0P2
                |> Maybe.map (\slope_ -> p0.x + round (slope_ * dy_))
                |> Maybe.withDefault (max p0.x p1.x)
    in
    Debug.log "(startX, endX)" <|
        case slopeP1P0 of
            Just sP1P0 ->
                case slopeP0P2 of
                    Just sP0P2 ->
                        ( p0.x + round (sP1P0 * dy_), p0.x + round (sP0P2 * dy_) )

                    Nothing ->
                        Debug.todo "TODO: Figure out what to do in this case"

            Nothing ->
                sort2 identity ( p0.x, p1.x )


type alias Buffer =
    Array Color


setPixel : Int -> Int -> Color -> Buffer -> Buffer
setPixel x y c buffer =
    computeIndex x y
        |> Maybe.map (\index -> Array.set index c buffer)
        |> Maybe.withDefault buffer


computeIndex : Int -> Int -> Maybe Int
computeIndex x y =
    if 0 <= x && x < width && 0 <= y && y < height then
        Just (x + y * width)

    else
        Nothing


aColor : Color -> String
aColor c =
    "rgb("
        ++ String.fromFloat c.x
        ++ ", "
        ++ String.fromFloat c.y
        ++ ", "
        ++ String.fromFloat c.z
        ++ ")"


renderBuffer : Buffer -> Html msg
renderBuffer buffer =
    let
        pixelSize =
            10
    in
    div
        [ Html.style "display" "block"
        , Html.style "width" (String.fromFloat (pixelSize * width) ++ "px")
        , Html.style "line-height" "0"
        , Html.style "padding" "12px"
        ]
        (buffer
            |> Array.map
                (\color ->
                    div
                        [ Html.style "background-color" (aColor color)
                        , Html.style "width" (String.fromFloat pixelSize ++ "px")
                        , Html.style "height" (String.fromFloat pixelSize ++ "px")
                        , Html.style "display" "inline-block"
                        , Html.style "margin" "0"
                        ]
                        []
                )
            |> Array.toList
        )


initBuffer : Buffer
initBuffer =
    Array.repeat (width * height) black


width : number
width =
    50


height : number
height =
    50



-------------


type alias Flags =
    ()


type alias Model =
    { t : Float }


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { t = 0 }, Cmd.none )


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick dtMilliseconds ->
            ( { model | t = model.t + dtMilliseconds / 1000 }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Browser.Events.onAnimationFrameDelta Tick
    Sub.none



---- USEFUL MISC ----


type alias Vec3i =
    { x : Int, y : Int, z : Int }


mainDiv : List (Html msg) -> Html msg
mainDiv =
    div
        [ Html.style "display" "flex"
        , Html.style "justify-content" "center"
        , Html.style "padding" "36px 0"
        , Html.style "background-color" "#aaa"
        , Html.style "height" "100%"
        ]


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


range : Int -> Int -> Array Int
range minY maxY =
    let
        len =
            maxY - minY + 1
    in
    Array.initialize len ((+) minY)


white : Color
white =
    vec3 255 255 255


black : Color
black =
    vec3 0 0 0


red : Color
red =
    vec3 255.0 0 0


green : Color
green =
    vec3 0 255.0 0


blue : Color
blue =
    vec3 0 0 255.0


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


mapThree : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapThree f ( a0, a1, a2 ) =
    ( f a0, f a1, f a2 )


toVec : Vec3i -> Vec3
toVec { x, y, z } =
    { x = toFloat x
    , y = toFloat y
    , z = toFloat z
    }


mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle =
    mapThree


{-| TODO: Remove this function, we should probably _never_ use something like this directly.
(Losing precision and important data)
-}
fromVec : Vec3 -> Vec3i
fromVec v =
    vec3i (round v.x) (round v.y) (round v.z)
