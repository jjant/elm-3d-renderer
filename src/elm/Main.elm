module Main exposing (main)

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 as Vec3 exposing (vec3)
import Array exposing (..)
import Browser
import Browser.Events
import Color exposing (black, blue, green, red)
import Examples.Cube as Cube
import Examples.ShaderToy as ShaderToy
import Examples.ShaderToy.CreationBySilexars as CreationBySilexars
import Examples.ShaderToy.PhantomStarByCineShader as PhantomStarByCineShader
import Html exposing (Html, div)
import Html.Attributes as Html
import Misc
import Raster
import Renderer exposing (Buffer, Color, Entity, Impl, PixelShader, Vertex)


width : number
width =
    50


height : number
height =
    50


pixelSize : number
pixelSize =
    10


view : Model -> Html Msg
view model =
    let
        angle =
            model.t

        transform =
            Mat4.makeRotate angle (Vec3.normalize (vec3 1 2 1))
                |> Mat4.mul (Mat4.makeScale3 0.5 0.5 0.5)
                |> Mat4.mul (Mat4.makeTranslate3 0 0 20)
    in
    mainDiv
        [ Html.text ""
        , renderBuffer (renderEntity Cube.uniforms (transformEntity transform Cube.mesh) initBuffer)
        ]



---- API ----


aColor : Color -> String
aColor c =
    "rgb("
        ++ String.fromFloat c.x
        ++ ", "
        ++ String.fromFloat c.y
        ++ ", "
        ++ String.fromFloat c.z
        ++ ")"


renderEntity : Cube.Uniforms -> Entity (Vertex Cube.Varyings) -> Buffer -> Buffer
renderEntity uniforms entity buffer =
    let
        ndcTransform =
            Renderer.ndcToScreen buffer
    in
    entity
        |> List.filter
            (\( v0, v1, v2 ) ->
                not
                    ((Vec3.cross
                        (Vec3.sub v1.position v0.position)
                        (Vec3.sub v2.position v0.position)
                        |> Vec3.dot v0.position
                     )
                        >= 0
                    )
            )
        |> transformEntity ndcTransform
        |> List.foldl
            (\tri buf ->
                Raster.renderTriangle
                    Cube.impl
                    tri
                    uniforms
                    Cube.pixelShader
                    buf
            )
            buffer


renderBuffer : Buffer -> Html msg
renderBuffer buffer =
    div
        [ Html.style "display" "block"
        , Html.style "width" (String.fromInt (pixelSize * buffer.width) ++ "px")

        -- , Html.style "height" (String.fromInt (pixelSize * buffer.width) ++ "px")
        , Html.style "line-height" "0"
        , Html.style "padding" "12px"
        ]
        (buffer.data
            |> Array.map
                (\color ->
                    div
                        [ Html.class "pixel"
                        , Html.style "width" (String.fromInt pixelSize ++ "px")
                        , Html.style "height" (String.fromInt pixelSize ++ "px")
                        , Html.style "background-color" (aColor color)
                        ]
                        []
                )
            |> Array.toList
        )


initBuffer : Buffer
initBuffer =
    Renderer.init
        { width = width
        , height = height
        , color = black
        }



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
            -- ( { model | t = model.t + 16 / 1000 }, Cmd.none )
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
subscriptions _ =
    Sub.batch
        [ Sub.none
        , Browser.Events.onAnimationFrameDelta Tick
        ]



---- USEFUL MISC ----


mainDiv : List (Html msg) -> Html msg
mainDiv =
    div
        [ Html.style "display" "flex"
        , Html.style "justify-content" "center"
        , Html.style "align-items" "center"
        , Html.style "flex-wrap" "wrap"
        , Html.style "padding" "36px 0"
        , Html.style "background-color" "#aaa"
        , Html.style "height" "500px"
        , Html.style "font-size" "0"
        ]


transformEntity : Mat4 -> Entity (Vertex varyings) -> Entity (Vertex varyings)
transformEntity mat entity =
    entity
        |> List.map
            (\tri ->
                Raster.mapTriangle (\v -> { v | position = Mat4.transform mat v.position }) tri
            )
