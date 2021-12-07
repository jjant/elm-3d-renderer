module Main exposing (main)

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 as Vec3 exposing (vec3)
import Array exposing (..)
import Browser
import Browser.Events
import Color exposing (black, blue, green, red)
import Examples.ShaderToy as ShaderToy exposing (Uniforms, Varyings)
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



-- cube : Entity (Vertex Varyings)
-- cube =
--     let
--         frontFace =
--             [ ( { position = vec3 -1 -1 -1, varyings = { color = red } }
--               , { position = vec3 -1 1 -1, varyings = { color = green } }
--               , { position = vec3 1 1 -1, varyings = { color = blue } }
--               )
--             , ( { position = vec3 -1 -1 -1, varyings = { color = red } }
--               , { position = vec3 1 1 -1, varyings = { color = blue } }
--               , { position = vec3 1 -1 -1, varyings = { color = green } }
--               )
--             ]
--         topFace =
--             transformEntity (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) frontFace
--         bottomFace =
--             transformEntity (Mat4.makeRotate (-pi / 2) (vec3 1 0 0)) frontFace
--         rightFace =
--             transformEntity (Mat4.makeRotate (-pi / 2) (vec3 0 1 0)) frontFace
--         leftFace =
--             transformEntity (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) frontFace
--         backFace =
--             transformEntity (Mat4.makeRotate pi (vec3 0 1 0)) frontFace
--     in
--     []
--         ++ topFace
--         ++ rightFace
--         ++ leftFace
--         ++ bottomFace
--         ++ backFace
--         ++ frontFace


view : Model -> Html Msg
view model =
    let
        angle =
            model.t

        transform =
            Mat4.makeRotate angle (Vec3.normalize (vec3 1 2 1))
                |> Mat4.mul (Mat4.makeScale3 0.5 0.5 0.5)
                |> Mat4.mul (Mat4.makeTranslate3 0 0 20)

        -- entity =
        --     cube
        --         |> transformEntity transform
        wholeScreen =
            [ ( { position = vec3 -1 -1 5, varyings = { fragCoord = vec2 0 0 } }
              , { position = vec3 -1 1 5, varyings = { fragCoord = vec2 0 height } }
              , { position = vec3 1 1 5, varyings = { fragCoord = vec2 width height } }
              )
            , ( { position = vec3 -1 -1 5, varyings = { fragCoord = vec2 0 0 } }
              , { position = vec3 1 1 5, varyings = { fragCoord = vec2 width height } }
              , { position = vec3 1 -1 5, varyings = { fragCoord = vec2 width 0 } }
              )
            ]

        uniforms =
            { iTime = model.t
            , iResolution = vec2 width height
            }
    in
    mainDiv
        [ Html.text ""
        , renderBuffer (renderEntity uniforms wholeScreen initBuffer)
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


renderEntity : Uniforms -> Entity (Vertex Varyings) -> Buffer -> Buffer
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
                    impl
                    tri
                    uniforms
                    ShaderToy.pixelShader
                    buf
            )
            buffer


impl : Impl Varyings
impl =
    { add = \v1 v2 -> { fragCoord = Vec2.add v1.fragCoord v2.fragCoord }
    , sub = \v1 v2 -> { fragCoord = Vec2.sub v1.fragCoord v2.fragCoord }
    , interpolate = \t v1 v2 -> { fragCoord = Misc.interpolate2 t v1.fragCoord v2.fragCoord }
    , scale = \s v -> { fragCoord = Vec2.scale s v.fragCoord }
    }


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
