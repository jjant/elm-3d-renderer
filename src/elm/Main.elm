module Main exposing (main)

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Array exposing (..)
import Browser
import Browser.Events
import Color exposing (black, blue, red)
import Html exposing (Html, div)
import Html.Attributes as Html
import Raster
import Renderer exposing (Buffer, Color, Entity)


width : number
width =
    100


height : number
height =
    100


pixelSize : number
pixelSize =
    5


cube : Entity { position : Vec3 }
cube =
    let
        frontFace =
            [ ( ( { position = vec3 -1 -1 -1 }, { position = vec3 1 1 -1 }, { position = vec3 -1 1 -1 } ), red )
            , ( ( { position = vec3 -1 -1 -1 }, { position = vec3 1 -1 -1 }, { position = vec3 1 1 -1 } ), blue )
            ]

        topFace =
            transformEntity (Mat4.makeRotate (pi / 2) (vec3 1 0 0)) frontFace

        bottomFace =
            transformEntity (Mat4.makeRotate (-pi / 2) (vec3 1 0 0)) frontFace

        rightFace =
            transformEntity (Mat4.makeRotate (-pi / 2) (vec3 0 1 0)) frontFace

        leftFace =
            transformEntity (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) frontFace

        backFace =
            transformEntity (Mat4.makeRotate pi (vec3 0 1 0)) frontFace
    in
    frontFace
        ++ topFace
        ++ rightFace
        ++ leftFace
        ++ bottomFace
        ++ backFace


view : Model -> Html Msg
view model =
    let
        ndcTransform =
            Renderer.ndcToScreen initBuffer

        angle =
            model.t

        transform =
            Mat4.makeRotate angle (Vec3.normalize (vec3 2 1 1))
                |> Mat4.mul (Mat4.makeScale3 0.5 0.5 0.5)
                |> Mat4.mul ndcTransform

        entity =
            cube
                |> transformEntity transform
    in
    mainDiv
        [ Html.text ""
        , renderBuffer (renderEntity entity initBuffer)
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


renderEntity : Entity { position : Vec3 } -> Buffer -> Buffer
renderEntity tris buffer =
    tris
        |> List.foldl (\( tri, color ) buf -> Raster.renderTriangleLines tri color buf) buffer


renderBuffer : Buffer -> Html msg
renderBuffer buffer =
    div
        [ Html.style "display" "block"
        , Html.style "width" (String.fromInt (pixelSize * buffer.width) ++ "px")
        , Html.style "line-height" "0"
        , Html.style "padding" "12px"
        ]
        (buffer.data
            |> Array.map
                (\color ->
                    div
                        [ Html.class "pixel"
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


transformEntity : Mat4 -> Entity { position : Vec3 } -> Entity { position : Vec3 }
transformEntity mat entity =
    entity
        |> List.map
            (\( tri, color ) ->
                ( Raster.mapTriangle (\{ position } -> { position = Mat4.transform mat position }) tri
                , color
                )
            )
