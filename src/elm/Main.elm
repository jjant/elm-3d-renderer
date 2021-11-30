module Main exposing (main)

import AltMath.Matrix4 as Mat4
import AltMath.Vector3 as Vec3 exposing (Vec3, vec3)
import Array exposing (..)
import Browser
import Browser.Events
import Color exposing (black, blue, green, red)
import Html exposing (Html, div)
import Html.Attributes as Html
import Raster
import Renderer exposing (Buffer, Color, Triangle)
import Vec3i exposing (Vec3i, vec3i)


someTriangle : Triangle { position : Vec3i, color : Color }
someTriangle =
    -- ( { position = vec3i 15 5 0, color = green }
    -- , { position = vec3i 10 5 0, color = red }
    -- , { position = vec3i 30 40 0, color = blue }
    -- )
    ( { position = vec3i 15 5 0, color = red }
    , { position = vec3i 10 5 0, color = red }
    , { position = vec3i 30 40 0, color = red }
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
            Raster.mapTriangle
                (\att ->
                    { att
                        | position =
                            att.position
                                |> Vec3i.toVec
                                |> Mat4.transform rotation
                                |> Vec3i.fromVec
                    }
                )
                someTriangle
    in
    mainDiv
        [ Html.text ""
        , renderBuffer (Raster.renderTriangle someTriangle initBuffer)

        -- , renderBuffer (Raster.renderTriangle someTriangle2 initBuffer)
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


renderBuffer : Buffer -> Html msg
renderBuffer buffer =
    let
        pixelSize =
            10
    in
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
    Renderer.init
        { width = 50
        , height = 50
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
subscriptions model =
    -- Browser.Events.onAnimationFrameDelta Tick
    Sub.none



---- USEFUL MISC ----


mainDiv : List (Html msg) -> Html msg
mainDiv =
    div
        [ Html.style "display" "flex"
        , Html.style "justify-content" "center"
        , Html.style "padding" "36px 0"
        , Html.style "background-color" "#aaa"
        , Html.style "height" "100%"
        ]


range : Int -> Int -> Array Int
range minY maxY =
    let
        len =
            maxY - minY + 1
    in
    Array.initialize len ((+) minY)
