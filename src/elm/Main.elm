module Main exposing (main)

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 as Vec3 exposing (vec3)
import Array exposing (..)
import Browser
import Browser.Events
import ElmGL
import Examples.Cube as Cube
import Examples.ShaderToy as ShaderToy
import Examples.ShaderToy.CreationBySilexars as CreationBySilexars
import Examples.ShaderToy.PhantomStarByCineShader as PhantomStarByCineShader
import Html exposing (Html, div)
import Html.Attributes as Html


width : number
width =
    50


height : number
height =
    50


pixelSize : number
pixelSize =
    10


resolution : Vec2
resolution =
    vec2 width height


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
        [ Cube.entity transform
            |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }

        -- , PhantomStarByCineShader.entity { iTime = model.t, iResolution = resolution }
        --     |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }
        , CreationBySilexars.entity { iTime = model.t, iResolution = resolution }
            |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }
        ]


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
