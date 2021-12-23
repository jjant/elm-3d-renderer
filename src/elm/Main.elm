module Main exposing (main)

import AltMath.Matrix4 as Mat4 exposing (Mat4)
import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import AltMath.Vector3 as Vec3 exposing (vec3)
import Browser
import Browser.Events
import ElmGL
import Examples.Cube as Cube
import Examples.FirstPerson as FirstPerson
import Examples.ShaderToy as ShaderToy
import Examples.ShaderToy.CreationBySilexars as CreationBySilexars
import Examples.ShaderToy.PhantomStarByCineShader as PhantomStarByCineShader
import Examples.SolidTriangle as SolidTriangle
import Html exposing (Html, div, map)
import Html.Attributes as Html
import Html.Events as Html


type Example
    = None
    | SolidTriangle
    | Cube
    | CreationBySilexars
    | FirstPerson


type ExampleModel
    = NoneModel
    | SolidTriangleModel
    | CubeModel
    | CreationBySilexarsModel
    | FirstPersonModel FirstPerson.Model


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
    mainDiv
        [ exampleDropdown model
        , viewExample model

        -- , Cube.entity { time = model.t }
        --     |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }
        -- , CreationBySilexars.entity { iTime = model.t, iResolution = resolution }
        --     |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }
        ]


viewExample : Model -> Html Msg
viewExample model =
    case model.example of
        NoneModel ->
            Html.text ""

        SolidTriangleModel ->
            SolidTriangle.entity {}
                |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }

        CubeModel ->
            Cube.entity { time = model.t }
                |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }

        CreationBySilexarsModel ->
            CreationBySilexars.entity { iTime = model.t, iResolution = resolution }
                |> ElmGL.render { width = width, height = height, pixelSize = pixelSize }

        FirstPersonModel firstPersonModel ->
            FirstPerson.view firstPersonModel
                |> htmlMap (ExampleMsg << FirstPersonMsg)


exampleDropdown : Model -> Html Msg
exampleDropdown model =
    Html.select
        [ Html.onInput ChangeExample
        , Html.value (toString <| toName <| model.example)
        , Html.style "font-size" "32px"
        , Html.style "position" "absolute"
        , Html.style "top" "40px"
        , Html.style "left" "40px"
        ]
        (List.map (\example -> Html.option [] [ Html.text <| toString example ]) examples)


type alias Flags =
    ()


type alias Model =
    { t : Float
    , example : ExampleModel
    }


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { t = 0
      , example = NoneModel
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | ChangeExample String
    | ExampleMsg ExampleMsg


type ExampleMsg
    = FirstPersonMsg FirstPerson.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.example ) of
        ( Tick dtMilliseconds, _ ) ->
            -- ( { model | t = model.t + 16 / 1000 }, Cmd.none )
            ( { model | t = model.t + dtMilliseconds / 1000 }, Cmd.none )

        ( ChangeExample exampleName, _ ) ->
            let
                example =
                    exampleName
                        |> fromString
                        |> Maybe.withDefault (toName model.example)
            in
            ( { model | example = initExample example }, Cmd.none )

        ( ExampleMsg (FirstPersonMsg firstPersonMsg), FirstPersonModel firstPersonModel ) ->
            let
                ( example, cmd ) =
                    FirstPerson.update firstPersonMsg firstPersonModel
            in
            ( { model | example = FirstPersonModel example }
            , Cmd.map (ExampleMsg << FirstPersonMsg) cmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


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
    Sub.batch
        [ Sub.none
        , Browser.Events.onAnimationFrameDelta Tick
        , exampleSubscriptions model
        ]



---- USEFUL MISC ----


mainDiv : List (Html msg) -> Html msg
mainDiv =
    div
        [ Html.style "display" "flex"
        , Html.style "justify-content" "center"
        , Html.style "align-items" "center"
        , Html.style "flex-wrap" "wrap"

        -- , Html.style "background-color" "#333"
        , Html.style "background-color" "rgba(0,0,0,0.95)"
        , Html.style "height" "100vh"
        , Html.style "font-size" "0"
        ]


fromString : String -> Maybe Example
fromString name =
    case name of
        "None" ->
            Just None

        "SolidTriangle" ->
            Just SolidTriangle

        "Cube" ->
            Just Cube

        "CreationBySilexars" ->
            Just CreationBySilexars

        "FirstPerson" ->
            Just FirstPerson

        _ ->
            Nothing


toString : Example -> String
toString example =
    case example of
        None ->
            "None"

        SolidTriangle ->
            "SolidTriangle"

        Cube ->
            "Cube"

        CreationBySilexars ->
            "CreationBySilexars"

        FirstPerson ->
            "FirstPerson"


examples : List Example
examples =
    [ None
    , SolidTriangle
    , Cube
    , CreationBySilexars
    , FirstPerson
    ]


initExample : Example -> ExampleModel
initExample example =
    case example of
        None ->
            NoneModel

        SolidTriangle ->
            SolidTriangleModel

        Cube ->
            CubeModel

        CreationBySilexars ->
            CreationBySilexarsModel

        FirstPerson ->
            -- TODO
            FirstPersonModel (Tuple.first FirstPerson.init)


toName : ExampleModel -> Example
toName model =
    case model of
        NoneModel ->
            None

        SolidTriangleModel ->
            SolidTriangle

        CubeModel ->
            Cube

        CreationBySilexarsModel ->
            CreationBySilexars

        FirstPersonModel _ ->
            -- TODO
            FirstPerson


htmlMap : (a -> msg) -> Html a -> Html msg
htmlMap =
    map


exampleSubscriptions : Model -> Sub Msg
exampleSubscriptions model =
    case model.example of
        FirstPersonModel firstPersonModel ->
            FirstPerson.subscriptions firstPersonModel
                |> Sub.map (ExampleMsg << FirstPersonMsg)

        _ ->
            Sub.none
