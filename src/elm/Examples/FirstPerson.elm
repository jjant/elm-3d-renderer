module Examples.FirstPerson exposing (Model, Msg, init, main, subscriptions, update, view)

{-
   Try adding the ability to crouch or to land on top of the crate.
-}

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Color
import ElmGL
import Examples.CrateTexture as CrateTexture
import Html exposing (Html, div, text)
import Html.Attributes as Html exposing (height, style, width)
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)
import Mat4 exposing (Mat4)
import Misc
import Renderer exposing (Color, Entity, Impl, Mesh, PixelShader, VertexShader)
import Task
import Texture exposing (Texture)
import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (Vec3, vec3)


type alias Model =
    { keys : Keys
    , size : { width : Float, height : Float }
    , person : Person
    }


type alias Person =
    { position : Vec3
    , velocity : Vec3
    }


type Msg
    = KeyChange Bool Int
    | Animate Float
      -- | Resize Int Int
    | GetViewport Viewport


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    }


main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


eyeLevel : Float
eyeLevel =
    2


init : ( Model, Cmd Msg )
init =
    ( { person = Person (vec3 0 eyeLevel -10) (vec3 0 0 0)
      , keys = Keys False False False False False
      , size = { width = 0, height = 0 }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onKeyDown (Decode.map (KeyChange True) keyCode)
        , onKeyUp (Decode.map (KeyChange False) keyCode)

        -- , onResize Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        KeyChange on code ->
            ( { model | keys = keyFunc on code model.keys }, Cmd.none )

        GetViewport { viewport } ->
            ( { model
                | size =
                    { width = viewport.width
                    , height = viewport.height
                    }
              }
            , Cmd.none
            )

        -- Resize width height ->
        --     ( { model
        --         | size =
        --             { width = toFloat width
        --             , height = toFloat height
        --             }
        --       }
        --     , Cmd.none
        --     )
        Animate dt ->
            ( { model
                | person =
                    model.person
                        |> move model.keys
                        |> gravity (dt / 500)
                        |> physics (dt / 500)
              }
            , Cmd.none
            )


keyFunc : Bool -> Int -> Keys -> Keys
keyFunc on keyCode keys =
    case keyCode of
        32 ->
            { keys | space = on }

        37 ->
            { keys | left = on }

        39 ->
            { keys | right = on }

        38 ->
            { keys | up = on }

        40 ->
            { keys | down = on }

        _ ->
            keys


move : Keys -> Person -> Person
move { left, right, up, down, space } person =
    let
        direction a b =
            if a == b then
                0

            else if a then
                1

            else
                -1

        vy =
            if space then
                2

            else
                person.velocity.y
    in
    if person.position.y <= eyeLevel then
        { person
            | velocity =
                vec3 (direction left right) vy (direction up down)
        }

    else
        person


physics : Float -> Person -> Person
physics dt person =
    let
        position =
            Vec3.add person.position (Vec3.scale dt person.velocity)
    in
    { person
        | position =
            if position.y < eyeLevel then
                Vec3.setY eyeLevel position

            else
                position
    }


gravity : Float -> Person -> Person
gravity dt person =
    if person.position.y > eyeLevel then
        { person
            | velocity =
                Vec3.setY
                    (person.velocity.y - 2 * dt)
                    person.velocity
        }

    else
        person



-- View


myTexture : Texture
myTexture =
    CrateTexture.crateTexture


mainDiv : List (Html msg) -> Html msg
mainDiv =
    div
        [ Html.style "display" "flex"
        , Html.style "justify-content" "center"
        , Html.style "align-items" "center"
        , Html.style "flex-wrap" "wrap"
        , Html.style "background-color" "black"
        , Html.style "height" "100vh"
        , Html.style "font-size" "0"
        ]


width =
    100


height =
    100


view : Model -> Html Msg
view { size, person } =
    mainDiv
        [ ElmGL.render
            { width = width
            , height = height
            , pixelSize = 10
            }
            (scene
                { width = width
                , height = height
                }
                person
                myTexture
            )
        , div
            [ style "position" "absolute"
            , style "font-family" "monospace"
            , style "color" "white"
            , style "text-align" "center"
            , style "left" "20px"
            , style "right" "20px"
            , style "top" "20px"
            ]
            [ text message ]
        ]


message : String
message =
    "Walk around with a first person perspective.\n"
        ++ "Arrows keys to move, space bar to jump."


scene : { width : Float, height : Float } -> Person -> Texture -> Entity Uniforms Vertex Varyings
scene size person texture =
    let
        perspective =
            Mat4.mul
                (Mat4.perspective { fovy = 45, aspect = size.width / size.height, zNear = 0.01, zFar = 100 })
                (Mat4.lookAt { eye = person.position, centerOfAttention = Vec3.add person.position Vec3.k, up = Vec3.j }
                    |> Misc.unwrap "FirstPerson.scene"
                )
    in
    { vertexShader = vertexShader
    , pixelShader = pixelShader
    , mesh = crate
    , uniforms =
        { texture = texture
        , perspective = perspective
        }
    , impl = impl
    }


impl : Impl Varyings
impl =
    { add = \v1 v2 -> { vcoord = Vec2.add v1.vcoord v2.vcoord }
    , sub = \v1 v2 -> { vcoord = Vec2.sub v1.vcoord v2.vcoord }
    , interpolate = \t v1 v2 -> { vcoord = Misc.interpolate2 t v1.vcoord v2.vcoord }
    , scale = \s v -> { vcoord = Vec2.scale s v.vcoord }
    }



-- Mesh


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


crate : Mesh Vertex
crate =
    [ ( 0, 0 ), ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, -90 ) ]
        |> List.concatMap rotatedSquare


rotatedSquare : ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedSquare ( angleXZ, angleYZ ) =
    let
        transformMat =
            Mat4.mul
                (Mat4.rotate (degrees angleXZ) Vec3.j)
                (Mat4.rotate (degrees angleYZ) Vec3.i)

        transform vertex =
            { vertex
                | position =
                    Mat4.transformPoint transformMat vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
    List.map transformTriangle square


square : List ( Vertex, Vertex, Vertex )
square =
    let
        topLeft =
            Vertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
    in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]



-- Shaders


type alias Uniforms =
    { texture : Texture
    , perspective : Mat4
    }


type alias Varyings =
    { vcoord : Vec2 }


vertexShader : VertexShader Uniforms Vertex Varyings
vertexShader { perspective } { position, coord } =
    { position =
        Mat4.transformPoint perspective position
    , varyings =
        { vcoord = coord
        }
    }


pixelShader : PixelShader Uniforms Varyings
pixelShader { texture } { vcoord } =
    Texture.get vcoord.x vcoord.y texture
