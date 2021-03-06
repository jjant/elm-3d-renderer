module ElmGL exposing (render)

import AltMath.Vector3 as Vec3 exposing (Vec3)
import Array
import Color exposing (black)
import Html exposing (Html)
import Html.Attributes as Html
import Raster
import Renderer exposing (Buffer, Color, Entity)


renderToBuffer : Entity uniforms attributes varyings -> Buffer -> Buffer
renderToBuffer { uniforms, mesh, impl, pixelShader } buffer =
    let
        ndcTransform =
            Renderer.ndcToScreen buffer
    in
    mesh
        |> cullTriangles
        |> Renderer.transformMesh ndcTransform
        |> List.foldl
            (\tri buf -> Raster.renderTriangle impl tri uniforms pixelShader buf)
            buffer


render : { width : Int, height : Int, pixelSize : Int } -> Entity uniforms attributes varyings -> Html msg
render { width, height, pixelSize } entity =
    let
        buffer =
            renderToBuffer entity
                (Renderer.init
                    { width = width
                    , height = height
                    , color = black
                    }
                )
    in
    Html.div
        [ Html.style "display" "block"
        , Html.style "width" (String.fromInt (pixelSize * buffer.width) ++ "px")

        -- , Html.style "height" (String.fromInt (pixelSize * buffer.width) ++ "px")
        , Html.style "line-height" "0"
        , Html.style "padding" "12px"
        ]
        (buffer.data
            |> Array.map
                (\color ->
                    Html.div
                        [ Html.class "pixel"
                        , Html.style "width" (String.fromInt pixelSize ++ "px")
                        , Html.style "height" (String.fromInt pixelSize ++ "px")
                        , Html.style "background-color" (aColor color)
                        ]
                        []
                )
            |> Array.toList
        )


aColor : Color -> String
aColor c =
    "rgb("
        ++ String.fromFloat c.x
        ++ ", "
        ++ String.fromFloat c.y
        ++ ", "
        ++ String.fromFloat c.z
        ++ ")"



---- CULL BACKFACES ----


cullTriangles triangleList =
    triangleList
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
