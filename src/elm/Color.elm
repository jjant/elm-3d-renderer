module Color exposing (black, blue, green, red, white)

import Renderer exposing (Color)
import Vec3 exposing (vec3)


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
