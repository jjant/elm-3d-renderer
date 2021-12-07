module Misc exposing (interpolate2, interpolate3, unwrap)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3)


unwrap : String -> Maybe a -> a
unwrap message mA =
    case mA of
        Just a ->
            a

        Nothing ->
            Debug.todo message


interpolate2 : Float -> Vec2 -> Vec2 -> Vec2
interpolate2 t from to =
    Vec2.add from (Vec2.scale t (Vec2.sub to from))


interpolate3 : Float -> Vec3 -> Vec3 -> Vec3
interpolate3 t from to =
    Vec3.add from (Vec3.scale t (Vec3.sub to from))
