module Vec3i exposing (Vec3i, fromVec, toVec, vec3i)

import AltMath.Vector3 exposing (Vec3)


type alias Vec3i =
    { x : Int, y : Int, z : Int }


{-| TODO: Remove this function, we should probably _never_ use something like this directly.
(Losing precision and important data)
-}
fromVec : Vec3 -> Vec3i
fromVec v =
    vec3i (round v.x) (round v.y) (round v.z)


toVec : Vec3i -> Vec3
toVec { x, y, z } =
    { x = toFloat x
    , y = toFloat y
    , z = toFloat z
    }


vec3i : Int -> Int -> Int -> Vec3i
vec3i =
    Vec3i
