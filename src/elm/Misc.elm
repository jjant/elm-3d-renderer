module Misc exposing (for, interpolate2, interpolate3, unwrap)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3)


unwrap : String -> Maybe a -> a
unwrap message mA =
    case mA of
        Just a ->
            a

        Nothing ->
            -- Debug.todo message
            unwrap message mA


interpolate2 : Float -> Vec2 -> Vec2 -> Vec2
interpolate2 t from to =
    Vec2.add from (Vec2.scale t (Vec2.sub to from))


interpolate3 : Float -> Vec3 -> Vec3 -> Vec3
interpolate3 t from to =
    Vec3.add from (Vec3.scale t (Vec3.sub to from))


for : (Int -> b -> b) -> b -> ( Int, Int ) -> b
for func acc ( min, max ) =
    forHelp func acc max min


forHelp : (Int -> b -> b) -> b -> Int -> Int -> b
forHelp func acc max val =
    if val > max then
        acc

    else if max - val > 10 then
        let
            res =
                func (val + 9) (func (val + 8) (func (val + 7) (func (val + 6) (func (val + 5) (func (val + 4) (func (val + 3) (func (val + 2) (func (val + 1) (func val acc)))))))))
        in
        forHelp func res max (val + 10)

    else
        forHelp func (func val acc) max (val + 1)
