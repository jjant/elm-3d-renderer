module Misc exposing
    ( for
    , interpolate2
    , interpolate3
    , mapThree
    , unwrap
    ,sort2,sort3
    )

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


mapThree : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapThree f ( a0, a1, a2 ) =
    ( f a0, f a1, f a2 )

sort2 : (a -> comparable) -> ( a, a ) -> ( a, a )
sort2 f ( a, b ) =
    if f a <= f b then
        ( a, b )

    else
        ( b, a )


{-| Taken from <https://www.reddit.com/r/programminghorror/comments/fpu16c/nicest_way_to_sort_3_numbers/>
-}
sort3 : (a -> comparable) -> ( a, a, a ) -> ( a, a, a )
sort3 f (( a, b, c ) as triplet) =
    let
        ( fa, fb, fc ) =
            mapThree f triplet
    in
    if fa < fb then
        if fa < fc then
            if fb < fc then
                ( a, b, c )

            else
                ( a, c, b )

        else
            ( c, a, b )

    else if fa > fc then
        if fb > fc then
            ( c, b, a )

        else
            ( b, c, a )

    else
        ( b, a, c )

