module TexVec exposing
    ( TexVec
    , add
    , interpolate
    , scale
    , sub
    )

import AltMath.Vector2 as Vec2 exposing (Vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3)


type alias TexVec =
    { position : Vec3
    , tc : Vec2
    }


interpolate2 : Float -> Vec2 -> Vec2 -> Vec2
interpolate2 t from to =
    Vec2.add from (Vec2.scale t (Vec2.sub to from))


interpolate3 : Float -> Vec3 -> Vec3 -> Vec3
interpolate3 t from to =
    Vec3.add from (Vec3.scale t (Vec3.sub to from))


interpolate : { t : Float, from : TexVec, to : TexVec } -> TexVec
interpolate { from, to, t } =
    { position = interpolate3 t from.position to.position
    , tc = interpolate2 t from.tc to.tc
    }


scale : Float -> TexVec -> TexVec
scale t texVec =
    { position = Vec3.scale t texVec.position
    , tc = Vec2.scale t texVec.tc
    }


sub : TexVec -> TexVec -> TexVec
sub a b =
    { position = Vec3.sub a.position b.position
    , tc = Vec2.sub a.tc b.tc
    }


add : TexVec -> TexVec -> TexVec
add a b =
    { position = Vec3.add a.position b.position
    , tc = Vec2.add a.tc b.tc
    }
