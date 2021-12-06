module TexVec exposing (TexVec, interpolate)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import AltMath.Vector3 as Vec3 exposing (Vec3)


type alias TexVec =
    { position : Vec3
    , tc : Vec2
    }


interpolate : { t : Float, from : TexVec, to : TexVec } -> TexVec
interpolate { from, to, t } =
    { position = Vec3.add from.position (Vec3.scale t (Vec3.sub to.position from.position))
    , tc = Vec2.add from.tc (Vec2.scale t (Vec2.sub to.tc from.tc))
    }
