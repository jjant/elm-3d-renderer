module TexVec exposing
    ( TexVec
    , add
    , interpolate
    , scale
    , sub
    )

import Misc
import Vec2 exposing (Vec2)
import Vec3 exposing (Vec3)


type alias TexVec =
    { position : Vec3
    , tc : Vec2
    }


interpolate : { t : Float, from : TexVec, to : TexVec } -> TexVec
interpolate { from, to, t } =
    { position = Misc.interpolate3 t from.position to.position
    , tc = Misc.interpolate2 t from.tc to.tc
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
