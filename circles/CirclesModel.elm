module CirclesModel where


import Color exposing (Color, rgb)
import Time exposing (Time)
import Random exposing (generate, int, initialSeed)


type alias Position = { x: Int, y: Int }
type alias CircleSpec = {
    radius: Int,
    xv: Int,
    yv: Int,
    col: Color,
    creationTime: Time
  }

type alias Circle = {
    position: Position,
    circleSpec: CircleSpec
  }

setRadius : Float -> Int
setRadius size =
  let
    s = size / 30
  in
    clamp 5 80 <| round s

makeCircleSpec : (Time, Float) -> CircleSpec
makeCircleSpec (time, size) =
  let seed1 = initialSeed (round time)
      radius = setRadius size
      (xv,seed2) = generate (int 10 50) seed1
      (yv,seed3) = generate (int 10 50) seed2
      (r,seed4) = generate (int 10 220) seed3
      (g,seed5) = generate (int 10 220) seed4
      (b,_) = generate (int 10 220) seed5
  in
    { radius = radius
    , xv = xv
    , yv = yv
    , col = rgb r g b
    , creationTime = time
    }
