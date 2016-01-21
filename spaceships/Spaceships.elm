module Spaceships where


import Graphics.Element exposing (show)
import Random exposing (generate, int, initialSeed)
import Signal exposing (Signal, filter, foldp, sampleOn)
import Signal.Extra exposing ((<~), (~))
import Signal.Time exposing (startTime)
import Time exposing (Time, fps, timestamp)
import Debug

width : Int
width = 600
height : Int
height = 800
type alias Star = { x: Int, y: Int, size: Int }

star : Int -> Int -> Int -> Star
star x y size = { x = x, y = y, size = size }

randomStar : Int -> Star
randomStar seed =
  let seed1 = initialSeed <| seed
      (x, seed2) = generate (int 0 width) seed1
      (y, seed3) = generate (int 0 height) seed2
      (size, _) = generate (int 1 4) seed3
  in
    star x y size

starField : Int -> Time -> List Star
starField density time =
  let starField' remaining stars =
        if remaining <= 0
        then stars
        else starField' (remaining - 1)
             <| randomStar (round time - remaining) :: stars
  in
    starField' density []

clockSignal : Signal Time
clockSignal = fps 50

-- updateStarField : Time -> List Star
-- updateStarField tick = [star 1 2 3]

-- starFieldSignal : Signal List Star
-- starFieldSignal =
--   foldp updateStarField (starField 250) clockSignal

main = show "Hello"
