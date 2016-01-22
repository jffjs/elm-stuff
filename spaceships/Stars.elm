module Stars where

import Color
import Graphics.Collage exposing (Form, filled, rect, move)
import Random
import Time exposing (Time)

type alias Position = (Int, Int)
type alias Star = { pos: Position, size: Int }

star : Int -> Int -> Int -> Star
star x y size = { pos = (x, y), size = size }


randomStar : Int -> Int -> Random.Seed -> (Star, Random.Seed)
randomStar w h seed =
  let seed1 =  seed
      (x, seed2) = Random.generate (Random.int 0 w) seed1
      (y, seed3) = Random.generate (Random.int 0 h) seed2
      (size, seed4) = Random.generate (Random.int 1 4) seed3
  in
    (star x y size, seed4)


makeStarField : Int -> Int -> Int -> Int -> List Star
makeStarField w h density seed =
  let makeStarField' remaining seed stars =
        let
          (star, nextSeed) = randomStar w h seed
        in
          if remaining <= 0
          then stars
          else makeStarField' (remaining - 1) nextSeed
                 <| star :: stars
  in
    makeStarField' density (Random.initialSeed seed) []


updateStarField : Int -> Time -> List Star -> List Star
updateStarField h deltaT starField =
  let updateStar star =
        let
          y' = (snd star.pos) + round deltaT
          y = if y' > h then 0 else y'
        in
          { star | pos = (fst star.pos, y) }
  in
    List.map updateStar starField


renderStars : (Float, Float) -> List Star -> List Form
renderStars (w, h) stars =
  let
    translatePos (x, y) =
      (toFloat x - w/2, h/2 - toFloat y)
    renderStar star =
      rect (toFloat star.size) (toFloat star.size)
        |> filled Color.white
        |> move (translatePos star.pos)
  in
    (rect w h |> filled Color.black)
      :: List.map renderStar stars

