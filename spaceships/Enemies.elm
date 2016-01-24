module Enemies where

import Color
import Graphics.Collage exposing (Form, filled, move, ngon, rotate)
import Random exposing (Seed, initialSeed, generate, int)
import Time exposing (Time)

type alias Position = (Int, Int)
type alias Enemy = { pos: Position, health: Int }

makeEnemy : Int -> Float -> Enemy
makeEnemy w seed =
  let seed1 = initialSeed <| round seed
      (x, _) = generate (int 20 (w - 20)) seed1
  in
    { pos = (x, -20), health = 1}

updateEnemies : Int -> Time -> List Enemy -> List Enemy
updateEnemies h deltaT enemies =
  let
    updateEnemy enemy =
      {enemy | pos = (fst enemy.pos, (snd enemy.pos) + round deltaT )}
    filterEnemy enemy =
      if enemy.health <= 0 || (snd enemy.pos) > h
      then False
      else True
  in
    List.map updateEnemy enemies
      |> List.filter filterEnemy


renderEnemies : (Float, Float) -> List Enemy -> List Form
renderEnemies (w, h) enemies =
  let
    translatePos (x, y) =
      (toFloat x - w/2, h/2 - toFloat y)
    renderEnemy enemy =
      ngon 3 15
        |> filled Color.green
        |> move (translatePos enemy.pos)
        |> rotate (degrees -90)
  in
    List.map renderEnemy enemies
