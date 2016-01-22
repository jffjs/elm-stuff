module Player where

import Color
import Graphics.Collage exposing (Form, filled, move, ngon)

type alias Position = (Int, Int)
type alias Player = { pos: Position, health: Int }

initialPlayer : Player
initialPlayer = { pos = (-100, -100), health = 0 }

makePlayer : Int -> Float -> Int -> Player
makePlayer w y health =
  { pos = ( w // 2 , round y), health = health }

renderPlayer : (Float, Float) -> Maybe Player -> Form
renderPlayer (w, h) player' =
  let
    player = Maybe.withDefault initialPlayer player'
    translatePos (x, y) =
      (toFloat x - w/2, h/2 - toFloat y)
  in
    ngon 3 10
         |> filled Color.red
         |> move (translatePos player.pos)
