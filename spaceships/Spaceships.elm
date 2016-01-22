module Spaceships where


import Graphics.Collage exposing (collage)
import Graphics.Element exposing (Element)
import Signal
import Signal.Extra exposing ((<~), (~))
import Time exposing (Time)
import Window

import Stars exposing (Star, makeStarField, renderStars, updateStarField)
import Player exposing (Player, makePlayer, renderPlayer)

type alias Position = (Int, Int)
type Input = Tick Time
type GameState = NewGame | Started | GameOver
type alias Game =
  { state: GameState
  , stars: List Star
  , player: Maybe Player
  }

initialGame : Game
initialGame = { state = NewGame, stars = [], player = Nothing }


step : (Input, (Int, Int)) -> Game -> Game
step (input, (w, h)) game =
  case (game.state, input) of
    (NewGame, Tick t) ->
      {game
        | state = Started
        , player = Just <| makePlayer w (toFloat h - 80) 1
        , stars = makeStarField w h 500 (round t)}
    (Started, Tick t) ->
      {game | state = Started, stars = updateStarField h t game.stars}
    (GameOver, _) ->
      game


delta : Signal Time
delta = Time.fps 60
timer : Signal Input
timer = Signal.sampleOn delta <| (\n -> Tick <| n / 10) <~ delta


render : (Int, Int) -> Game -> Element
render (w', h') game =
  let
    (w, h) = (toFloat w', toFloat h')
  in
    collage w' h'
              <| renderPlayer (w, h) game.player
                   :: renderStars (w, h) game.stars


inputSignal : Signal Input
inputSignal = timer


gameSignal : Signal Game
gameSignal = Signal.foldp step initialGame <| (,) <~ inputSignal ~ Window.dimensions


main : Signal Element
main = Signal.map2 render Window.dimensions gameSignal
