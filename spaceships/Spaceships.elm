module Spaceships where


import Graphics.Collage exposing (collage)
import Graphics.Element exposing (Element)
import Mouse
import Signal
import Signal.Extra exposing ((<~), (~))
import Time exposing (Time, second)
import Window

import Enemies exposing (Enemy, makeEnemy, renderEnemies, updateEnemies)
import Player exposing (Player, initialPlayer, makePlayer, renderPlayer, updatePlayer)
import Stars exposing (Star, makeStarField, renderStars, updateStarField)

type alias Position = (Int, Int)
type Input = Tick Time | MouseX Int | NewEnemy Enemy
type GameState = NewGame | Started | GameOver
type alias Game =
  { state: GameState
  , stars: List Star
  , player: Maybe Player
  , enemies: List Enemy
  }

newEnemyInterval : Time
newEnemyInterval = 2 * second

initialGame : Game
initialGame = { state = NewGame, stars = [], player = Nothing, enemies = [] }


step : (Input, (Int, Int)) -> Game -> Game
step (input, (w, h)) game =
  let player = Maybe.withDefault initialPlayer game.player
  in
    case (game.state, input) of
      (NewGame, Tick t) ->
        {game
          | state = Started
          , player = Just <| makePlayer w (toFloat h - 80) 1
          , stars = makeStarField w h 500 (round t)}
      (Started, Tick t) ->
        {game
          | stars = updateStarField h t game.stars
          , enemies = updateEnemies h t game.enemies}
      (Started, MouseX x) ->
        {game | player = Just <| updatePlayer x player}
      (Started, NewEnemy enemy) ->
        {game | enemies = enemy :: game.enemies}
      (_, _) ->
        game


render : (Int, Int) -> Game -> Element
render (w', h') game =
  let
    (w, h) = (toFloat w', toFloat h')
  in
    collage w' h'
              <| List.concat [ ( renderStars (w, h) game.stars )
                             , ( renderEnemies (w, h) game.enemies)
                             , [ renderPlayer (w, h) game.player ]
                             ]


delta : Signal Time
delta = Time.fps 60


timer : Signal Input
timer = Signal.sampleOn delta <| (\n -> Tick <| n / 10) <~ delta


mouseXSignal : Signal Input
mouseXSignal = (\x -> MouseX x) <~ Mouse.x


newEnemySignal : Signal Input
newEnemySignal =
  let newEnemy w t =
        NewEnemy <| makeEnemy w t
  in
    newEnemy <~ Window.width ~ Time.every newEnemyInterval

inputSignal : Signal Input
inputSignal = Signal.mergeMany [ timer
                               , mouseXSignal
                               , newEnemySignal
                               ]


gameSignal : Signal Game
gameSignal = Signal.foldp step initialGame <| (,) <~ inputSignal ~ Window.dimensions


main : Signal Element
main = Signal.map2 render Window.dimensions gameSignal
