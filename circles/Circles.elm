module Circles where


import CirclesModel exposing (..)
import CirclesView exposing (..)
import List exposing ((::), map)
import Mouse
import Signal exposing (Signal, filter, foldp, sampleOn)
import Signal.Extra exposing ((<~), (~))
import Time exposing (Time, fps, timestamp)
import Debug



clockSignal : Signal Time
clockSignal = fst <~ timestamp (fps 50)


type MousePress = Up Time | Down

mousePressSignal : Signal MousePress
mousePressSignal =
  let
    acc (ts, isDown) (stateTs, stateIsDown) =
      if isDown then
        (ts, isDown)
      else
        (ts - stateTs, isDown)
    toMousePress (t, isDown) =
      if isDown then Down else Up t
  in
    toMousePress <~ (foldp acc (0, False) <| timestamp Mouse.isDown)

mouseUpSignal : Signal MousePress
mouseUpSignal = filter (\press ->
                          case press of
                            Up _ -> True
                            Down -> False)
                       (Up 0) mousePressSignal


mouseDownTimeSignal : Signal Time
mouseDownTimeSignal =
  let
    mouseDownTime press =
      case press of
        Up t -> t
        Down -> 0
  in
    mouseDownTime <~ mouseUpSignal

clickPositionsSignal : Signal (Int, Int)
clickPositionsSignal = sampleOn mouseUpSignal Mouse.position


inBoxClickPositionsSignal : Int -> Int -> Signal (Int, Int)
inBoxClickPositionsSignal w h =
  let positionInBox pos = fst pos <= w && snd pos <= h
  in
    filter positionInBox (0, 0) clickPositionsSignal


creationTimeSignal : Int -> Int -> Signal (Time, Time)
creationTimeSignal w h =
    timestamp <| sampleOn (inBoxClickPositionsSignal w h) mouseDownTimeSignal


newCircleSpecSignal : Int -> Int -> Signal CircleSpec
newCircleSpecSignal w h =
  makeCircleSpec <~ creationTimeSignal w h


newCircleSignal : Int -> Int -> Signal Circle
newCircleSignal w h =
    let makeCircle (x,y) spec = { position = { x = x, y = y }, circleSpec = spec }
    in
        makeCircle
            <~ inBoxClickPositionsSignal w h
            ~ newCircleSpecSignal w h


allCirclesSpecSignal : Int -> Int -> Signal (List Circle)
allCirclesSpecSignal w h =
    foldp (::) [] (newCircleSignal w h)


computeCoordinate : Int -> Int -> Float -> Float -> Int
computeCoordinate startingPointCoordinate boxSize velocity time =
    let distance = startingPointCoordinate + round(velocity * time / 1000)
        distanceMod = distance % boxSize
        distanceDiv = distance // boxSize
  in
      if (distanceDiv % 2 == 0)
          then distanceMod
          else boxSize - distanceMod


positionedCircle : Int -> Int -> Float -> Circle -> Circle
positionedCircle w h time circle =
    let {position, circleSpec} = circle
        {radius, xv, yv, creationTime} = circleSpec
        relativeTime = time - creationTime
        boxSizeX = w - radius*2
        boxSizeY = h - radius*2
        x = radius + computeCoordinate (position.x-radius) boxSizeX (toFloat xv) relativeTime
        y = radius + computeCoordinate (position.y-radius) boxSizeY (toFloat yv) relativeTime
    in
        { position = { x=x, y=y }, circleSpec = circleSpec }


positionedCircles : Int -> Int -> Float -> List Circle -> List Circle
positionedCircles w h time circles =
    map (positionedCircle w h time) circles


circlesSignal : Int -> Int -> Signal (List Circle)
circlesSignal w h = positionedCircles w h <~ clockSignal
                                           ~ allCirclesSpecSignal w h


main =
  let main' w h = view w h <~ circlesSignal w h
  in
    main' 400 400
