{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Gloss(play,Display(..))
import Graphics.Gloss.Data.Color(white,red,black)
import Graphics.Gloss.Data.Picture(Picture,circle,color,pictures)
import ClassyPrelude
import Control.Lens(to,(&),(.~),(+~),makeLenses,(^.))
import Numeric.Lens(dividing)
import Phys.Number
import Phys.PictureHelper
import Phys.Tick
import Phys.Point
import Phys.Mass

data Matter = Matter {
    _matterPos :: Point
  , _matterMass :: Mass
  } deriving(Show)

$(makeLenses ''Matter)                

data Ball = Ball {
    _ballPos :: Point
  , _ballVel :: Point
  , _ballMass :: Mass
  } deriving(Show)

$(makeLenses ''Ball)                

data World = World {
    _worldBall :: Ball
  , _worldMatters :: [Matter]
  , _worldTicks :: Tick
  } deriving(Show)

$(makeLenses ''World)

worldToPicture :: World -> Picture
worldToPicture w =
  let
    ball = translate (w ^. worldBall . ballPos) (color white (circle 4))
    matters = map (\m -> translate (m ^. matterPos) (color red (circle (m ^. matterMass . to abs . dividing 1000)))) (w ^. worldMatters)
  in
    pictures $ [ball] <> matters

data Derivative = Derivative {
    _derivPos :: Point
  , _derivVel :: Point
  } deriving(Show)

$(makeLenses ''Derivative)                 

gravityForceMag :: Mass -> Mass -> Number -> Number
gravityForceMag m1 m2 dist = (m1 * m2) / (dist*dist)

matterForce :: Ball -> Matter -> Point
matterForce b m = 
  let
    ballToMatter = m ^. matterPos - b ^. ballPos
    dist = norm ballToMatter
    matMass = m ^. matterMass
    bMass = b ^. ballMass
    gravityForce = (ballToMatter ^/ dist) ^* gravityForceMag matMass bMass dist
  in
    gravityForce ^/ bMass

acceleration :: World -> Point
acceleration s = sum (matterForce (s ^. worldBall) <$> (s ^. worldMatters))

rk4Evaluate :: World -> Tick -> Derivative -> Derivative
rk4Evaluate s dt d =
  let
    x = s ^. worldBall . ballPos + d ^. derivPos ^* dt
    v = s ^. worldBall . ballVel + d ^. derivVel ^* dt
    tempWorld = s & worldBall . ballPos .~ x & worldBall . ballVel .~ v
  in
    Derivative{
        _derivPos = v
      , _derivVel = acceleration tempWorld
      }

rk4Step :: World -> Tick -> World
rk4Step s dt =
  let
    a = rk4Evaluate s 0 (Derivative (point 0 0) (point 0 0))
    b = rk4Evaluate s (dt*0.5) a
    c = rk4Evaluate s (dt*0.5) b
    d = rk4Evaluate s dt c
    mean4 v0 v1 v2 v3 = (1/6) * (v0 + 2 * (v1 + v2) + v3)
    dxdt = mean4 (a ^. derivPos) (b ^. derivPos) (c ^. derivPos) (d ^. derivPos)
    dvdt = mean4 (a ^. derivVel) (b ^. derivVel) (c ^. derivVel) (d ^. derivVel)
  in
    s & worldBall . ballPos +~ dxdt ^* dt & worldBall . ballVel +~ dvdt ^* dt

stepWorld :: Tick -> World -> World
stepWorld d w = rk4Step w d

main :: IO ()
main =
  let
    windowSize = (640,480)
    windowPosition = (0,0)
    displayMode = InWindow "window title" windowSize windowPosition
    backgroundColor = black
    fps = 60
    initialBall =
      Ball{
          _ballPos = point (-100) 50
        , _ballVel = point 30 0
        , _ballMass = 1
      }
    initialMatters =
      [ Matter {
          _matterPos = point (-100) 0
        , _matterMass = 30000
        },
        Matter {
          _matterPos = point 100 0
        , _matterMass = 30000
        }
      ]
    initialWorld =
      World{
            _worldBall = initialBall
          , _worldMatters = initialMatters
          , _worldTicks = 0
          }
    handleEvent event world = world
  in
    play displayMode backgroundColor fps initialWorld worldToPicture handleEvent stepWorld
 
