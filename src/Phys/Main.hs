{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Gloss(play,Display(..))
import Graphics.Gloss.Data.Color(green,white,red,black)
import Data.Monoid(Any(..))
import Data.Foldable(foldMap)
import Graphics.Gloss.Data.Picture(Picture,circle,color,pictures,thickCircle,scale,text)
import ClassyPrelude hiding (lines,foldMap)
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
  , _ballRadius :: Number
  } deriving(Show)

$(makeLenses ''Ball)                

data Goal = Goal {
    _goalPos :: Point
  , _goalRadius :: Number
  } deriving(Show)

$(makeLenses ''Goal)           

data World = World {
    _worldBall :: Ball
  , _worldMatters :: [Matter]
  , _worldTicks :: Tick
  , _worldGoal :: Goal
  } deriving(Show)

data GameState =
    GameStateRunning World
  | GameStateFinished World
  | GameStateGameOver World

$(makeLenses ''World)

matterToPicture :: Tick -> Matter -> Picture
matterToPicture t m =
  let 
    radiusBase = m ^. matterMass . to abs . dividing 1000
    radius = radiusBase + 5 * (abs (sin (5 * t)))
    smallLine circlePoint = (circlePoint ^* radius,circlePoint ^* (radius+10))
  in
    color red (lines ((smallLine . angle . (+t) . (*(2*pi)) . (/10)) <$> [0..20]))

ballToPicture :: Ball -> Picture
ballToPicture b = color white (circle (b ^. ballRadius))

goalToPicture :: Goal -> Picture
goalToPicture g = color green (circle (g ^. goalRadius))

stateToPicture :: GameState -> Picture
stateToPicture (GameStateRunning w) = worldToPicture w
stateToPicture (GameStateFinished w) = worldToPicture w <> (scale 0.1 0.1 $ color white $ translate (point 0 0) $ text "You did it!")
stateToPicture (GameStateGameOver w) = worldToPicture w <> (scale 0.1 0.1 $ color white $ translate (point 0 0) $ text "Far out dude, game over!")

worldToPicture :: World -> Picture
worldToPicture w =
  let
    ball = translate (w ^. worldBall . ballPos) (ballToPicture (w ^. worldBall))
    goal = translate (w ^. worldGoal . goalPos) (goalToPicture (w ^. worldGoal))
    matters = (\m -> translate (m ^. matterPos) (matterToPicture (w ^. worldTicks) m)) <$> (w ^. worldMatters)
  in
    ball <> goal <> pictures matters

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

collisionWithGoal :: World -> Bool
collisionWithGoal w = distance (w ^. worldBall . ballPos) (w ^. worldGoal . goalPos) < (w ^. worldBall . ballRadius + w ^. worldGoal . goalRadius)

outsideField :: World -> Bool
outsideField w = getAny (foldMap Any (((> 1000) . abs) <$> (w ^. worldBall . ballPos)))

collisionWithObstacle _ = False              

stepState :: Tick -> GameState -> GameState
stepState d (GameStateRunning w) =
  let
    physicsWorld = rk4Step w (10 * d)
  in
    if collisionWithGoal w
    then GameStateFinished physicsWorld
    else if outsideField w || collisionWithObstacle w
         then GameStateGameOver w
         else GameStateRunning (physicsWorld & worldTicks +~ d)
stepState _ s = s

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
        , _ballRadius = 4
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
    goal =
      Goal {
          _goalPos = point 300 0
        , _goalRadius = 40
      }
    initialWorld =
      World{
            _worldBall = initialBall
          , _worldMatters = initialMatters
          , _worldTicks = 0
          , _worldGoal = goal             
          }
    handleEvent event world = world
  in
    play displayMode backgroundColor fps (GameStateRunning initialWorld) stateToPicture handleEvent stepState
