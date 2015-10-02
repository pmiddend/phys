{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Gloss(play,Display(..))
import Graphics.Gloss.Data.Color(white,black)
import Graphics.Gloss.Data.Picture(Picture(..))
import ClassyPrelude
import Linear.V2
import Control.Lens(makeLenses)

type FloatType = Float

type Point = V2 FloatType

data World = World {
    _worldBall :: Point
  }

$(makeLenses ''World)

main :: IO ()
main =
  let
    displayMode = InWindow "window title" (100,100) (0,0)
    backgroundColor = black
    fps = 60
    initialWorld = 1
    toPicture world = Color white (Circle 30)
    handleEvent event world = 1
    stepFunction stepDur world = 1
  in
    play displayMode backgroundColor fps initialWorld toPicture handleEvent stepFunction
 
