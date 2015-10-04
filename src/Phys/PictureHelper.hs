module Phys.PictureHelper(translate) where

import qualified Graphics.Gloss.Data.Picture as P
import Phys.Point
import Control.Lens((^.))

translate :: Point -> P.Picture -> P.Picture
translate pos pic = P.translate (pos ^. _x) (pos ^. _y) pic
