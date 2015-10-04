module Phys.PictureHelper(translate,lines,path,line) where

import qualified Graphics.Gloss.Data.Picture as P
import Phys.Point
import Control.Lens((^.))
import ClassyPrelude hiding (lines)

instance Semigroup P.Picture where
  a <> b = P.pictures [a,b]

translate :: Point -> P.Picture -> P.Picture
translate pos pic = P.translate (pos ^. _x) (pos ^. _y) pic

path :: [Point] -> P.Picture
path ps =
  let v2ToPair p = (p ^. _x,p ^. _y)
  in P.line (v2ToPair <$> ps)

line :: (Point,Point) -> P.Picture
line (a,b) = path [a,b]

lines :: [(Point,Point)] -> P.Picture
lines ps = P.pictures (line <$> ps)
