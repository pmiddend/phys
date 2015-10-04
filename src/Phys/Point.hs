module Phys.Point(Point,_x,_y,point,(^*),(*^),norm,(^/)) where

import Linear.V2
import Linear.Vector
import Linear.Metric
import Phys.Number

type Point = V2 Number

point = V2
