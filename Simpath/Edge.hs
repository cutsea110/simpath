module Simpath.Edge where

import Prelude hiding (either)

import Control.Applicative (liftA2)
import qualified Data.Foldable as Foldable
import Data.Function (on)
import Data.Set

type Node = Int
data Edge = Edge { left :: Node, right :: Node } deriving (Show, Eq, Ord)

edge :: Node -> Node -> Edge
edge  = (liftA2 . liftA2) Edge min max

start :: Edge
start = edge 0 1

onBoth :: (a -> a -> b) -> (Node -> a) -> Edge -> b
onBoth f g (Edge l r) = on f g l r

either:: (Node -> Bool) -> Edge -> Bool
either = onBoth (||)

contains :: Node -> Edge -> Bool
contains = either . (==)

modify :: (Node -> Node) -> Edge -> Edge
modify = onBoth edge

opposite :: Node -> Edge -> Node
opposite n (Edge l r) = if l == n then r else l

isOpen :: Edge -> Bool
isOpen = onBoth (/=) id

find :: Node -> Set Edge -> Maybe Edge
find = Foldable.find . contains

connect :: Node -> Node -> Edge -> Edge
connect from to = edge from . opposite to
