module Solver where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type LightName = String
type SwitchName = String

data Switch lightName switchName = Switch {switchName::switchName, lights::[lightName]}

solve :: (Ord lightName,Ord switchName) => [lightName] -> [Switch lightName switchName] -> [switchName]
solve ls ss = 
  Set.toList $
  solve' [(Set.singleton (switchName s), Set.fromList (lights s)) | s <- ss]
         (Set.fromList ls)

-- xor is addition as well as subtraction modulo 2
xor :: Ord a => Set a -> Set a -> Set a
xor x y = Set.difference (Set.union x y) (Set.intersection x y)

-- Gaussian elimination
solve' :: (Ord l,Ord s) => [(Set s, Set l)] -> Set l -> Set s
solve' us xs
  | Set.null xs = Set.empty
  | otherwise = fst u `xor` solve' us' xs'
  where
  x = Set.findMin xs
  Just u = find ((x `Set.member`) . snd) us
  us' = [if x `Set.member` snd v then (fst u `xor` fst v, snd u `xor` snd v) else v | v<-us]
  xs' = xs `xor` snd u
