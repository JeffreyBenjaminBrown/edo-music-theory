-- | This compares grid arrangements of EDOs,
-- by showing how far your hand must stray from the
-- column containing note 0 in order to reach
-- (the approximations to) any of the first few harmonics.
-- For instance, to reveal that the only arrangement of 87-edo
-- with a maximum deviation of less than 7 is
-- the one that puts 10\87 between each column:
-- > myPrint $ compareGrids_filt 6 87
-- (10,4,9,[1,-2,0,0,1])

-- | TODO : It ought to consider how far the note is
-- from any pitch enharmonic to 0. For instance, in 87-edo,
-- if the grid uses a space of 10\87, then note 87 lies in
-- column 9 at row -3, so a -3 in the 7th harmonic would be
-- right next to it, and therefore even easier to reach
-- than a 0.
--
-- Here's one solution: Let the L1-nearest octave equivalent
-- of the origin (0) pitch be P, and suppose it lies in
-- column C and row R. Then to the output of `algienments`
-- subtract some fraction of R, where the fraction is
-- equal to C' / C, where C' is the column containing the harmonic.
--
-- Unfortunately it's possible that the best way to play
-- an alignment is not along that origin-octave vector,
-- but a different one.

{-# LANGUAGE ScopedTypeVariables #-}

module Grid where

import           Control.Lens
import           Data.Ratio
import           Data.Fixed

import Lib
import Util


myMod :: Int -> Int -> Int
myMod x b =
  let m = mod' x b
  in if m > round (fromIntegral b/2)
     then m-b else m

alignments :: Int -> Int -> [Int]
alignments spacing edo =
  map ( flip myMod spacing
        . fromIntegral
        . (^. _1)
        . best (fromIntegral edo) )
  [3/2,5/4,7/4,11/8,13/8]

compareGrids :: Int -> [(Int,Int,Int,[Int])]
compareGrids edo = let
  width n = round $ fromIntegral edo / fromIntegral n
  als n = alignments n edo
  in [ (n, sum $ map abs $ als n, width n, als n)
     | n <- [6..20]]

compareGrids_filt :: Int -> Int -> [(Int,Int,Int,[Int])]
compareGrids_filt maxSummedDev =
  filter ((< maxSummedDev) . (^. _2))
  . compareGrids
