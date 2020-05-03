{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Lens
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Decimal
import           Data.Ratio
import           Data.Fixed
import           Data.Ord (comparing)

import Util
import Harmonics


minNotes = 30
maxNotes = 60
tols = -- This list can have any length.
  -- It describes the maximum error for the first harmonics.
  (*10) <$> repeat 12
--    example:
--    [ 20 -- approx 3/2 to within 2 cents
--    , 40 -- 5/4 to within 4 cents
--    , 60 -- 7/4
--    , 80 ] -- 11/8

-- | terms: r = ratio
--          d = denominator
--          n = numerator
--          e = edo frac

go = myPrint looking

myPrint :: forall a t. (Foldable t, Show a)
  => t a -> IO ()
myPrint = mapM_ $ putStrLn . show

looking = let
  f (d,errs) =
    ( and $ zipWith (>=) tols $ map abs errs )
  in filter f matrix

matrix :: [(Integer, [Integer])]
matrix =
  [ (d, map (round . (^. _2 . _3)) $ bests d)
  | d <- [minNotes .. maxNotes] ]

bests :: Integer -> [(Rational, (Integer, Integer, Double))]
bests d = (\r -> (r, best d r))
          <$> [3%2,5%4,7%4,11%8,13%8]

best :: Integer -> Rational -> (Integer, Integer, Double)
best d r = L.minimumBy (comparing $ abs . (^. _3))
           $ errs d r

errs :: Floating a
     => Integer -> Rational -> [(Integer, Integer, a)]
errs n r =
  [ ( i
    , round $ stretch $ fromIntegral i / fromIntegral n
    , err r           $ fromIntegral i / fromIntegral n )
  | i <- [0..n-1 :: Integer] ]

err :: Floating a => Rational -> a -> a
err true_frac approx_edo =
  stretch approx_edo - cents true_frac

truth :: Floating c => Int -> [(c,Rational)]
truth p = f <$> harmonics p
  where f h = (cents h, h)

type Report = (Integer, Rational, Integer, Integer, Integer)

px = mapM_ putStrLn . map f where
  f :: Report -> String
  f (i,r,j,k,l) =
    let t = "\t"
    in show i ++ t ++ show r ++ "\t\t" ++
       show j ++ t ++ show k ++ t ++ show l

x :: Integer -> [Report]
x d = map f just_intervals where
  less x = round $ x / 10
  f (c,r) = let
    (note, cNote, errNote) = best d r
    in (less c, r, note, cNote, less errNote)

just_intervals :: [(Double, Rational)]
just_intervals = let
  f x = if x >= 2 then x/2 else x
  pair n = (cents $ fromRational n, n)
  in map (pair . f) $ L.sort $ lim_11

-- | `sum_errs d` gives the sum of the absolute values of the errors
-- of `d`-edo in approximating the harmonics of interest.
sum_errs d = sum $ abs . err_of_best <$> bests d
err_of_best (_,(_,_,e)) = e
s = stretch
c = cents
