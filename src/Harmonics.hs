module Harmonics where

import qualified Data.Set as S


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

-- | ALl the 15-odd-limit intervals.
lim_15 :: Fractional a => [a]
lim_15 =
  [3/2,4/3,5/4,5/3,6/5
  ,7/6,7/5,7/4
  ,8/7,8/5
  ,9/8,9/7,9/5
  ,10/9,10/7
  ,11/10,11/9,11/8,11/7,11/6
  ,12/11,12/7
  ,13/12,13/11,13/10,13/9,13/8,13/7
  ,14/13,14/11,14/9
  ,15/14,15/13,15/11,15/8
  ,16/15,16/13,16/11,16/9
  ,18/13,18/11
  ,20/13,20/11
  ,22/15,22/13
  ,24/13
  ,26/15
  ,28/15
  ]

-- | ALl the 15-odd-limit intervals except those involving 13.
lim_15_no_13 :: Fractional a => [a]
lim_15_no_13 = [3/2,4/3,5/4,5/3,6/5
               ,7/6,7/5,7/4
               ,8/7,8/5
               ,9/8,9/7,9/5
               ,10/9,10/7
               ,11/10,11/9,11/8,11/7,11/6
               ,12/11,12/7
               ,14/11,14/9
               ,15/14,15/11,15/8
               ,16/15,16/11,16/9
               ,18/11
               ,20/11
               ]

-- | The 13-odd-limit intervals.
lim_13 :: Fractional a => [a]
lim_13 = [3/2,4/3,5/4,5/3,6/5
         ,7/6,7/5,7/4
         ,8/7,8/5
         ,9/8,9/7,9/5
         ,10/9,10/7
         ,11/10,11/9,11/8,11/7,11/6
         ,12/11,12/7
         ,13/12,13/11,13/10,13/9,13/8,13/7
         ,14/13,14/11,14/9
         ,16/13,16/11,16/9
         ,18/13,18/11
         ,20/13,20/11
         ,22/13
         ,24/13
         ]

lim_13_no_7 :: Fractional a => [a]
lim_13_no_7 = [3/2,4/3,5/4,5/3,6/5
              ,8/5
              ,9/8,9/5
              ,10/9,11/10,11/9,11/8,11/6
              ,12/11,13/12,13/11,13/10,13/9,13/8
              ,16/13,16/11,16/9
              ,18/13,18/11
              ,20/13,20/11
              ,22/13
              ,24/13
              ]

lim_13_no_3 :: Fractional a => [a]
lim_13_no_3 =
  [5/4
  ,7/5,7/4
  ,8/7,8/5
  ,10/7
  ,11/10,11/8,11/7
  ,13/11,13/10,13/8,13/7
  ,14/13,14/11
  ,16/13,16/11
  ,20/13,20/11
  ,22/13
  ]

-- | The 11-odd-limit intervals.
lim_11 :: Fractional a => [a]
lim_11 = [3/2,4/3,5/4,5/3,6/5
         ,7/6,7/5,7/4,8/7,8/5
         ,9/8,9/7,9/5,10/9,10/7
         ,11/10,11/9,11/8,11/7,11/6
         ,12/11,12/7
         ,14/11,14/9
         ,16/11,16/9
         ,18/11,20/11
         ]

-- | The 11-odd-limit intervals.
lim_11_no_7 :: Fractional a => [a]
lim_11_no_7 = [3/2,4/3,5/4,5/3,6/5,8/5
              ,9/8,9/5,10/9
              ,11/10,11/9,11/8,11/6
              ,12/11
              ,16/11,16/9
              ,18/11,20/11
              ]

-- | The 11-odd-limit intervals.
lim_7 :: Fractional a => [a]
lim_7 = [3/2,4/3
        ,5/4,5/3,6/5
        ,7/6,7/5,7/4,8/7,8/5
        ,9/8,9/7,9/5,10/9,10/7
        ,12/7,14/9,16/9
        ]

harmonics_2 :: (Ord a, Fractional a) => Int -> [a]
harmonics_2 p =
  S.toList . S.fromList $
  harmonics p ++ [ a * b
                 | a <- harmonics p
                 , b <- harmonics p ]

harmonics :: Fractional a => Int -> [a]
harmonics n = harmonics_up n ++ harmonics_down n

harmonics_down :: Fractional a => Int -> [a]
harmonics_down n = map ((*2) . (1/)) $ harmonics_up n

-- | `harmonics_up n` gives the first `n` prime harmonics, remapped to
-- the first octave. (Nobody uses more than 10 prime harmonics, surely.)
harmonics_up :: Fractional a => Int -> [a]
harmonics_up n = take n $
  [ 3/2,   5/4,   7/4,   11/8,  13/8
  , 17/16, 19/16, 23/16, 29/16, 31/16]
