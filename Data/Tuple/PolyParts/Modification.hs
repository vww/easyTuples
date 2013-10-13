{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Tuple.PolyParts.Modification where

import Data.Tuple.PolyParts.OneTuple (OneTuple (..))
import Data.Tuple.PolyParts.Size (sizeTuple, TupleSize)
import Data.Tuple.PolyParts.Select (fst, snd, lst, prelast, PartTLst, 
            PartTFst, PartTSnd, TupleLst, PartTPrelast, TupleSnd, TupleFst, TuplePrelast)

import Prelude (error, even, ($), (++), show, otherwise)

-- | This module provide easy manipulations with tuples:
-- 
--   @
--      halfTuple ("d", "s", True, False) == ( ("d", "s"), (True, False) )
--      swap      ("d", "s", True, False) == (False, True, "s", "d")
--      cutTupleL ("d", "s", True, False) == ("s", True, False)
--      cutTupleR ("d", "s", True, False) == ("d", "s", True)
--      2 `extendTupleL` ("d", "s") ==  (2, "d", "s")
--      ("d", "s") `extendTupleR` 2 ==  ("d", "s", 2)
--      exchangeT2R ( ("d", "s", "f"), (True, False, False) ) == ( ("d", "s"), ("f", True, False, False) )
--      exchangeT2L ( ("d", "s", "f"), (True, False, False) ) == ( ("d", "s", "f", True), (False, False) )
--      flatTupleR . flatTupleR $ ("d", ("s", (True, False))) == ("d", "s", True, False)
--   @
--
--

-- | halfTuple function divide tuple at 2 halfs. Note, if size is odd, left half is bigger, then right half:
-- 
--   @
--      halfTuple ("d", "s", True, False) == ( ("d", "s"), (True, False) )
--      halfTuple ("d", "s", True, False, False) == ( ("d", "s", True), (False, False) )
--   @
--   
class TupleHalf a where
    type PartTHalfL a
    type PartTHalfR a
    halfTuple :: a -> (PartTHalfL a, PartTHalfR a)


instance (PartTHalfL (OneTuple a) ~ OneTuple a, 
          PartTHalfR (OneTuple a) ~ ()
          ) => TupleHalf (OneTuple a) where
    type  PartTHalfL (OneTuple a) = OneTuple a
    type  PartTHalfR (OneTuple a) = ()
    halfTuple (OneTuple a) = (OneTuple a, () )

instance (PartTHalfL (a1, a2) ~ OneTuple a1, 
          PartTHalfR (a1, a2) ~ OneTuple a2
          ) => TupleHalf (a1, a2) where
    type  PartTHalfL (a1, a2) = OneTuple a1
    type  PartTHalfR (a1, a2) = OneTuple a2
    halfTuple (a1, a2) = (OneTuple a1, OneTuple a2)

instance (PartTHalfL (a1, a2, a3) ~ (a1, a2), 
          PartTHalfR (a1, a2, a3) ~ OneTuple a3
          ) => TupleHalf (a1, a2, a3) where
    type  PartTHalfL (a1, a2, a3) = (a1, a2)
    type  PartTHalfR (a1, a2, a3) = OneTuple a3
    halfTuple (a1, a2, a3) = ((a1, a2), OneTuple a3)

instance (PartTHalfL (a1, a2, a3, a4) ~ (a1, a2), 
          PartTHalfR (a1, a2, a3, a4) ~ (a3, a4)
          ) => TupleHalf (a1, a2, a3, a4) where
    type  PartTHalfL (a1, a2, a3, a4) = (a1, a2)
    type  PartTHalfR (a1, a2, a3, a4) = (a3, a4)
    halfTuple (a1, a2, a3, a4) = ((a1, a2), (a3, a4))

instance (PartTHalfL (a1, a2, a3, a4, a5) ~ (a1, a2, a3), 
          PartTHalfR (a1, a2, a3, a4, a5) ~ (a4, a5)
          ) => TupleHalf (a1, a2, a3, a4, a5) where
    type  PartTHalfL (a1, a2, a3, a4, a5) = (a1, a2, a3)
    type  PartTHalfR (a1, a2, a3, a4, a5) = (a4, a5)
    halfTuple (a1, a2, a3, a4, a5) = ((a1, a2, a3), (a4, a5))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6) ~ (a1, a2, a3), 
          PartTHalfR (a1, a2, a3, a4, a5, a6) ~ (a4, a5, a6)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6) = (a1, a2, a3)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6) = (a4, a5, a6)
    halfTuple (a1, a2, a3, a4, a5, a6) = ((a1, a2, a3), (a4, a5, a6))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7) ~ (a1, a2, a3, a4), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7) ~ (a5, a6, a7)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7) = (a1, a2, a3, a4)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7) = (a5, a6, a7)
    halfTuple (a1, a2, a3, a4, a5, a6, a7) = ((a1, a2, a3, a4), (a5, a6, a7))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8) ~ (a1, a2, a3, a4), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8) ~ (a5, a6, a7, a8)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8) = (a1, a2, a3, a4)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8) = (a5, a6, a7, a8)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8) = ((a1, a2, a3, a4), (a5, a6, a7, a8))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ (a1, a2, a3, a4, a5), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ (a6, a7, a8, a9)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a1, a2, a3, a4, a5)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a6, a7, a8, a9)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ((a1, a2, a3, a4, a5), (a6, a7, a8, a9))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ (a1, a2, a3, a4, a5), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ (a6, a7, a8, a9, a10)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a1, a2, a3, a4, a5 )
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a6, a7, a8, a9, a10)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = ((a1, a2, a3, a4, a5), (a6, a7, a8, a9, a10))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ (a1, a2, a3, a4,  a5, a6), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ (a7, a8, a9, a10, a11)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a1, a2, a3, a4,  a5, a6)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a7, a8, a9, a10, a11)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = ((a1, a2, a3, a4, a5, a6), (a7, a8, a9, a10, a11))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ (a1, a2, a3, a4,  a5,  a6), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ (a7, a8, a9, a10, a11, a12)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a1, a2, a3, a4,  a5,  a6)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a7, a8, a9, a10, a11, a12)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = ((a1, a2, a3, a4, a5, a6), (a7, a8, a9, a10, a11, a12))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ (a1, a2, a3,  a4,  a5,  a6, a7), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ (a8, a9, a10, a11, a12, a13)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a1, a2, a3,  a4,  a5,  a6, a7)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a8, a9, a10, a11, a12, a13)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = ((a1, a2, a3, a4, a5, a6, a7), (a8, a9, a10, a11, a12, a13))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ (a1, a2, a3, a4, a5, a6, a7), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ (a8, a9, a10, a11, a12, a13, a14)
          ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a1, a2, a3,  a4,  a5,  a6,  a7)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a8, a9, a10, a11, a12, a13, a14)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = ((a1, a2, a3, a4, a5, a6, a7), (a8, a9, a10, a11, a12, a13, a14))

instance (PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ (a1, a2,  a3,  a4,  a5,  a6,  a7, a8), 
          PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ (a9, a10, a11, a12, a13, a14, a15)
         ) => TupleHalf (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTHalfL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a1, a2,  a3,  a4,  a5,  a6,  a7, a8)
    type  PartTHalfR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a9, a10, a11, a12, a13, a14, a15)
    halfTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = ((a1, a2, a3, a4, a5, a6, a7, a8), (a9, a10, a11, a12, a13, a14, a15))	


-- |  the same as 'halfTuple'
--
halfTupleL :: TupleHalf a => a -> (PartTHalfL a, PartTHalfR a)
halfTupleL = halfTuple

-- |  halfTupleEvenR is a halfTupleR for odd-sized tuples.
--   @
--      halfTupleEvenR ("d", "s", True, False, False) == ( ("d", "s"), (True, False, False) )
--   @
--
halfTupleOddR :: (TupleSize a, TupleHalf a, TupleLst (PartTHalfL a), TupleExtendL (PartTHalfR a) (PartTLst (PartTHalfL a)), TupleCutR (PartTHalfL a) ) => 
                  a -> (PartTCutR (PartTHalfL a), TupleExtendedL (PartTLst (PartTHalfL a)) (PartTHalfR a))
halfTupleOddR t 
        | even (sizeTuple t) = error $ "halfTupleEdenR: not odd size of tuple, it size is " ++ (show $ sizeTuple t )
        | otherwise          = exchangeT2R $ halfTuple t



class TupleSwap a where
    type SwapTuple a
    swap :: a -> SwapTuple a


instance (SwapTuple () ~ () ) => TupleSwap () where
    type  SwapTuple () = ()
    swap () = ()

instance (SwapTuple (OneTuple a) ~ (OneTuple a) ) => TupleSwap (OneTuple a) where
    type  SwapTuple (OneTuple a) = (OneTuple a)
    swap (OneTuple a) = (OneTuple a)

instance (SwapTuple (a, b) ~ (b, a) ) => TupleSwap (a, b) where
    type  SwapTuple (a, b) = (b, a)
    swap (a, b) = (b, a)

instance (SwapTuple (a, b, c) ~ (c, b, a) ) => TupleSwap (a, b, c) where
    type  SwapTuple (a, b, c) = (c, b, a)
    swap (a, b, c) = (c, b, a)

instance (SwapTuple (a1, a2, a3, a4) ~ (a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4) where
    type  SwapTuple (a1, a2, a3, a4) = (a4, a3, a2, a1)
    swap (a1, a2, a3, a4) = (a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5) ~ (a5, a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4, a5) where
    type  SwapTuple (a1, a2, a3, a4, a5) = (a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5) = (a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6) ~ (a6, a5, a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4, a5, a6) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6) = (a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6) = (a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7) ~ (a7, a6, a5, a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7) = (a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7) = (a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8) ~ (a8, a7, a6, a5, a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8) = (a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8) = (a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ (a9, a8, a7, a6, a5, a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a9, a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ (a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
          ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ (a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
          ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ (a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
          ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ (a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
          ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)

instance (SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ (a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
          ) => TupleSwap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  SwapTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
    swap (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)



class TupleCutL a where
    type PartTCutL a
    cutTupleL :: a -> PartTCutL a


instance (PartTCutL (OneTuple a) ~ () ) => TupleCutL (OneTuple a) where
    type  PartTCutL (OneTuple a) = ()
    cutTupleL (OneTuple _) = ()	

instance (PartTCutL (a, b) ~ OneTuple b ) => TupleCutL (a, b) where
    type  PartTCutL (a, b) = OneTuple b
    cutTupleL (_, a) = OneTuple a

instance (PartTCutL (a, b, c) ~ (b, c) ) => TupleCutL (a, b, c) where
    type  PartTCutL (a, b, c) = (b, c)
    cutTupleL (_, b, c) = (b, c)

instance (PartTCutL (a1, a2, a3, a4) ~ (a2, a3, a4) ) => TupleCutL (a1, a2, a3, a4) where
    type  PartTCutL (a1, a2, a3, a4) = (a2, a3, a4)
    cutTupleL (_, a2, a3, a4) = (a2, a3, a4)

instance (PartTCutL (a1, a2, a3, a4, a5) ~ (a2, a3, a4, a5) ) => TupleCutL (a1, a2, a3, a4, a5) where
    type  PartTCutL (a1, a2, a3, a4, a5) = (a2, a3, a4, a5)
    cutTupleL (_, a2, a3, a4, a5) = (a2, a3, a4, a5)

instance (PartTCutL (a1, a2, a3, a4, a5, a6) ~ (a2, a3, a4, a5, a6) ) => TupleCutL (a1, a2, a3, a4, a5, a6) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6) = (a2, a3, a4, a5, a6)
    cutTupleL (_, a2, a3, a4, a5, a6) = (a2, a3, a4, a5, a6)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7) ~ ( a2, a3, a4, a5, a6, a7) ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7) = (a2, a3, a4, a5, a6, a7)
    cutTupleL (_, a2, a3, a4, a5, a6, a7) = (a2, a3, a4, a5, a6, a7)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8) ~ (a2, a3, a4, a5, a6, a7, a8) ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8) = (a2, a3, a4, a5, a6, a7, a8)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8) = (a2, a3, a4, a5, a6, a7, a8)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ (a2, a3, a4, a5, a6, a7, a8, a9) ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a2, a3, a4, a5, a6, a7, a8, a9)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9) = (a2, a3, a4, a5, a6, a7, a8, a9)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ (a2, a3, a4, a5, a6, a7, a8, a9, a10) ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a2, a3, a4, a5, a6, a7, a8, a9, a10)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
          ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
          ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
          ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
          ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

instance (PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
          ) => TupleCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTCutL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    cutTupleL (_, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)



class TupleCutR a where
    type PartTCutR a
    cutTupleR :: a -> PartTCutR a


instance (PartTCutR (OneTuple a) ~ () ) => TupleCutR (OneTuple a) where
    type  PartTCutR (OneTuple a) = ()
    cutTupleR (OneTuple _) = ()	

instance (PartTCutR (a, b) ~ OneTuple a ) => TupleCutR (a, b) where
    type  PartTCutR (a, b) = OneTuple a
    cutTupleR (a, _) = OneTuple a

instance (PartTCutR (a, b, c) ~ (a, b) ) => TupleCutR (a, b, c) where
    type  PartTCutR (a, b, c) = (a, b)
    cutTupleR (a, b, _) = (a, b)

instance (PartTCutR (a1, a2, a3, a4) ~ (a1, a2, a3) ) => TupleCutR (a1, a2, a3, a4) where
    type  PartTCutR (a1, a2, a3, a4) = (a1, a2, a3)
    cutTupleR (a1, a2, a3, _) = (a1, a2, a3)

instance (PartTCutR (a1, a2, a3, a4, a5) ~ (a1, a2, a3, a4) ) => TupleCutR (a1, a2, a3, a4, a5) where
    type  PartTCutR (a1, a2, a3, a4, a5) = (a1, a2, a3, a4)
    cutTupleR (a1, a2, a3, a4, _) = (a1, a2, a3, a4)

instance (PartTCutR (a1, a2, a3, a4, a5, a6) ~ (a1, a2, a3, a4, a5) ) => TupleCutR (a1, a2, a3, a4, a5, a6) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6) = (a1, a2, a3, a4, a5)
    cutTupleR (a1, a2, a3, a4, a5, _) = (a1, a2, a3, a4, a5)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7) ~ (a1, a2, a3, a4, a5, a6) ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7) = (a1, a2, a3, a4, a5, a6)
    cutTupleR (a1, a2, a3, a4, a5, a6, _) = (a1, a2, a3, a4, a5, a6)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8) ~ (a1, a2, a3, a4, a5, a6, a7) ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8) = (a1, a2, a3, a4, a5, a6, a7)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, _) = (a1, a2, a3, a4, a5, a6, a7)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ (a1, a2, a3, a4, a5, a6, a7, a8) ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a1, a2, a3, a4, a5, a6, a7, a8)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, _) = (a1, a2, a3, a4, a5, a6, a7, a8)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9) ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, _) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
          ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, _) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
          ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, _) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
          ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, _) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
          ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, _) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

instance (PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
          ) => TupleCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTCutR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    cutTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, _) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)	



class TupleExtendR a b where
    type TupleExtendedR a b
    extendTupleR :: a -> b -> TupleExtendedR a b


instance (TupleExtendedR () b ~ (OneTuple b) ) => TupleExtendR () b where
    type  TupleExtendedR () b = (OneTuple b)
    extendTupleR () b = (OneTuple b)	

instance (TupleExtendedR (OneTuple a) b ~ (a, b) ) => TupleExtendR (OneTuple a) b where
    type  TupleExtendedR (OneTuple a) b = (a, b)
    extendTupleR (OneTuple a) b = (a, b)

instance (TupleExtendedR (a1, a2) b ~ (a1, a2, b) ) => TupleExtendR (a1, a2) b where
    type  TupleExtendedR (a1, a2) b = (a1, a2, b)
    extendTupleR (a1, a2) b = (a1, a2, b)

instance (TupleExtendedR (a1, a2, a3) b ~ (a1, a2, a3, b) ) => TupleExtendR (a1, a2, a3) b where
    type  TupleExtendedR (a1, a2, a3) b = (a1, a2, a3, b)
    extendTupleR (a1, a2, a3) b = (a1, a2, a3, b)

instance (TupleExtendedR (a1, a2, a3, a4) b ~ (a1, a2, a3, a4, b) ) => TupleExtendR (a1, a2, a3, a4) b where
    type  TupleExtendedR (a1, a2, a3, a4) b = (a1, a2, a3, a4, b)
    extendTupleR (a1, a2, a3, a4) b = (a1, a2, a3, a4, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5) b ~ (a1, a2, a3, a4, a5, b) ) => TupleExtendR (a1, a2, a3, a4, a5) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5) b = (a1, a2, a3, a4, a5, b)
    extendTupleR (a1, a2, a3, a4, a5) b = (a1, a2, a3, a4, a5, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6) b ~ (a1, a2, a3, a4, a5, a6, b) ) => TupleExtendR (a1, a2, a3, a4, a5, a6) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6) b = (a1, a2, a3, a4, a5, a6, b)
    extendTupleR (a1, a2, a3, a4, a5, a6) b = (a1, a2, a3, a4, a5, a6, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7) b ~ (a1,  a2, a3, a4, a5, a6, a7, b) ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7) b = (a1, a2, a3, a4, a5, a6, a7, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7) b = (a1, a2, a3, a4, a5, a6, a7, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, b) ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8) b = (a1, a2, a3, a4, a5, a6, a7, a8, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8) b = (a1, a2, a3, a4, a5, a6, a7, a8, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, b) ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8, a9) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, b) ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, b)
          ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, b)
          ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, b)
          ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, b)

instance (TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) b ~ (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, b)
          ) => TupleExtendR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) b where
    type  TupleExtendedR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, b)
    extendTupleR (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) b = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, b)


class TupleExtendL a b where
    type TupleExtendedL b a
    extendTupleL :: b -> a -> TupleExtendedL b a


instance (TupleExtendedL a () ~ (OneTuple a) ) => TupleExtendL () a where
    type  TupleExtendedL a () = (OneTuple a)
    extendTupleL a () = (OneTuple a)	

instance (TupleExtendedL b (OneTuple a) ~ (b, a) ) => TupleExtendL (OneTuple a) b where
    type  TupleExtendedL b (OneTuple a) = (b, a)
    extendTupleL a (OneTuple b) = (a, b)

instance (TupleExtendedL a0 (a1, a2) ~ (a0, a1, a2) ) => TupleExtendL (a1, a2) a0 where
    type  TupleExtendedL a0 (a1, a2) = (a0, a1, a2)
    extendTupleL a0 (a1, a2) = (a0, a1, a2)

instance (TupleExtendedL a0 (a1, a2, a3) ~ (a0, a1, a2, a3) ) => TupleExtendL (a1, a2, a3) a0 where
    type  TupleExtendedL a0 (a1, a2, a3) = (a0, a1, a2, a3)
    extendTupleL a0 (a1, a2, a3) = (a0, a1, a2, a3)

instance (TupleExtendedL a0 (a1, a2, a3, a4) ~ (a0, a1, a2, a3, a4) ) => TupleExtendL (a1, a2, a3, a4) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4) = (a0, a1, a2, a3, a4)
    extendTupleL a0 (a1, a2, a3, a4) = (a0, a1, a2, a3, a4)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5) ~ (a0, a1, a2, a3, a4, a5) ) => TupleExtendL (a1, a2, a3, a4, a5) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5) = (a0, a1, a2, a3, a4, a5)
    extendTupleL a0 (a1, a2, a3, a4, a5) = (a0, a1, a2, a3, a4, a5)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6) ~ (a0, a1, a2, a3, a4, a5, a6) ) => TupleExtendL (a1, a2, a3, a4, a5, a6) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6) = (a0, a1, a2, a3, a4, a5, a6)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6) = (a0, a1, a2, a3, a4, a5, a6)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7) ~ (a0, a1,  a2, a3, a4, a5, a6, a7) ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7) = (a0, a1, a2, a3, a4, a5, a6, a7)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7) = (a0, a1, a2, a3, a4, a5, a6, a7)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8) ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8) = (a0, a1, a2, a3, a4, a5, a6, a7, a8)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8) = (a0, a1, a2, a3, a4, a5, a6, a7, a8)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8, a9) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
          ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
          ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
          ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

instance (TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
          ) => TupleExtendL (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) a0 where
    type  TupleExtendedL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    extendTupleL a0 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)	
	

flatTupleL :: (TupleSnd (PartTFst a), TupleFst a, TupleFst (PartTFst a),
      TupleExtendL
        (TupleExtendedL (PartTSnd (PartTFst a)) (PartTCutL a))
        (PartTFst (PartTFst a)),
      TupleExtendL (PartTCutL a) (PartTSnd (PartTFst a)), TupleCutL a) =>
     a
     -> TupleExtendedL
          (PartTFst (PartTFst a))
          (TupleExtendedL (PartTSnd (PartTFst a)) (PartTCutL a))
flatTupleL ts = t1 `extendTupleL` (t2 `extendTupleL` (cutTupleL ts))
        where
           t = fst ts
           t1 = fst t
           t2 = snd t

flatTupleR :: (TuplePrelast (PartTLst a), TupleLst a, TupleLst (PartTLst a),
      TupleExtendR
        (TupleExtendedR (PartTCutR a) (PartTPrelast (PartTLst a)))
        (PartTLst (PartTLst a)),
      TupleExtendR (PartTCutR a) (PartTPrelast (PartTLst a)),
      TupleCutR a) =>
     a
     -> TupleExtendedR
          (TupleExtendedR (PartTCutR a) (PartTPrelast (PartTLst a)))
          (PartTLst (PartTLst a))
flatTupleR ts = ((cutTupleR ts) `extendTupleR` tbefore) `extendTupleR` tlst
        where
           t = lst ts
           tbefore = prelast t
           tlst    = lst t
	

exchangeT2R :: (TupleLst a, TupleExtendL b (PartTLst a), TupleCutR a) => (a, b) -> (PartTCutR a, TupleExtendedL (PartTLst a) b)
exchangeT2R (t1, t2) = (cutTupleR t1, t `extendTupleL` t2)
        where 
           t = lst t1

exchangeT2L :: (TupleFst b, TupleExtendR a (PartTFst b), TupleCutL b) => (a, b) -> (TupleExtendedR a (PartTFst b), PartTCutL b)
exchangeT2L (t1, t2) = (t1 `extendTupleR` t, cutTupleL t2)
        where 
           t = fst t2