{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Tuple.PolyParts.Size where

import Data.Tuple.PolyParts.OneTuple (OneTuple (..))

import Prelude (Int)

-- | sizeTuple is an analog of 'length' function from list
--   
--   @
--     sizeTuple (5,6) == 2
--     sizeTuple ()    == 0
--   @
--
class TupleSize a where
    sizeTuple :: a -> Int


instance TupleSize () where
    sizeTuple ~() = 0

instance TupleSize (OneTuple a) where
    sizeTuple ~(OneTuple _) = 1

instance TupleSize (a, b) where
    sizeTuple ~(_, _) = 2   

instance TupleSize (a1, a2, a3) where
    sizeTuple ~(_, _, _) = 3

instance TupleSize (a1, a2, a3, a4) where
    sizeTuple ~(_, _, _, _) = 4

instance TupleSize (a1, a2, a3, a4, a5) where
    sizeTuple ~(_, _, _, _, _) = 5

instance TupleSize (a1, a2, a3, a4, a5, a6) where
    sizeTuple ~(_, _, _, _, _, _) = 6

instance TupleSize (a1, a2, a3, a4, a5, a6, a7) where
    sizeTuple ~(_, _, _, _, _, _, _) = 7

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8) where
    sizeTuple ~(_, _, _, _, _, _, _, _) = 8

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _) = 9

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _, _) = 10

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _, _, _) = 11

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _, _, _, _) = 12

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _, _, _, _, _) = 13

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _, _, _, _, _, _) = 14

instance TupleSize (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    sizeTuple ~(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = 15
