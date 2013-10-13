{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Tuple.PolyParts.List where

import Data.Tuple.PolyParts.OneTuple (OneTuple (..))

import Prelude (error)

import Data.Tuple.PolyParts.Modification (extendTupleR, TupleExtendedR, TupleExtendR)


-- | This module provide easy convertion to list and from list:
-- 
--   @
--      tupleToList ("d","s") == ["d","s"]
--      stepTFromList . stepTFromList $ ( (), ["d","s"] ) == ( ("d","s") , [] )
--   @
--
--

-- | One step to convert list to tuple. It extend tuple to right the head of list and remain tail of list. If list is empty function throw an error
--
--   @
--      stepTFromList ( (), ["s","d","f"] ) == ( OneTuple "s", ["d","f"] )
--   @
-- 
--   Function could replicated:
--
--   @
--     stepTFromList . stepTFromList . stepTFromList $ ( (), ["s","d","f"] ) ==  ( ("s","d","f"), [] )
--   @
--
stepTFromList :: TupleExtendR a b => (a, [b]) -> (TupleExtendedR a b, [b])
stepTFromList (t, (x:xs)) =  (t `extendTupleR` x,  xs)
stepTFromList (t, [])     =  error "stepTFromList: empty list"


-- | tupleToList converts mono-tuple to list
--
--   @
--     tupleToList ("s","d","f") ==  ["s","d","f"]
--     tupleToList ('H','e','l','l','o') == "Hello"
--     tupleToList ((1,2) :: Num a => (a,a)) == [1,2] :: Num a => [a]
--   @
--
class TupleToList a where
    type PartTupleMono a
    tupleToList :: a -> [PartTupleMono a]

	
instance (PartTupleMono () ~ () ) => TupleToList () where
    type  PartTupleMono () = ()
    tupleToList () = []
	
instance (PartTupleMono (OneTuple a) ~ a ) => TupleToList (OneTuple a) where
    type  PartTupleMono (OneTuple a) = a
    tupleToList (OneTuple a) =[a]
	
instance (PartTupleMono (a, a) ~ a ) => TupleToList (a, a) where
    type  PartTupleMono (a, a) = a
    tupleToList (a, b) = [a, b]

instance (PartTupleMono (a, a, a) ~ a ) => TupleToList (a, a, a) where
    type  PartTupleMono (a, a, a) = a
    tupleToList (a1, a2, a3) = [a1, a2, a3]
	
instance (PartTupleMono (a, a, a, a) ~ a ) => TupleToList (a, a, a, a) where
    type  PartTupleMono (a, a, a, a) = a
    tupleToList (a1, a2, a3, a4) = [a1, a2, a3, a4]

instance (PartTupleMono (a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5) = [a1, a2, a3, a4, a5]

instance (PartTupleMono (a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6) = [a1, a2, a3, a4, a5, a6]

instance (PartTupleMono (a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7) = [a1, a2, a3, a4, a5, a6, a7]

instance (PartTupleMono (a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8) = [a1, a2, a3, a4, a5, a6, a7, a8]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9) =  [a1, a2, a3, a4, a5, a6, a7, a8, a9]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14]

instance (PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) ~ a ) => TupleToList (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) where
    type  PartTupleMono (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) = a
    tupleToList (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15]