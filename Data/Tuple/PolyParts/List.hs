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


--type MonoTuple0 a  = ()
type MonoTuple1 a  = OneTuple a
type MonoTuple2 a  = (a, a)
type MonoTuple3 a  = (a, a, a)
type MonoTuple4 a  = (a, a, a, a)
type MonoTuple5 a  = (a, a, a, a, a)
type MonoTuple6 a  = (a, a, a, a, a, a)
type MonoTuple7 a  = (a, a, a, a, a, a, a)
type MonoTuple8 a  = (a, a, a, a, a, a, a, a)
type MonoTuple9 a  = (a, a, a, a, a, a, a, a, a)
type MonoTuple10 a = (a, a, a, a, a, a, a, a, a, a)
type MonoTuple11 a = (a, a, a, a, a, a, a, a, a, a, a)
type MonoTuple12 a = (a, a, a, a, a, a, a, a, a, a, a, a)
type MonoTuple13 a = (a, a, a, a, a, a, a, a, a, a, a, a, a)
type MonoTuple14 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
type MonoTuple15 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

-- | tupleToList converts mono-tuple to list
--
--   @
--     tupleToList ("s","d","f") ==  ["s","d","f"]
--     tupleToList ('H','e','l','l','o') == "Hello"
--     tupleToList ((1,2) :: Num a => (a,a)) == [1,2] :: Num a => [a]
--   @
--
class TupleFromList a where
    tupleFromList :: [a] -> a

    
instance TupleToList () where
    tupleFromList _ = []
    
instance TupleToList (MonoTuple1 a) where
    tupleFromList (a:_) = OneTuple a
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple1"
    
instance TupleToList (MonoTuple2 a) where
    tupleFromList (a:b:_) = (a, b)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple2"

instance TupleToList (MonoTuple3 a) where
    tupleFromList (a1:a2:a3:_) = (a1, a2, a3)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple3"

instance TupleToList (MonoTuple4 a) where
    tupleFromList (a1:a2:a3::a4_) = (a1, a2, a3, a4)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple4"

instance TupleToList (MonoTuple5 a) where
    tupleFromList (a1:a2:a3::a4:a5:_) = (a1, a2, a3, a4, a5)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple5"

instance TupleToList (MonoTuple6 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:_) = (a1, a2, a3, a4, a5, a6)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple6"

instance TupleToList (MonoTuple7 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:_) = (a1, a2, a3, a4, a5, a6, a7)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple7"

instance TupleToList (MonoTuple8 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:_) = (a1, a2, a3, a4, a5, a6, a7, a8)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple8"

instance TupleToList (MonoTuple9 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple9"

instance TupleToList (MonoTuple10 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:a10:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple10"

instance TupleToList (MonoTuple11 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:a10:a11:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple11"

instance TupleToList (MonoTuple12 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:a10:a11:a12:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple12"

instance TupleToList (MonoTuple13 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple13"

instance TupleToList (MonoTuple14 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple14"

instance TupleToList (MonoTuple15 a) where
    tupleFromList (a1:a2:a3::a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:_) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    tupleFromList _     = error "TupleToList: not enoght elements in list for MonoTuple15"

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