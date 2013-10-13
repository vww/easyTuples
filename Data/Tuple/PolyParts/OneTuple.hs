
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Tuple.PolyParts.OneTuple (
	OneTuple (..),
	only
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Ix
import Data.Foldable
import Data.Traversable
import Data.Typeable

-- | OneTuple is a tuple data type with one field.
newtype OneTuple a = OneTuple a 
          deriving (Eq, Ord, Bounded, Show, Read, Ix, Functor, Foldable, Traversable, Typeable)


-- | Extracting data from OneTuple
only :: OneTuple a -> a
only (OneTuple a) = a 

instance Applicative OneTuple where
    pure = return
    (<*>) = ap

instance Monad OneTuple where
    return = OneTuple
    (OneTuple x) >>= f = f x

instance MonadFix OneTuple where
    mfix f = let a = f (only a) in a