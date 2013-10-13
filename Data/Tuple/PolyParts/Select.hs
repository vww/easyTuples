{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Tuple.PolyParts.Select where

import Data.Tuple.PolyParts.OneTuple (OneTuple (..))


-- | This module provides easy exraction from tuples:
-- 
--   @
--      fst (2,"s") == 2
--      fst (2,"s",True) == 2
--   @
--
--   It could be extracted from the begining: 'fst','snd','thrd','frth','fifth','sixth','seventh','eighth','ninth','tenth'
--   and from the end: 'lst', 'prelast'
--
class TupleFst a where
    type PartTFst a
    fst :: a -> PartTFst a

-- | fst' is the same as fst, but it can be imported without hiding Prelude.fst
fst' :: TupleFst a => a -> PartTFst a
fst' = fst
	
instance (PartTFst (OneTuple a) ~ a ) => TupleFst (OneTuple a) where
    type  PartTFst (OneTuple a) = a
    fst (OneTuple a) = a
	
instance (PartTFst (a, b) ~ a ) => TupleFst (a, b) where
    type  PartTFst (a, b) = a
    fst (a,_) = a

instance (PartTFst (a, b, c) ~ a ) => TupleFst (a, b, c) where
    type  PartTFst (a, b, c) = a
    fst (a, _, _) = a
	
instance (PartTFst (a1, a2, a3, a4) ~ a1 ) => TupleFst (a1, a2, a3, a4) where
    type  PartTFst (a1, a2, a3, a4) = a1
    fst (a, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5) where
    type  PartTFst (a1, a2, a3, a4, a5) = a1
    fst (a, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6) where
    type  PartTFst (a1, a2, a3, a4, a5, a6) = a1
    fst (a, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7) = a1
    fst (a, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8) = a1
    fst (a, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1
    fst (a, _, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a1
    fst (a, _, _, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a1
    fst (a, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a1
    fst (a, _, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a1
    fst (a, _, _, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a1
    fst (a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a1 ) => TupleFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a1
    fst (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a


class TupleSnd a where
    type PartTSnd a
    snd :: a -> PartTSnd a

-- | snd' is the same as snd, but it can be imported without hiding Prelude.snd
snd' :: TupleSnd a => a -> PartTSnd a
snd' = snd
	
instance (PartTSnd (a, b) ~ b ) => TupleSnd (a, b) where
    type  PartTSnd (a, b) = b
    snd (_, a) = a

instance (PartTSnd (a, b, c) ~ b ) => TupleSnd (a, b, c) where
    type  PartTSnd (a, b, c) = b
    snd (_, a, _) = a
	
instance (PartTSnd (a1, a2, a3, a4) ~ a2 ) => TupleSnd (a1, a2, a3, a4) where
    type  PartTSnd (a1, a2, a3, a4) = a2
    snd (_, a, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5) where
    type  PartTSnd (a1, a2, a3, a4, a5) = a2
    snd (_, a, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6) = a2
    snd (_, a, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7) = a2
    snd (_, a, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8) = a2
    snd (_, a, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a2
    snd (_, a, _, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a2
    snd (_, a, _, _, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a2
    snd (_, a, _, _, _, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a2
    snd (_, a, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a2
    snd (_, a, _, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a2
    snd (_, a, _, _, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a2 ) => TupleSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a2
    snd (_, a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a



class TupleThrd a where
    type PartTThrd a
    thrd :: a -> PartTThrd a
	
instance (PartTThrd (a, b, c) ~ c ) => TupleThrd (a, b, c) where
    type  PartTThrd (a, b, c) = c
    thrd (_, _, a) = a
	
instance (PartTThrd (a1, a2, a3, a4) ~ a3 ) => TupleThrd (a1, a2, a3, a4) where
    type  PartTThrd (a1, a2, a3, a4) = a3
    thrd (_, _, a, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5) where
    type  PartTThrd (a1, a2, a3, a4, a5) = a3
    thrd (_, _, a, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6) = a3
    thrd (_, _, a, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7) = a3
    thrd (_, _, a, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8) = a3
    thrd (_, _, a, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a3
    thrd (_, _, a, _, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a3
    thrd (_, _, a, _, _, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a3
    thrd (_, _, a, _, _, _, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a3
    thrd (_, _, a, _, _, _, _, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a3
    thrd (_, _, a, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a3
    thrd (_, _, a, _, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a3 ) => TupleThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTThrd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a3
    thrd (_, _, a, _, _, _, _, _, _, _, _, _, _, _, _) = a


	
class TupleFrth a where
    type PartTFrth a
    frth :: a -> PartTFrth a
	
instance (PartTFrth (a1, a2, a3, a4) ~ a4 ) => TupleFrth (a1, a2, a3, a4) where
    type  PartTFrth (a1, a2, a3, a4) = a4
    frth (_, _, _, a) = a

instance (PartTFrth (a1, a2, a3, a4, a5) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5) where
    type  PartTFrth (a1, a2, a3, a4, a5) = a4
    frth (_, _, _, a, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6) = a4
    frth (_, _, _, a, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7) = a4
    frth (_, _, _, a, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8) = a4
    frth (_, _, _, a, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a4
    frth (_, _, _, a, _, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a4
    frth (_, _, _, a, _, _, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a4
    frth (_, _, _, a, _, _, _, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a4
    frth (_, _, _, a, _, _, _, _, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a4
    frth (_, _, _, a, _, _, _, _, _, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a4
    frth (_, _, _, a, _, _, _, _, _, _, _, _, _, _) = a

instance (PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a4 ) => TupleFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTFrth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a4
    frth (_, _, _, a, _, _, _, _, _, _, _, _, _, _, _) = a
	
	

class TupleFifth a where
    type PartTFifth a
    fifth :: a -> PartTFifth a

instance (PartTFifth (a1, a2, a3, a4, a5) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5) where
    type  PartTFifth (a1, a2, a3, a4, a5) = a5
    fifth (_, _, _, _, a) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6) = a5
    fifth (_, _, _, _, a, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7) = a5
    fifth (_, _, _, _, a, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8) = a5
    fifth (_, _, _, _, a, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a5
    fifth (_, _, _, _, a, _, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a5
    fifth (_, _, _, _, a, _, _, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a5
    fifth (_, _, _, _, a, _, _, _, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a5
    fifth (_, _, _, _, a, _, _, _, _, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a5
    fifth (_, _, _, _, a, _, _, _, _, _, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a5
    fifth (_, _, _, _, a, _, _, _, _, _, _, _, _, _) = a

instance (PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a5 ) => TupleFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTFifth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a5
    fifth (_, _, _, _, a, _, _, _, _, _, _, _, _, _, _) = a
	
	
	

class TupleSixth a where
    type PartTSixth a
    sixth :: a -> PartTSixth a


instance (PartTSixth (a1, a2, a3, a4, a5, a6) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6) = a6
    sixth (_, _, _, _, _, a) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7) = a6
    sixth (_, _, _, _, _, a, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8) = a6
    sixth (_, _, _, _, _, a, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a6
    sixth (_, _, _, _, _, a, _, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a6
    sixth (_, _, _, _, _, a, _, _, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a6
    sixth (_, _, _, _, _, a, _, _, _, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a6
    sixth (_, _, _, _, _, a, _, _, _, _, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a6
    sixth (_, _, _, _, _, a, _, _, _, _, _, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a6
    sixth (_, _, _, _, _, a, _, _, _, _, _, _, _, _) = a

instance (PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a6 ) => TupleSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTSixth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a6
    sixth (_, _, _, _, _, a, _, _, _, _, _, _, _, _, _) = a	
	
	

class TupleSeventh a where
    type PartTSeventh a
    seventh :: a -> PartTSeventh a


instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7) = a7
    seventh (_, _, _, _, _, _, a) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8) = a7
    seventh (_, _, _, _, _, _, a, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a7
    seventh (_, _, _, _, _, _, a, _, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a7
    seventh (_, _, _, _, _, _, a, _, _, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a7
    seventh (_, _, _, _, _, _, a, _, _, _, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a7
    seventh (_, _, _, _, _, _, a, _, _, _, _, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a7
    seventh (_, _, _, _, _, _, a, _, _, _, _, _, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a7
    seventh (_, _, _, _, _, _, a, _, _, _, _, _, _, _) = a

instance (PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a7 ) => TupleSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTSeventh (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a7
    seventh (_, _, _, _, _, _, a, _, _, _, _, _, _, _, _) = a		
	


class TupleEighth a where
    type PartTEighth a
    eighth :: a -> PartTEighth a


instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8) = a8
    eighth (_, _, _, _, _, _, _, a) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a8
    eighth (_, _, _, _, _, _, _, a, _) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a8
    eighth (_, _, _, _, _, _, _, a, _, _) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a8
    eighth (_, _, _, _, _, _, _, a, _, _, _) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a8
    eighth (_, _, _, _, _, _, _, a, _, _, _, _) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a8
    eighth (_, _, _, _, _, _, _, a, _, _, _, _, _) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a8
    eighth (_, _, _, _, _, _, _, a, _, _, _, _, _, _) = a

instance (PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a8 ) => TupleEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTEighth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a8
    eighth (_, _, _, _, _, _, _, a, _, _, _, _, _, _, _) = a



class TupleNinth a where
    type PartTNinth a
    ninth :: a -> PartTNinth a


instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a9
    ninth (_, _, _, _, _, _, _, _, a) = a

instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a9
    ninth (_, _, _, _, _, _, _, _, a, _) = a

instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a9
    ninth (_, _, _, _, _, _, _, _, a, _, _) = a

instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a9
    ninth (_, _, _, _, _, _, _, _, a, _, _, _) = a

instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a9
    ninth (_, _, _, _, _, _, _, _, a, _, _, _, _) = a

instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a9
    ninth (_, _, _, _, _, _, _, _, a, _, _, _, _, _) = a

instance (PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a9 ) => TupleNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTNinth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a9
    ninth (_, _, _, _, _, _, _, _, a, _, _, _, _, _, _) = a	
	


class TupleTenth a where
    type PartTTenth a
    tenth :: a -> PartTTenth a


instance (PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a10 ) => TupleTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a10
    tenth (_, _, _, _, _, _, _, _, _, a) = a

instance (PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a10 ) => TupleTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a10
    tenth (_, _, _, _, _, _, _, _, _, a, _) = a

instance (PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a10 ) => TupleTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a10
    tenth (_, _, _, _, _, _, _, _, _, a, _, _) = a

instance (PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a10 ) => TupleTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a10
    tenth (_, _, _, _, _, _, _, _, _, a, _, _, _) = a

instance (PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a10 ) => TupleTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a10
    tenth (_, _, _, _, _, _, _, _, _, a, _, _, _, _) = a

instance (PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a10 ) => TupleTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTTenth (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a10
    tenth (_, _, _, _, _, _, _, _, _, a, _, _, _, _, _) = a
	
	

class TupleLst a where
    type PartTLst a
    lst :: a -> PartTLst a


instance (PartTLst (OneTuple a) ~ a ) => TupleLst (OneTuple a) where
    type  PartTLst (OneTuple a) = a
    lst (OneTuple a) = a
	
instance (PartTLst (a, b) ~ b ) => TupleLst (a, b) where
    type  PartTLst (a, b) = b
    lst (_,a) = a

instance (PartTLst (a, b, c) ~ c ) => TupleLst (a, b, c) where
    type  PartTLst (a, b, c) = c
    lst (_, _, a) = a
	
instance (PartTLst (a1, a2, a3, a4) ~ a4 ) => TupleLst (a1, a2, a3, a4) where
    type  PartTLst (a1, a2, a3, a4) = a4
    lst (_, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5) ~ a5 ) => TupleLst (a1, a2, a3, a4, a5) where
    type  PartTLst (a1, a2, a3, a4, a5) = a5
    lst (_, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6) ~ a6 ) => TupleLst (a1, a2, a3, a4, a5, a6) where
    type  PartTLst (a1, a2, a3, a4, a5, a6) = a6
    lst (_, _, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6, a7) ~ a7 ) => TupleLst (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTLst (a1, a2, a3, a4, a5, a6, a7) = a7
    lst (_, _, _, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6, a7, a8) ~ a8 ) => TupleLst (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTLst (a1, a2, a3, a4, a5, a6, a7, a8) = a8
    lst (_, _, _, _, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a9 ) => TupleLst (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a9
    lst (_, _, _, _, _, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a10 ) => TupleLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a10
    lst (_, _, _, _, _, _, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a11 ) => TupleLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a11
    lst (_, _, _, _, _, _, _, _, _, _, a) = a

instance (PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a12 ) => TupleLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTLst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a12
    lst (_, _, _, _, _, _, _, _, _, _, _, a) = a

	

class TuplePrelast a where
    type PartTPrelast a
    prelast :: a -> PartTPrelast a

	
instance (PartTPrelast (a, b) ~ a ) => TuplePrelast (a, b) where
    type  PartTPrelast (a, b) = a
    prelast (a, _) = a

instance (PartTPrelast (a, b, c) ~ b ) => TuplePrelast (a, b, c) where
    type  PartTPrelast (a, b, c) = b
    prelast (_, a, _) = a
	
instance (PartTPrelast (a1, a2, a3, a4) ~ a3 ) => TuplePrelast (a1, a2, a3, a4) where
    type  PartTPrelast (a1, a2, a3, a4) = a3
    prelast (_, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5) ~ a4 ) => TuplePrelast (a1, a2, a3, a4, a5) where
    type  PartTPrelast (a1, a2, a3, a4, a5) = a4
    prelast (_, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6) ~ a5 ) => TuplePrelast (a1, a2, a3, a4, a5, a6) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6) = a5
    prelast (_, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7) ~ a6 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7) = a6
    prelast (_, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8) ~ a7 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8) = a7
    prelast (_, _, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9) ~ a8 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a8
    prelast (_, _, _, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) ~ a9 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a9
    prelast (_, _, _, _, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) ~ a10 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = a10
    prelast (_, _, _, _, _, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) ~ a11 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = a11
    prelast (_, _, _, _, _, _, _, _, _, _, a, _) = a
	
instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) ~ a12 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = a12
    prelast (_, _, _, _, _, _, _, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) ~ a13 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a13
    prelast (_, _, _, _, _, _, _, _, _, _, _, _, a, _) = a

instance (PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) ~ a14 ) => TuplePrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    type  PartTPrelast (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = a14
    prelast (_, _, _, _, _, _, _, _, _, _, _, _, _, a, _) = a	