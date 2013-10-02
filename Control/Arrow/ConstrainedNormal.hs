{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances, NoMonomorphismRestriction #-}

-- This module provides constrained normalised comonads, by Dominic Orchard,
--  based on the work of Neil Sculthorpe et al. documented in their paper: 
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>


module Control.Arrow.ConstrainedNormal where

import GHC.Exts (Constraint)

import Control.Category
import Control.Arrow

data NCA' :: (* -> Constraint) -> (* -> * -> *) -> * -> * -> * where
  Arr :: (x -> y) -> NCA' c a x y
  Unit :: NCA' c a x x
  Comp ::  NCA c a y z -> a x y -> NCA' c a x z
  First :: a x y -> NCA' c a (x, z) (y, z)

data NCA :: (* -> Constraint) -> (* -> * -> *) -> * -> * -> * where
  NCA :: NCA' c a x y -> (forall x y z . a x y -> a (x, z) (y, z)) -> NCA c a x y
  First' :: NCA c a x y -> NCA c a (x, z) (y, z) -- POSSIBLE HACK

instance Category (NCA c a) where
    id  = NCA (Arr (\x -> x)) undefined -- don't know a "first" to put here

    (.) h (NCA (Comp g f) rst) = NCA (Comp (h <<< g) f) rst  -- associativity
    (.) h (NCA Unit rst)       = h                           -- right-unit law

instance Arrow (NCA c a) where
    arr f = NCA (Arr f) undefined -- UH oh - don't know a "first" to put here

    first (NCA (Arr f) firstm) = NCA (Arr (first f)) firstm -- first/arr law
    first (NCA (Comp g f) firstm) = NCA (Comp (First' g) (firstm f)) firstm -- POSSIBLE HACK
    first (NCA (First f) firstm) = NCA (First (firstm f)) firstm


{-
liftNCA :: (forall u v w . a u v -> a (u, w) (v, w)) -> a x y -> NCA c a x y
liftNCA first axy = NCA (Comp Unit axy) first
-}

{-
lowerNCA :: forall x a c z . 
             a z z 
              -> (forall u v . a v z -> a u v -> a u z)
              -> (forall u v . (u -> v) -> a u v) 
              -> NCA c a x z -> a x z
lowerNCA unit comp pure = lowerNCA'
                            where
                              lowerNCA' :: forall u . NCA c a u z -> a u z
                              lowerNCA' (NCA Unit _) = unit
                              lowerNCA' (NCA (Comp g f) fstm) = comp (lowerNCA' (NCA g fstm)) f
                              lowerNCA' (NCA (Arr f) _) = pure f
                              lowerNCA' (NCA (ArrFirst f) fstm) = pure $ first f -- uses first for (->)
                              --lowerNCA' (NCA (CompFirst g f) fstm) = (fstm (lowerNCA' (NCA g fstm))) `comp` (fstm f)
                              
  
    

liftNCA' :: a x y -> NCA' c a x y
liftNCA' f = Comp Unit f

lowerNCA' :: forall x a c z . 
            a z z 
              -> (forall u v . a v z -> a u v -> a u z)
              -> (forall u v . (u -> v) -> a u v) 
              -> (forall u v z . a u v -> a (u, z) (v, z))
              -> NCA' c a x z -> a x z
lowerNCA' unit comp pure first = lowerNCA''
                            where 
                              lowerNCA'' :: forall u . NCA' c a u z -> a u z
                              lowerNCA'' Unit       = unit
                              lowerNCA'' (Comp h g) = comp (lowerNCA'' h) g
                              lowerNCA'' (Arr f)     = pure f
-}