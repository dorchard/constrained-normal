{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

-- This module provides constrained normalised comonads, by Dominic Orchard,
--  based on the work of Neil Sculthorpe et al. documented in their paper: 
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>


module Control.Comonad.ConstrainedNormal
  ( -- * Constrained Normalised Comonads
    NCM(..), liftNCM, lowerNCM
  )
where

import GHC.Exts (Constraint)

import Control.Comonad

-------------------------------------------------------------------------------------------------

data NCoKleisli :: (* -> Constraint) -> (* -> *) -> * -> * -> * where
    NCK :: (NCM c d a -> b) -> NCoKleisli c d a b
    Extract :: NCoKleisli c d a a

data NCM' :: (* -> Constraint) -> (* -> *) -> * -> * where
    Extend :: c x => (NCoKleisli c d x y) -> d x -> NCM' c d y

-- The constrained 'extract' operation is contained in the normalised computation

data NCM :: (* -> Constraint) -> (* -> *) -> * -> * where
    NCM :: NCM' c d x -> (forall w . c w => d w -> w) -> NCM c d x

-- Comonad operations are split across the lifting and lowering functions

liftNCM :: c x => (forall w . c w => d w -> w) -> d x -> NCM c d x
liftNCM counit dx = NCM (Extend Extract dx) counit

lowerNCM :: forall a c d . (forall x . c x => (d x -> a) -> d x -> d a) -> NCM c d a -> d a
lowerNCM ext = lowerNCM' where lowerNCM' :: NCM c d a -> d a
                               lowerNCM' (NCM (Extend Extract dx) _)      = dx
                               lowerNCM' (NCM (Extend (NCK k) dx) counit) = ext (k . (liftNCM counit)) dx

instance Functor (NCM c d) where
    fmap = liftW

instance Comonad (NCM c d) where
    extract (NCM (Extend (NCK k) dx) counit) = k (liftNCM counit dx)
    extract (NCM (Extend Extract dx) counit) = counit dx

    extend k (NCM (Extend Extract dx) counit) = NCM (Extend (NCK k) dx) counit
    extend k (NCM (Extend (NCK g) dx) counit) = NCM (Extend (NCK $ k . extend g) dx) counit 

