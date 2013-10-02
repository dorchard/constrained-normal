{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

-- This module provides constrained normalised comonads, by Dominic Orchard,
--  based on the work of Neil Sculthorpe et al. documented in their paper: 
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>


module Control.Comonad.ConstrainedNormalAlt
  ( -- * Constrained Normalised Comonads
    NCM, liftNCM, lowerNCM
  )
where

import GHC.Exts (Constraint)

import Control.Comonad

-------------------------------------------------------------------------------------------------

data NCM' :: (* -> Constraint) -> (* -> *) -> * -> * where
    Extend :: c x => (NCM c d x -> y) -> NCM c d x -> NCM' c d y
    ExtendExtract :: c x => d x -> NCM' c d x -- extension of extract, captures parameter for extract

-- The constrained 'extract' operation is contained in the normalised computation

data NCM :: (* -> Constraint) -> (* -> *) -> * -> * where
    NCM :: NCM' c d x -> (forall w . c w => d w -> w) -> NCM c d x

-- Comonad operations are split across the lifting and lowering functions

liftNCM :: c x => (forall w . c w => d w -> w) -> d x -> NCM c d x
liftNCM counit dx = NCM (ExtendExtract dx) counit  -- right-unit law

lowerNCM :: forall a c d . c a => (forall x y . (c x, c y) => (d x -> y) -> d x -> d y) -> NCM c d a -> d a
lowerNCM ext = lowerNCM' where lowerNCM' :: NCM c d a -> d a
                               lowerNCM' (NCM (ExtendExtract dx) _) = dx
                               lowerNCM' (NCM (Extend k dx) counit) = ext (k . (liftNCM counit)) (lowerNCM ext dx)

instance Functor (NCM c d) where
    fmap = liftW

instance Comonad (NCM c d) where
    extract (NCM (Extend k dx) counit)      = k dx       -- left-unit law
    extract (NCM (ExtendExtract dx) counit) = counit dx  -- right-unit law

    extend k (NCM (ExtendExtract dx) counit) = NCM (Extend k (liftNCM counit dx)) counit -- right-unit law/lift
    extend k (NCM (Extend g dx) counit)      = NCM (Extend (k . extend g) dx) counit  -- associativity law

