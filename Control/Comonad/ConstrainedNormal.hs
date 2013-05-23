{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

-- This module provides constrained normalised comonads, by Dominic Orchard,
--  based on the work of Neil Sculthorpe et al. documented in their paper:
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>


module Control.Comonad.ConstrainedNormal
  ( -- * Constrained Normalised Comonads
    NCM, liftNCM, lowerNCM,
    CopointedFunctor(..), NCPF(..), liftNCPF, lowerNCPF
  )
where

import GHC.Exts (Constraint)

import Control.Comonad

-------------------------------------------------------------------------------------------------

data NCM' :: (* -> Constraint) -> (* -> *) -> * -> * where
    Extend :: c x => (NCM c d x -> y) -> d x -> NCM' c d y
    ExtendExtract :: c x => d x -> NCM' c d x -- extension of extract, captures parameter for extract

-- The constrained 'extract' operation is contained in the normalised computation

data NCM :: (* -> Constraint) -> (* -> *) -> * -> * where
    NCM :: NCM' c d x -> (forall w . c w => d w -> w) -> NCM c d x

-- Comonad operations are split across the lifting and lowering functions

liftNCM :: c x => (forall w . c w => d w -> w) -> d x -> NCM c d x
liftNCM counit dx = NCM (ExtendExtract dx) counit  -- right-unit law

lowerNCM :: forall a c d . (forall x . c x => (d x -> a) -> d x -> d a) -> NCM c d a -> d a
lowerNCM ext = lowerNCM' where lowerNCM' :: NCM c d a -> d a
                               lowerNCM' (NCM (ExtendExtract dx) _) = dx
                               lowerNCM' (NCM (Extend k dx) counit) = ext (k . (liftNCM counit)) dx

instance Functor (NCM c d) where
    fmap = liftW

instance Comonad (NCM c d) where
    extract (NCM (Extend k dx) counit)      = k (liftNCM counit dx) -- left-unit law
    extract (NCM (ExtendExtract dx) counit) = counit dx             -- left-unit law

    extend k (NCM (ExtendExtract dx) counit) = NCM (Extend k dx) counit               -- right-unit law
    extend k (NCM (Extend g dx) counit)      = NCM (Extend (k . extend g) dx) counit  -- associativity law

-------------------------------------------------------------------------------------------------

class Functor f => CopointedFunctor (f :: * -> *) where
  copoint :: f a -> a

data NCPF :: (* -> Constraint) -> (* -> *) -> * -> * where
  FMap :: c x => (x -> a) -> t x -> (t x -> x) -> NCPF c t a

instance Functor (NCPF c t) where
  fmap :: (a -> b) -> NCPF c t a -> NCPF c t b
  fmap g (FMap h tx counit)  = FMap (g . h) tx counit -- composition law

instance CopointedFunctor (NCPF c t) where
  copoint :: NCPF c t a -> a
  copoint (FMap g tx counit) = g (counit tx)

-- destructive copoint operation must be given when lifting
liftNCPF :: c a => (t a -> a) -> t a -> NCPF c t a
liftNCPF counit ta = FMap id ta counit -- identity law

-- constructive fmap operation can be given when folding
foldNCPF :: (forall x. c x => (x -> a) -> t x -> r) -> NCPF c t a -> r
foldNCPF fmp (FMap g tx _) = fmp g tx

lowerNCPF :: (forall x. c x => (x -> a) -> t x -> t a) -> NCPF c t a -> t a
lowerNCPF  = foldNCPF

-------------------------------------------------------------------------------------------------
