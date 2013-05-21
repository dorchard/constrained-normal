{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

-- |
-- Module: Control.Monad.ConstrainedNormal
-- Copyright: (c) 2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
-- Stability: alpha
-- Portability: ghc
--
-- This module provides constrained normalised type classes.  The ideas behind this module are documented in the following paper:
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>

module Control.Monad.ConstrainedNormal
  ( -- * Constrained Normalised Functors
    NF(..), liftNF, lowerNF, foldNF,
    -- * Constrained Normalised Pointed Functors
    PointedFunctor(..), NPF(..), liftNPF, lowerNPF, foldNPF,
    -- * Constrained Normalised Applicative Functors
    NAF(..), liftNAF, lowerNAF, foldNAF,
    -- * Constrained Normalised Monads
    NM(..), liftNM, lowerNM, foldNM,
    -- * Constrained Normalised MonadPlus
    NMP(..), NMP'(..), liftNMP, lowerNMP, foldNMP,
    -- * Utilities
    Unconstrained
  )
where

import GHC.Exts (Constraint)

import Control.Applicative
import Control.Monad

-------------------------------------------------------------------------------------------------

data NF :: (* -> Constraint) -> (* -> *) -> * -> * where
  FMap :: c x => (x -> a) -> t x -> NF c t a

instance Functor (NF c t) where
  fmap :: (a -> b) -> NF c t a -> NF c t b
  fmap g (FMap h tx)  = FMap (g . h) tx  -- composition law

liftNF :: c a => t a -> NF c t a
liftNF ta = FMap id ta    -- identity law

foldNF :: (forall x. c x => (x -> a) -> t x -> r) -> NF c t a -> r
foldNF fmp (FMap g tx) = fmp g tx

lowerNF :: (forall x. c x => (x -> a) -> t x -> t a) -> NF c t a -> t a
lowerNF  = foldNF

-------------------------------------------------------------------------------------------------

class Functor f => PointedFunctor (f :: * -> *) where
  point :: a -> f a

data NPF :: (* -> Constraint) -> (* -> *) -> * -> * where
  Point   :: a        -> NPF c t a
  Functor :: NF c t a -> NPF c t a

instance Functor (NPF c t) where
  fmap :: (a -> b) -> NPF c t a -> NPF c t b
  fmap g (Point a)     = Point (g a)  -- pointed law
  fmap g (Functor fa)  = Functor (fmap g fa)

instance PointedFunctor (NPF c t) where
  point :: a -> NPF c t a
  point = Point

liftNPF :: c a => t a -> NPF c t a
liftNPF = Functor . liftNF

foldNPF :: (a -> r) -> (forall x. c x => (x -> a) -> t x -> r) -> NPF c t a -> r
foldNPF poi _ (Point a)     = poi a
foldNPF _ fmp (Functor fa)  = foldNF fmp fa

lowerNPF :: (a -> t a) -> (forall x. c x => (x -> a) -> t x -> t a) -> NPF c t a -> t a
lowerNPF  = foldNPF

-------------------------------------------------------------------------------------------------

data NM :: (* -> Constraint) -> (* -> *) -> * -> * where
  Return :: a                             -> NM c t a
  Bind   :: c x => t x -> (x -> NM c t a) -> NM c t a

instance Functor (NM c t) where
  fmap :: (a -> b) -> NM c t a -> NM c t b
  fmap = liftM

instance PointedFunctor (NM c t) where
  point :: a -> NM c t a
  point = return

instance Applicative (NM c t) where
  pure :: a -> NM c t a
  pure = return

  (<*>) :: NM c t (a -> b) -> NM c t a -> NM c t b
  (<*>) = ap

instance Monad (NM c t) where
  return :: a -> NM c t a
  return = Return

  (>>=) :: NM c t a -> (a -> NM c t b) -> NM c t b
  (Return a)   >>= k  = k a                         -- left-identity law
  (Bind ta h)  >>= k  = Bind ta (\ a -> h a >>= k)  -- associativity law

liftNM :: c a => t a -> NM c t a
liftNM ta = Bind ta Return -- right-identity law

foldNM :: forall a c r t. (a -> r) -> (forall x. c x => t x -> (x -> r) -> r) -> NM c t a -> r
foldNM ret bind = foldNM'
  where
    foldNM' :: NM c t a -> r
    foldNM' (Return a)   = ret a
    foldNM' (Bind tx k)  = bind tx (foldNM' . k)

lowerNM :: forall a c t. (a -> t a) -> (forall x. c x => t x -> (x -> t a) -> t a) -> NM c t a -> t a
lowerNM = foldNM

-------------------------------------------------------------------------------------------------

data NMP (c :: * -> Constraint) (t :: * -> *) (a :: *)
  =  MZero
  |  MPlus (NMP' c t a) (NMP c t a)

data NMP' :: (* -> Constraint) -> (* -> *) -> * -> * where
  MPReturn :: a                              -> NMP' c t a
  MPBind   :: c x => t x -> (x -> NMP c t a) -> NMP' c t a

instance Functor (NMP c t) where
  fmap :: (a -> b) -> NMP c t a -> NMP c t b
  fmap = liftM

instance PointedFunctor (NMP c t) where
  point :: a -> NMP c t a
  point = return

instance Applicative (NMP c t) where
  pure :: a -> NMP c t a
  pure = return

  (<*>) :: NMP c t (a -> b) -> NMP c t a -> NMP c t b
  (<*>) = ap

toNMP :: NMP' c t a -> NMP c t a
toNMP n = MPlus n MZero -- right-unit law

instance Monad (NMP c t) where
  return :: a -> NMP c t a
  return a = toNMP (MPReturn a)

  (>>=) :: NMP c t a -> (a -> NMP c t b) -> NMP c t b
  MZero         >>= _  = MZero                             -- left-zero law
  MPlus n1 n2   >>= k  = mplus (bindNMP' n1 k) (n2 >>= k)  -- left-distribution law

bindNMP' :: NMP' c t a -> (a -> NMP c t b) -> NMP c t b
bindNMP' (MPReturn a)   k  = k a                                   -- left-identity law
bindNMP' (MPBind tx h)  k  = toNMP (MPBind tx (\ a -> h a >>= k))  -- associativity law

instance MonadPlus (NMP c t) where
  mzero :: NMP c t a
  mzero = MZero

  mplus :: NMP c t a -> NMP c t a -> NMP c t a
  mplus MZero n            = n                       -- left-unit law
  mplus (MPlus n1 n2) n3   = MPlus n1 (mplus n2 n3)  -- associativity law

liftNMP :: c a => t a -> NMP c t a
liftNMP ta = toNMP (MPBind ta return) -- right-identity law

foldNMP :: forall a c r t. r -> (r -> r -> r) -> (a -> r) -> (forall x. c x => t x -> (x -> r) -> r) -> NMP c t a -> r
foldNMP zero plus ret bind = foldNMPmonoid
  where
    foldNMPmonoid :: NMP c t a -> r
    foldNMPmonoid MZero          = zero
    foldNMPmonoid (MPlus n1 n2)  = plus (foldNMPmonad n1) (foldNMPmonoid n2)

    foldNMPmonad :: NMP' c t a -> r
    foldNMPmonad (MPReturn a)   = ret a
    foldNMPmonad (MPBind tx k)  = bind tx (foldNMPmonoid . k)

lowerNMP :: forall a c t. t a -> (t a -> t a -> t a) -> (a -> t a) -> (forall x. c x => t x -> (x -> t a) -> t a) -> NMP c t a -> t a
lowerNMP = foldNMP

-------------------------------------------------------------------------------------------------

data NAF :: (* -> Constraint) -> (* -> *) -> * -> * where
  Pure :: a                              -> NAF c t a
  Ap   :: c x => NAF c t (x -> a) -> t x -> NAF c t a

instance Functor (NAF c t) where
  fmap :: (a -> b) -> NAF c t a -> NAF c t b
  fmap f n = pure f <*> n

instance PointedFunctor (NAF c t) where
  point :: a -> NAF c t a
  point = pure

instance Applicative (NAF c t) where
  pure :: a -> NAF c t a
  pure = Pure

  (<*>) :: NAF c t (a -> b) -> NAF c t a -> NAF c t b
  (Pure g) <*> (Pure a)  = Pure (g a)                  -- homomorphism law
  n1 <*> (Pure a)    = Pure (\ g -> g a) <*> n1        -- interchange law
  n1 <*> (Ap n2 tx)  = Ap (Pure (.) <*> n1 <*> n2) tx  -- composition law

liftNAF :: c a => t a -> NAF c t a
liftNAF ta = Ap (Pure id) ta  -- identity law

foldNAF :: forall a c r t. (forall x. x -> r x) -> (forall y z. c y => r (y -> z) -> t y -> r z) -> NAF c t a -> r a
foldNAF pur app = foldNAF'
  where
    foldNAF' :: forall b. NAF c t b -> r b
    foldNAF' (Pure b)   = pur b
    foldNAF' (Ap n tx)  = app (foldNAF' n) tx

lowerNAF :: (forall x. x -> t x) -> (forall y z. c y => t (y -> z) -> t y -> t z) -> NAF c t a -> t a
lowerNAF = foldNAF

-------------------------------------------------------------------------------------------------

-- | An empty type class.  This can be used when a parameter of kind @*@ @->@ 'Constraint' is needed, but no constraints need to be imposed.
class Unconstrained (a :: *) where

instance Unconstrained a where

-------------------------------------------------------------------------------------------------
