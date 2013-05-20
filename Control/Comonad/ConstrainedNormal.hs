{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

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

module Control.Comonad.ConstrainedNormal
  ( -- * Constrained Normalised Comonads
    NC(..)--, liftNC, lowerNC, foldNC,
  )
where

import GHC.Exts (Constraint)

import Control.Comonad
import Control.Category hiding ((.))
import qualified Control.Category as Category


-------------------------------------------------------------------------------------------------

--data CoKleisli :: (* -> Constraint) -> (* -> *) -> * -> * -> * where
--    CoKleisli :: (NC c d a -> b) -> CoKleisli c d a b
--    Extract :: (d a -> a) -> CoKleisli c d a a

--unCoKleisli :: CoKleisli c d a b -> (NC c d a -> b)
---unCoKleisli (Extract k) = k
--unCoKleisli (CoKleisli k) = k


--class Lower a c x z where
--    lower :: a z z -> (forall x y . Lower a c y z => a y z -> a x y -> a x z) -> NA c a x z -> a x z
--    lower unit comp (Comp g f) = comp (lower unit comp g) f

--    lower unit comp Unit = unit

--instance Lower a c x z where


data NC :: (* -> Constraint) -> (* -> *) -> * -> * where
    -- Extract :: (NC c d a -> a, NC c d a) -> NC c d a
    Extract :: d a -> NC c d a
    -- Ext :: CoKleisli c d a a -> NC c d a a
    -- Extt :: a -> NC c d a a
    --Extend :: c a => (CoKleisli c d a b) -> d a -> NC c d b
    --Extend :: c a => (NC c d a -> b) -> d a -> NC c d b
    Extend :: c x => (NC c d x -> y) -> d x -> NC c d y
    -- Extend :: c x => (d x -> y) -> NC c d x -> NC c d y

instance Functor (NC c t) where
  fmap :: (a -> b) -> NC c t a -> NC c t b
  fmap = liftW

instance Comonad (NC c t) where
  --extract :: NC c t a -> a
  --extract = 

  extend :: (NC c t a -> b) -> NC c t a -> NC c t b
  -- extend k (Extract a)   = Extract (k a)                   -- right-identity law
  --extend k (Extend h da) = Extend (CoKleisli $ \a -> k (extend (unCoKleisli h) a)) da -- associativity law
  -- extend k (Extend h da) = Extend (\a -> k (extend h a)) da -- associativity law
  --extend k (Extend h da) = Extend (\a -> k (extend h a)) da -- associativity law
 

--dextend :: (NC c t a -> b) -> NC c t a -> NC c t b
--dextend k (Extend h da) = Extend da undefined

-- Extend da (\a -> k (extend h a)) -- associativity law

--liftNC :: c a => (d a -> a) -> d a -> NC c d a
--liftNC ext da = Extend (Extract ext) da -- right-identity law

--liftNC :: c a => (d a -> a) -> d a -> NC c d a
--liftNC counit da = Extend counit (Extract da) -- right-identity law


--foldNC :: forall a c r d . (r -> a) -> (forall x. c a => (r -> a) -> r -> d a) -> r -> NC c d a  
--foldNC coret cobind = foldNC'
--                        where
--                          foldNC' :: r -> NC c d a
                          -- foldNC' (Extract da) = coret da
--                          foldNC' (Extend k dx)  = extend k (foldNC' dx)

--lowerNM :: forall a c t. (d a -> a) -> (forall x. c x => (d x -> a) -> d x -> d a) -> d a -> NC c d a
--lowerNM coret cobind = foldNM'
--                           where
--                             foldNM' :: NM c t a -> r
--                             foldNM' (Return a)   = ret a
--                             foldNM' (Bind tx k)  = bind tx (foldNM' . k)


lowerNC :: forall a c d . (forall x . c x => (d x -> a) -> d x -> d a) -> NC c d a -> d a
lowerNC = undefined

--lowerNM :: forall a c t. (a -> t a) -> (forall x. c x => t x -> (x -> t a) -> t a) -> NM c t a -> t a
--lowerNM = foldNM

--data CY (c :: * -> Constraint) (d :: * -> *) (a :: *) where CY :: c r => ((d a -> r) -> d r) -> CY c d a

--liftCY :: (forall r . c r => (d a -> r) -> d a -> d r) -> d a -> CY c d a
--liftCY cbind da = CY (\k -> cbind k da)
    
