{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

-- |
-- Module: Control.Category.ConstrainedNormal
-- Copyright: (c) 2013 The University of Kansas
-- License: BSD3
--
-- Maintainer: Neil Sculthorpe <neil@ittc.ku.edu>
--             (Control.Category.ConstrainedNormal added by Dominic Orchard)
-- Stability: alpha
-- Portability: ghc
--
-- This module provides constrained normalised type classes.  The ideas behind this module are documented in the following paper:
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>

module Control.Category.ConstrainedNormal 
    (NC(..), liftNC, lowerNC) where

import GHC.Exts (Constraint)

import Control.Category hiding ((.))
import qualified Control.Category as Cat((.))

data NC :: (* -> Constraint) -> (* -> * -> *) -> * -> * -> * where
    Unit :: NC c a x x
    Comp :: c y => NC c a y z -> a x y -> NC c a x z 

instance Category (NC c a) where
    id      = Unit

    (.) :: NC c a y z -> NC c a x y -> NC c a x z
    (.) h (Comp g f) = Comp (h <<< g) f  -- associativity
    (.) h Unit       = h                 -- right-unit law

liftNC :: (c z) => a x z -> NC c a x z
liftNC f = Comp Unit f                  -- left-unit law

lowerNC :: forall x a c z . a z z -> (forall u v . c v => a v z -> a u v -> a u z) -> NC c a x z -> a x z
lowerNC unit comp = lowerNA' where lowerNA' :: forall y . NC c a y z -> a y z
                                   lowerNA' Unit       = unit
                                   lowerNA' (Comp g f) = comp (lowerNA' g) f

