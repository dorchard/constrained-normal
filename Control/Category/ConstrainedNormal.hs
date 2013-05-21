{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

-- This module provides constrained normalised categories, by Dominic Orchard,
--  based on the work of Neil Sculthorpe et al. documented in their paper:
--
--   /The Constrained-Monad Problem/.  Neil Sculthorpe and Jan Bracker and George Giorgidze and Andy Gill.  2013. <http://www.ittc.ku.edu/~neil/papers_and_talks/constrained-monad-problem.pdf>

module Control.Category.ConstrainedNormal
    ( NC(..),
      liftNC,
      lowerNC,
      PointedCategory(..),
      NPC(..),
      liftNPC
) where

import GHC.Exts (Constraint)

import Prelude hiding (id,(.))
import Control.Category

-------------------------------------------------------------------------------------------------

-- In a normal form that associates to the left

data NC :: (* -> Constraint) -> (* -> * -> *) -> * -> * -> * where
    Unit :: NC c a x x
    Comp :: c y => NC c a y z -> a x y -> NC c a x z

instance Category (NC c a) where
    id  :: NC c a x x
    id  = Unit

    (.) :: NC c a y z -> NC c a x y -> NC c a x z
    (.) h (Comp g f) = Comp (h <<< g) f  -- associativity
    (.) h Unit       = h                 -- right-unit law

liftNC :: c z => a x z -> NC c a x z
liftNC f = Comp Unit f                  -- left-unit law

foldNC :: forall a b c r t. r b b -> (forall x y. c y => r y b -> t x y -> r x b) -> NC c t a b -> r a b
foldNC unit comp = foldNC'
  where
    foldNC' :: forall z. NC c t z b -> r z b
    foldNC' Unit        = unit
    foldNC' (Comp h g)  = comp (foldNC' h) g

lowerNC :: forall x a c z . a z z -> (forall u v . c v => a v z -> a u v -> a u z) -> NC c a x z -> a x z
lowerNC = foldNC

-------------------------------------------------------------------------------------------------

-- In a normal form that associates to the right, where all Arrs must be interspersed with Prims.

data NPC :: (* -> Constraint) -> (* -> * -> *) -> * -> * -> * where
  Arr    ::               (a -> b)                          -> NPC c t a b
  Comp2  :: (c x, c y) => (y -> b) -> t x y -> NPC c t a x  -> NPC c t a b

liftNPC :: (c a, c b) => t a b -> NPC c t a b
liftNPC g = Comp2 id g (Arr id)

-- TODO: Think about a suitable fold definition.

instance Category (NPC c t) where
  id :: NPC c t a a
  id  = Arr id

  (.) :: NPC c t b d -> NPC c t a b -> NPC c t a d
  (Arr g)       . (Arr h)        = Arr (g . h)         -- distributivity law
  (Arr g)       . (Comp2 h i j)  = Comp2 (g . h) i j   -- associativity law
  (Comp2 g h i) . j              = Comp2 g h (i . j)   -- associativity law


class Category cat => PointedCategory (cat :: * -> * -> *) where
  arr :: (a -> b) -> cat a b

instance PointedCategory (NPC c t) where
  arr :: (a -> b) -> NPC c t a b
  arr = Arr

-------------------------------------------------------------------------------------------------
