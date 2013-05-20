{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

module Control.Category.ConstrainedNormal 
    (NC(..), liftNC, lowerNC) where

import GHC.Exts (Constraint)

import Control.Category hiding ((.))
import qualified Control.Category as Cat((.))

instance Category (NC c a) where
    id      = Unit

    (.) :: NC c a y z -> NC c a x y -> NC c a x z
    (.) h (Comp g f) = Comp (h <<< g) f  -- associativity
    (.) h Unit       = h                 -- right-unit law

data NC :: (* -> Constraint) -> (* -> * -> *) -> * -> * -> * where
    Unit :: NC c a x x
    Comp :: c y => NC c a y z -> a x y -> NC c a x z 

liftNC :: (c y) => a x y -> NC c a x y
liftNC f = Comp Unit f                 -- left-unit law

lowerNC :: forall x a c z . a z z -> (forall x y . c y => a y z -> a x y -> a x z) -> NC c a x z -> a x z
lowerNC unit comp = lowerNA' where lowerNA' :: forall y . NC c a y z -> a y z
                                   lowerNA' Unit       = unit
                                   lowerNA' (Comp g f) = comp (lowerNA' g) f