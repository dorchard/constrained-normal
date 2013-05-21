{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

module Control.Comonad.ConstrainedNormal
 -- ( -- * Constrained Normalised Comonads
 --   NC(..)--, liftNC, lowerNC, foldNC,
 -- )
where

import GHC.Exts (Constraint)

import Control.Comonad
-- import Control.Category.ConstrainedNormal

-------------------------------------------------------------------------------------------------

data CoK1 :: (* -> Constraint) -> (* -> *) -> * -> * -> * where
    CoK1 :: (NCC c d a -> b) -> CoK1 c d a b
    Extract1 :: CoK1 c d a a

data NC1 :: (* -> Constraint) -> (* -> *) -> * -> * where
    --Counit1 :: c x => x -> NC1 c d x 
    Cobind1 :: c x => (CoK1 c d x y) -> d x -> NC1 c d y

data NCC :: (* -> Constraint) -> (* -> *) -> * -> * where
    NCC :: NC1 c d x -> (forall w . c w => d w -> w) -> NCC c d x

liftNCo :: c x => (forall w . c w => d w -> w) -> d x -> NCC c d x
liftNCo counit dx = NCC (Cobind1 Extract1 dx) counit
                    --NCC (Counit1 (counit dx)) counit

lowerNCo :: forall a c d . (forall x . c x => (d x -> a) -> d x -> d a) -> NCC c d a -> d a
lowerNCo ext = lowerNC' where lowerNC' :: NCC c d a -> d a
                              lowerNC' (NCC (Cobind1 (CoK1 k) dx) counit) = ext (k . (liftNCo counit)) dx

instance Functor (NCC c d) where
    fmap = liftW

instance Comonad (NCC c d) where
    --extract (NCC (Counit1 x) _) = x

    extract (NCC (Cobind1 (CoK1 k) dx) counit) = k (liftNCo counit dx)
    extract (NCC (Cobind1 (Extract1) dx) counit) = counit dx
    
    --extend k (NCC (Counit1 x) counit) = NCC (Cobind1 (CoK1 k) ) counit

    extend k (NCC (Cobind1 Extract1 dx) counit) = NCC (Cobind1 (CoK1 k) dx) counit
    extend k (NCC (Cobind1 (CoK1 g) dx) counit) = 
         let w = Cobind1 (CoK1 $ k . extend g) dx -- undefined -- (k ((extend g) (liftNC1 counit dx)))
         in NCC w counit 

