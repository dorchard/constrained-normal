> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

Example of the ConstrainedNormal extension for comonads
Defines an "unboxed" array comonad which has IArray UArray constrains on its operations.

> import Control.Comonad.ConstrainedNormal

Uses codo-notation 

> import Language.Haskell.Codo

> import Control.Comonad
> import Data.Array.IArray
> import Data.Array.Unboxed

> data Arr i a = Arr (UArray i a) i 
> unArr (Arr a _) = a

Constrained-comonad definition

> extractUA :: (Ix i, IArray UArray e) => Arr i e -> e
> extractUA (Arr a c) = a!c

> extendUA :: (Ix i, IArray UArray a, IArray UArray b) => (Arr i a -> b) -> Arr i a -> Arr i b
> extendUA f (Arr a c) = let es' = map (\i -> (i, f (Arr a i))) (indices a)
>                      in  Arr (array (bounds a) es') c

> liftUA :: (IArray UArray a, Ix i) => Arr i a -> NCM (IArray UArray) (Arr i) a
> liftUA = liftNCM extractUA

> lowerUA :: (IArray UArray a, Ix i) => NCM (IArray UArray) (Arr i) a -> Arr i a
> lowerUA = lowerNCM extendUA

> x :: Arr (Int, Int) Float
> x = Arr (array ((0, 0), (2, 2)) [((0,0),1.0), ((0,1), 2.0), ((0,2), 3.0),
>                                  ((1,0),4.0), ((1,1), 5.0), ((1,2), 6.0),
>                                  ((2,0),7.0), ((2,1), 8.0), ((2,2), 9.0)]) (0,0)


> foo = [codo| x => y <- laplace2D (lowerUA x)
>                   z <- (extract y) + (extract x) 
>                   extract z |]

 *Main> assocs . unArr . lowerUA . extend foo . liftUA $ x
[((0,0),3.0),((0,1),3.0),((0,2),-1.0),((1,0),1.0),((1,1),5.0),((1,2),-1.0),((2,0),-9.0),((2,1),-3.0),((2,2),-13.0)]


> laplace2D :: (Fractional a, IArray UArray a) => Arr (Int, Int) a -> a
> laplace2D a = a ? (-1, 0) + a ? (1, 0) + a ? (0, -1) + a ? (0, 1) - 4 * a ? (0, 0)

> (?) :: (Ix i, IArray UArray a, Fractional a, Num i) => Arr i a -> i -> a
> (Arr a i) ? i' = if (inRange (bounds a) (i+i')) then a!(i+i') else 0.0

> instance (Num a, Num b) => Num (a, b) where
>     (x, y) + (a, b) = (x + a, y  + b)
>     (x, y) - (a, b) = (x - a, y - b)
>     (x, y) * (a, b) = (x * a, y * b)
>     abs (x, y) = (abs x, abs y)
>     signum (x, y) = (signum x, signum y)
>     fromInteger x = (fromInteger x, fromInteger x)