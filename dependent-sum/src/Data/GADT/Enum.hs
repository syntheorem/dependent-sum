{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PolyKinds #-}
#endif
module Data.GADT.Enum where

import Data.Some

class GEnum tag where
  gsucc :: tag a -> Some tag
  gpred :: tag a -> Some tag
  gtoEnum :: Int -> Some tag
  gfromEnum :: tag a -> Int
  genumFrom :: tag a -> [Some tag]
  genumFromThen :: tag a -> tag b -> [Some tag]
  genumFromTo :: tag a -> tag b -> [Some tag]
  genumFromThenTo :: tag a -> tag b -> tag c -> [Some tag]

  gsucc                   = gtoEnum . (+ 1)  . gfromEnum
  gpred                   = gtoEnum . (subtract 1) . gfromEnum
  genumFrom x             = map gtoEnum [gfromEnum x ..]
  genumFromThen x y       = map gtoEnum [gfromEnum x, gfromEnum y ..]
  genumFromTo x y         = map gtoEnum [gfromEnum x .. gfromEnum y]
  genumFromThenTo x1 x2 y = map gtoEnum [gfromEnum x1, gfromEnum x2 .. gfromEnum y]

class GBounded tag where
  gmaxBound :: Some tag
  gminBound :: Some tag

-- | Default method for bounded enumerations.
gboundedEnumFrom :: forall tag a. (GEnum tag, GBounded tag) => tag a -> [Some tag]
gboundedEnumFrom n = map gtoEnum [gfromEnum n .. withSome (gmaxBound @tag) gfromEnum]

-- | Default method for bounded enumerations.
gboundedEnumFromThen :: forall tag a b. (GEnum tag, GBounded tag) => tag a -> tag b -> [Some tag]
gboundedEnumFromThen n1 n2
  | i_n2 >= i_n1  = map gtoEnum [i_n1, i_n2 .. withSome (gmaxBound @tag) gfromEnum]
  | otherwise     = map gtoEnum [i_n1, i_n2 .. withSome (gminBound @tag) gfromEnum]
  where
    i_n1 = gfromEnum n1
    i_n2 = gfromEnum n2

instance GEnum tag => Enum (Some tag) where
  succ           (Some a)                   = gsucc a
  pred           (Some a)                   = gpred a
  toEnum                                    = gtoEnum
  fromEnum       (Some a)                   = gfromEnum a
  enumFrom       (Some a)                   = genumFrom a
  enumFromThen   (Some a) (Some b)          = genumFromThen a b
  enumFromTo     (Some a) (Some b)          = genumFromTo a b
  enumFromThenTo (Some a) (Some b) (Some c) = genumFromThenTo a b c

instance GBounded tag => Bounded (Some tag) where
  maxBound = gmaxBound
  minBound = gminBound

