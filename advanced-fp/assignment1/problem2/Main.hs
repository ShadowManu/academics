{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2018
License: MIT

Maintainer: manuelalejandropm@gmail.com

Example main to explain and verify the solution functions

-}

{-# LANGUAGE DeriveFoldable #-}

module Main (main) where

import Data.Foldable hiding (mapM_)
import Data.Monoid
import Data.Semigroup ((<>))

-- Max newtype
newtype Max a = Max { getMax :: Maybe a }
  deriving (Show, Eq)

-- Semigroup/Monoid Instance for Max
instance (Ord a) => Semigroup (Max a) where
  Max Nothing <> Max Nothing = Max Nothing
  Max Nothing <> y = y
  x <> Max Nothing = x

  Max (Just x) <> Max (Just y) = Max . Just $ max x y

instance (Ord a) => Monoid (Max a) where
  mempty = Max Nothing

-- Examples information
type Something = Int
data Node a = Node a [Node a]
  deriving (Eq, Show, Foldable)
-- Foldable is derived, but easily implementable as
-- instance Foldable Node where
  -- foldMap f (Node e ns) = f e `mappend` foldMap (foldMap f) ns

main :: IO ()
main = do
  -- Examples found on the assignment
  let e1 = foldMap (Max . Just) [] :: Max Something
  let e2 = foldMap (Max . Just) ["foo", "bar", "baz"]
  let e3 = foldMap (Max . Just) (Node 6 [Node 42 [], Node 7 [] ])
  let e4 = foldMap (Max . Just) (Node [] []) :: Max [Something]

  putStrLn "Monoid evaluation of the examples in the assignment"
  putStrLn "---------------------------------------------------"
  print e1
  print e2
  print e3
  print e4
