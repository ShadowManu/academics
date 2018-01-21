{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2018
License: MIT

Maintainer: manuelalejandropm@gmail.com

Data types used to represent the linear regression concepts

-}
module Types
( Sample(Sample)
, x
, y

, Hypothesis(Hypothesis)
, c
) where

-- A solution sample, represented by the vector and asssociated value
data Sample a = Sample { x :: [ a ] , y :: a }
  deriving (Show)

-- Coefficients vector to minimize the prediction error
newtype Hypothesis a = Hypothesis { c :: [a] }
  deriving (Show)