{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2018
License: MIT

Maintainer: manuelalejandropm@gmail.com

Defines constants used throughout the solution

-}
module Constants
( alpha
, epsilon
, guess
) where

import Types

alpha :: Double
alpha = 0.03

epsilon :: Double
epsilon = 0.0000001

guess :: Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }
