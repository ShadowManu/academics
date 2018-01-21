{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2018
License: MIT

Maintainer: manuelalejandropm@gmail.com

Example main to explain and verify algorithm implementation of linear regressions in haskell

-}
module Main (main) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Types (Sample(Sample), x, y)
import Constants (alpha, guess)
import Samples (training)
import Solution (gd, theta)

sample :: Sample Double
sample = Sample { x = [1.0, -0.44127, -0.22368], y = undefined }

main :: IO ()
main = do
  let
    results = gd alpha guess training
    lastResult = last results
    (_, vector, _) = lastResult

  putStrLn "Printing first three algorithm results"
  putStrLn "--------------------------------------"
  mapM_ print . take 3 $ results
  putStr "\n"

  putStrLn "Printing last result"
  putStrLn "--------------------"
  print lastResult
  putStr "\n"

  print $ "The vector " ++ show (x sample) ++ " has value " ++ show (theta vector sample)
  putStr "\n"

  -- Generate the image file from iterations and (decreasing) cost
  toFile def "works.png" $ do
    layout_title .= "Cost over iterations"
    plot (line "am" . (:[]) . map (\(x,_,y) -> (x,y)) $ results)

  return ()
