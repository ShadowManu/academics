module Main where

data Bonus f a = Malus a
               | Bonus (f (Bonus f a))

instance Functor f => Functor (Bonus f) where
  fmap g (Malus a) = Malus $ g a
  fmap g (Bonus f) = Bonus $ fmap (fmap g) f

instance Functor f => Applicative (Bonus f) where
  pure = Malus
  (Malus g) <*> b = fmap g b
  b <*> (Malus a) = Malus ($ a) <*> b
  b <*> (Bonus f2) = Bonus $ fmap (b <*>) f2

instance Functor f => Monad (Bonus f) where
  (Malus a) >>= g = g a
  (Bonus f) >>= g = Bonus $ fmap (>>= g) f

main = do
  putStrLn "This program does not have a particular execution. See the source code or modify the main function."
  return ()