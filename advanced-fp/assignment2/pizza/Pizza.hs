module Main where

data Want a = Want ((a -> Pizza) -> Pizza)

data Pizza = Eat (IO Pizza)
             | Combo Pizza Pizza
             | Happy

instance Show Pizza where
  show (Eat x) = " :-P "
  show (Combo x y) = " combo(" 
                       ++ show x
                       ++ ","
                       ++ show y 
                       ++ ") "
  show Happy = " :-D "

want :: Want a -> Pizza
want (Want outer) = outer $ const Happy

happy :: Want a
happy = Want outer
  where outer inner = Happy

nomnom :: IO a -> Want a
nomnom io = Want outer
  where outer inner = Eat $ inner <$> io

combo :: Want a -> Want ()
combo w = Want outer
  where
    outer inner = Combo pizza1 pizza2
      where
        pizza1 = want w
        pizza2 = inner ()

pana :: Want a -> Want a -> Want a
pana (Want outer1) (Want outer2) = Want outer
  where outer inner = Combo (outer1 inner) (outer2 inner)

pizzeria :: [Pizza] -> IO ()
pizzeria [] = putStrLn ""
pizzeria ((Eat io) : ps) = io >>= \p -> pizzeria (ps ++ [p])
pizzeria ((Combo p1 p2) : ps) = pizzeria (ps ++ [p1, p2])
pizzeria (Happy : ps) = pizzeria ps

instance Functor Want where
  fmap f a = pure f <*> a

instance Applicative Want where
  pure = return
  fab <*> fa = do
    ab <- fab
    a <- fa
    return $ ab a

instance Monad Want where
  return x = happy
  m >>= f = Want $ \inner -> getWant m $ \a -> getWant (f a) inner
    where getWant (Want x) = x

hambre :: Want ()
hambre = pana (ponle (topping 42))
              (pana (ponle (topping 69))
                    (pana (ponle (topping 17))
                          (ponle (topping 23) >> nomnom (putStrLn ""))))

tengo :: Want a -> IO ()
tengo x = pizzeria [want x]

topping :: Int -> String
topping 17 = "/nlmce"
topping 23 = "/y./p6"
topping 42 = "htptuc2"
topping 69 = "t:irofr"

ponle :: String -> Want ()
ponle xs = mapM_ (nomnom . putChar) xs

main = do
  putStrLn "Start..."
  tengo hambre >> putStrLn ""
  putStrLn "End..."
