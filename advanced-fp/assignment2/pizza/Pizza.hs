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
want = undefined

happy :: Want a
happy = undefined

nomnom :: IO a -> Want a
nomnom = undefined

combo :: Want a -> Want ()
combo = undefined

pana :: Want a -> Want a -> Want a
pana = undefined

pizzeria :: [Pizza] -> IO ()
pizzeria = undefined

instance Functor Want where
  fmap f x = f <$> x

instance Applicative Want where
  pure = return

instance Monad Want where
  return x = undefined
  (Want f) >>= g = undefined

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

main = tengo hambre
