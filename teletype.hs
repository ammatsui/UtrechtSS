{-# language FlexibleInstances, MultiParamTypeClasses, DeriveTraversable, FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Trans.RWS 

data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)
 

-- Read c, if it's not a newline append it to str and recurse, 
-- reading the next char, and so on.
upDate :: String -> Teletype String
upDate str = Get (\c -> if c == '\n' then End str else upDate (str ++ [c]))


getline :: Teletype String
getline = upDate ""


instance Functor Teletype where
  fmap f (End x)   = End (f x)
  fmap f (Get g)   = Get (fmap f . g)
  fmap f (Put c x) = Put c (fmap f x)


instance Applicative Teletype where
  pure a            = End a
  (<*>) (End f)   x = fmap f x
  (<*>) (Get g)   x = Get (\c -> g c <*> x)
  (<*>) (Put c f) x = Put c (f <*> x)


instance Monad Teletype where
  return x          = End x
  (>>=) (End x)   f = f x
  (>>=) (Get g)   f = Get (\c -> g c >>= f)
  (>>=) (Put c x) f = Put c (x >>= f)


getchar :: Teletype Char 
getchar = Get (\c -> End c)


putchar :: Char -> Teletype ()
putchar c = Put c (End ())


instance MonadState Char Teletype where
  get      = getchar
  put      = putchar 
  state f  =  Get (\c -> let (a, c') = f c in Put c' (End a))

-- (1) "get" the Char
-- (2) apply the function to it
-- (3) "put" the resulting new Char
-- (4) return the "result" of the function

--  get = state $ \s -> (s, s)
--  put s = state $ \_ -> ((), s)
--  it is correct because  state :: (s -> (a, s)) -> m a, so it turns the first into a Teletype. Which is exactly what is needed here


runConsole :: Teletype a -> IO a
runConsole (End a)   = return a 
runConsole (Get g)   = do c <- getChar; runConsole (g c)
runConsole (Put c x) = do    putChar c; runConsole x

--type RWS r w s = RWST r w s Identity
--A monad containing an environment of type r, output of type w and an updatable state of type s.
--runRWS :: RWS r w s a -> r -> s -> (a, s, w)
--Unwrap an RWS computation as a function. (The inverse of rws.)

--betterFunction :: Int -> RWS ProgramConfig String ProgramState Int

--type TeletypeRW = RWS [Char] () [Char]
--runRWS :: Teletype a -> TeletypeRW a 
--runnRWS (End c) = RWS listget () [c]
--runnRWS (Get g) = RWS (fmap g listget) () liststate
--runnRWS (Put c x) = RWS 