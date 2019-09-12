{-# language FlexibleInstances, MultiParamTypeClasses, DeriveTraversable, FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Trans.RWS 


-- A value of type Teletype can be used to describe programs that read and write characters and return a final result of type a. 
-- Such a program can end immediately (End). 
-- If it reads a character, the rest of the program is described as a function depending on this character (Get). 
-- If the program writes a character (Put), the value to show and the rest of the program are recorded.

--For example, the following expression describes a program that continuously echo characters:

--echo = Get (\c -> Put c echo)


data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)
 

-- Exercise 1. Write a Teletype-program getLine which reads characters until it finds a newline character, and returns the complete string.

-- Read c, if it's not a newline append it to str and recurse, 
-- reading the next char, and so on and so forth.
upDate :: String -> Teletype String
upDate str = Get (\c -> if c == '\n' then End str else upDate (str ++ [c]))


getline :: Teletype String
getline = upDate ""


instance Functor Teletype where
  fmap f (End x)   = End (f x)
  fmap f (Get g)   = Get (fmap f . g)
  fmap f (Put c x) = Put c (fmap f x)


-- Exercise 2. Define sensible Applicative and Monad instances for Teletype.
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



-- Exercise 3. Define those functions getChar :: Teletype Char and putChar :: Char -> Teletype ().
-- for the do- notation
getchar :: Teletype Char 
getchar = Get (\c -> End c)


putchar :: Char -> Teletype ()
putchar c = Put c (End ())


-- Exercise 4. Define a MonadState instance for Teletype. 
-- How is the behavior of this instance different from the usual State type? 
instance MonadState Char Teletype where
  get      = getchar
  put      = putchar 
  state f  =  Get (\c -> let (a, c') = f c in Put c' (End a))

-- 1 - "get" the Char
-- 2 - apply the function to it
-- 3 - "put" the resulting new Char
-- 4 - return the "result" of the function

--  get = state $ \s -> (s, s)
--  put s = state $ \_ -> ((), s)
--  it is correct because  state :: (s -> (a, s)) -> m a, so it turns the first into a Teletype. Which is exactly what is needed here



-- Exercise 5. A Teletype-program can be thought as a description of an interaction with the console. 
-- Write a function runConsole :: Teletype a -> IO a which runs a Teletype-program in the IO monad. 
-- A Get should read a character from the console and Put should write a character to the console.

runConsole :: Teletype a -> IO a
runConsole (End a)   = return a 
runConsole (Get g)   = do c <- getChar; runConsole (g c)
runConsole (Put c x) = do    putChar c; runConsole x



-- Exercise 6. Write an interpretation of a Teletype-program into the monad RWS [Char] () [Char] (documentation). 
-- In other words, write a function, runRWS :: Teletype a -> TeletypeRW a
-- Using it, write a function mockConsole :: Teletype a -> [Char] -> (a, [Char]).


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
