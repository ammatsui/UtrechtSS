{-# language FlexibleInstances, MultiParamTypeClasses, DeriveTraversable, FlexibleContexts #-}


-- A state monad is monad with additional monadic operations get and put:

class Monad m => MonadState m s | m -> s where
  get    ::             m s
  put    :: s        -> m ()
  modify :: (s -> s) -> m s

--Apart from the usual three monad laws, state monads should also satisfy:

put s1 >> put s2               == put s2
put s  >> get                  == put s >> return s
get    >>= put                 == return ()
get    >>= (\s -> get >>= k s) == get >>= (\s -> k s s)

-- Exercise 1: Give default implementations of get and put in terms of modify, 
-- and a default implementation of modify in terms of get and put.



import Control.Monad.State.Lazy
import Control.Monad

-- making new monad which counts

data Counts = Counts { binds   :: Int,
                       returns :: Int,
                       gets    :: Int,
                       puts    :: Int
                     } deriving (Eq, Show)

-- Exercise 2: As a convenience, give a Monoid instance for Count that sums the counts pairwise. Define constants
-- oneBind, oneReturn, oneGet, onePut :: Counts
-- that represent a count of one (>>=), return, get and put operation, respectively.

oneBind   = Counts 1 0 0 0
oneReturn = Counts 0 1 0 0
oneGet    = Counts 0 0 1 0
onePut    = Counts 0 0 0 1


instance Monoid Counts where
  mempty  = Counts 0 0 0 0
  mappend (Counts x1 x2 x3 x4) (Counts y1 y2 y3 y4) = Counts (x1+y1) (x2+y2) (x3+y3) (x4+y4)


-- <> is the same as mappend, different versions of Haskell => Monoid/Semigroup
instance Semigroup Counts where
  (Counts x1 x2 x3 x4) <> (Counts y1 y2 y3 y4) = Counts (x1+y1) (x2+y2) (x3+y3) (x4+y4)


newtype State' s a = State' { runState' :: (s, Counts) -> (a, s, Counts) }


-- Exercise 3: Give Monad and MonadState instances for State' that count the number of (>>=), return, get and put operations.

instance Monad (State' s) where
  return a   = State' $ \(s, count) -> (a, s, (count <> oneReturn)) 
  (>>=) st f = State' $ \(s, count) -> let (x, s', count') = runState' st (s, count) in runState' (f x) (s', (count' <> oneBind) ) 


instance MonadState s (State' s) where
  get    = State' $ \(s, count) -> (s, s, (count <> oneGet))
  put st = State' $ \(s, count) -> ((), st, (count <> onePut))


instance Applicative (State' s) where
  pure = return
  mf <*> mx = do f <- mf; x <- mx; return (f x)


instance Functor (State' s) where
  fmap = Control.Monad.liftM


-- Exercise 4: Write a function that labels a tree with integers increasingly, using a depth-first in-order traversal.

data Tree a = Branch (Tree a) a (Tree a) | Leaf deriving (Functor, Foldable, Traversable, Show)


-- gives the next value and updates the counter
next :: MonadState Int m => m Int
next = do x <- get 
          modify (+ 1) 
          return x


-- labels one element
labelElt :: MonadState Int m => a -> m (Int, a)
labelElt a = fmap (,) next <*> pure a


label :: MonadState Int m => Tree a -> m (Tree (Int, a))
label = traverse labelElt

-- Exercise 5: Write a function that runs a state monadic computation in the instrumented state monad, 
-- given some initial state of type s, and returns the computed value and the number of operations counted. 

run :: State' s a -> s -> (a, Counts)
run a st = case runState' a (st, mempty) of
   (x, st', count) -> (x, count)


