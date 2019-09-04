{-# language FlexibleInstances, MultiParamTypeClasses, DeriveTraversable, FlexibleContexts #-}


import Control.Monad.State.Lazy
import Control.Monad


data Counts = Counts { binds   :: Int,
                       returns :: Int,
                       gets    :: Int,
                       puts    :: Int
                     } deriving (Eq, Show)


-- constants
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


data Tree a = Branch (Tree a) a (Tree a) | Leaf deriving (Functor, Foldable, Traversable, Show)


-- gives the next value and updates the counter
next :: MonadState Int m => m Int
next = do x <- get 
          modify (+ 1) 
          return x


-- labels one element
labelElt :: MonadState Int m => a -> m (Int, a)
labelElt a = (,) <$> next <*> pure a


label :: MonadState Int m => Tree a -> m (Tree (Int, a))
label = traverse labelElt


run :: State' s a -> s -> (a, Counts)
run action st = case runState' action (st, mempty) of
   (x, _st', count) -> (x, count)


