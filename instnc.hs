import Control.Applicative
import Data.Functor
        

data Tree a = Leaf a | Node (Tree a) (Tree a)


data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf a


--class Functor f where
--  fmap :: (a -> b) -> f a -> f b 


instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)


instance Functor RoseTree where
  fmap f (RoseLeaf a)              = RoseLeaf (f a)
  fmap f (RoseNode a roselist) = RoseNode (f a) (map (fmap f) roselist)

 
--class Functor f => Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b


instance Applicative Tree where
  pure a              = Leaf a  
  Leaf f <*> Leaf a   = Leaf (f a)
  Leaf f <*> Node l r = Node (fmap f l) (fmap f r)


instance Applicative RoseTree where
  pure                                             = RoseLeaf
  (RoseNode f froselist) <*> (RoseNode a roselist) = RoseNode (f a) ( (map (fmap f) roselist) ++ (map (<*> (RoseNode a roselist) ) froselist ) ) 

 
--class Applicative f => Monad f where
--  return :: a -> f a
--  (>>=) :: f a -> (a -> f b) -> f b


instance Monad Tree where
  return a       = Leaf a
  Leaf a   >>= f = f a
  Node l r >>= f = Node (l >>= f) (r >>= f) 


instance Monad RoseTree where
  return                      = RoseLeaf
  (RoseNode a roselist) >>= f = RoseNode (f a) ((map (>>= f) roselist) )   


