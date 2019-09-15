-- http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=7C29C1EF88F4C46600FC2DCFFA8A8C0C?doi=10.1.1.456.357&rep=rep1&type=pdf


{-# language RankNTypes, TypeSynonymInstances, FlexibleInstances #-}


type Square      = Square' Nil 
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)
data Nil       a = Nil
data Cons    t a = Cons a (t a)

-- Exercise 1. Give Haskell code that represents the following two square matrices as elements of the Square data type

let row1    = Cons 1 (Cons 0 Nil) 
let row2    = Cons 0 (Cons 1 Nil) 
let matrix1 = Succ (Succ (Zero (Cons row1 (Cons row2 Nil))))

let row1'   = Cons 1 (Cons 2 (Cons 3 Nil))
let row2'   = Cons 4 (Cons 5 (Cons 6 Nil))
let row3'   = Cons 7 (Cons 8 (Cons 9 Nil))
let matrix2 = Succ (Succ (Succ (Zero (Cons row1' (Cons row2' (Cons row3' Nil))))))



eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil eqA Nil Nil = True


eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
       -> (a -> a -> Bool)
       -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys



eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
          -> (a -> a -> Bool)
          -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' eqT eqA _         _         = False


eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil


instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)


--Exercise 4. Systematically follow the scheme just presented in order to define a Functor instance for square matrices. 
-- I.e., derive a function mapSquare such that you can define fmap = mapSquare


mapNil :: ((a -> b) -> a -> b) -> ((a -> b) -> Nil a -> Nil b )
mapNil mapA f Nil = Nil


mapCons :: (forall b . ((a -> b) -> a -> b) -> ((a -> b) -> t a -> t b))
        -> ((a -> b) -> a -> b)
        -> ((a -> b) -> Cons t a -> Cons t b)
mapCons mapT mapA f (Cons x consx) =  Cons (f x) (mapT mapA f consx)


mapSquare' :: (forall b . ((a -> b) -> a -> b) -> ((a -> b) -> t a -> t b))
           -> ((a -> b) -> a -> b)
           -> ((a -> b) -> Square' t a -> Square' t b)
mapSquare' mapT mapA f (Zero xs) = Zero (mapT (mapT mapA) f xs) -- it is wrong
mapSquare' mapT mapA f (Succ xs) = Succ (mapSquare' (mapCons mapT) mapA f xs)


mapSquare :: (a - > b) -> Square a -> Square b
mapSquare = mapSquare' mapNil


instance Functor Square where
  fmap = mapSquare
