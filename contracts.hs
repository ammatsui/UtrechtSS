{-# language KindSignatures, GADTs #-}

data Contract' :: * -> * where
  Pred' :: (a -> Bool) -> Contract' a
  Fun   :: Contract' a -> Contract' b -> Contract' (a -> b)

-- A contract can be a predicate for a value of arbitrary type. 
-- For functions, we offer contracts that contain a precondition on the arguments, and a postcondition on the results.

assert' :: Contract' a -> a -> a
assert' (Pred' p)      x = if p x then x else error "contract' violation"
assert' (Fun pre post) f = assert' post . f . assert' pre

-- assert will cause run-time failure if a contract is violated, and otherwise return the original result
-- for function contracts, first check the precondition on the value, then apply the original function, and finally check the postcondition on the result.




-- states that a number is positive
pos :: (Num a, Ord a) => Contract a
pos = Pred (> 0)




-- Exercise 1. Define a contract such that for all values x the equation
-- assert true x == x
-- holds. Prove this equation using equational reasoning.

true :: Contract Bool
true = Pred id




-- Want the postcondition of a function to be able to refer to the actual argument that has been passed to the function
-- The postcondition now depends on the function argument.
-- Exercise 2: Adapt the function assert to the new type of DFun.

data Contract :: * -> * where
  Pred :: (a -> Bool) -> Contract a
  DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b)

assert :: Contract a -> a -> a
assert (Pred p)         x = if p x then x else error "contract violation"
assert (DFun pre dpost) f = (\a -> (assert (dpost a) . f) a) . assert pre
-- again, first check precondition, apply, check postcondition depending on the result




-- Exercise 3: Define a combinator that reexpresses the behaviour of the old Fun constructor in terms of the new and more general one

(==>) :: Contract a -> Contract b -> Contract (a -> b)
(==>) pre dpost = DFun pre (\a -> dpost)



-- Exercise 4: Define a contract suitable for the list index function (!!)
-- i.e., a contract of type Contract ([a] -> Int -> a) that checks if the integer is a valid index for the given list.

index :: Contract ([a] -> Int -> a)
index = undefined


-- Exercise 5: Define a contract where assert (preserves p) f x fails if and only if the value of p x is different from the value of p (f x). 
-- Examples:
-- assert (preserves length) reverse  "Hello"       --> "olleH"
-- assert (preserves length) (take 5) "Hello"       --> "Hello"
-- assert (preserves length) (take 5) "Hello world" --> ⊥



preserves :: Eq b => (a -> b) -> Contract (a -> a)
preserves p = DFun (Pred (\x -> True)) (\x -> (Pred (\y -> ((p y) == (p x)) ) ) ) 



-- Exercise 6: Is there a difference between (assert preservesPos) and (assert preservesPos')? 

preservesPos = preserves (>0)
preservesPos' = pos ==> pos


-- We can add another contract constructor:

List :: Contract a -> Contract [a]

-- The corresponding case of assert is as follows:

assert (List c) xs = map (assert c) xs

-- Exercise 7:

allPos  = List pos
allPos' = Pred (all (> 0))


-- Describe the differences between assert allPos and assert allPos', and more generally between using List versus using Pred to describe a predicate on lists. 
-- Hint: Think carefully and consider different situations before giving your answer. What about using the allPos and allPos' contracts as parts of other contracts? 
-- What about lists of functions? 
-- What about infinite lists? What about strict and non-strict functions working on lists?)