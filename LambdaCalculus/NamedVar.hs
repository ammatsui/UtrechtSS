{-# language DataKinds #-}


module LambdaCalculus where

-- Lambda calculus terms with explicit names
type TermVariable = String
data Term = Var TermVariable 
          | App Term Term 
          | Lam TermVariable Term 
          | Num Integer  deriving (Show, Eq)




-- Examples
-- * \x -> x
identity = Lam "x" (Var "x")
-- * (\x -> x) 3
example = App identity (Num 3)


-- For each representation of lambda calculus,
-- where "Var" and "Term" refer to the concrete types:

-- * write an alpha-equivalence function
--      (\x. x) alpha-equiv to (\y. y) and so on
--      (\x y. x) is not alpha-equiv to (\x y. y)
--
alphaEquiv :: Term -> Term -> Bool
alphaEquiv (Num a)           (Num b)           = a == b
alphaEquiv (Var v)           (Var w)           = True                             -- this is just a name
alphaEquiv (App lterm rterm) (App terml termr) = (alphaEquiv lterm terml) && (alphaEquiv rterm termr)
alphaEquiv (Lam v term1)     (Lam w term2)     = True                     && (alphaEquiv term1 term2)
alphaEquiv _                  _                = False


-- * write a substitution function
-- Evaluation of a lambda term ((λx.e)a) proceeds by substitution of all occurrences of the variable x with the argument a in e.
-- replace Var with Term1 in Term2

subst :: (TermVariable, Term) -> Term -> Term
subst (x, (Num b))       term = Num b
subst (x, (Var v))       term = if x == v then (Var v) else term
subst (x, (App fun arg)) term = (App (subst (x, fun) arg) term)
subst (x, (Lam v term1)) term = if x == v then (Lam v term1) else (Lam v (subst (x, term) term1)) 
                                                       


-- note: to ensure that variables are not accidentally captured
-- you have to implement a function to "freshen" all the
-- variables from lambdas
-- ?
-- freshen :: Term -> Term

-- * write an evaluator which implements the beta-reduction rule
--      (\x. e) v --> subst x by v in e
  
eval :: Term -> Term
eval (Num x)                            = Num x
eval (Var x)                            = Var x
eval (App (Lam x term) t@(Lam y term')) = eval (subst (x, t) term)
eval (App (Lam x term) term')           = eval (subst (x, term') term)
eval (App terml termr)                  = eval (App (eval terml) termr)
eval (Lam x term)                       = (Lam x (eval term))
