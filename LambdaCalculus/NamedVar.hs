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
-- Identity = Lam "x" (Var "x")
-- * (\x -> x) 3
-- Example = App Identity (Num 3)


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

subst :: (Var, Term) -> Term -> Term
subst _         (Num b)               = Num b
subst (Var x, term) (Var v)           = if x == v then term else (Var v)
subst (Var x, term) (App lterm rterm) = (App lterm' rterm')
                                      where lterm' = subst (var, term) lterm
                                            rterm' = subst (var, term) rterm
subst (Var x, term) (Lam v term1)     = if x == v then (Lam v term1) else (Lam v term1') 
                                                                       where term1' = subst (var, term) term1


-- note: to ensure that variables are not accidentally captured
-- you have to implement a function to "freshen" all the
-- variables from lambdas
-- ?
-- freshen :: Term -> Term

-- * write an evaluator which implements the beta-reduction rule
--      (\x. e) v --> subst x by v in e
  
eval :: Term -> Term
eval Num b             = Num b
eval Var v             = Var v
eval (App lterm rterm) = App (eval lterm) (eval rterm)
eval (Lam v term)      = subst (v, (eval term)) term