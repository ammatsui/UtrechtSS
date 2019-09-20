{-# language DataKinds #-}


module LambdaCalculus where


-- Lambda calculus using de Bruijn indices
type DeBruijnIndex = Integer
data Term
  = Var DeBruijnIndex
  | App Term Term
  | Lam Term
  | Num Integer
  deriving (Show, Eq)

-- Examples
-- * \x -> x
identity = Lam (Var 0)
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
alphaEquiv (Var v)           (Var w)           = v == w              
alphaEquiv (App lterm rterm) (App terml termr) = (alphaEquiv lterm terml) && (alphaEquiv rterm termr)
alphaEquiv (Lam term1)     (Lam term2)         = (alphaEquiv term1 term2)
alphaEquiv _                  _                = False


-- * write a substitution function
--
subst :: (DeBruijnIndex, Term) -> Term -> Term
subst _             (Num b)               = Num b
subst (x, term) (Var v)           = if x == v then term else (Var v)
subst (x, term) (App lterm rterm) = (App lterm' rterm')
                                      where lterm' = subst (x, term) lterm
                                            rterm' = subst (x, term) rterm
subst (x, term) (Lam term1)     = (Lam term1') where term1' = subst (x+1, term) term1



-- * write an evaluator which implements the beta-reduction rule
--      (\x. e) v --> subst x by v in e
-- 
eval :: Term -> Term
eval (Num x)       = Num x
eval (App fun arg) = subst (0, arg) fun
eval (Lam term)    = eval term