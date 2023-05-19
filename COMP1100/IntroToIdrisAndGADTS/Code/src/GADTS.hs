{-# LANGUAGE GADTs #-}
module GADTS where
-- gives us GADT syntax


-- recall * is a 'kind' or 'the type of types' in Haskell.
data List :: * -> * where
  Nil :: List a
  Cons :: a -> List a -> List a

data Expr :: * -> * where
  EInt :: Int -> Expr Int
  EBool :: Bool -> Expr Bool
  EAdd :: Expr Int -> Expr Int -> Expr Int
  EAnd :: Expr Bool -> Expr Bool -> Expr Bool
  EIsZero :: Expr Int -> Expr Bool

-- A polymorphic evaluator
eval :: (Expr a) -> a
eval (EInt k) = k
eval (EBool b) = b
eval (EAdd x y) = eval x + eval y
eval (EAnd b1 b2) = eval b1 && eval b2
eval (EIsZero k) = eval k == 0

ex1 :: Bool
ex1 = eval $ 
  EIsZero 
    (EAdd
      (EAdd (EInt (-3)) (EInt 3)) 
      (EAdd (EInt 2) (EInt (-2)))
    )  

-- this will not type check.
ex2 :: Bool
ex2 = undefined -- eval $ EAdd (EInt 2) (EInt 3)
