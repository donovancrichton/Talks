{-# LANGUAGE GADTs #-}
data Expr a where
  Lift :: (Show a) => a -> Expr a
  Add  :: (Num a) => Expr a -> Expr a -> Expr a
  And  :: Expr Bool -> Expr Bool -> Expr Bool

alwaysAdd :: Num a => Expr a -> Expr a
alwaysAdd (Add x y) = Add x y
alwaysAdd (And x y) = Add x y

