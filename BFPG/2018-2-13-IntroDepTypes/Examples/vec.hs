{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

infixr 5 :::

data Nat = Z | S Nat

data Vec :: Nat -> * -> * where
  Nil :: Vec Z a
  (:::) :: a -> Vec n a -> Vec (S n) a

x :: Vec (S (S (S Z))) Char
x = 'a' ::: 'b' ::: 'c' ::: Nil

--this will not typecheck
y :: Vec (S (S (S (S Z)))) Char
y = 'a' ::: 'b' ::: 'c' ::: Nil
