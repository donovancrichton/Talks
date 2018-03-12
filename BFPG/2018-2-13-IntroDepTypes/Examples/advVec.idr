infixr 5 ::: 

interface NatIntegral nat where
  zero : nat
  succ : nat -> nat

NatIntegral Nat where
  zero = Z
  succ x = S x

NatIntegral Int where
  zero = 0
  succ x = x + 1

NatIntegral Integer where
  zero = 0
  succ x = x + 1

NatIntegral Bits8 where
  zero = 0
  succ x = x + 1

NatIntegral Bits16 where
  zero = 0
  succ x = x + 1

NatIntegral Bits32 where
  zero = 0
  succ x = x + 1

NatIntegral Bits64 where
  zero = 0
  succ x = x + 1

One : Bits8
One = 1

data Vec : NatIntegral nat => nat -> Type -> Type where
  Nil : NatIntegral nat => Vec (zero {nat}) a
  (::) : NatIntegral nat => {n : nat} -> a -> Vec n a -> Vec (succ n) a

test : NatIntegral nat => Vec (succ (zero {nat})) Char
test = 'a' :: Nil

test2 : Vec One Char
test2 = test

