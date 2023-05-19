%default total 

-- data Nat : Type where
--   Z : Nat
--   S : Nat -> Nat

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

lemmaAddZeroRightNeutral : (k : Nat) -> plus k 0 = k
lemmaAddZeroRightNeutral = ?hole


-- What is going on here? (rewrite + with lemma)
(++) : {k : Nat} -> Vect k a -> Vect j a -> Vect (k + j) a
(++) [] ys = ?h1
(++) (x :: xs) [] = ?h2
(++) (x :: xs) ys = ?h3


