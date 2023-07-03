%default total 

-- data Nat : Type where
--   Z : Nat
--   S : Nat -> Nat

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

lemmaAddZeroRightNeutral : (k : Nat) -> plus k 0 = k
lemmaAddZeroRightNeutral Z = Refl
lemmaAddZeroRightNeutral (S k) =
  let rec : (plus k 0) === k -- let expressions and = can be hard to parse.
      rec = lemmaAddZeroRightNeutral k
  in rewrite rec in Refl


-- What is going on here? (rewrite + with lemma)
(++) : {k : Nat} -> Vect k a -> Vect j a -> Vect (k + j) a
(++) [] ys = ys
(++) (x :: xs) [] = ?h1 -- (x :: xs)
(++) (x :: xs) ys = x :: xs ++ ys


