infixr 5 ::: 

data Vec : Nat -> Type -> Type where
  VNil : Vec 0 a
  (:::) : (x : a) -> (xs : Vec n a) -> Vec (n + 1) a

x : Vec 3 Char
x = 'a' ::: 'b' ::: 'c' ::: VNil

-- this wont typecheck!
--y : Vec 4 Char
--y = 'a' ::: 'b' ::: 'c' ::: VNil





