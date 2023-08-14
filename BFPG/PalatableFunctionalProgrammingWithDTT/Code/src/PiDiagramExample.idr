module PiDiagramExample
CharOrNat : Bool -> Type
CharOrNat True = Char
CharOrNat False = Nat

bOrOne : (b : Bool) -> CharOrNat b
bOrOne True = 'b'
bOrOne False = 1
