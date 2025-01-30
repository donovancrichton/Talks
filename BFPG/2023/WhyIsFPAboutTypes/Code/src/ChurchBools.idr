module ChurchBools
data Boolean : Type where
  True : Boolean
  False : Boolean

true : a -> a -> a
true = \t => \f => t

false : a -> a -> a
false = \t => \f => f
