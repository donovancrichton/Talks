module IdrisRoseBad
%default total -- WHY!??

data Rose : Type -> Type 
  where
  MkRose : a -> List (Rose a)
    -> Rose a

implementation Functor Rose 
  where
  map f 
   (MkRose node children) 
      = MkRose (f node) 
        (map (map f) 
          children)
