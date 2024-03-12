module VerifiedFunctor

%default total
%hide Prelude.(<$>)
%hide Prelude.Ops.infixr.(<$>)

infixr 8 <$>

public export
interface VerifiedFunctor (f : Type -> Type) where
  -- our map function.
  (<$>) : (a -> b) -> f a -> f b
  -- what about our laws?
  -- recall map id = id and map (f . g) = map f . map g
  mapIdLaw : (xs : f a) -> (<$>) {b = a} Prelude.id xs = Prelude.id xs
  mapComposeLaw : (xs : f a) -> (<$>) {a} (f . g) xs = ((<$>) f . (<$>) g) xs

public export
implementation VerifiedFunctor List where
  f <$> [] = []
  f <$> (x :: xs) = (f x) :: (f <$> xs)
  mapIdLaw [] = Refl
  mapIdLaw (x :: xs) = cong (\u => (x :: u)) (mapIdLaw xs)
  mapComposeLaw [] = Refl
  mapComposeLaw (x :: xs) = cong (\p => List (g x) :: p) (mapComposeLaw {g} xs)
