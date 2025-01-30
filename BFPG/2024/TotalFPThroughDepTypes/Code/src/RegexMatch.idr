module RegexMatch

data Token =
  TBigId
  | TSmallId
  | TArrow
  | TLambda
  | TNull

data ValidToken : Token -> Type where
  VTNull     : ValidToken TNull
  VTBigId    : ValidToken TBigId
  VTSmallId  : ValidToken TSmallId
  VTArrow    : ValidToken TArrow
  VTLambda   : ValidToken TLambda

data Term : Type where
  MkRef     : Term
  MkDataCon : Term
  MkApp     : Term -> Term -> Term
  MkLambda  : Term -> Term -> Term

-- null Token to satisfy totality checker.
total
match : String -> (a : Token ** ValidToken a)
match "foo" = (_ ** VTSmallId)
match "bar" = (_ ** VTSmallId)
match "baz" = (_ ** VTSmallId)
match "Foo" = (_ ** VTBigId)
match "=>"  = (_ ** VTArrow)
match "\\"  = (_ ** VTLambda)
match _     = (_ ** VTNull)

total
parseSmallId : ValidToken TSmallId -> Term
parseSmallId VTSmallId = MkRef

total 
example : Term
example = parseSmallId (DPair.snd (match "foo"))

∘ : {a : Type} 
 -> {b : a -> Type}
 -> {c : {x : a} -> b x -> Type}
 -> ({x : a} -> (y : b x) -> c y)
 -> (g : (x : a) -> b x)
 -> ((x : a) -> c (g x))
∘ f g = \x => f (g x)

example2 : (x : String) 
        -> ValidToken (DPair.fst (match x))
example2 = (DPair.snd {p = ValidToken} `∘` match)

