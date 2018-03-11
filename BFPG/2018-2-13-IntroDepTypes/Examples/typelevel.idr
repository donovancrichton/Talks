
IntOrString : Bool -> Type
IntOrString True  = Int
IntOrString False = String

intOrString : (x: Bool) -> IntOrString x
intOrString True = 6
intorString False = "Six"


