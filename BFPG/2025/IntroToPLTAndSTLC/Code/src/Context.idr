module Context

%hide Prelude.Basics.Bool
%hide Prelude.Basics.True
%hide Prelude.Basics.False
%hide Prelude.Basics.not
-- assuming ◇ empty context here.
-- assuming A → B : Type here.
data Bool = True | False

not : Bool -> Bool
not True  = False
not False = True
