module TotalBadExample
contradiction1 : Bool -> Bool
contradiction1 True = False

contradiction2 : Bool -> Bool
contradiction2 x = not $ contradiction x
