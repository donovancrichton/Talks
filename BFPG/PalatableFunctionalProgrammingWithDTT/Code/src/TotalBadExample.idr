module TotalBadExample
contradiction : Bool -> Bool
contradiction True = False

contradiction : Bool -> Bool
contradiction x = not $ contradiction x
