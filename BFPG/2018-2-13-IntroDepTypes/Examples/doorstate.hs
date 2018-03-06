{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data DoorState = DoorOpen | DoorClosed

data DoorCmd :: DoorState -> DoorState -> * -> * where
  Open     :: DoorCmd DoorClosed DoorOpen   ()
  Close    :: DoorCmd DoorOpen   DoorClosed ()
  RingBell :: DoorCmd DoorClosed DoorClosed ()
  Pure     :: a -> DoorCmd state state a
  Bind     :: DoorCmd state1 state2 a -> (a -> DoorCmd state2 state3 b) ->
              DoorCmd state1 state3 b

doorProg :: DoorCmd DoorClosed DoorClosed ()
doorProg = RingBell `Bind` \x -> 
           Open `Bind` \y -> Close
