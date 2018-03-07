{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data DoorState = DoorOpen | DoorClosed | DoorLocked

data IsClosed :: DoorState -> * where
  ClosedState :: IsClosed DoorClosed
  LockedState :: IsClosed DoorLocked

data DoorCmd :: DoorState -> DoorState -> * -> * where
  Lock     :: DoorCmd DoorClosed DoorLocked ()
  Unlock   :: DoorCmd DoorLocked DoorClosed ()
  Open     :: DoorCmd DoorClosed DoorOpen   ()
  Close    :: DoorCmd DoorOpen   DoorClosed ()
  RingBell :: IsClosed state -> DoorCmd state state ()
  -- Convenience Constructors
  Pure     :: a -> DoorCmd state state a
  Bind     :: DoorCmd state1 state2 a -> (a -> DoorCmd state2 state3 b) ->
              DoorCmd state1 state3 b

doorProg :: DoorCmd DoorLocked DoorLocked ()
doorProg = RingBell LockedState `Bind` \x ->
           Unlock `Bind` \x ->
           RingBell ClosedState `Bind` \x -> 
           Open `Bind` \x -> 
           Close `Bind` \x -> 
           Lock
