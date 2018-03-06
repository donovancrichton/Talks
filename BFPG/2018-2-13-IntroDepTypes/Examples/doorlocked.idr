
data DoorState = DoorOpen | DoorClosed | DoorLocked

data DoorCmd : Type -> DoorState -> DoorState -> Type where
  Unlock   : DoorCmd () DoorLocked DoorClosed
  Lock     : DoorCmd () DoorClosed DoorLocked
  Open     : DoorCmd () DoorClosed DoorOpen
  Close    : DoorCmd () DoorOpen   DoorClosed
  RingBell : DoorCmd () DoorClosed DoorClosed
  Pure     : a -> DoorCmd a state state
  (>>=)    : DoorCmd a state1 state2 -> (a -> DoorCmd b state2 state3) 
               -> DoorCmd b state1 state3

doorProg : DoorCmd () DoorLocked DoorLocked

