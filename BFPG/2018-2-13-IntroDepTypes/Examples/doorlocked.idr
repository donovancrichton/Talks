
data DoorState = DoorOpen | DoorClosed | DoorLocked

data IsClosed : DoorState -> Type where
  ClosedState : IsClosed DoorClosed
  LockedState : IsClosed DoorLocked

data DoorCmd : Type -> DoorState -> DoorState -> Type where
  Unlock   : DoorCmd () DoorLocked DoorClosed
  Lock     : DoorCmd () DoorClosed DoorLocked
  Open     : DoorCmd () DoorClosed DoorOpen
  Close    : DoorCmd () DoorOpen   DoorClosed
  RingBell : {auto prf: IsClosed state} -> DoorCmd () state state

  Pure     : a -> DoorCmd a state state
  (>>=)    : DoorCmd a state1 state2 -> (a -> DoorCmd b state2 state3) 
               -> DoorCmd b state1 state3

doorProg : DoorCmd () DoorLocked DoorLocked
doorProg = do RingBell
              Unlock
              Open
              Close
              Lock

