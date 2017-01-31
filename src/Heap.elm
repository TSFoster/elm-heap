module Heap exposing (Heap, empty, peek)

import Heap.Internal as I


type Heap a
    = Heap (I.Heap a)


empty : Heap comparable
empty =
    Heap <| I.emptyWith compare


peek : Heap a -> Maybe a
peek (Heap h) =
    I.peek h
