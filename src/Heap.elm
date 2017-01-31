module Heap
    exposing
        ( Heap
        , empty
        , singleton
        , isEmpty
        , peek
        , push
        , pop
        , popBlind
        )

import Heap.Internal as I


type Heap a
    = Heap (I.Heap a)


empty : Heap comparable
empty =
    Heap I.empty


singleton : comparable -> Heap comparable
singleton a =
    Heap <| I.singleton a


isEmpty : Heap a -> Bool
isEmpty (Heap h) =
    I.isEmpty h


peek : Heap a -> Maybe a
peek (Heap h) =
    I.peek h


push : a -> Heap a -> Heap a
push a (Heap h) =
    Heap <| I.push a h


pop : Heap a -> Maybe ( a, Heap a )
pop (Heap h) =
    Maybe.map (Tuple.mapSecond Heap) <| I.pop h


popBlind : Heap a -> Maybe (Heap a)
popBlind =
    Maybe.map Tuple.second << pop
