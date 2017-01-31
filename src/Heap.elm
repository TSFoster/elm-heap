module Heap
    exposing
        ( Heap
        , empty
        , singleton
        , fromList
        , isEmpty
        , peek
        , push
        , pop
        , popBlind
        , toList
        , toListReverse
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


fromList : List comparable -> Heap comparable
fromList =
    Heap << List.foldl I.push I.empty


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


toList : Heap a -> List a
toList =
    List.reverse << toListReverse


toListReverse : Heap a -> List a
toListReverse (Heap h) =
    let
        toListHelper popped heap =
            case I.pop heap of
                Nothing ->
                    popped

                Just ( el, subheap ) ->
                    toListHelper (el :: popped) subheap
    in
        toListHelper [] h
