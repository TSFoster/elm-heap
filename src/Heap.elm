module Heap
    exposing
        ( Heap
        , empty
        , emptySortedWith
        , emptySortedBy
        , singleton
        , singletonSortedWith
        , singletonSortedBy
        , fromList
        , fromListSortedBy
        , fromListSortedWith
        , isEmpty
        , size
        , peek
        , push
        , pop
        , popBlind
        , merge
        , toList
        , toListReverse
        )

{-| Data structure for heaps.

This package  exposes a data  structure to implement  heaps/priority queues/fast
in-place sorting.

The heap  is implemented as a  pairing heap, as it  is simple but fast,  and has
been shown to work well in real-world situations.


# Definition

@docs Heap


# Creating heaps

@docs empty, singleton, isEmpty


# Inserting values

@docs push, merge


# Inspecting heaps

@docs isEmpty, size, peek, pop, popBlind


# Bulk operations

@docs fromList, toList, toListReverse


# Non-comparable elements

@docs emptySortedBy, singletonSortedBy, emptySortedWith
@docs singletonSortedWith, fromListSortedBy, fromListSortedWith


# Running times

* peek: **Θ(1)**
* pop: **O(log n) (amortized)**
* push: **Θ(1)**
* size: **Θ(1)**
* merge: **Θ(1)**

-}

import Heap.Internal as I


{-| Heaps are exposed as an opaque union type.
-}
type Heap a
    = Heap (I.Heap a)


{-| An empty heap for any  comparable type (ints, floats, chars, strings, lists,
or tuples).
-}
empty : Heap comparable
empty =
    Heap <| I.emptySortedWith compare


{-| An empty heap for any type, given a function to compare values.

    Heap.emptySortedWith (\a b -> compare (List.maximum a) (List.maximum b))
-}
emptySortedWith : (a -> a -> Order) -> Heap a
emptySortedWith =
    Heap << I.emptySortedWith


{-| An empty heap for any type, given a function to make a comprable value.

    Heap.emptySortedBy .surname
-}
emptySortedBy : (a -> comparable) -> Heap a
emptySortedBy =
    Heap << I.emptySortedWith << compFn


{-| A heap containing one comparable value (ints, floats, chars, strings, lists,
or tuples).

    Heap.singleton (3, 4)
-}
singleton : comparable -> Heap comparable
singleton a =
    Heap <| I.singletonSortedWith compare a


{-| A heap containing one value, given a function to compare values.

    Heap.SingletonSortedWith [ 1, 2, 3, 56 ]
-}
singletonSortedWith : (a -> a -> Order) -> a -> Heap a
singletonSortedWith fn =
    Heap << I.singletonSortedWith fn


{-| A heap containing one value, given a function to make a comparable value.

    Heap.singletonSortedBy .surname { firstname = "Buzz", surname = "Aldrin" }
-}
singletonSortedBy : (a -> comparable) -> a -> Heap a
singletonSortedBy fn =
    Heap << I.singletonSortedWith (compFn fn)


compFn : (a -> comparable) -> a -> a -> Order
compFn fn a b =
    compare (fn a) (fn b)


{-| A heap containing all values in the list of comparable types.

    Heap.fromList []

    Heap.fromList [ 8, 3, 8, 3, 6, 67, 23 ]
-}
fromList : List comparable -> Heap comparable
fromList =
    Heap << List.foldl I.push (I.emptySortedWith compare)


{-| A heap  containing all  values in  the list,  given a  function to  compare
values.

    Heap.fromListSortedWith (\a b -> compare (List.maximum a) (List.maximum b))
        [ [ 1, 999 ]
        , [ 6, 4, 3, 8, 9, 347, 34, 132, 546 ]
        ]
-}
fromListSortedWith : (a -> a -> Order) -> List a -> Heap a
fromListSortedWith fn =
    Heap << List.foldl I.push (I.emptySortedWith fn)


{-| A heap  containing all  values in  the  list, given  a function  to make  a
comparable value.

    Heap.fromListSortedBy .surname
        [ { firstname = "Buzz", surname = "Aldrin" }
        , { firstname = "Norman", surname = "Bates" }
        , { firstname = "Bruce", surname = "Campbell" }
        ]
-}
fromListSortedBy : (a -> comparable) -> List a -> Heap a
fromListSortedBy fn =
    Heap << List.foldl I.push (I.emptySortedWith <| compFn fn)


{-| `True` if the Heap is empty, otherwise `False`.

    >>> Heap.isEmpty Heap.empty
    True

    >>> Heap.isEmpty (Heap.singleton 3)
    False
-}
isEmpty : Heap a -> Bool
isEmpty (Heap h) =
    I.isEmpty h


{-| Number of elements in heap.

    >>> Heap.size Heap.empty
    0

    >>> Heap.size (Heap.fromList [ 1, 2, 3, 4, 5, 6, 7, 8 ])
    8
-}
size : Heap a -> Int
size (Heap h) =
    h.size


{-| Look at smallest value in heap without applying any transformations.

    >>> Heap.peek Heap.empty
    Nothing

    >>> Heap.peek (Heap.fromList [ 3, 56, 8, 367, 0, 4 ])
    Just 0
-}
peek : Heap a -> Maybe a
peek (Heap h) =
    I.peek h


{-| Add a value to a heap.

    >>> Heap.fromList [ 1, 6, 7 ]
    ...     |> Heap.push 4
    ...     |> Heap.peek
    Just 1

    >>> Heap.fromList [ 5, 6, 7 ]
    ...     |> Heap.push 4
    ...     |> Heap.peek
    Just 4
-}
push : a -> Heap a -> Heap a
push a (Heap h) =
    Heap <| I.push a h


{-| Try to remove the smallest value  from the heap, returning the value and the
new heap. If the heap is empty, return Nothing.

    >>> Heap.pop Heap.empty
    Nothing

    >>> Heap.fromList [ 3, 5, 7, 7, 2, 9 ]
    ...     |> Heap.pop
    ...     |> Maybe.map (Tuple.mapSecond Heap.size)
    Just (2, 5)
-}
pop : Heap a -> Maybe ( a, Heap a )
pop (Heap h) =
    Maybe.map (Tuple.mapSecond Heap) <| I.pop h


{-| Try to remove the smallest value from the heap, returning just the new heap.
If the heap is empty, return Nothing.

    >>> Heap.popBlind Heap.empty
    Nothing

    >>> Heap.singleton 3
    ...     |> Heap.popBlind
    ...     |> Maybe.map Heap.size
    Just 0
-}
popBlind : Heap a -> Maybe (Heap a)
popBlind =
    Maybe.map Tuple.second << pop


{-| Merge the second heap into the first heap.

**Note** This function assumes that both heaps are sorted using the same method.
Strictly speaking,  the merged  heap has  the same sorting  method as  the first
argument.

    >>> Heap.isEmpty (Heap.merge Heap.empty Heap.empty)
    True

    >>> Heap.merge (Heap.fromList [ 2, 4, 6, 7 ]) (Heap.fromList [ 5, 7, 9, 3 ])
    ...     |> Heap.size
    8
-}
merge : Heap a -> Heap a -> Heap a
merge (Heap h1) (Heap h2) =
    Heap <| I.mergeInto h1 h2


{-| Get all values from the heap, in order.

    >>> Heap.toList (Heap.fromList [ 9, 3, 6, 4, 1, 2, 8, 5, 7 ])
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
-}
toList : Heap a -> List a
toList =
    List.reverse << toListReverse


{-| Get all values from the heap, in reverse order.

    >>> Heap.toListReverse (Heap.fromList [ 9, 3, 6, 4, 1, 2, 8, 5, 7 ])
    [ 9, 8, 7, 6, 5, 4, 3, 2, 1 ]
-}
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
