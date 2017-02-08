module Heap
    exposing
        ( Heap
        , SortOrder(..)
        , Compare(..)
        , Options
        , defaultOptions
        , empty
        , emptyWith
        , singleton
        , singletonWith
        , fromList
        , fromListWith
        , isEmpty
        , size
        , peek
        , push
        , pop
        , popBlind
        , mergeInto
        , toList
        , toListReverse
        , toListUnordered
        , compare
        )

{-| Data structure for heaps.

This package  exposes a data  structure to implement  heaps/priority queues/fast
in-place sorting.

The heap  is implemented as a  pairing heap, as it  is simple but fast,  and has
been shown to work well in real-world situations.


# Definition

@docs Heap, Options, SortOrder, Compare, defaultOptions


# Creating heaps

@docs empty, emptyWith, singleton, singletonWith


# Inserting/removing values

@docs push, pop, popBlind


# Inspecting heaps

@docs isEmpty, size, peek, compare


# Bulk operations

@docs mergeInto, fromList, fromListWith, toList, toListReverse, toListUnordered


# Running times

* peek: **Θ(1)**
* pop: **O(log n) (amortized)**
* push: **Θ(1)**
* size: **Θ(1)**
* merge: **Θ(1)**

-}


{-| Heaps are exposed as an opaque union type.

-}
type Heap a
    = Heap (Model a)


type alias Model a =
    { structure : Node a
    , size : Int
    , compare : SortOrder -> a -> a -> Order
    , order : SortOrder
    }


type Node a
    = Branch a (List (Node a))
    | Leaf


{-| Heaps keep  either the smallest value  at the top (MinFirst)  or the largest
(MaxFirst).

-}
type SortOrder
    = MinFirst
    | MaxFirst


{-| You must provide  either a function to compare two values,  or a function to
make a value `comparable`.

-}
type Compare a comparable
    = With (a -> a -> Order)
    | By (a -> comparable)


{-| A record defining the type of heap to create.

-}
type alias Options a comparable =
    { order : SortOrder
    , compare : Compare a comparable
    }


{-| By default,  heaps  are  min-heaps that  contain  comparable values  (ints,
floats, chars, strings, lists, or tuples).


-}
defaultOptions : Options comparable1 comparable2
defaultOptions =
    { order = MinFirst
    , compare = With Basics.compare
    }


{-| An empty heap for any  comparable type (ints, floats, chars, strings, lists,
or tuples).

Same as `Heap.emptyWith Heap.defaultOptions`.

-}
empty : Heap comparable
empty =
    emptyWith defaultOptions



    Heap.emptyWith { order = MinFirst, compare = (\a b -> Basics.compare (List.maximum a) (List.maximum b)) }
{-| An empty heap for any type, given an Options record.

-}
emptyWith : Options a comparable -> Heap a
emptyWith { compare, order } =
    Heap
        { structure = Leaf
        , size = 0
        , compare = makeCompare compare
        , order = order
        }


{-| A heap containing one comparable value (ints, floats, chars, strings, lists,
or tuples).

    Heap.singleton (3, 4)

Same as `Heap.singletonWith Heap.defaultOptions`.

-}
singleton : comparable -> Heap comparable
singleton =
    singletonWith defaultOptions


{-| A heap containing one value, given Heap.Options

    Heap.SingletonWith
        { order = MinFirst, compare = By .age }
        { name = "Cher", age = 12 }

-}
singletonWith : Options a comparable -> a -> Heap a
singletonWith { compare, order } value =
    Heap
        { structure = Branch value []
        , size = 1
        , compare = makeCompare compare
        , order = order
        }


{-| A heap containing all values in the list of comparable types.

    >>> Heap.fromList []
    ...    |> Heap.size
    0

    >>> Heap.fromList [ 8, 3, 8, 3, 6, 67, 23 ]
    ...    |> Heap.size
    7

Same as `Heap.fromListWith Heap.defaultOptions`.

-}
fromList : List comparable -> Heap comparable
fromList =
    fromListWith defaultOptions


{-| A heap containing all values in the list, given Heap.Options

    Heap.fromListWith { order = MaxFirst, compare = By List.minimum }
        [ [ 1, 999 ]
        , [ 6, 4, 3, 8, 9, 347, 34, 132, 546 ]
        ]

-}
fromListWith : Options a comparable -> List a -> Heap a
fromListWith options =
    List.foldl push (emptyWith options)


{-| `True` if the Heap is empty, otherwise `False`.

    >>> Heap.isEmpty Heap.empty
    True

    >>> Heap.isEmpty (Heap.singleton 3)
    False

-}
isEmpty : Heap a -> Bool
isEmpty (Heap { size }) =
    size == 0


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
peek (Heap { structure }) =
    case structure of
        Leaf ->
            Nothing

        Branch a _ ->
            Just a


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
push a (Heap heap) =
    mergeInto (Heap heap) (Heap { heap | structure = Branch a [], size = 1 })


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
pop (Heap heap) =
    case heap.structure of
        Leaf ->
            Nothing

        Branch a subheap ->
            Just ( a, Heap { heap | structure = mergePairs heap subheap, size = heap.size - 1 } )


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

    >>> Heap.isEmpty (Heap.mergeInto Heap.empty Heap.empty)
    True

    >>> Heap.mergeInto (Heap.fromList [ 2, 4, 6, 7 ]) (Heap.fromList [ 5, 7, 9, 3 ])
    ...     |> Heap.size
    8

-}
mergeInto : Heap a -> Heap a -> Heap a
mergeInto (Heap heap) (Heap toMerge) =
    Heap <|
        case heap.structure of
            Leaf ->
                { heap
                    | structure = toMerge.structure
                    , size = toMerge.size
                }

            Branch elem1 subheap1 ->
                case toMerge.structure of
                    Leaf ->
                        heap

                    Branch elem2 subheap2 ->
                        if heap.compare heap.order elem1 elem2 == LT then
                            { heap
                                | structure = Branch elem1 (toMerge.structure :: subheap1)
                                , size = heap.size + toMerge.size
                            }
                        else
                            { heap
                                | structure = Branch elem2 (heap.structure :: subheap2)
                                , size = heap.size + toMerge.size
                            }


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
toListReverse =
    let
        toListHelper popped heap =
            case pop heap of
                Nothing ->
                    popped

                Just ( el, subheap ) ->
                    toListHelper (el :: popped) subheap
    in
        toListHelper []


{-| Get all values out as fast as possible, regardless of order

-}
toListUnordered : Heap a -> List a
toListUnordered (Heap { structure }) =
    flattenStructure structure


flattenStructure : Node a -> List a
flattenStructure nodes =
    case nodes of
        Leaf ->
            []

        Branch a rest ->
            a :: List.concat (List.map flattenStructure rest)


{-| Compare two heaps (using compare method of first heap):

* An empty heap is less than a non-empty heap.
* If one heap's min-value is less than another, the heap is less than the other
* If two heaps share the same min-value, remove the min-values are compare the resulting heaps


    >>> Heap.compare Heap.empty Heap.empty
    EQ

    >>> Heap.compare Heap.empty (Heap.singleton 3)
    LT

    >>> Heap.compare (Heap.fromList [ 1, 2, 3, 4 ]) (Heap.fromList [ 1, 2, 3 ])
    GT

-}
compare : Heap a -> Heap a -> Order
compare ((Heap ha) as heapA) ((Heap hb) as heapB) =
    case ( peek heapA, peek heapB ) of
        ( Nothing, Nothing ) ->
            EQ

        ( _, Nothing ) ->
            GT

        ( Nothing, _ ) ->
            LT

        ( Just a, Just b ) ->
            let
                minOrder =
                    ha.compare ha.order a b
            in
                case minOrder of
                    EQ ->
                        Maybe.map2 compare (popBlind heapA) (popBlind heapB)
                            |> Maybe.withDefault EQ

                    _ ->
                        minOrder


makeCompare : Compare a comparable -> SortOrder -> a -> a -> Order
makeCompare compare order =
    let
        fn a b =
            case compare of
                With f ->
                    f a b

                By f ->
                    Basics.compare (f a) (f b)
    in
        case order of
            MinFirst ->
                fn

            MaxFirst ->
                reverseCompare fn


reverseCompare : (a -> a -> Order) -> (a -> a -> Order)
reverseCompare fn a b =
    case fn a b of
        GT ->
            LT

        LT ->
            GT

        EQ ->
            EQ


mergePairs : Model a -> List (Node a) -> Node a
mergePairs heap nodes =
    case List.filter ((/=) Leaf) nodes of
        [] ->
            Leaf

        node :: [] ->
            node

        node1 :: node2 :: rest ->
            let
                (Heap { structure }) =
                    mergeInto
                        (mergeInto (Heap { heap | structure = node1 }) (Heap { heap | structure = node2 }))
                        (Heap { heap | structure = mergePairs heap rest })
            in
                structure
