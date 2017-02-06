module Heap.Internal
    exposing
        ( Heap
        , emptySortedWith
        , singletonSortedWith
        , isEmpty
        , peek
        , push
        , pop
        , mergeInto
        )


type alias Heap a =
    { structure : Node a
    , size : Int
    , compareFn : a -> a -> Order
    }


type Node a
    = Branch a (List (Node a))
    | Leaf


emptySortedWith : (a -> a -> Order) -> Heap a
emptySortedWith fn =
    { structure = Leaf
    , size = 0
    , compareFn = fn
    }


isEmpty : Heap a -> Bool
isEmpty =
    .structure >> (==) Leaf


peek : Heap a -> Maybe a
peek heap =
    case heap.structure of
        Leaf ->
            Nothing

        Branch x _ ->
            Just x


pop : Heap a -> Maybe ( a, Heap a )
pop heap =
    case heap.structure of
        Leaf ->
            Nothing

        Branch a subheap ->
            Just ( a, { heap | structure = mergePairs heap subheap, size = heap.size - 1 } )


mergePairs : Heap a -> List (Node a) -> Node a
mergePairs heap nodes =
    case List.filter ((/=) Leaf) nodes of
        [] ->
            Leaf

        node :: [] ->
            node

        node1 :: node2 :: rest ->
            .structure <|
                mergeInto
                    (mergeInto { heap | structure = node1 } { heap | structure = node2 })
                    { heap | structure = mergePairs heap rest }


singletonSortedWith : (a -> a -> Order) -> a -> Heap a
singletonSortedWith fn a =
    let
        heap =
            emptySortedWith fn
    in
        { heap
            | structure = Branch a []
            , size = 1
        }


dummySingleton : a -> Heap a
dummySingleton =
    singletonSortedWith (\_ _ -> EQ)


push : a -> Heap a -> Heap a
push a heap =
    mergeInto heap (dummySingleton a)


mergeInto : Heap a -> Heap a -> Heap a
mergeInto heap toMerge =
    case heap.structure of
        Leaf ->
            { toMerge | compareFn = heap.compareFn }

        Branch elem1 subheap1 ->
            case toMerge.structure of
                Leaf ->
                    heap

                Branch elem2 subheap2 ->
                    if heap.compareFn elem1 elem2 == LT then
                        { heap
                            | structure = Branch elem1 (toMerge.structure :: subheap1)
                            , size = heap.size + toMerge.size
                        }
                    else
                        { heap
                            | structure = Branch elem2 (heap.structure :: subheap2)
                            , size = heap.size + toMerge.size
                        }
