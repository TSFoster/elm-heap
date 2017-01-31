module Heap.Internal exposing (Heap, emptyWith, peek)


type alias Heap a =
    { structure : Node a
    , size : Int
    , compareFn : a -> a -> Order
    }


type Node a
    = Branch a
    | Leaf


emptyWith : (a -> a -> Order) -> Heap a
emptyWith fn =
    { structure = Leaf
    , size = 0
    , compareFn = fn
    }


peek : Heap a -> Maybe a
peek heap =
    case heap.structure of
        Leaf ->
            Nothing

        Branch x ->
            Just x
