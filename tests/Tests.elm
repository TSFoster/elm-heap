module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple)
import Heap exposing (Heap)


type BasicUnion
    = AnInt Int


union : Fuzzer BasicUnion
union =
    Fuzz.map AnInt int


intFromUnion : BasicUnion -> Int
intFromUnion (AnInt i) =
    i


all : Test
all =
    describe "Heap"
        [ describe "Basics"
            [ test "Can create an empty heap with no items" <|
                \() ->
                    Expect.equal Nothing <| Heap.peek Heap.empty
            , fuzz int "Can insert an element and pop it" <|
                \i ->
                    Heap.singleton i
                        |> Heap.pop
                        |> Maybe.map (Tuple.mapSecond Heap.isEmpty)
                        |> Expect.equal (Just ( i, True ))
            , fuzz (tuple ( int, int )) "Can insert two elements and pop lower one" <|
                \( i, j ) ->
                    Heap.singleton i
                        |> Heap.push j
                        |> Heap.pop
                        |> Maybe.map (Tuple.mapSecond Heap.isEmpty)
                        |> Expect.equal (Just ( min i j, False ))
            , fuzz (list int) "Can insert elements and peek lowest one" <|
                \xs ->
                    List.foldl Heap.push Heap.empty xs
                        |> Heap.peek
                        |> Expect.equal (List.minimum xs)
            , fuzz (list int) "Can get all inserted elements in order" <|
                \xs ->
                    Heap.fromList xs
                        |> Heap.toList
                        |> Expect.equal (List.sort xs)
            , fuzz (tuple ( list int, list int )) "Can merge two heaps" <|
                \( xs, ys ) ->
                    Heap.fromList xs
                        |> Heap.merge (Heap.fromList ys)
                        |> Heap.toList
                        |> Expect.equal (Heap.toList <| Heap.fromList <| xs ++ ys)
            , describe "Size"
                [ test "Can count size of empty heap" <|
                    \() ->
                        Heap.empty
                            |> Heap.size
                            |> Expect.equal 0
                , fuzz int "Can count size of singleton heap" <|
                    Heap.singleton
                        >> Heap.merge Heap.empty
                        >> Heap.size
                        >> Expect.equal 1
                , fuzz (list int) "Can count size of heap" <|
                    \xs ->
                        Heap.fromList xs
                            |> Heap.size
                            |> Expect.equal (List.length xs)
                ]
            ]
        , describe "Non-comparable values"
            [ fuzz (tuple ( list int, list int )) "Can create heaps of non-comparable values with custom compare function" <|
                \( xs, ys ) ->
                    let
                        compareFn a b =
                            case ( a.list, b.list ) of
                                ( [], [] ) ->
                                    EQ

                                ( _, [] ) ->
                                    GT

                                ( [], _ ) ->
                                    LT

                                _ ->
                                    Maybe.map2 compare (List.maximum a.list) (List.maximum b.list)
                                        |> Maybe.withDefault EQ
                    in
                        Heap.emptySortedWith compareFn
                            |> Heap.push { list = xs }
                            |> Heap.push { list = ys }
                            |> Heap.peek
                            |> Maybe.andThen (.list >> List.maximum)
                            |> Expect.equal (Maybe.map2 min (List.maximum xs) (List.maximum ys))
            , fuzz (tuple ( list int, list int )) "Can create heaps of non-comparable values with cutom hashing function" <|
                \( xs, ys ) ->
                    let
                        hashingFn =
                            .list >> List.sum
                    in
                        Heap.singletonSortedBy hashingFn { list = xs }
                            |> Heap.push { list = ys }
                            |> Heap.peek
                            |> Maybe.map (.list >> List.sum)
                            |> Expect.equal (Just <| min (List.sum xs) (List.sum ys))
            , fuzz (tuple ( list union, list union )) "Can merge heaps of non-comparable values" <|
                \( xs, ys ) ->
                    List.foldl Heap.push (Heap.emptySortedBy intFromUnion) xs
                        |> Heap.merge (Heap.fromListSortedBy intFromUnion ys)
                        |> Heap.toList
                        |> Expect.equal (List.sortBy intFromUnion (xs ++ ys))
            ]
        ]
