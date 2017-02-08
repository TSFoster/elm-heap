module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple)
import Heap exposing (Heap, SortOrder(..), Compare(..))


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
                        |> Heap.mergeInto (Heap.fromList ys)
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
                        >> Heap.mergeInto Heap.empty
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
                        compare a b =
                            case ( a.list, b.list ) of
                                ( [], [] ) ->
                                    EQ

                                ( _, [] ) ->
                                    GT

                                ( [], _ ) ->
                                    LT

                                _ ->
                                    Maybe.map2 Basics.compare (List.maximum a.list) (List.maximum b.list)
                                        |> Maybe.withDefault EQ
                    in
                        Heap.emptyWith { order = MinFirst, compare = With compare }
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
                        Heap.singletonWith { order = MinFirst, compare = By hashingFn } { list = xs }
                            |> Heap.push { list = ys }
                            |> Heap.peek
                            |> Maybe.map (.list >> List.sum)
                            |> Expect.equal (Just <| min (List.sum xs) (List.sum ys))
            , fuzz (tuple ( list union, list union )) "Can merge heaps of non-comparable values" <|
                \( xs, ys ) ->
                    List.foldl Heap.push (Heap.emptyWith { order = MinFirst, compare = By intFromUnion }) xs
                        |> Heap.mergeInto (Heap.fromListWith { order = MinFirst, compare = By intFromUnion } ys)
                        |> Heap.toList
                        |> Expect.equal (List.sortBy intFromUnion (xs ++ ys))
            ]
        , describe "Comparing Heaps"
            [ test "Two empty heaps are equal" <|
                \() ->
                    Heap.compare Heap.empty Heap.empty
                        |> Expect.equal EQ
            , fuzz (list (tuple ( int, int ))) "Heap with same elements sorted differently are not equal" <|
                \tuples ->
                    let
                        theTuples =
                            ( -2, 2 ) :: ( -1, 1 ) :: tuples

                        byFirst =
                            Heap.fromListWith { order = MinFirst, compare = By Tuple.first } theTuples

                        bySecond =
                            Heap.fromListWith { order = MinFirst, compare = By Tuple.second } theTuples
                    in
                        Heap.compare byFirst bySecond
                            |> Expect.notEqual EQ
            , fuzz int "(heapA < heapB) <=> (peek heapA < peek heapB)" <|
                \i ->
                    Heap.compare (Heap.singleton i) (Heap.singleton (i + 1))
                        |> Expect.equal LT
            , fuzz int "(heapA > heapB) <=> (peek heapA > peek heapB)" <|
                \i ->
                    Heap.compare (Heap.singleton (i + 1)) (Heap.singleton i)
                        |> Expect.equal GT
            , fuzz int "An empty heap is less than a non-empty heap" <|
                \i ->
                    Heap.compare Heap.empty (Heap.singleton i)
                        |> Expect.equal LT
            ]
        , describe "Max heaps"
            [ fuzz (list int) "Max heaps produce the reverse of Min heaps" <|
                \ints ->
                    Heap.fromListWith { order = MaxFirst, compare = With Basics.compare } ints
                        |> Heap.toListReverse
                        |> Expect.equal (Heap.fromList ints |> Heap.toList)
            , fuzz (list int) "Max heaps have biggest value first" <|
                \ints ->
                    Heap.fromListWith { order = MaxFirst, compare = With Basics.compare } ints
                        |> Heap.peek
                        |> Expect.equal (List.maximum ints)
            ]
        ]
