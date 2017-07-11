module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple)
import Heap exposing (Heap, smallest, biggest, by, thenBy, using)


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
                    Expect.equal Nothing <| Heap.peek <| Heap.empty smallest
            , fuzz int "Can insert an element and pop it" <|
                \i ->
                    Heap.singleton smallest i
                        |> Heap.pop
                        |> Maybe.map (Tuple.mapSecond Heap.isEmpty)
                        |> Expect.equal (Just ( i, True ))
            , fuzz (tuple ( int, int )) "Can insert two elements and pop lower one" <|
                \( i, j ) ->
                    Heap.singleton smallest i
                        |> Heap.push j
                        |> Heap.pop
                        |> Maybe.map (Tuple.mapSecond Heap.isEmpty)
                        |> Expect.equal (Just ( Basics.min i j, False ))
            , fuzz (list int) "Can insert elements and peek lowest one" <|
                \xs ->
                    List.foldl Heap.push (Heap.empty smallest) xs
                        |> Heap.peek
                        |> Expect.equal (List.minimum xs)
            , fuzz (list int) "Can get all inserted elements in order" <|
                \xs ->
                    Heap.fromList smallest xs
                        |> Heap.toList
                        |> Expect.equal (List.sort xs)
            , fuzz (tuple ( list int, list int )) "Can merge two heaps" <|
                \( xs, ys ) ->
                    Heap.fromList smallest xs
                        |> Heap.mergeInto (Heap.fromList smallest ys)
                        |> Heap.toList
                        |> Expect.equal (Heap.toList <| Heap.fromList smallest <| xs ++ ys)
            , describe "Size"
                [ test "Can count size of empty heap" <|
                    \() ->
                        Heap.empty smallest
                            |> Heap.size
                            |> Expect.equal 0
                , fuzz int "Can count size of singleton heap" <|
                    Heap.singleton smallest
                        >> Heap.mergeInto (Heap.empty smallest)
                        >> Heap.size
                        >> Expect.equal 1
                , fuzz (list int) "Can count size of heap" <|
                    \xs ->
                        Heap.fromList smallest xs
                            |> Heap.size
                            |> Expect.equal (List.length xs)
                ]
            ]
        , describe "Non-comparable values"
            [ fuzz (tuple ( list int, list int )) "Can create heaps of non-comparable values with custom hashing function" <|
                \( xs, ys ) ->
                    let
                        hashingFn =
                            .list >> List.sum
                    in
                        Heap.singleton (smallest |> by hashingFn) { list = xs }
                            |> Heap.push { list = ys }
                            |> Heap.peek
                            |> Maybe.map (.list >> List.sum)
                            |> Expect.equal (Just <| min (List.sum xs) (List.sum ys))
            , fuzz (tuple ( list int, list int )) "Can create heaps of non-comparable values with two custom hashing functions" <|
                \( xs, ys ) ->
                    let
                        hash a =
                            if List.isEmpty a.list then
                                0
                            else
                                1

                        hash2 a =
                            List.maximum a.list |> Maybe.withDefault 0
                    in
                        Heap.empty (smallest |> by hash |> thenBy hash2)
                            |> Heap.push { list = xs }
                            |> Heap.push { list = ys }
                            |> Heap.peek
                            |> Maybe.andThen (.list >> List.maximum)
                            |> Expect.equal (Maybe.map2 min (List.maximum xs) (List.maximum ys))
            , test "Can create heaps of non-comparable values with custom compare function" <|
                \() ->
                    let
                        closestToZero : (Int -> Int -> Int) -> (Int -> Int -> Int) -> Basics.Order
                        closestToZero a b =
                            compare (abs <| a 1 2) (abs <| b 1 2)
                    in
                        [ (+), (-), (*) ]
                            |> Heap.fromList (smallest |> using closestToZero)
                            |> Heap.peek
                            |> Maybe.map (\fn -> fn 1 2)
                            |> Expect.equal (Just -1)
            , fuzz (tuple ( list union, list union )) "Can merge heaps of non-comparable values" <|
                \( xs, ys ) ->
                    let
                        opts =
                            smallest |> by intFromUnion
                    in
                        List.foldl Heap.push (Heap.empty opts) xs
                            |> Heap.mergeInto (Heap.fromList opts ys)
                            |> Heap.toList
                            |> Expect.equal (List.sortBy intFromUnion (xs ++ ys))
            ]
        , describe "Max heaps"
            [ fuzz (list int) "Max heaps produce the reverse of Min heaps" <|
                \ints ->
                    Heap.fromList biggest ints
                        |> Heap.toListReverse
                        |> Expect.equal (Heap.fromList smallest ints |> Heap.toList)
            , fuzz (list int) "Max heaps have biggest value first" <|
                \ints ->
                    Heap.fromList biggest ints
                        |> Heap.peek
                        |> Expect.equal (List.maximum ints)
            ]
        ]
