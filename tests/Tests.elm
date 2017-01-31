module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple)
import Heap exposing (Heap)


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
            ]
        ]
