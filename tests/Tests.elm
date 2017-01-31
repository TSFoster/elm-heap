module Tests exposing (..)

import Test exposing (..)
import Expect
import Heap exposing (Heap)


all : Test
all =
    describe "Heap"
        [ describe "Basics"
            [ test "Can create an empty heap with no items" <|
                \() ->
                    Expect.equal Nothing <| Heap.peek Heap.empty
            ]
        ]
