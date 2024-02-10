module IkedaPatternTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pages.IkedaPattern exposing (primes, quadraticResidueSet)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Ikeda pattern module"
        [ describe "uniformly redundant array"
            [ test "build quadratic residue modulo p where p is a prime number"
                (\_ ->
                    quadraticResidueSet 11
                        |> Expect.equal (Set.fromList [ 1, 3, 4, 5, 9 ])
                )
            , test "return empty set when p not in our list of primes"
                (\_ ->
                    quadraticResidueSet 16
                        |> Expect.equal Set.empty
                )
            ]
        ]
