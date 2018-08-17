module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Translate exposing (Translation, get, getBrowserLang, load, setDefaultLang, use)


ja : Translation
ja =
    use "ja" load


suite : Test
suite =
    describe "Translate module"
        [ describe "Translate.get"
            [ test "get Japanese translation" <|
                \_ ->
                    get "key" [] ja
                        |> Expect.equal "translated text"
            ]
        ]
