module VersionCheck exposing (suite)

{- To use this test module, copy the released Unicode.elm to OldUnicode.elm. -}

import Expect exposing (Expectation)
import Hex
import Main
import OldUnicode
import Test exposing (Test, test)
import Unicode


suite : Test
suite =
    test "Is coherent with older version" <|
        \_ ->
            Expect.all
                (List.map (\input _ -> checkCode input) Main.inputs)
                ()


checkCode : Int -> Expectation
checkCode code =
    let
        char : Char
        char =
            Char.fromCode code

        errorMessage : String
        errorMessage =
            "[U+" ++ Hex.toString code ++ " - " ++ String.fromChar char ++ "] OldUnicode.getCategory = " ++ Debug.toString oldCategory ++ " then Unicode.getCategory should be the same, but it was " ++ Debug.toString newCategory

        newCategory : String
        newCategory =
            Debug.toString <| Unicode.getCategory char

        oldCategory : String
        oldCategory =
            Debug.toString <| OldUnicode.getCategory char
    in
    newCategory
        |> Expect.equal oldCategory
        |> Expect.onFail errorMessage
