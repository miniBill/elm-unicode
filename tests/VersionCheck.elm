module VersionCheck exposing (suite)

{- To use this test module, move Unicode.elm to OldUnicode.elm, regen Unicode.elm and then uncomment the relevant lines. -}
-- import OldUnicode

import Expect exposing (Expectation)
import Hex
import Main
import Test exposing (Test, test)
import Unicode


suite : Test
suite =
    if False then
        test "Is coherent with older version" <|
            \_ ->
                Expect.all
                    (List.map (\input _ -> checkCode input) Main.inputs)
                    ()

    else
        test "Read the top of the file" <| \_ -> Expect.pass


checkCode : Int -> Expectation
checkCode code =
    let
        char : Char
        char =
            Char.fromCode code

        errorMessage : String
        errorMessage =
            "[U+" ++ Hex.toString code ++ " - " ++ String.fromChar char ++ "] OldUnicode.getCategory = " ++ Debug.toString oldCategory ++ " then Unicode.getCategory should be the same, but it was" ++ Debug.toString newCategory

        newCategory : String
        newCategory =
            Debug.toString <| Unicode.getCategory char

        oldCategory : String
        oldCategory =
            -- Debug.toString <| OldUnicode.getCategory char
            Debug.toString <| Unicode.getCategory char
    in
    newCategory
        |> Expect.equal oldCategory
        |> Expect.onFail errorMessage
