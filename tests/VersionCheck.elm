module VersionCheck exposing (suite)

{- To use this test module, move Unicode.elm to OldUnicode.elm, regen Unicode.elm and then uncomment the relevant lines. -}
-- import OldUnicode

import Expect exposing (Expectation)
import Hex
import Main
import Set
import Test exposing (Test, describe, test)
import Unicode


suite : Test
suite =
    if False then
        (List.range 0 0x20FF ++ Main.specials)
            |> Set.fromList
            |> Set.toList
            |> List.map (\code -> test ("for \\u{" ++ Hex.toString code ++ "} - " ++ String.fromChar (Char.fromCode code)) <| \_ -> checkCode code)
            |> describe "Is coherent with older version"

    else
        test "Read the top of the file" <| \_ -> Expect.pass


checkCode : Int -> Expectation
checkCode codepoint =
    let
        char : Char
        char =
            Char.fromCode codepoint
    in
    (Debug.toString <| Unicode.getCategory char)
        -- |> Expect.equal (Debug.toString <| OldUnicode.getCategory char)
        |> (\_ -> Expect.pass)
