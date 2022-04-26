module Main exposing (..)

import Expect exposing (Expectation)
import Hex
import Test exposing (..)
import Unicode exposing (Category(..))


suite : Test
suite =
    describe "Is coherent" <|
        List.map (\code -> test ("for \\u{" ++ Hex.toString code ++ "} - " ++ String.fromChar (Char.fromCode code)) <| \_ -> checkCode code) <|
            List.range 0 0x0010FFFF


checkCode : Int -> Expectation
checkCode code =
    let
        char =
            Char.fromCode code

        category =
            Unicode.getCategory char

        no =
            { isUpper = False
            , isLower = False
            , isAlpha = False
            , isAlphaNum = False
            , isDigit = False
            }

        letter =
            { no | isAlpha = True, isAlphaNum = True }

        digit =
            { no | isDigit = True, isAlphaNum = True }

        { isUpper, isLower, isAlpha, isAlphaNum, isDigit } =
            case category of
                Just LetterUppercase ->
                    { letter | isUpper = True }

                Just LetterLowercase ->
                    { letter | isLower = True }

                Just LetterTitlecase ->
                    letter

                Just NumberDecimalDigit ->
                    digit

                Just NumberLetter ->
                    digit

                Just NumberOther ->
                    digit

                Just LetterModifier ->
                    letter

                Just LetterOther ->
                    letter

                _ ->
                    no

        expectations =
            [ ( "isUpper", Unicode.isUpper, isUpper )
            , ( "isLower", Unicode.isLower, isLower )
            , ( "isAlpha", Unicode.isAlpha, isAlpha )
            , ( "isAlphaNum", Unicode.isAlphaNum, isAlphaNum )
            , ( "isDigit", Unicode.isDigit, isDigit )
            ]
                |> List.map
                    (\( name, function, expected ) ->
                        if expected then
                            Expect.true
                                ("If getCategory is " ++ Debug.toString category ++ " then " ++ name ++ " should be True")
                                << function

                        else
                            Expect.false
                                ("If getCategory is " ++ Debug.toString category ++ " then " ++ name ++ " should be False")
                                << function
                    )
    in
    Expect.all expectations char
