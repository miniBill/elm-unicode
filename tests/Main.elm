module Main exposing (specials, suite)

import Expect exposing (Expectation)
import Hex
import Set
import Test exposing (..)
import Unicode exposing (Category(..))


suite : Test
suite =
    List.range 0 0x20FF
        |> (++) (List.map Char.toCode specials)
        |> Set.fromList
        |> Set.toList
        |> List.map (\code -> test ("for \\u{" ++ Hex.toString code ++ "} - " ++ String.fromChar (Char.fromCode code)) <| \_ -> checkCode code)
        |> describe "Is coherent"


{-| Some special cases that are worth checking explicitly.
-}
specials : List Char
specials =
    [ 'ǲ' -- Upper and lower are both different
    , 'ﬀ'
    , 'K'
    , 'ẞ' -- (toUpper >> toLower) /= toUpper
    , 'ῼ'
    , '\u{0378}' -- Missing from Unicode table
    ]


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
                    (\( name, function, expected ) v ->
                        if expected then
                            function v
                                |> Expect.equal True
                                |> Expect.onFail ("If getCategory is " ++ Debug.toString category ++ " then " ++ name ++ " should be True")

                        else
                            function v
                                |> Expect.equal False
                                |> Expect.onFail ("If getCategory is " ++ Debug.toString category ++ " then " ++ name ++ " should be False")
                    )
    in
    Expect.all expectations char
