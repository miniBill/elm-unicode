module Main exposing (inputs, suite)

import Expect exposing (Expectation)
import Hex
import Set
import Test exposing (Test, test)
import TestData
import Unicode exposing (Category(..))


inputs : List Int
inputs =
    (specials ++ TestData.testData)
        |> List.concatMap (\n -> [ n - 1, n, n + 1 ])
        |> Set.fromList
        |> Set.toList


suite : Test
suite =
    test "Is coherent" <|
        \_ ->
            Expect.all (List.map (\input _ -> checkCode input) inputs) ()


{-| Some special cases that are worth checking explicitly.
-}
specials : List Int
specials =
    [ 'ǲ' -- Upper and lower are both different
    , 'ﬀ'
    , 'K'
    , 'ẞ' -- (toUpper >> toLower) /= toUpper
    , 'ῼ'
    , '\u{0378}' -- Missing from Unicode table
    ]
        |> List.map Char.toCode


checkCode : Int -> Expectation
checkCode code =
    let
        char : Char
        char =
            Char.fromCode code

        category : Maybe Category
        category =
            Unicode.getCategory char

        no : { isUpper : Bool, isLower : Bool, isAlpha : Bool, isAlphaNum : Bool, isDigit : Bool }
        no =
            { isUpper = False
            , isLower = False
            , isAlpha = False
            , isAlphaNum = False
            , isDigit = False
            }

        letter : { isUpper : Bool, isLower : Bool, isAlpha : Bool, isAlphaNum : Bool, isDigit : Bool }
        letter =
            { no | isAlpha = True, isAlphaNum = True }

        digit : { isUpper : Bool, isLower : Bool, isAlpha : Bool, isAlphaNum : Bool, isDigit : Bool }
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

        errorPrefix : String -> String -> String
        errorPrefix name expected =
            "[U+" ++ Hex.toString code ++ " - " ++ String.fromChar char ++ "] If getCategory is " ++ Debug.toString category ++ " then " ++ name ++ " should be " ++ expected

        expectations : List (Char -> Expectation)
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
                                |> Expect.onFail (errorPrefix name "True")

                        else
                            function v
                                |> Expect.equal False
                                |> Expect.onFail (errorPrefix name "False")
                    )
    in
    Expect.all expectations char
