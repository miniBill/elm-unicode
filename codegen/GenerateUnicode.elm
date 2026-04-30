module GenerateUnicode exposing (main)

import Categories exposing (Category(..), categoryFromString)
import Dict exposing (Dict)
import Elm exposing (Declaration, Expression, File)
import Elm.Annotation as Type
import Elm.Arg
import Elm.Let
import Elm.Op
import Gen.Basics
import Gen.Char
import Gen.CodeGen.Generate as Generate
import GenerateCategories
import Hex
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import Result
import Result.Extra
import Set exposing (Set)


main : Program Json.Decode.Value () ()
main =
    Platform.worker
        { init =
            \flags ->
                case Json.Decode.decodeValue Generate.directoryDecoder flags of
                    Ok input ->
                        case flagsToFile input of
                            Ok file ->
                                ( ()
                                , Generate.files [ file ]
                                )

                            Err e ->
                                ( ()
                                , Generate.error
                                    [ { title = "Error generating file"
                                      , description = e
                                      }
                                    ]
                                )

                    Err e ->
                        ( ()
                        , Generate.error
                            [ { title = "Error decoding flags"
                              , description = Json.Decode.errorToString e
                              }
                            ]
                        )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


{-| Tries to aggregate consecutive elements. When the aggregation fails they get added to the output list.

The function is passed, in order, the current element and the last one.

-}
foldWithLast : (a -> a -> Maybe a) -> List a -> List a
foldWithLast step list =
    list
        |> List.foldl
            (\e ( last, acc ) ->
                case last of
                    Nothing ->
                        ( Just e, acc )

                    Just lastElem ->
                        case step e lastElem of
                            Just c ->
                                ( Just c, acc )

                            Nothing ->
                                ( Just e, lastElem :: acc )
            )
            ( Nothing, [] )
        |> (\( l, a ) ->
                case l of
                    Nothing ->
                        List.reverse a

                    Just le ->
                        List.reverse <| le :: a
           )


flagsToFile : Generate.Directory -> Result String File
flagsToFile (Generate.Directory dir) =
    Dict.get "UnicodeData.txt" dir.files
        |> Result.fromMaybe "Missing UnicodeData.txt"
        |> Result.andThen
            (\unicodeData ->
                unicodeData
                    |> String.split "\n"
                    |> Result.Extra.combineMap parseUnicodeDataLine
            )
        |> Result.map Maybe.Extra.values
        |> Result.andThen
            (Result.Extra.foldlWhileOk
                (\e ( last, acc ) ->
                    case last of
                        Nothing ->
                            Ok ( Just e, acc )

                        Just l ->
                            case ( l.codeValue, e.codeValue ) of
                                ( First li, Last ei ) ->
                                    ( Nothing
                                    , { from = li
                                      , to = ei
                                      , category = l.category
                                      }
                                        :: acc
                                    )
                                        |> Ok

                                ( Single ls, _ ) ->
                                    ( Just e
                                    , { from = ls
                                      , to = ls
                                      , category = l.category
                                      }
                                        :: acc
                                    )
                                        |> Ok

                                _ ->
                                    Err ("Invalid input file: found " ++ lineToString e ++ " after " ++ lineToString l)
                )
                ( Nothing, [] )
            )
        |> Result.andThen
            (\( last, acc ) ->
                case last of
                    Nothing ->
                        Ok (List.reverse acc)

                    Just l ->
                        case l.codeValue of
                            Single ls ->
                                ({ from = ls
                                 , to = ls
                                 , category = l.category
                                 }
                                    :: acc
                                )
                                    |> List.reverse
                                    |> Ok

                            _ ->
                                Err ("Invalid input file: list ended with " ++ lineToString l)
            )
        |> Result.map
            (foldWithLast
                (\e last ->
                    if
                        (last.to + 1 == e.from || e.category == OtherPrivateUse)
                            && (last.category == e.category)
                    then
                        Just { last | to = e.to }

                    else
                        Nothing
                )
            )
        |> Result.map
            (\ranges ->
                let
                    declarations : List Declaration
                    declarations =
                        [ Elm.group lettersDeclarations
                        , Elm.group digitsDeclarations
                        , Elm.group categoriesDeclarations
                        , Elm.group separatorsDeclarations
                        ]

                    lettersDeclarations : List Declaration
                    lettersDeclarations =
                        [ Elm.docs "## Letters"
                        , categoriesToDeclarationWithSimpleCheck
                            { name = "isUpper"
                            , categories = [ LetterUppercase ]
                            , comment = "Detect upper case characters (Unicode category Lu)"
                            , group = "Letters"
                            , simpleCheck = \c -> Char.toUpper c == c && Char.toLower c /= c
                            , simpleCheckExpression =
                                \c ->
                                    Elm.Op.and
                                        (Elm.Op.equal (Gen.Char.call_.toUpper c) c)
                                        (Elm.Op.notEqual (Gen.Char.call_.toLower c) c)
                            }
                            ranges
                        , categoriesToDeclarationWithSimpleCheck
                            { name = "isLower"
                            , categories = [ LetterLowercase ]
                            , comment = "Detect lower case characters (Unicode category Ll)"
                            , group = "Letters"
                            , simpleCheck = \c -> Char.toLower c == c && Char.toUpper c /= c
                            , simpleCheckExpression =
                                \c ->
                                    Elm.Op.and
                                        (Elm.Op.equal (Gen.Char.call_.toLower c) c)
                                        (Elm.Op.notEqual (Gen.Char.call_.toUpper c) c)
                            }
                            ranges
                        , categoriesToDeclaration
                            { name = "isAlpha"
                            , categories =
                                [ LetterLowercase
                                , LetterUppercase
                                , LetterTitlecase
                                , LetterModifier
                                , LetterOther
                                ]
                            , comment = "Detect letters (Unicode categories Lu, Ll, Lt, Lm, Lo)"
                            , group = "Letters"
                            }
                            ranges
                        , categoriesToDeclaration
                            { name = "isAlphaNum"
                            , categories =
                                [ LetterLowercase
                                , LetterUppercase
                                , LetterTitlecase
                                , LetterModifier
                                , LetterOther
                                , NumberDecimalDigit
                                , NumberLetter
                                , NumberOther
                                ]
                            , comment = "Detect letters or digits (Unicode categories Lu, Ll, Lt, Lm, Lo, Nd, Nl, No)"
                            , group = "Letters"
                            }
                            ranges
                        ]

                    digitsDeclarations : List Declaration
                    digitsDeclarations =
                        [ Elm.docs "## Digits"
                        , categoriesToDeclaration
                            { name = "isDigit"
                            , categories = [ NumberDecimalDigit, NumberLetter, NumberOther ]
                            , comment = "Detect digits (Unicode categories Nd, Nl, No)"
                            , group = "Digits"
                            }
                            ranges
                        ]

                    categoriesDeclarations : List Declaration
                    categoriesDeclarations =
                        List.take 2 GenerateCategories.declarations
                            ++ (getCategoryDeclaration ranges
                                    :: List.drop 2 GenerateCategories.declarations
                               )

                    separatorsDeclarations =
                        [ Elm.docs "## Separators"
                        , categoriesToDeclaration
                            { name = "isSpace"
                            , categories = [ SeparatorSpace ]
                            , comment = "Detect spaces (Unicode category Zs)"
                            , group = "Separators"
                            }
                            ranges
                        , categoriesToDeclaration
                            { name = "isSeparator"
                            , categories = [ SeparatorSpace, SeparatorLine, SeparatorParagraph ]
                            , comment = "Detect spaces (Unicode categories Zs, Zl, Zp)"
                            , group = "Separators"
                            }
                            ranges
                        ]
                in
                Elm.fileWith [ "Unicode" ]
                    { docs = "Unicode aware functions for working with characters."
                    , aliases = []
                    }
                    declarations
            )


lineToString : { codeValue : CodeValue, category : Category } -> String
lineToString { codeValue, category } =
    "{ codeValue = "
        ++ codeValueToString codeValue
        ++ ", category = "
        ++ Categories.categoryToString category
        ++ "}"


codeValueToString : CodeValue -> String
codeValueToString codeValue =
    case codeValue of
        Single i ->
            "Single " ++ String.fromInt i

        First i ->
            "First " ++ String.fromInt i

        Last i ->
            "Last " ++ String.fromInt i


splitAt : Int -> Tree a -> Tree a
splitAt at tree =
    case tree of
        Split cat l r ->
            if at < cat then
                Split cat (splitAt at l) r

            else if at > cat then
                Split cat l (splitAt at r)

            else
                tree

        Node n ->
            let
                splat :
                    List ( Int, Int, c )
                    -> ( List ( Int, Int, c ), List ( Int, Int, c ) )
                splat =
                    List.foldr
                        (\(( elf, elt, ecat ) as e) ( lacc, hacc ) ->
                            if elt < at then
                                ( e :: lacc, hacc )

                            else if elf >= at then
                                ( lacc, e :: hacc )

                            else
                                ( ( elf, at - 1, ecat ) :: lacc
                                , ( at, elt, ecat ) :: hacc
                                )
                        )
                        ( [], [] )

                ( la, ha ) =
                    splat n.all

                ( le, he ) =
                    splat n.even

                ( lo, ho ) =
                    splat n.odd
            in
            if List.isEmpty la && List.isEmpty le && List.isEmpty lo then
                tree

            else if List.isEmpty ha && List.isEmpty he && List.isEmpty ho then
                tree

            else
                Split at
                    (Node { all = la, even = le, odd = lo })
                    (Node { all = ha, even = he, odd = ho })


toEvenOddRange : ({ a | from : Int, to : Int } -> payload) -> { a | from : Int, to : Int } -> EvenOddRange payload
toEvenOddRange getPayload ({ from, to } as input) =
    let
        payload =
            getPayload input
    in
    if from == to then
        if modBy 2 from == 0 then
            Even from from payload

        else
            Odd from from payload

    else
        All from to payload


type EvenOddRange a
    = All Int Int a
    | Odd Int Int a
    | Even Int Int a


type Tree a
    = Split Int (Tree a) (Tree a)
    | Node
        { all : List ( Int, Int, a )
        , even : List ( Int, Int, a )
        , odd : List ( Int, Int, a )
        }


type CodeValue
    = Single Int
    | First Int
    | Last Int


parseUnicodeDataLine : String -> Result String (Maybe { codeValue : CodeValue, category : Category })
parseUnicodeDataLine line =
    case String.split ";" line of
        codeValueHex :: characterName :: generalCategory :: _ ->
            Result.map2
                (\codeValue category ->
                    { codeValue =
                        if String.endsWith "First>" characterName then
                            First codeValue

                        else if String.endsWith "Last>" characterName then
                            Last codeValue

                        else
                            Single codeValue
                    , category = category
                    }
                        |> Just
                )
                (Hex.fromString <| String.toLower codeValueHex)
                (case categoryFromString generalCategory of
                    Just category ->
                        Ok category

                    Nothing ->
                        Err ("Invalid category: " ++ generalCategory)
                )

        [ "" ] ->
            Ok Nothing

        _ ->
            Err ("Invalid UnicodeData.txt line: " ++ Json.Encode.encode 0 (Json.Encode.string line))


rangeToCondition : Expression -> ( Int, Int, a ) -> Expression
rangeToCondition code ( from, to, _ ) =
    if from == to then
        Elm.Op.equal code (Elm.hex from)

    else if from == 0 then
        Elm.Op.lte code (Elm.hex to)

    else
        Elm.Op.parens
            (Elm.Op.and
                (Elm.Op.lte (Elm.hex from) code)
                (Elm.Op.lte code (Elm.hex to))
            )


joinOr : List Expression -> Expression
joinOr lst =
    case List.reverse lst of
        [] ->
            Elm.bool False

        last :: init ->
            List.foldl Elm.Op.or last init


modIs : Expression -> Int -> Expression
modIs code n =
    Elm.Op.equal
        (Elm.apply
            (Elm.value
                { importFrom = []
                , name = "modBy"
                , annotation = Just (Type.function [ Type.int, Type.int ] Type.int)
                }
            )
            [ Elm.int 2, code ]
        )
        (Elm.int n)


letCode :
    (Expression -> Expression)
    -> Expression
    -> Expression
letCode inner c =
    Elm.Let.letIn inner
        |> Elm.Let.value "code" (Gen.Char.call_.toCode c)
        |> Elm.Let.toExpression


letCodeWithSimpleCheck :
    { simpleCheckExpression : Expression -> Expression
    , body :
        { code : Expression
        , simple : Expression
        }
        -> Expression
    }
    -> Expression
    -> Expression
letCodeWithSimpleCheck { body, simpleCheckExpression } c =
    Elm.Let.letIn
        (\code simple ->
            body
                { code = code
                , simple = simple
                }
        )
        |> Elm.Let.value "code"
            (Gen.Char.call_.toCode c)
        |> Elm.Let.value "simple"
            (simpleCheckExpression c)
        |> Elm.Let.toExpression


getCategoryDeclaration :
    List { category : Category, from : Int, to : Int }
    -> Declaration
getCategoryDeclaration ranges =
    let
        doc =
            """Get the Unicode category. Warning: this function is very big. You should usually use one of the `isXXX` ones instead."""

        group { all, even, odd } =
            let
                go cat =
                    { category = cat
                    , all = List.filter (\( _, _, category ) -> category == cat) all
                    , even = List.filter (\( _, _, category ) -> category == cat) even
                    , odd = List.filter (\( _, _, category ) -> category == cat) odd
                    }
                        :: group
                            { all = List.filter (\( _, _, category ) -> category /= cat) all
                            , even = List.filter (\( _, _, category ) -> category /= cat) even
                            , odd = List.filter (\( _, _, category ) -> category /= cat) odd
                            }
            in
            case ( all, even, odd ) of
                ( [], [], [] ) ->
                    []

                ( ( _, _, category ) :: _, _, _ ) ->
                    go category

                ( _, ( _, _, category ) :: _, _ ) ->
                    go category

                ( _, _, ( _, _, category ) :: _ ) ->
                    go category

        treeToExpression :
            Expression
            -> Tree Category
            -> Expression
        treeToExpression code =
            let
                rangesToConditions : List ( Int, Int, a ) -> List Expression
                rangesToConditions =
                    List.map (rangeToCondition code)

                go t =
                    case t of
                        Node n ->
                            n
                                |> group
                                |> List.map
                                    (\{ category, all, odd, even } ->
                                        let
                                            modded =
                                                if List.isEmpty even then
                                                    if List.isEmpty odd then
                                                        []

                                                    else
                                                        [ Elm.Op.and
                                                            (modIs code 1)
                                                            (joinOr <| rangesToConditions odd)
                                                        ]

                                                else if List.isEmpty odd then
                                                    [ Elm.Op.and
                                                        (modIs code 0)
                                                        (joinOr <| rangesToConditions even)
                                                    ]

                                                else
                                                    [ Elm.ifThen (modIs code 0)
                                                        (joinOr <| rangesToConditions even)
                                                        (joinOr <| rangesToConditions odd)
                                                    ]
                                        in
                                        ( just (categoryToConstructor category)
                                        , joinOr
                                            (rangesToConditions all ++ modded)
                                        )
                                    )
                                |> List.foldr (\( category, condition ) acc -> Elm.ifThen condition category acc) nothing

                        Split at l r ->
                            Elm.ifThen (Elm.Op.lt code (Elm.hex at))
                                (go l)
                                (go r)
            in
            go

        checks : Expression -> Expression
        checks code =
            Elm.ifThen (isNaN code)
                (Elm.maybe (Just (Elm.val "OtherSurrogate")))
                (ranges
                    |> rangesToTree True .category
                    |> split
                    |> treeToExpression code
                )
    in
    Elm.fn (Elm.Arg.varWith "c" <| Type.named [] "Char")
        (\c ->
            letCode checks c
                |> Elm.withType (Type.maybe <| Type.named [] "Category")
        )
        |> Elm.declaration "getCategory"
        |> Elm.expose
        |> Elm.withDocumentation doc


isNaN : Expression -> Expression
isNaN code =
    Gen.Basics.call_.isNaN (Gen.Basics.call_.toFloat code)


nothing : Expression
nothing =
    Elm.value
        { importFrom = []
        , name = "Nothing"
        , annotation = Nothing
        }


just : Expression -> Expression
just value =
    Elm.apply
        (Elm.value
            { importFrom = []
            , name = "Just"
            , annotation = Nothing
            }
        )
        [ value ]


categoryToConstructor : Category -> Expression
categoryToConstructor category =
    Elm.value
        { importFrom = []
        , name = Categories.categoryToConstructor category
        , annotation = Just (Type.named [] "Category")
        }


categoriesToDeclaration :
    { name : String
    , categories : List Category
    , comment : String
    , group : String
    }
    -> List { category : Category, from : Int, to : Int }
    -> Declaration
categoriesToDeclaration { name, categories, comment, group } ranges =
    let
        treeToExpression :
            Expression
            -> Tree a
            -> Expression
        treeToExpression code =
            let
                rangesToCondition : List ( Int, Int, a ) -> List Expression
                rangesToCondition =
                    List.map (rangeToCondition code)

                go t =
                    case t of
                        Node { all, even, odd } ->
                            let
                                modded =
                                    if List.isEmpty even then
                                        if List.isEmpty odd then
                                            []

                                        else
                                            [ Elm.Op.and
                                                (modIs code 1)
                                                (joinOr (rangesToCondition odd))
                                            ]

                                    else if List.isEmpty odd then
                                        [ Elm.Op.and
                                            (modIs code 0)
                                            (joinOr (rangesToCondition even))
                                        ]

                                    else
                                        [ Elm.ifThen (modIs code 0)
                                            (joinOr (rangesToCondition even))
                                            (joinOr (rangesToCondition odd))
                                        ]
                            in
                            joinOr
                                (rangesToCondition all ++ modded)

                        Split at l r ->
                            Elm.ifThen (Elm.Op.lt code (Elm.hex at))
                                (go l)
                                (go r)
            in
            go

        checks : Expression -> Expression
        checks code =
            Elm.ifThen (isNaN code)
                (Elm.bool False)
                (ranges
                    |> List.filter (\{ category } -> List.member category categories)
                    |> rangesToTree True (\_ -> ())
                    |> treeToExpression code
                )
    in
    Elm.fn (Elm.Arg.varWith "c" <| Type.named [] "Char")
        (\c ->
            letCode checks c
                |> Elm.withType Type.bool
        )
        |> Elm.declaration name
        |> Elm.withDocumentation comment
        |> Elm.expose


categoriesToDeclarationWithSimpleCheck :
    { name : String
    , categories : List Category
    , comment : String
    , group : String
    , simpleCheck : Char -> Bool
    , simpleCheckExpression : Expression -> Expression
    }
    -> List { from : Int, to : Int, category : Category }
    -> Declaration
categoriesToDeclarationWithSimpleCheck { name, categories, comment, group, simpleCheck, simpleCheckExpression } ranges =
    let
        inCategory : Set Int
        inCategory =
            ranges
                |> List.concatMap
                    (\range ->
                        if List.member range.category categories then
                            List.range range.from range.to

                        else
                            []
                    )
                |> Set.fromList

        toBody :
            { code : Expression
            , simple : Expression
            }
            -> Expression
        toBody { code, simple } =
            let
                rangesToCondition : List ( Int, Int, a ) -> List Expression
                rangesToCondition =
                    List.map (rangeToCondition code)

                go : Tree a -> Expression
                go t =
                    case t of
                        Node { all, even, odd } ->
                            let
                                modded =
                                    if List.isEmpty even then
                                        if List.isEmpty odd then
                                            []

                                        else
                                            [ Elm.Op.and
                                                (modIs code 1)
                                                (joinOr <| rangesToCondition odd)
                                            ]

                                    else if List.isEmpty odd then
                                        [ Elm.Op.and
                                            (modIs code 0)
                                            (joinOr <| rangesToCondition even)
                                        ]

                                    else
                                        [ Elm.ifThen (modIs code 0)
                                            (joinOr <| rangesToCondition even)
                                            (joinOr <| rangesToCondition odd)
                                        ]
                            in
                            joinOr
                                (rangesToCondition all ++ modded)

                        Split at l r ->
                            Elm.ifThen (Elm.Op.lt code (Elm.hex at))
                                (go l)
                                (go r)
            in
            Elm.ifThen (isNaN code)
                (Elm.bool False)
                (Elm.ifThen simple
                    (go simpleTree)
                    (go nonsimpleTree)
                )

        simpleRanges : List { from : Int, to : Int, hasAny : Bool }
        simpleRanges =
            List.range 0 0x000F0000
                |> List.filterMap
                    (\codeValue ->
                        let
                            belongs : Bool
                            belongs =
                                Set.member codeValue inCategory

                            isSimple : Bool
                            isSimple =
                                simpleCheck (Char.fromCode codeValue) == belongs
                        in
                        if isSimple || belongs then
                            Just
                                { from = codeValue
                                , to = codeValue
                                , hasAny = belongs
                                }

                        else
                            Nothing
                    )
                |> foldWithLast
                    (\e last ->
                        if e.from /= last.to + 1 then
                            Nothing

                        else
                            Just
                                { from = last.from
                                , to = e.to
                                , hasAny = last.hasAny || e.hasAny
                                }
                    )
                |> List.filter .hasAny

        nonsimpleRanges : List { from : Int, to : Int, hasAnyException : Bool }
        nonsimpleRanges =
            ranges
                |> List.concatMap
                    (\{ from, to, category } ->
                        List.map (Tuple.pair category) <|
                            List.range from to
                    )
                |> List.filterMap
                    (\( category, codeValue ) ->
                        let
                            belongs =
                                List.member category categories

                            isSimple =
                                simpleCheck (Char.fromCode codeValue)
                        in
                        if belongs || isSimple then
                            Just
                                { from = codeValue
                                , to = codeValue
                                , hasAnyException = not isSimple
                                }

                        else
                            Nothing
                    )
                |> foldWithLast
                    (\e last ->
                        if e.from /= last.to + 1 then
                            Nothing

                        else
                            Just
                                { from = last.from
                                , to = e.to
                                , hasAnyException = last.hasAnyException || e.hasAnyException
                                }
                    )
                |> List.filter .hasAnyException

        simpleTree : Tree ()
        simpleTree =
            rangesToTree False (\_ -> ()) simpleRanges

        nonsimpleTree : Tree ()
        nonsimpleTree =
            rangesToTree True (\_ -> ()) nonsimpleRanges
    in
    Elm.fn (Elm.Arg.varWith "c" <| Type.named [] "Char")
        (\c ->
            letCodeWithSimpleCheck
                { simpleCheckExpression = simpleCheckExpression
                , body = toBody
                }
                c
                |> Elm.withType Type.bool
        )
        |> Elm.declaration name
        |> Elm.withDocumentation comment
        |> Elm.expose


rangesToTree : Bool -> ({ a | from : Int, to : Int } -> payload) -> List { a | from : Int, to : Int } -> Tree payload
rangesToTree splitAt100 getPayload ranges =
    ranges
        |> List.map (toEvenOddRange getPayload)
        |> foldWithLast
            (\curr last ->
                case ( curr, last ) of
                    ( Even cf ct cc, Even lf lt lc ) ->
                        if cf == lt + 2 && cc == lc then
                            Just <| Even lf ct cc

                        else
                            Nothing

                    ( Odd cf ct cc, Odd lf lt lc ) ->
                        if cf == lt + 2 && cc == lc then
                            Just <| Odd lf ct cc

                        else
                            Nothing

                    ( Even cf ct cc, Odd lf lt lc ) ->
                        if lf == lt && cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing

                    ( Odd cf ct cc, Even lf lt lc ) ->
                        if lf == lt && cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing

                    ( Even cf ct cc, All lf lt lc ) ->
                        if cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing

                    ( Odd cf ct cc, All lf lt lc ) ->
                        if cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing

                    ( All cf ct cc, All lf lt lc ) ->
                        if cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing

                    ( All cf ct cc, Even lf lt lc ) ->
                        if lf == lt && cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing

                    ( All cf ct cc, Odd lf lt lc ) ->
                        if lf == lt && cf == lt + 1 && cc == lc then
                            Just <| All lf ct cc

                        else
                            Nothing
            )
        |> categorize
        |> Node
        |> (if splitAt100 then
                splitAt 0x0100

            else
                identity
           )
        |> split


categorize : List (EvenOddRange a) -> { all : List ( Int, Int, a ), even : List ( Int, Int, a ), odd : List ( Int, Int, a ) }
categorize =
    List.foldr
        (\e n ->
            case e of
                All f t c ->
                    { n | all = ( f, t, c ) :: n.all }

                Even f t c ->
                    if f == t then
                        { n | all = ( f, t, c ) :: n.all }

                    else
                        { n | even = ( f, t, c ) :: n.even }

                Odd f t c ->
                    if f == t then
                        { n | all = ( f, t, c ) :: n.all }

                    else
                        { n | odd = ( f, t, c ) :: n.odd }
        )
        { all = [], even = [], odd = [] }


split : Tree a -> Tree a
split t =
    case t of
        Split at l r ->
            Split at (split l) (split r)

        Node n ->
            let
                len =
                    List.length n.all
            in
            if len <= 12 then
                t

            else
                case List.Extra.getAt (len // 2) n.all of
                    Just ( f, _, _ ) ->
                        split <| splitAt (f - 1) <| Node n

                    Nothing ->
                        t
