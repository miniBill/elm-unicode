module GenerateUnicode exposing (main)

import Categories exposing (Category(..), categoryFromString)
import Elm exposing (Declaration, Expression, File)
import Elm.Annotation as Type
import Elm.Let
import Elm.Op
import Gen.Char
import Gen.CodeGen.Generate as Generate
import GenerateCategories
import Hex
import List.Extra
import Result


main : Program String () ()
main =
    Generate.withFeedback <|
        \input ->
            let
                ( file, logs ) =
                    flagsToFile input
            in
            Ok
                { files =
                    [ file
                    ]
                , info = List.reverse logs
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
                        a

                    Just le ->
                        le :: a
           )
        |> List.reverse


flagsToFile : String -> ( File, List String )
flagsToFile csv =
    let
        toRange : { a | codeValue : Int, category : Category } -> { from : Int, to : Int, category : Category }
        toRange e =
            { from = e.codeValue
            , to = e.codeValue
            , category = e.category
            }

        raw : List { codeValue : Int, characterName : String, category : Category }
        raw =
            csv
                |> String.split "\n"
                |> List.filterMap parseLine

        ranges : List { from : Int, to : Int, category : Category }
        ranges =
            raw
                |> List.map toRange
                |> foldWithLast
                    (\e last ->
                        if last.to + 1 == e.from && last.category == e.category then
                            Just { last | to = e.to }

                        else
                            Nothing
                    )

        ( declarations, logs ) =
            [ categoriesToDeclarationWithSimpleCheck
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
                raw
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
                raw
            , categoriesToDeclarationWithSimpleCheck
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
                , simpleCheck = \c -> Char.toLower c /= c || Char.toUpper c /= c
                , simpleCheckExpression =
                    \c ->
                        Elm.Op.or
                            (Elm.Op.notEqual (Gen.Char.call_.toLower c) c)
                            (Elm.Op.notEqual (Gen.Char.call_.toUpper c) c)
                }
                raw
            , categoriesToDeclarationWithSimpleCheck
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
                , simpleCheck = \c -> Char.toLower c /= c || Char.toUpper c /= c
                , simpleCheckExpression =
                    \c ->
                        Elm.Op.or
                            (Elm.Op.notEqual (Gen.Char.call_.toLower c) c)
                            (Elm.Op.notEqual (Gen.Char.call_.toUpper c) c)
                }
                raw
            , ( categoriesToDeclaration
                    { name = "isDigit"
                    , categories = [ NumberDecimalDigit, NumberLetter, NumberOther ]
                    , comment = "Detect digits (Unicode categories Nd, Nl, No)"
                    , group = "Digits"
                    }
                    ranges
              , []
              )
            ]
                |> List.unzip
    in
    ( Elm.fileWith [ "Unicode" ]
        { docs =
            \groups ->
                "Unicode aware functions for working with characters."
                    :: List.map Elm.docs
                        (List.sortBy
                            (\{ group } ->
                                case group of
                                    Just "Letters" ->
                                        0

                                    Just "Digits" ->
                                        1

                                    Just "Categories" ->
                                        2

                                    _ ->
                                        3
                            )
                            groups
                        )
        , aliases = []
        }
        (declarations
            ++ List.take 1 GenerateCategories.declarations
            ++ (getCategoryDeclaration ranges
                    :: List.drop 1 GenerateCategories.declarations
               )
        )
    , List.concat logs
    )


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
                splot =
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
                    splot n.all

                ( le, he ) =
                    splot n.even

                ( lo, ho ) =
                    splot n.odd
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


parseLine : String -> Maybe { codeValue : Int, characterName : String, category : Category }
parseLine line =
    case String.split ";" line of
        codeValueHex :: characterName :: generalCategory :: _ ->
            Maybe.map2
                (\codeValue category ->
                    { codeValue = codeValue
                    , characterName = characterName
                    , category = category
                    }
                )
                (Result.toMaybe <| Hex.fromString <| String.toLower codeValueHex)
                (categoryFromString generalCategory)

        _ ->
            Nothing


rangeToCondition : Expression -> ( Int, Int, a ) -> Expression
rangeToCondition code ( from, to, _ ) =
    Elm.parens <|
        if from == to then
            Elm.Op.equal code (Elm.hex from)

        else
            Elm.Op.and
                (Elm.Op.gte code <| Elm.hex from)
                (Elm.Op.lte code <| Elm.hex to)


joinOr : List Expression -> Expression
joinOr lst =
    case lst of
        [] ->
            Elm.bool False

        fst :: tail ->
            List.foldl (\e a -> Elm.Op.or a e) fst tail


modIs : Expression -> Int -> Expression
modIs code n =
    Elm.parens <|
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


letCode : (Expression -> Expression) -> Expression -> Expression
letCode inner code =
    Elm.Let.letIn inner
        |> Elm.Let.value "code"
            (Elm.apply
                (Elm.value
                    { importFrom = [ "Char" ]
                    , name = "toCode"
                    , annotation = Just (Type.function [ Type.named [] "Char" ] Type.int)
                    }
                )
                [ code ]
            )
        |> Elm.Let.toExpression


letCodeWithSimpleCheck : { simpleCheckExpression : Expression -> Expression, body : { code : Expression, simple : Expression } -> Expression } -> Expression -> Expression
letCodeWithSimpleCheck { body, simpleCheckExpression } code =
    Elm.Let.letIn (\codeVar simpleVar -> body { code = codeVar, simple = simpleVar })
        |> Elm.Let.value "code"
            (Elm.apply
                (Elm.value
                    { importFrom = [ "Char" ]
                    , name = "toCode"
                    , annotation = Just (Type.function [ Type.named [] "Char" ] Type.int)
                    }
                )
                [ code ]
            )
        |> Elm.Let.value "simple"
            (simpleCheckExpression
                (Elm.value
                    { importFrom = []
                    , name = "c"
                    , annotation = Just Type.int
                    }
                )
            )
        |> Elm.Let.toExpression


getCategoryDeclaration :
    List { category : Category, from : Int, to : Int }
    -> Declaration
getCategoryDeclaration ranges =
    let
        annotation =
            Type.function [ Type.char ] (Type.maybe <| Type.named [] "Category")

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

        treeToExpression code t =
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
                                                [ Elm.parens <|
                                                    Elm.Op.and
                                                        (modIs code 1)
                                                        (Elm.parens <| joinOr (List.map (rangeToCondition code) odd))
                                                ]

                                        else if List.isEmpty odd then
                                            [ Elm.parens <|
                                                Elm.Op.and
                                                    (modIs code 0)
                                                    (Elm.parens <| joinOr (List.map (rangeToCondition code) even))
                                            ]

                                        else
                                            [ Elm.ifThen (modIs code 0)
                                                (joinOr (List.map (rangeToCondition code) even))
                                                (joinOr (List.map (rangeToCondition code) odd))
                                            ]
                                in
                                ( Elm.apply (Elm.value { importFrom = [], name = "Just", annotation = Nothing }) [ categoryToConstructor category ]
                                , joinOr
                                    (List.map (rangeToCondition code) all ++ modded)
                                )
                            )
                        |> List.foldr (\( category, condition ) acc -> Elm.ifThen condition category acc) (Elm.value { importFrom = [], name = "Nothing", annotation = Nothing })

                Split at l r ->
                    Elm.ifThen (Elm.Op.lt code (Elm.hex at))
                        (treeToExpression code l)
                        (treeToExpression code r)

        checks code =
            ranges
                |> rangesToTree .category
                |> split
                |> treeToExpression code
    in
    Elm.fn ( "c", Just <| Type.named [] "Char" ) (letCode checks)
        |> Elm.withType annotation
        |> Elm.declaration "getCategory"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Categories" }
        |> Elm.withDocumentation doc


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
        treeToExpression : Expression -> Tree a -> Expression
        treeToExpression code t =
            case t of
                Node { all, even, odd } ->
                    let
                        modded =
                            if List.isEmpty even then
                                if List.isEmpty odd then
                                    []

                                else
                                    [ Elm.parens <|
                                        Elm.Op.and
                                            (modIs code 1)
                                            (Elm.parens <| joinOr (List.map (rangeToCondition code) odd))
                                    ]

                            else if List.isEmpty odd then
                                [ Elm.parens <|
                                    Elm.Op.and
                                        (modIs code 0)
                                        (Elm.parens <| joinOr (List.map (rangeToCondition code) even))
                                ]

                            else
                                [ Elm.parens <|
                                    Elm.ifThen (modIs code 0)
                                        (joinOr (List.map (rangeToCondition code) even))
                                        (joinOr (List.map (rangeToCondition code) odd))
                                ]
                    in
                    joinOr
                        (List.map (rangeToCondition code) all ++ modded)

                Split at l r ->
                    Elm.ifThen (Elm.Op.lt code (Elm.hex at))
                        (treeToExpression code l)
                        (treeToExpression code r)

        checks code =
            ranges
                |> List.filter (\{ category } -> List.member category categories)
                |> rangesToTree (\_ -> ())
                |> treeToExpression code
    in
    Elm.fn ( "c", Just <| Type.named [] "Char" ) (letCode checks)
        |> Elm.withType (Type.function [ Type.named [] "Char" ] Type.bool)
        |> Elm.declaration name
        |> Elm.withDocumentation comment
        |> Elm.exposeWith { exposeConstructor = False, group = Just group }


categoriesToDeclarationWithSimpleCheck :
    { name : String
    , categories : List Category
    , comment : String
    , group : String
    , simpleCheck : Char -> Bool
    , simpleCheckExpression : Expression -> Expression
    }
    -> List { codeValue : Int, characterName : String, category : Category }
    -> ( Declaration, List String )
categoriesToDeclarationWithSimpleCheck { name, categories, comment, group, simpleCheck, simpleCheckExpression } raw =
    let
        toBody : { code : Expression, simple : Expression } -> Expression
        toBody { code, simple } =
            let
                rangeToConditionWithSimple : ( Int, Int, CorrectChecks ) -> Expression
                rangeToConditionWithSimple (( _, _, payload ) as range) =
                    case payload of
                        Either ->
                            Elm.parens <|
                                Elm.Op.and simple (rangeToCondition code range)

                        CategoryOnly ->
                            rangeToCondition code range

                        SimpleOnly ->
                            Elm.parens <|
                                Elm.Op.and simple (rangeToCondition code range)

                go : Tree CorrectChecks -> Expression
                go t =
                    case t of
                        Node { all, even, odd } ->
                            let
                                modded =
                                    if List.isEmpty even then
                                        if List.isEmpty odd then
                                            []

                                        else
                                            [ Elm.parens <|
                                                Elm.Op.and
                                                    (modIs code 1)
                                                    (Elm.parens <| joinOr (List.map rangeToConditionWithSimple odd))
                                            ]

                                    else if List.isEmpty odd then
                                        [ Elm.parens <|
                                            Elm.Op.and
                                                (modIs code 0)
                                                (Elm.parens <| joinOr (List.map rangeToConditionWithSimple even))
                                        ]

                                    else
                                        [ Elm.parens <|
                                            Elm.ifThen (modIs code 0)
                                                (joinOr (List.map rangeToConditionWithSimple even))
                                                (joinOr (List.map rangeToConditionWithSimple odd))
                                        ]
                            in
                            joinOr
                                (List.map rangeToConditionWithSimple all ++ modded)

                        Split at l r ->
                            Elm.ifThen (Elm.Op.lt code (Elm.hex at))
                                (go l)
                                (go r)
            in
            go tree

        rearrangedRanges : List { from : Int, to : Int, hasAny : Bool, correctChecks : CorrectChecks }
        rearrangedRanges =
            raw
                |> List.filterMap
                    (\{ codeValue, category } ->
                        let
                            belongs =
                                List.member category categories

                            isSimple =
                                simpleCheck (Char.fromCode codeValue) == belongs

                            maybeChecks =
                                if belongs then
                                    if isSimple then
                                        Just Either

                                    else
                                        Just CategoryOnly

                                else if isSimple then
                                    Just SimpleOnly

                                else
                                    Nothing
                        in
                        Maybe.map
                            (\c ->
                                { from = codeValue
                                , to = codeValue
                                , hasAny = belongs
                                , correctChecks = c
                                }
                            )
                            maybeChecks
                    )
                |> foldWithLast
                    (\e last ->
                        if not last.hasAny then
                            Just e

                        else if e.from /= last.to + 1 then
                            Nothing

                        else
                            let
                                correctChecks : Maybe CorrectChecks
                                correctChecks =
                                    case ( last.correctChecks, e.correctChecks ) of
                                        ( Either, _ ) ->
                                            Just e.correctChecks

                                        ( _, Either ) ->
                                            Just last.correctChecks

                                        _ ->
                                            if last.correctChecks == e.correctChecks then
                                                Just last.correctChecks

                                            else
                                                Nothing
                            in
                            Maybe.map
                                (\cc ->
                                    { from = last.from
                                    , to = e.to
                                    , hasAny = True
                                    , correctChecks = cc
                                    }
                                )
                                correctChecks
                    )
                -- The last element might have `hasAny = False`, if so filter it out.
                |> List.filter .hasAny

        tree : Tree CorrectChecks
        tree =
            rangesToTree .correctChecks rearrangedRanges
    in
    ( Elm.fn ( "c", Just <| Type.named [] "Char" )
        (letCodeWithSimpleCheck
            { simpleCheckExpression = simpleCheckExpression
            , body = toBody
            }
        )
        |> Elm.withType (Type.function [ Type.named [] "Char" ] Type.bool)
        |> Elm.declaration name
        |> Elm.withDocumentation comment
        |> Elm.exposeWith { exposeConstructor = False, group = Just group }
    , rearrangedRangesToStrings name rearrangedRanges
    )


rearrangedRangesToStrings : String -> List { from : Int, to : Int, hasAny : Bool, correctChecks : CorrectChecks } -> List String
rearrangedRangesToStrings name ranges =
    let
        fromInt : Int -> String
        fromInt arg =
            String.padLeft 4 '0' (Hex.toString arg)
    in
    ranges
        |> List.map (\{ from, to, correctChecks } -> name ++ " " ++ fromInt from ++ "-" ++ fromInt to ++ ": " ++ correctChecksToString correctChecks)


correctChecksToString : CorrectChecks -> String
correctChecksToString correctChecks =
    case correctChecks of
        Either ->
            "Either"

        SimpleOnly ->
            "SimpleOnly"

        CategoryOnly ->
            "CategoryOnly"


type CorrectChecks
    = Either
    | SimpleOnly
    | CategoryOnly


rangesToTree : ({ a | from : Int, to : Int } -> payload) -> List { a | from : Int, to : Int } -> Tree payload
rangesToTree getPayload ranges =
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
        |> splitAt 0x0100
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
