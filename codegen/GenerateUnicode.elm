module GenerateUnicode exposing (main)

import Categories exposing (Category(..), categoryFromString)
import Dict exposing (Dict)
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
    Generate.fromText <|
        \input ->
            [ flagsToFile input
            ]


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


flagsToFile : String -> File
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

        rawDict : Dict Int Category
        rawDict =
            raw
                |> List.map (\{ codeValue, category } -> ( codeValue, category ))
                |> Dict.fromList

        ranges : List { from : Int, to : Int, category : Category }
        ranges =
            raw
                |> List.map toRange
                |> foldWithLast
                    (\e last ->
                        if
                            (last.to + 1 == e.from || e.category == OtherPrivateUse)
                                && (last.category == e.category)
                        then
                            Just { last | to = e.to }

                        else
                            Nothing
                    )

        declarations : List Declaration
        declarations =
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
                rawDict
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
                rawDict
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
                { name = "isDigit"
                , categories = [ NumberDecimalDigit, NumberLetter, NumberOther ]
                , comment = "Detect digits (Unicode categories Nd, Nl, No)"
                , group = "Digits"
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
    in
    Elm.fileWith [ "Unicode" ]
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


rangeToCondition : { equals : Int -> Expression, inRange : Int -> Int -> Expression } -> ( Int, Int, a ) -> Expression
rangeToCondition { equals, inRange } ( from, to, _ ) =
    if from == to then
        equals from

    else
        inRange from to


joinOr : List Expression -> Expression
joinOr lst =
    case lst of
        [] ->
            Elm.bool False

        fst :: tail ->
            List.foldl (\e a -> Elm.Op.or a e) fst tail


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
    ({ code : Expression
     , lessThan : Int -> Expression
     , equals : Int -> Expression
     , inRange : Int -> Int -> Expression
     }
     -> Expression
    )
    -> Expression
    -> Expression
letCode inner code =
    Elm.Let.letIn
        (\codeVar ->
            Elm.Let.letIn
                (\lessThan equals inRange ->
                    inner
                        { code = codeVar
                        , lessThan =
                            \upperBound -> Elm.apply lessThan [ Elm.hex upperBound ]
                        , equals =
                            \i ->
                                Elm.apply equals [ Elm.hex i ]
                        , inRange =
                            \from to ->
                                if from == 0 then
                                    Elm.Op.lte codeVar (Elm.hex to)

                                else
                                    Elm.apply inRange [ Elm.hex from, Elm.hex to ]
                        }
                )
                |> Elm.Let.value "l" (lessThanDef codeVar)
                |> Elm.Let.value "e" (equalsDef codeVar)
                |> Elm.Let.value "r" (inRangeDef codeVar)
                |> Elm.Let.toExpression
        )
        |> Elm.Let.value "code" (Gen.Char.call_.toCode code)
        |> Elm.Let.toExpression


letCodeWithSimpleCheck :
    { simpleCheckExpression : Expression -> Expression
    , body :
        { code : Expression
        , simple : Expression
        , lessThan : Int -> Expression
        , equals : Int -> Expression
        , inRange : Int -> Int -> Expression
        }
        -> Expression
    }
    -> Expression
    -> Expression
letCodeWithSimpleCheck { body, simpleCheckExpression } code =
    Elm.Let.letIn
        (\codeVar simpleVar ->
            Elm.Let.letIn
                (\lessThan equals inRange ->
                    body
                        { code = codeVar
                        , simple = simpleVar
                        , lessThan =
                            \upperBound -> Elm.apply lessThan [ Elm.hex upperBound ]
                        , equals =
                            \i ->
                                Elm.apply equals [ Elm.hex i ]
                        , inRange =
                            \from to ->
                                if from == 0 then
                                    Elm.Op.lte codeVar (Elm.hex to)

                                else
                                    Elm.apply inRange [ Elm.hex from, Elm.hex to ]
                        }
                )
                |> Elm.Let.value "l" (lessThanDef codeVar)
                |> Elm.Let.value "e" (equalsDef codeVar)
                |> Elm.Let.value "r" (inRangeDef codeVar)
                |> Elm.Let.toExpression
        )
        |> Elm.Let.value "code"
            (Gen.Char.call_.toCode code)
        |> Elm.Let.value "simple"
            (simpleCheckExpression code)
        |> Elm.Let.toExpression


lessThanDef : Expression -> Expression
lessThanDef code =
    Elm.fn ( "hex", Just Type.int ) <| \hex -> Elm.Op.lt code hex


equalsDef : Expression -> Expression
equalsDef code =
    Elm.fn ( "hex", Just Type.int ) <| \hex -> Elm.Op.equal hex code


inRangeDef : Expression -> Expression
inRangeDef code =
    Elm.fn2 ( "from", Just Type.int )
        ( "to", Just Type.int )
        (\from to ->
            Elm.Op.and
                (Elm.Op.lte from code)
                (Elm.Op.lte code to)
        )


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

        treeToExpression :
            { code : Expression
            , lessThan : Int -> Expression
            , equals : Int -> Expression
            , inRange : Int -> Int -> Expression
            }
            -> Tree Category
            -> Expression
        treeToExpression { equals, lessThan, inRange, code } =
            let
                rangesToConditions : List ( Int, Int, a ) -> List Expression
                rangesToConditions =
                    List.map
                        (rangeToCondition
                            { equals = equals
                            , inRange = inRange
                            }
                        )

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
                            Elm.ifThen (lessThan at)
                                (go l)
                                (go r)
            in
            go

        checks code =
            ranges
                |> rangesToTree True .category
                |> split
                |> treeToExpression code
    in
    Elm.fn ( "c", Just <| Type.named [] "Char" ) (letCode checks)
        |> Elm.withType annotation
        |> Elm.declaration "getCategory"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Categories" }
        |> Elm.withDocumentation doc


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
            { code : Expression
            , lessThan : Int -> Expression
            , equals : Int -> Expression
            , inRange : Int -> Int -> Expression
            }
            -> Tree a
            -> Expression
        treeToExpression { code, lessThan, equals, inRange } =
            let
                rangesToCondition : List ( Int, Int, a ) -> List Expression
                rangesToCondition =
                    List.map
                        (rangeToCondition
                            { equals = equals
                            , inRange = inRange
                            }
                        )

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
                            Elm.ifThen (lessThan at)
                                (go l)
                                (go r)
            in
            go

        checks inputs =
            ranges
                |> List.filter (\{ category } -> List.member category categories)
                |> rangesToTree True (\_ -> ())
                |> treeToExpression inputs
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
    -> Dict Int Category
    -> Declaration
categoriesToDeclarationWithSimpleCheck { name, categories, comment, group, simpleCheck, simpleCheckExpression } rawDict =
    let
        toBody :
            { code : Expression
            , simple : Expression
            , lessThan : Int -> Expression
            , equals : Int -> Expression
            , inRange : Int -> Int -> Expression
            }
            -> Expression
        toBody { code, simple, lessThan, equals, inRange } =
            let
                rangesToCondition : List ( Int, Int, a ) -> List Expression
                rangesToCondition ranges =
                    List.map
                        (rangeToCondition
                            { equals = equals
                            , inRange = inRange
                            }
                        )
                        ranges

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
                            Elm.ifThen (lessThan at)
                                (go l)
                                (go r)
            in
            Elm.Op.or
                (Elm.Op.and simple (go simpleTree))
                (go categoryTree)

        simpleRanges : List { from : Int, to : Int, hasAny : Bool }
        simpleRanges =
            List.range 0 0x000F0000
                |> List.filterMap
                    (\codeValue ->
                        let
                            belongs =
                                case Dict.get codeValue rawDict of
                                    Nothing ->
                                        False

                                    Just category ->
                                        List.member category categories

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

        categoryRanges : List { from : Int, to : Int, hasAnyException : Bool }
        categoryRanges =
            rawDict
                |> Dict.toList
                |> List.filterMap
                    (\( codeValue, category ) ->
                        let
                            belongs =
                                List.member category categories
                        in
                        if belongs then
                            let
                                isSimple =
                                    simpleCheck (Char.fromCode codeValue)
                            in
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

        categoryTree : Tree ()
        categoryTree =
            rangesToTree True (\_ -> ()) categoryRanges
    in
    Elm.fn ( "c", Just <| Type.named [] "Char" )
        (letCodeWithSimpleCheck
            { simpleCheckExpression = simpleCheckExpression
            , body = toBody
            }
        )
        |> Elm.withType (Type.function [ Type.named [] "Char" ] Type.bool)
        |> Elm.declaration name
        |> Elm.withDocumentation comment
        |> Elm.exposeWith { exposeConstructor = False, group = Just group }


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
