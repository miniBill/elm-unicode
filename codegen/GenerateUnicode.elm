module GenerateUnicode exposing (main)

import Categories exposing (Category(..), categoryFromString)
import Common
import Elm exposing (Declaration, Expression, File)
import Elm.Annotation as Type
import Elm.Let
import Elm.Op
import Gen.Char
import GenerateCategories
import Hex
import List.Extra
import Result


main : Program Common.Flags Common.Model Common.Msg
main =
    Common.program flagsToFile


foldWithLast : (a -> a -> Maybe a) -> List a -> List a
foldWithLast step list =
    list
        |> List.foldl
            (\e ( last, acc ) ->
                case last of
                    Nothing ->
                        ( Just e, acc )

                    Just le ->
                        case step e le of
                            Just c ->
                                ( Just c, acc )

                            Nothing ->
                                ( Just e, le :: acc )
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


flagsToFile : Common.Flags -> File
flagsToFile csv =
    let
        toRange e =
            { from = e.codeValue
            , to = e.codeValue
            , category = e.category
            }

        ranges =
            csv
                |> String.split "\n"
                |> List.filterMap parseLine
                |> List.map toRange
                |> foldWithLast
                    (\e last ->
                        if last.category == e.category then
                            Just { last | to = e.to }

                        else
                            Nothing
                    )

        declarations =
            [ categoriesToDeclaration
                { name = "isUpper"
                , categories = [ LetterUppercase ]
                , comment = "Detect upper case characters (Unicode category Lu)"
                , group = "Letters"
                , simple =
                    ( \c -> Char.toUpper c == c && Char.toLower /= c
                    , \c ->
                        Elm.Op.and
                            (Elm.Op.equal (Gen.Char.toUpper c) c)
                            (Elm.Op.notEqual (Gen.Char.toLower c) c)
                    )
                }
                ranges
            , categoriesToDeclaration
                { name = "isLower"
                , categories = [ LetterLowercase ]
                , comment = "Detect lower case characters (Unicode category Ll)"
                , group = "Letters"
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
            , categoriesToDeclaration
                { name = "isDigit"
                , categories = [ NumberDecimalDigit, NumberLetter, NumberOther ]
                , comment = "Detect digits (Unicode categories Nd, Nl, No)"
                , group = "Digits"
                }
                ranges
            ]
                ++ List.take 1 GenerateCategories.declarations
                ++ (getCategoryDeclaration ranges
                        :: List.drop 1 GenerateCategories.declarations
                   )
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
        declarations


splitAt : Int -> Tree -> Tree
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


toEvenOddRange : { a | from : Int, to : Int, category : Category } -> EvenOddRange
toEvenOddRange { from, to, category } =
    if from == to then
        if modBy 2 from == 0 then
            Even from from category

        else
            Odd from from category

    else
        All from to category


type EvenOddRange
    = All Int Int Category
    | Odd Int Int Category
    | Even Int Int Category


type Tree
    = Split Int Tree Tree
    | Node
        { all : List ( Int, Int, Category )
        , even : List ( Int, Int, Category )
        , odd : List ( Int, Int, Category )
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
                |> rangesToTree
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
    let
        name : String
        name =
            case category of
                LetterUppercase ->
                    "LetterUppercase"

                LetterLowercase ->
                    "LetterLowercase"

                LetterTitlecase ->
                    "LetterTitlecase"

                MarkNonSpacing ->
                    "MarkNonSpacing"

                MarkSpacingCombining ->
                    "MarkSpacingCombining"

                MarkEnclosing ->
                    "MarkEnclosing"

                NumberDecimalDigit ->
                    "NumberDecimalDigit"

                NumberLetter ->
                    "NumberLetter"

                NumberOther ->
                    "NumberOther"

                SeparatorSpace ->
                    "SeparatorSpace"

                SeparatorLine ->
                    "SeparatorLine"

                SeparatorParagraph ->
                    "SeparatorParagraph"

                OtherControl ->
                    "OtherControl"

                OtherFormat ->
                    "OtherFormat"

                OtherSurrogate ->
                    "OtherSurrogate"

                OtherPrivateUse ->
                    "OtherPrivateUse"

                OtherNotAssigned ->
                    "OtherNotAssigned"

                LetterModifier ->
                    "LetterModifier"

                LetterOther ->
                    "LetterOther"

                PunctuationConnector ->
                    "PunctuationConnector"

                PunctuationDash ->
                    "PunctuationDash"

                PunctuationOpen ->
                    "PunctuationOpen"

                PunctuationClose ->
                    "PunctuationClose"

                PunctuationInitialQuote ->
                    "PunctuationInitialQuote"

                PunctuationFinalQuote ->
                    "PunctuationFinalQuote"

                PunctuationOther ->
                    "PunctuationOther"

                SymbolMath ->
                    "SymbolMath"

                SymbolCurrency ->
                    "SymbolCurrency"

                SymbolModifier ->
                    "SymbolModifier"

                SymbolOther ->
                    "SymbolOther"
    in
    Elm.value
        { importFrom = []
        , name = name
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
        treeToExpression : Expression -> Tree -> Expression
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
                -- Squash all the categories into one
                |> List.map (\r -> { r | category = SymbolOther })
                |> rangesToTree
                |> treeToExpression code
    in
    Elm.fn ( "c", Just <| Type.named [] "Char" ) (letCode checks)
        |> Elm.withType (Type.function [ Type.named [] "Char" ] Type.bool)
        |> Elm.declaration name
        |> Elm.withDocumentation comment
        |> Elm.exposeWith { exposeConstructor = False, group = Just group }


rangesToTree : List { a | from : Int, to : Int, category : Category } -> Tree
rangesToTree ranges =
    ranges
        |> List.map toEvenOddRange
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


categorize : List EvenOddRange -> { all : List ( Int, Int, Category ), even : List ( Int, Int, Category ), odd : List ( Int, Int, Category ) }
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


split : Tree -> Tree
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
