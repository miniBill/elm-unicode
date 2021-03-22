port module Main exposing (main)

import Elm.CodeGen as Elm exposing (Declaration(..), Expression, and, apply, applyBinOp, boolAnn, charAnn, construct, emptyDocComment, emptyFileComment, equals, fqVal, fun, funAnn, funDecl, funExpose, gte, hex, ifExpr, int, letExpr, letVal, lt, lte, maybeAnn, normalModule, openTypeExpose, or, parens, typed, varPattern)
import Elm.Pretty
import Hex
import Result
import Unicode exposing (Category(..), categoryFromString)


port output : String -> Cmd msg


type alias Flags =
    String


type alias Model =
    ()


type alias Msg =
    ()


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update () () =
    ( (), Cmd.none )


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


init : Flags -> ( Model, Cmd Msg )
init csv =
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

        moduleDef =
            normalModule [ "Unicode" ]
                [ funExpose "isUpper"
                , funExpose "isLower"
                , funExpose "isAlpha"
                , funExpose "isDigit"
                , funExpose "isAlphaNum"
                , openTypeExpose "Category"
                , funExpose "getCategory"
                , funExpose "categoryFromString"
                , funExpose "categoryToString"
                , funExpose "categoryToDescription"
                ]

        imports =
            []

        declarations =
            [ categoriesToDeclaration
                { name = "isUpper"
                , categories = [ LetterUppercase ]
                , comment = "Detect upper case characters (UTF-8 category Lu)"
                }
                ranges
            , categoriesToDeclaration
                { name = "isLower"
                , categories = [ LetterLowercase ]
                , comment = "Detect lower case characters (UTF-8 category Lo)"
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
                , comment = "Detect letters (UTF-8 categories Lu, Ll, Lt, Lm, Lo)"
                }
                ranges
            , categoriesToDeclaration
                { name = "isDigit"
                , categories = [ NumberDecimalDigit ]
                , comment = "Detect digits (UTF-8 category Nd)"
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
                    ]
                , comment = "Detect letters or digits (UTF-8 categories Lu, Ll, Lt, Lm, Lo, Nd)"
                }
                ranges
            , getCategoryDeclaration ranges
            ]

        fileComment =
            emptyFileComment
                |> Elm.markdown """UTF-8 aware functions for working with characters.

# Letters
@docs isUpper, isLower, isAlpha, isAlphaNum

# Digits
@docs isDigit

# Categories
@docs Category, getCategory, categoryFromString, categoryToString, categoryToDescription"""

        file =
            Elm.file
                moduleDef
                imports
                declarations
                (Just fileComment)
    in
    Elm.Pretty.pretty 120 file
        |> output
        |> Tuple.pair ()


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


rangeToCondition : ( Int, Int, a ) -> Expression
rangeToCondition ( from, to, _ ) =
    parens <|
        if from == to then
            applyBinOp (fun "code") equals (hex from)

        else
            applyBinOp
                (applyBinOp (fun "code") gte (hex from))
                and
                (applyBinOp (fun "code") lte (hex to))


joinOr : List Expression -> Expression
joinOr lst =
    case lst of
        [] ->
            construct "False" []

        fst :: tail ->
            List.foldl (\e a -> applyBinOp a or e) fst tail


modIs : Int -> Expression
modIs n =
    applyBinOp
        (apply [ fun "modBy", int 2, fun "code" ])
        equals
        (int n)


letCode : Expression -> Expression
letCode inner =
    letExpr
        [ letVal "code" <|
            apply
                [ fqVal [ "Char" ] "toCode"
                , fun "c"
                ]
        ]
        inner


getCategoryDeclaration :
    List { category : Category, from : Int, to : Int }
    -> Declaration
getCategoryDeclaration ranges =
    let
        typeAnnotation =
            funAnn charAnn (maybeAnn <| typed "Category" [])

        doc =
            emptyDocComment
                |> Elm.markdown """Get the UTF category. Warning: this function is very big. You should usually use one of the `isXXX` ones instead."""

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

        treeToExpression t =
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
                                                [ parens <|
                                                    applyBinOp
                                                        (modIs 1)
                                                        and
                                                        (joinOr (List.map rangeToCondition odd))
                                                ]

                                        else if List.isEmpty odd then
                                            [ parens <|
                                                applyBinOp
                                                    (modIs 0)
                                                    and
                                                    (joinOr (List.map rangeToCondition even))
                                            ]

                                        else
                                            [ parens <|
                                                ifExpr (modIs 0)
                                                    (joinOr (List.map rangeToCondition even))
                                                    (joinOr (List.map rangeToCondition odd))
                                            ]
                                in
                                ( apply [ fun "Just", fun <| categoryToConstructor category ]
                                , joinOr
                                    (List.map rangeToCondition all ++ modded)
                                )
                            )
                        |> List.foldr (\( category, condition ) acc -> ifExpr condition category acc) (fun "Nothing")

                Split at l r ->
                    ifExpr (applyBinOp (fun "code") lt (hex at))
                        (treeToExpression l)
                        (treeToExpression r)

        checks =
            ranges
                |> rangesToTree
                |> split
                |> treeToExpression
    in
    funDecl (Just doc)
        (Just typeAnnotation)
        "getCategory"
        [ varPattern "c" ]
        (letCode checks)


categoryToConstructor category =
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


categoriesToDeclaration :
    { name : String
    , categories : List Category
    , comment : String
    }
    -> List { category : Category, from : Int, to : Int }
    -> Declaration
categoriesToDeclaration { name, categories, comment } ranges =
    let
        treeToExpression t =
            case t of
                Node { all, even, odd } ->
                    let
                        modded =
                            if List.isEmpty even then
                                if List.isEmpty odd then
                                    []

                                else
                                    [ parens <|
                                        applyBinOp
                                            (modIs 1)
                                            and
                                            (joinOr (List.map rangeToCondition odd))
                                    ]

                            else if List.isEmpty odd then
                                [ parens <|
                                    applyBinOp
                                        (modIs 0)
                                        and
                                        (joinOr (List.map rangeToCondition even))
                                ]

                            else
                                [ parens <|
                                    ifExpr (modIs 0)
                                        (joinOr (List.map rangeToCondition even))
                                        (joinOr (List.map rangeToCondition odd))
                                ]
                    in
                    joinOr
                        (List.map rangeToCondition all ++ modded)

                Split at l r ->
                    ifExpr (applyBinOp (fun "code") lt (hex at))
                        (treeToExpression l)
                        (treeToExpression r)

        checks =
            ranges
                |> List.filter (\{ category } -> List.member category categories)
                -- Squash all the categories into one
                |> List.map (\r -> { r | category = SymbolOther })
                |> rangesToTree
                |> treeToExpression

        doc =
            emptyDocComment
                |> Elm.markdown comment

        typeAnnotation =
            funAnn charAnn boolAnn
    in
    funDecl (Just doc)
        (Just typeAnnotation)
        name
        [ varPattern "c" ]
        (letCode checks)


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
                splitLen =
                    List.length n.all // 2

                pivot =
                    n.all
                        |> List.drop splitLen
                        |> List.head
            in
            if List.length n.all <= 12 then
                t

            else
                case pivot of
                    Just ( f, _, _ ) ->
                        split <| splitAt (f - 1) <| Node n

                    Nothing ->
                        t
