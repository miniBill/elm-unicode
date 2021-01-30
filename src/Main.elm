port module Main exposing (main)

import Elm.CodeGen as Elm exposing (Declaration(..), and, apply, applyBinOp, boolAnn, charAnn, construct, emptyDocComment, equals, fqVal, fun, funAnn, funDecl, funExpose, gte, hex, ifExpr, int, letExpr, letVal, lt, lte, normalModule, or, parens, varPattern)
import Elm.Pretty
import Hex
import Result
import Unicode exposing (Category(..), categoryFromCode)


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
            normalModule [ "Internal" ]
                [ funExpose "isUpper"
                , funExpose "isLower"
                , funExpose "isAlpha"
                , funExpose "isDigit"
                , funExpose "isAlphanum"
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
            ]

        file =
            Elm.file
                moduleDef
                imports
                declarations
                Nothing
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
                        (\( elf, elt ) ( lacc, hacc ) ->
                            if elt < at then
                                ( ( elf, elt ) :: lacc, hacc )

                            else if elf >= at then
                                ( lacc, ( elf, elt ) :: hacc )

                            else
                                ( ( elf, at - 1 ) :: lacc, ( at, elt ) :: hacc )
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


toEvenOddRange : { a | from : Int, to : Int } -> EvenOddRange
toEvenOddRange { from, to } =
    if from == to then
        if modBy 2 from == 0 then
            Even from from

        else
            Odd from from

    else
        All from to


type EvenOddRange
    = All Int Int
    | Odd Int Int
    | Even Int Int


type Tree
    = Node
        { all : List ( Int, Int )
        , even : List ( Int, Int )
        , odd : List ( Int, Int )
        }
    | Split Int Tree Tree


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
                (categoryFromCode generalCategory)

        _ ->
            Nothing


categoriesToDeclaration :
    { name : String
    , categories : List Category
    , comment : String
    }
    -> List { category : Category, from : Int, to : Int }
    -> Declaration
categoriesToDeclaration { name, categories, comment } ranges =
    let
        categorize =
            List.foldr
                (\e n ->
                    case e of
                        All f t ->
                            { n | all = ( f, t ) :: n.all }

                        Even f t ->
                            if f == t then
                                { n | all = ( f, t ) :: n.all }

                            else
                                { n | even = ( f, t ) :: n.even }

                        Odd f t ->
                            if f == t then
                                { n | all = ( f, t ) :: n.all }

                            else
                                { n | odd = ( f, t ) :: n.odd }
                )
                { all = [], even = [], odd = [] }

        rangeToExpression ( from, to ) =
            parens <|
                if from == to then
                    applyBinOp (fun "code") equals (hex from)

                else
                    applyBinOp
                        (applyBinOp (fun "code") gte (hex from))
                        and
                        (applyBinOp (fun "code") lte (hex to))

        joinOr lst =
            case lst of
                [] ->
                    construct "False" []

                fst :: tail ->
                    List.foldl (\e a -> applyBinOp a or e) fst tail

        rangesToExpression t =
            case t of
                Node { all, even, odd } ->
                    let
                        modIs n =
                            applyBinOp
                                (apply [ fun "modBy", int 2, fun "code" ])
                                equals
                                (int n)

                        modded =
                            if List.isEmpty even then
                                if List.isEmpty odd then
                                    []

                                else
                                    [ parens <|
                                        applyBinOp
                                            (modIs 1)
                                            and
                                            (joinOr (List.map rangeToExpression odd))
                                    ]

                            else if List.isEmpty odd then
                                [ parens <|
                                    applyBinOp
                                        (modIs 0)
                                        and
                                        (joinOr (List.map rangeToExpression even))
                                ]

                            else
                                [ parens <|
                                    ifExpr (modIs 0)
                                        (joinOr (List.map rangeToExpression even))
                                        (joinOr (List.map rangeToExpression odd))
                                ]
                    in
                    joinOr
                        (List.map rangeToExpression all ++ modded)

                Split at l r ->
                    ifExpr (applyBinOp (fun "code") lt (hex at))
                        (rangesToExpression l)
                        (rangesToExpression r)

        checks =
            ranges
                |> List.filter (\{ category } -> List.member category categories)
                |> List.map toEvenOddRange
                |> foldWithLast
                    (\curr last ->
                        case ( curr, last ) of
                            ( Even cf ct, Even lf lt ) ->
                                if cf == lt + 2 then
                                    Just <| Even lf ct

                                else
                                    Nothing

                            ( Odd cf ct, Odd lf lt ) ->
                                if cf == lt + 2 then
                                    Just <| Odd lf ct

                                else
                                    Nothing

                            ( Even cf ct, Odd lf lt ) ->
                                if lf == lt && cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing

                            ( Odd cf ct, Even lf lt ) ->
                                if lf == lt && cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing

                            ( Even cf ct, All lf lt ) ->
                                if cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing

                            ( Odd cf ct, All lf lt ) ->
                                if cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing

                            ( All cf ct, All lf lt ) ->
                                if cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing

                            ( All cf ct, Even lf lt ) ->
                                if lf == lt && cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing

                            ( All cf ct, Odd lf lt ) ->
                                if lf == lt && cf == lt + 1 then
                                    Just <| All lf ct

                                else
                                    Nothing
                    )
                |> categorize
                |> Node
                |> split
                |> rangesToExpression

        doc =
            emptyDocComment
                |> Elm.markdown comment

        typeAnnotation =
            funAnn charAnn boolAnn

        code =
            letExpr
                [ letVal "code" <|
                    apply
                        [ fqVal [ "Char" ] "toCode"
                        , fun "c"
                        ]
                ]
                checks
    in
    funDecl (Just doc)
        (Just typeAnnotation)
        name
        [ varPattern "c" ]
        code


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
                    Just ( f, _ ) ->
                        split <| splitAt (f - 1) <| Node n

                    Nothing ->
                        t
