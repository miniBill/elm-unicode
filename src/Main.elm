port module Main exposing (main)

import Elm.CodeGen as Elm exposing (Declaration(..), and, apply, applyBinOp, boolAnn, charAnn, construct, emptyDocComment, equals, fqVal, fun, funAnn, funDecl, funExpose, gte, hex, ifExpr, int, letExpr, letVal, lt, lte, normalModule, or, parens, varPattern)
import Elm.Pretty
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
            normalModule [ "Internal" ] [ funExpose "isLower" ]

        imports =
            []

        declarations =
            [ isLowerDeclaration ranges ]

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

        Node ( a, e, o ) ->
            let
                splot =
                    List.foldr
                        (\( elf, elt ) ( lacc, hacc ) ->
                            if elf < at then
                                ( ( elf, elt ) :: lacc, hacc )

                            else
                                ( lacc, ( elf, elt ) :: hacc )
                        )
                        ( [], [] )

                ( la, ha ) =
                    splot a

                ( le, he ) =
                    splot e

                ( lo, ho ) =
                    splot o
            in
            if List.isEmpty la && List.isEmpty le && List.isEmpty lo then
                tree

            else if List.isEmpty ha && List.isEmpty he && List.isEmpty ho then
                tree

            else
                Split at
                    (Node ( la, le, lo ))
                    (Node ( ha, he, ho ))


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
    = Node ( List ( Int, Int ), List ( Int, Int ), List ( Int, Int ) )
    | Split Int Tree Tree


parseLine : String -> Maybe { codeValue : Int, characterName : String, category : Category }
parseLine line =
    case String.split ";" line of
        codeValueHex :: characterName :: generalCategory :: _ ->
            Maybe.map
                (\category ->
                    { codeValue = fromHex codeValueHex
                    , characterName = characterName
                    , category = category
                    }
                )
                (categoryFromCode generalCategory)

        _ ->
            Nothing


toHex : Int -> String
toHex i =
    if i == 0 then
        "0"

    else
        let
            toHexChar k =
                if k < 0x0A then
                    String.fromInt k

                else
                    String.fromChar (Char.fromCode (Char.toCode 'A' + k - 0x0A))

            go n acc =
                if n == 0 then
                    acc

                else
                    go (n // 16) (toHexChar (modBy 16 n) ++ acc)
        in
        "0x" ++ go i ""


fromHex : String -> Int
fromHex =
    let
        fromHexChar c =
            case String.toInt (String.fromChar c) of
                Just i ->
                    i

                Nothing ->
                    -- We can just assume the input is well formed tbh
                    Char.toCode (Char.toUpper c) - Char.toCode 'A' + 0x0A
    in
    String.toList
        >> List.map fromHexChar
        >> List.foldl (\e a -> a * 16 + e) 0


isLowerDeclaration :
    List { category : Category, from : Int, to : Int }
    -> Declaration
isLowerDeclaration ranges =
    let
        categorize =
            List.foldr
                (\e ( aa, ae, ao ) ->
                    case e of
                        All f t ->
                            ( ( f, t ) :: aa, ae, ao )

                        Even f t ->
                            if f == t then
                                ( ( f, t ) :: aa, ae, ao )

                            else
                                ( aa, ( f, t ) :: ae, ao )

                        Odd f t ->
                            if f == t then
                                ( ( f, t ) :: aa, ae, ao )

                            else
                                ( aa, ae, ( f, t ) :: ao )
                )
                ( [], [], [] )

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
                Node ( alls, evens, odds ) ->
                    let
                        modIs n =
                            applyBinOp
                                (apply [ fun "modBy", int 2, fun "code" ])
                                equals
                                (int n)

                        modded =
                            if List.isEmpty evens then
                                if List.isEmpty odds then
                                    []

                                else
                                    [ parens <|
                                        applyBinOp
                                            (modIs 1)
                                            and
                                            (joinOr (List.map rangeToExpression odds))
                                    ]

                            else if List.isEmpty odds then
                                [ parens <|
                                    applyBinOp
                                        (modIs 0)
                                        and
                                        (joinOr (List.map rangeToExpression evens))
                                ]

                            else
                                [ parens <|
                                    ifExpr (modIs 0)
                                        (joinOr (List.map rangeToExpression evens))
                                        (joinOr (List.map rangeToExpression odds))
                                ]
                    in
                    joinOr
                        (List.map rangeToExpression alls ++ modded)

                Split at l r ->
                    ifExpr (applyBinOp (fun "code") lt (hex at))
                        (rangesToExpression l)
                        (rangesToExpression r)

        lowers =
            ranges
                |> List.filter (\{ category } -> category == LetterLowercase)
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

                            _ ->
                                Nothing
                    )
                |> categorize
                |> Node
                |> splitAt 0x0100
                |> splitAt 0x1D2C
                |> splitAt 0x0001D6A6
                |> splitAt 0x00010000
                |> splitAt 0x0240
                |> splitAt 0x2110
                |> rangesToExpression

        doc =
            emptyDocComment
                |> Elm.markdown "Detect lower case characters"

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
                lowers
    in
    funDecl (Just doc)
        (Just typeAnnotation)
        "isLower"
        [ varPattern "c" ]
        code
