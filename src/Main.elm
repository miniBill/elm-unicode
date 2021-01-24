port module Main exposing (main)

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

        header =
            [ "module Internal exposing (..)"
            , ""
            , ""
            ]

        isLower =
            let
                categorize =
                    List.foldr
                        (\e ( aa, ae, ao ) ->
                            case e of
                                All f t ->
                                    ( ( f, t ) :: aa, ae, ao )

                                Even f t ->
                                    ( aa, ( f, t ) :: ae, ao )

                                Odd f t ->
                                    ( aa, ae, ( f, t ) :: ao )
                        )
                        ( [], [], [] )

                rangeToString ( from, to ) =
                    if from == to then
                        "(code == " ++ toHex from ++ ")"

                    else
                        "(code >= " ++ toHex from ++ " && code <= " ++ toHex to ++ ")"

                joinOr lst =
                    if List.isEmpty lst then
                        "False"

                    else
                        String.join " || " lst

                rangesToString t =
                    case t of
                        Node ( alls, evens, odds ) ->
                            joinOr (List.map rangeToString alls)
                                ++ (if List.isEmpty evens && List.isEmpty odds then
                                        ""

                                    else
                                        "|| (if modBy 2 code == 0 then "
                                            ++ joinOr (List.map rangeToString evens)
                                            ++ " else "
                                            ++ joinOr (List.map rangeToString odds)
                                            ++ ")"
                                   )

                        Split at l r ->
                            "if code < "
                                ++ toHex at
                                ++ " then "
                                ++ rangesToString l
                                ++ " else "
                                ++ rangesToString r

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
                        |> splitAt 0x1EB8
                        |> splitAt 0x0243
                        |> splitAt 0xA734
                        |> splitAt 0x0506
                        |> splitAt 0x2CC6
                        |> splitAt 0x017D
                        --|> splitAt 0x1D2C
                        --|> splitAt 0x0001D6A6
                        --|> splitAt 0x00010000
                        --|> splitAt 0x0240
                        --|> splitAt 0x2110
                        |> rangesToString
            in
            [ "isLower : Char -> Bool"
            , "isLower c ="
            , "    let"
            , "        code ="
            , "            Char.toCode c"
            , "    in"
            , "    " ++ lowers
            ]

        lines =
            List.concat [ header, isLower ]
    in
    lines
        |> String.join "\n"
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
    if from == to && False then
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
