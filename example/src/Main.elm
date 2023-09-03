module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Unicode


main : Program () String String
main =
    Browser.sandbox
        { view = view
        , init = ""
        , update = always
        }


view : String -> Html String
view model =
    checks
        |> List.map (viewCheck model)
        |> (::) (header model)
        |> Html.table []
        |> List.singleton
        |> (::) (input model)
        |> List.intersperse (Html.br [] [])
        |> Html.div []


input : String -> Html String
input model =
    Html.input [ Html.Events.onInput identity, Html.Attributes.value model ] []


header : String -> Html msg
header model =
    model
        |> String.toList
        |> List.map (\c -> Html.th [] [ Html.text <| String.fromChar c ])
        |> (::) (Html.td [] [])
        |> Html.tr []


checks : List ( String, Char -> String )
checks =
    [ ( "isUpper", Unicode.isUpper >> boolToString )
    , ( "isLower", Unicode.isLower >> boolToString )
    , ( "isAlpha", Unicode.isAlpha >> boolToString )
    , ( "isAlphaNum", Unicode.isAlphaNum >> boolToString )
    , ( "isDigit", Unicode.isDigit >> boolToString )
    , ( "getCategory", Unicode.getCategory >> Maybe.map Unicode.categoryToString >> Maybe.withDefault "?" )
    ]


viewCheck : String -> ( String, Char -> String ) -> Html msg
viewCheck model ( label, check ) =
    model
        |> String.toList
        |> List.map
            (\char ->
                Html.td [] [ Html.text <| check char ]
            )
        |> (::) (Html.th [ Html.Attributes.style "text-align" "left" ] [ Html.text label ])
        |> Html.tr []


boolToString : Bool -> String
boolToString b =
    if b then
        "Y"

    else
        "N"
