module OldMain exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import OldUnicode


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
    [ ( "isUpper", OldUnicode.isUpper >> boolToString )
    , ( "isLower", OldUnicode.isLower >> boolToString )
    , ( "isAlpha", OldUnicode.isAlpha >> boolToString )
    , ( "isAlphaNum", OldUnicode.isAlphaNum >> boolToString )
    , ( "isDigit", OldUnicode.isDigit >> boolToString )

    -- , ( "getCategory", OldUnicode.getCategory >> Maybe.map OldUnicode.categoryToString >> Maybe.withDefault "?" )
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
