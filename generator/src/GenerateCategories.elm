module GenerateCategories exposing (declarations, main)

import Common
import Elm exposing (Declaration, File)
import Elm.Annotation as Type exposing (Annotation)
import Elm.Case


main : Program Common.Flags Common.Model Common.Msg
main =
    Common.program flagsToFile


flagsToFile : Common.Flags -> File
flagsToFile _ =
    Elm.file [ "Categories" ] declarations


declarations : List Declaration
declarations =
    [ category
    , categoryFromString
    , categoryToString
    , categoryToDescription
    ]


categoryList : List ( String, String, String )
categoryList =
    [ ( "Lu", "LetterUppercase", "Letter, Uppercase" )
    , ( "Ll", "LetterLowercase", "Letter, Lowercase" )
    , ( "Lt", "LetterTitlecase", "Letter, Titlecase" )
    , ( "Mn", "MarkNonSpacing", "Mark, Non-Spacing" )
    , ( "Mc", "MarkSpacingCombining", "Mark, Spacing Combining" )
    , ( "Me", "MarkEnclosing", "Mark, Enclosing" )
    , ( "Nd", "NumberDecimalDigit", "Number, Decimal Digit" )
    , ( "Nl", "NumberLetter", "Number, Letter" )
    , ( "No", "NumberOther", "Number, Other" )
    , ( "Zs", "SeparatorSpace", "Separator, Space" )
    , ( "Zl", "SeparatorLine", "Separator, Line" )
    , ( "Zp", "SeparatorParagraph", "Separator, Paragraph" )
    , ( "Cc", "OtherControl", "Other, Control" )
    , ( "Cf", "OtherFormat", "Other, Format" )
    , ( "Cs", "OtherSurrogate", "Other, Surrogate" )
    , ( "Co", "OtherPrivateUse", "Other, Private Use" )
    , ( "Cn", "OtherNotAssigned", "Other, Not Assigned" )
    , ( "Lm", "LetterModifier", "Letter, Modifier" )
    , ( "Lo", "LetterOther", "Letter, Other" )
    , ( "Pc", "PunctuationConnector", "Punctuation, Connector" )
    , ( "Pd", "PunctuationDash", "Punctuation, Dash" )
    , ( "Ps", "PunctuationOpen", "Punctuation, Open" )
    , ( "Pe", "PunctuationClose", "Punctuation, Close" )
    , ( "Pi", "PunctuationInitialQuote", "Punctuation, Initial quote" )
    , ( "Pf", "PunctuationFinalQuote", "Punctuation, Final quote" )
    , ( "Po", "PunctuationOther", "Punctuation, Other" )
    , ( "Sm", "SymbolMath", "Symbol, Math" )
    , ( "Sc", "SymbolCurrency", "Symbol, Currency" )
    , ( "Sk", "SymbolModifier", "Symbol, Modifier" )
    , ( "So", "SymbolOther", "Symbol, Other" )
    ]


category : Declaration
category =
    [ Elm.variant "LetterUppercase"
    , Elm.variant "LetterLowercase"
    , Elm.variant "LetterTitlecase"
    , Elm.variant "MarkNonSpacing"
    , Elm.variant "MarkSpacingCombining"
    , Elm.variant "MarkEnclosing"
    , Elm.variant "NumberDecimalDigit"
    , Elm.variant "NumberLetter"
    , Elm.variant "NumberOther"
    , Elm.variant "SeparatorSpace"
    , Elm.variant "SeparatorLine"
    , Elm.variant "SeparatorParagraph"
    , Elm.variant "OtherControl"
    , Elm.variant "OtherFormat"
    , Elm.variant "OtherSurrogate"
    , Elm.variant "OtherPrivateUse"
    , Elm.variant "OtherNotAssigned"
    , Elm.variant "LetterModifier"
    , Elm.variant "LetterOther"
    , Elm.variant "PunctuationConnector"
    , Elm.variant "PunctuationDash"
    , Elm.variant "PunctuationOpen"
    , Elm.variant "PunctuationClose"
    , Elm.variant "PunctuationInitialQuote"
    , Elm.variant "PunctuationFinalQuote"
    , Elm.variant "PunctuationOther"
    , Elm.variant "SymbolMath"
    , Elm.variant "SymbolCurrency"
    , Elm.variant "SymbolModifier"
    , Elm.variant "SymbolOther"
    ]
        |> Elm.customType "Category"
        |> Elm.withDocumentation "A category as defined by the Unicode standard."
        |> Elm.exposeWith { exposeConstructor = True, group = Just "Categories" }


categoryFromString : Declaration
categoryFromString =
    let
        body generalCategory =
            Elm.Case.string generalCategory
                { cases =
                    categoryList
                        |> List.map
                            (\( short, long, _ ) ->
                                ( short
                                , Elm.apply
                                    (Elm.value
                                        { importFrom = []
                                        , name = "Just"
                                        , annotation = Nothing
                                        }
                                    )
                                    [ Elm.value
                                        { importFrom = []
                                        , name = long
                                        , annotation = Nothing
                                        }
                                    ]
                                )
                            )
                , otherwise =
                    Elm.value
                        { importFrom = []
                        , name = "Nothing"
                        , annotation = Nothing
                        }
                }
    in
    Elm.fn "generalCategory" body
        |> Elm.withType
            (Type.function
                [ Type.string ]
                (Type.namedWith [] "Maybe" [ categoryAnnotation ])
            )
        |> Elm.declaration "categoryFromString"
        |> Elm.withDocumentation "Parses a category name (Lu, Ll, Lt, ...)."
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Categories" }


categoryToString : Declaration
categoryToString =
    let
        body generalCategory =
            categoryList
                |> List.map
                    (\( short, long, _ ) ->
                        Elm.Case.branch0 [] long (Elm.string short)
                    )
                |> Elm.Case.custom generalCategory
    in
    Elm.fn "generalCategory" body
        |> Elm.withType (Type.function [ categoryAnnotation ] Type.string)
        |> Elm.declaration "categoryToString"
        |> Elm.withDocumentation "Convert a category to its short category name (Lu, Ll, Lt, ...)."
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Categories" }


categoryToDescription : Declaration
categoryToDescription =
    let
        body generalCategory =
            categoryList
                |> List.map
                    (\( _, long, longer ) ->
                        Elm.Case.branch0 [] long (Elm.string longer)
                    )
                |> Elm.Case.custom generalCategory
    in
    Elm.fn "generalCategory" body
        |> Elm.withType (Type.function [ categoryAnnotation ] Type.string)
        |> Elm.declaration "categoryToDescription"
        |> Elm.withDocumentation "Converts a category to its English description. Mostly useful for debugging purposes."
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Categories" }


categoryAnnotation : Annotation
categoryAnnotation =
    Type.named [] "Category"
