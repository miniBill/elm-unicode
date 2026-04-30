module GenerateCategories exposing (declarations, main)

import Elm exposing (Declaration, File)
import Elm.Annotation as Type exposing (Annotation)
import Elm.Arg
import Elm.Case
import Gen.CodeGen.Generate as Generate


main : Program {} () ()
main =
    Generate.run [ file ]


file : File
file =
    Elm.file [ "Categories" ] [ declarations, categoryToConstructor ]


declarations : Declaration
declarations =
    [ Elm.docs "## Categories"
    , category
    , categoryFromString
    , categoryToString
    , categoryToDescription
    ]
        |> Elm.group


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
    categoryList
        |> List.map (\( _, name, _ ) -> Elm.variant name)
        |> Elm.customType "Category"
        |> Elm.withDocumentation "A category as defined by the Unicode standard."
        |> Elm.exposeConstructor


categoryFromString : Declaration
categoryFromString =
    let
        body : Elm.Expression -> Elm.Expression
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
                |> Elm.withType (Type.namedWith [] "Maybe" [ categoryAnnotation ])
    in
    Elm.fn (Elm.Arg.varWith "generalCategory" Type.string) body
        |> Elm.declaration "categoryFromString"
        |> Elm.withDocumentation "Parses a category name (Lu, Ll, Lt, ...)."
        |> Elm.expose


categoryToString : Declaration
categoryToString =
    let
        body : Elm.Expression -> Elm.Expression
        body generalCategory =
            categoryList
                |> List.map
                    (\( short, long, _ ) ->
                        Elm.Case.branch (Elm.Arg.customType long short) Elm.string
                    )
                |> Elm.Case.custom generalCategory categoryAnnotation
                |> Elm.withType Type.string
    in
    Elm.fn (Elm.Arg.varWith "generalCategory" categoryAnnotation) body
        |> Elm.declaration "categoryToString"
        |> Elm.withDocumentation "Convert a category to its short category name (Lu, Ll, Lt, ...)."
        |> Elm.expose


categoryToConstructor : Declaration
categoryToConstructor =
    let
        body : Elm.Expression -> Elm.Expression
        body generalCategory =
            categoryList
                |> List.map
                    (\( _, long, _ ) ->
                        Elm.Case.branch (Elm.Arg.customType long long) Elm.string
                    )
                |> Elm.Case.custom generalCategory categoryAnnotation
                |> Elm.withType Type.string
    in
    Elm.fn (Elm.Arg.varWith "generalCategory" categoryAnnotation) body
        |> Elm.declaration "categoryToConstructor"
        |> Elm.withDocumentation "Convert a category to its constructor name."
        |> Elm.expose


categoryToDescription : Declaration
categoryToDescription =
    let
        body : Elm.Expression -> Elm.Expression
        body generalCategory =
            categoryList
                |> List.map
                    (\( _, long, longer ) ->
                        Elm.Case.branch (Elm.Arg.customType long longer) Elm.string
                    )
                |> Elm.Case.custom generalCategory categoryAnnotation
                |> Elm.withType Type.string
    in
    Elm.fn (Elm.Arg.varWith "generalCategory" categoryAnnotation) body
        |> Elm.declaration "categoryToDescription"
        |> Elm.withDocumentation "Converts a category to its English description. Mostly useful for debugging purposes."
        |> Elm.expose


categoryAnnotation : Annotation
categoryAnnotation =
    Type.named [] "Category"
