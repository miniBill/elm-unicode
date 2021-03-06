module Unicode exposing (Category(..), categoryFromString, categoryToDescription, categoryToString)

{-| A category as defined by the Unicode standard. -}
type Category
    = LetterUppercase
    | LetterLowercase
    | LetterTitlecase
    | MarkNonSpacing
    | MarkSpacingCombining
    | MarkEnclosing
    | NumberDecimalDigit
    | NumberLetter
    | NumberOther
    | SeparatorSpace
    | SeparatorLine
    | SeparatorParagraph
    | OtherControl
    | OtherFormat
    | OtherSurrogate
    | OtherPrivateUse
    | OtherNotAssigned
    | LetterModifier
    | LetterOther
    | PunctuationConnector
    | PunctuationDash
    | PunctuationOpen
    | PunctuationClose
    | PunctuationInitialQuote
    | PunctuationFinalQuote
    | PunctuationOther
    | SymbolMath
    | SymbolCurrency
    | SymbolModifier
    | SymbolOther

{-| Parses a category name (Lu, Ll, Lt, ...). -}
categoryFromString : String -> Maybe Category
categoryFromString generalCategory =
    case generalCategory of
        "Lu" ->
            Just LetterUppercase

        "Ll" ->
            Just LetterLowercase

        "Lt" ->
            Just LetterTitlecase

        "Mn" ->
            Just MarkNonSpacing

        "Mc" ->
            Just MarkSpacingCombining

        "Me" ->
            Just MarkEnclosing

        "Nd" ->
            Just NumberDecimalDigit

        "Nl" ->
            Just NumberLetter

        "No" ->
            Just NumberOther

        "Zs" ->
            Just SeparatorSpace

        "Zl" ->
            Just SeparatorLine

        "Zp" ->
            Just SeparatorParagraph

        "Cc" ->
            Just OtherControl

        "Cf" ->
            Just OtherFormat

        "Cs" ->
            Just OtherSurrogate

        "Co" ->
            Just OtherPrivateUse

        "Cn" ->
            Just OtherNotAssigned

        "Lm" ->
            Just LetterModifier

        "Lo" ->
            Just LetterOther

        "Pc" ->
            Just PunctuationConnector

        "Pd" ->
            Just PunctuationDash

        "Ps" ->
            Just PunctuationOpen

        "Pe" ->
            Just PunctuationClose

        "Pi" ->
            Just PunctuationInitialQuote

        "Pf" ->
            Just PunctuationFinalQuote

        "Po" ->
            Just PunctuationOther

        "Sm" ->
            Just SymbolMath

        "Sc" ->
            Just SymbolCurrency

        "Sk" ->
            Just SymbolModifier

        "So" ->
            Just SymbolOther

        _ ->
            Nothing


{-| Convert a category to its short category name (Lu, Ll, Lt, ...) -}
categoryToString : Category -> String
categoryToString generalCategory =
    case generalCategory of
        LetterUppercase ->
            "Lu"

        LetterLowercase ->
            "Ll"

        LetterTitlecase ->
            "Lt"

        MarkNonSpacing ->
            "Mn"

        MarkSpacingCombining ->
            "Mc"

        MarkEnclosing ->
            "Me"

        NumberDecimalDigit ->
            "Nd"

        NumberLetter ->
            "Nl"

        NumberOther ->
            "No"

        SeparatorSpace ->
            "Zs"

        SeparatorLine ->
            "Zl"

        SeparatorParagraph ->
            "Zp"

        OtherControl ->
            "Cc"

        OtherFormat ->
            "Cf"

        OtherSurrogate ->
            "Cs"

        OtherPrivateUse ->
            "Co"

        OtherNotAssigned ->
            "Cn"

        LetterModifier ->
            "Lm"

        LetterOther ->
            "Lo"

        PunctuationConnector ->
            "Pc"

        PunctuationDash ->
            "Pd"

        PunctuationOpen ->
            "Ps"

        PunctuationClose ->
            "Pe"

        PunctuationInitialQuote ->
            "Pi"

        PunctuationFinalQuote ->
            "Pf"

        PunctuationOther ->
            "Po"

        SymbolMath ->
            "Sm"

        SymbolCurrency ->
            "Sc"

        SymbolModifier ->
            "Sk"

        SymbolOther ->
            "So"

{-| Converts a category to its English description. Mostly useful for debugging purposes. -}
categoryToDescription : Category -> String
categoryToDescription cat =
    case cat of
        LetterUppercase ->
            "Letter, Uppercase"

        LetterLowercase ->
            "Letter, Lowercase"

        LetterTitlecase ->
            "Letter, Titlecase"

        MarkNonSpacing ->
            "Mark, Non-Spacing"

        MarkSpacingCombining ->
            "Mark, Spacing Combining"

        MarkEnclosing ->
            "Mark, Enclosing"

        NumberDecimalDigit ->
            "Number, Decimal Digit"

        NumberLetter ->
            "Number, Letter"

        NumberOther ->
            "Number, Other"

        SeparatorSpace ->
            "Separator, Space"

        SeparatorLine ->
            "Separator, Line"

        SeparatorParagraph ->
            "Separator, Paragraph"

        OtherControl ->
            "Other, Control"

        OtherFormat ->
            "Other, Format"

        OtherSurrogate ->
            "Other, Surrogate"

        OtherPrivateUse ->
            "Other, Private Use"

        OtherNotAssigned ->
            "Other, Not Assigned"

        LetterModifier ->
            "Letter, Modifier"

        LetterOther ->
            "Letter, Other"

        PunctuationConnector ->
            "Punctuation, Connector"

        PunctuationDash ->
            "Punctuation, Dash"

        PunctuationOpen ->
            "Punctuation, Open"

        PunctuationClose ->
            "Punctuation, Close"

        PunctuationInitialQuote ->
            "Punctuation, Initial quote"

        PunctuationFinalQuote ->
            "Punctuation, Final quote"

        PunctuationOther ->
            "Punctuation, Other"

        SymbolMath ->
            "Symbol, Math"

        SymbolCurrency ->
            "Symbol, Currency"

        SymbolModifier ->
            "Symbol, Modifier"

        SymbolOther ->
            "Symbol, Other"
