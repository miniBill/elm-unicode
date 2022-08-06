module Unicode exposing
    ( isUpper, isLower, isAlpha, isAlphaNum
    , isDigit
    , Category(..), getCategory, categoryFromString, categoryToString, categoryToDescription
    )

{-| Unicode aware functions for working with characters.


## Letters

@docs isUpper, isLower, isAlpha, isAlphaNum


## Digits

@docs isDigit


## Categories

@docs Category, getCategory, categoryFromString, categoryToString, categoryToDescription

-}

import Char


{-| Detect upper case characters (Unicode category Lu)
-}
isUpper : Char -> Bool
isUpper c =
    let
        simple =
            Char.toUpper c == c && Char.toLower c /= c

        code =
            Char.toCode c
    in
    (simple
        && (if code < 0x0100 then
                code >= 0x00 && code <= 0xFF

            else
                (code >= 0x0100 && code <= 0x215F)
                    || (code >= 0x2170 && code <= 0x24B5)
                    || (code >= 0x24D0 && code <= 0x000F0000)
           )
    )
        || (if code < 0x0001D4CF then
                if code < 0x213D then
                    (code >= 0x03D2 && code <= 0x03D4)
                        || (code == 0x2102)
                        || (code == 0x2107)
                        || (code >= 0x210B && code <= 0x210D)
                        || (code >= 0x2110 && code <= 0x2112)
                        || (code == 0x2115)
                        || (code >= 0x2119 && code <= 0x211D)
                        || (code == 0x2124)
                        || (code == 0x2128)
                        || (code >= 0x212A && code <= 0x212D)
                        || (code >= 0x2130 && code <= 0x2133)

                else
                    (code >= 0x213E && code <= 0x213F)
                        || (code == 0x2145)
                        || (code >= 0x0001D400 && code <= 0x0001D419)
                        || (code >= 0x0001D434 && code <= 0x0001D44D)
                        || (code >= 0x0001D468 && code <= 0x0001D481)
                        || (code == 0x0001D49C)
                        || (code >= 0x0001D49E && code <= 0x0001D49F)
                        || (code == 0x0001D4A2)
                        || (code >= 0x0001D4A5 && code <= 0x0001D4A6)
                        || (code >= 0x0001D4A9 && code <= 0x0001D4AC)
                        || (code >= 0x0001D4AE && code <= 0x0001D4B5)

            else if code < 0x0001D59F then
                (code >= 0x0001D4D0 && code <= 0x0001D4E9)
                    || (code >= 0x0001D504 && code <= 0x0001D505)
                    || (code >= 0x0001D507 && code <= 0x0001D50A)
                    || (code >= 0x0001D50D && code <= 0x0001D514)
                    || (code >= 0x0001D516 && code <= 0x0001D51C)
                    || (code >= 0x0001D538 && code <= 0x0001D539)
                    || (code >= 0x0001D53B && code <= 0x0001D53E)
                    || (code >= 0x0001D540 && code <= 0x0001D544)
                    || (code == 0x0001D546)
                    || (code >= 0x0001D54A && code <= 0x0001D550)
                    || (code >= 0x0001D56C && code <= 0x0001D585)

            else
                (code >= 0x0001D5A0 && code <= 0x0001D5B9)
                    || (code >= 0x0001D5D4 && code <= 0x0001D5ED)
                    || (code >= 0x0001D608 && code <= 0x0001D621)
                    || (code >= 0x0001D63C && code <= 0x0001D655)
                    || (code >= 0x0001D670 && code <= 0x0001D689)
                    || (code >= 0x0001D6A8 && code <= 0x0001D6C0)
                    || (code >= 0x0001D6E2 && code <= 0x0001D6FA)
                    || (code >= 0x0001D71C && code <= 0x0001D734)
                    || (code >= 0x0001D756 && code <= 0x0001D76E)
                    || (code >= 0x0001D790 && code <= 0x0001D7A8)
                    || (code == 0x0001D7CA)
           )


{-| Detect lower case characters (Unicode category Ll)
-}
isLower : Char -> Bool
isLower c =
    let
        simple =
            Char.toLower c == c && Char.toUpper c /= c

        code =
            Char.toCode c
    in
    (simple
        && (if code < 0x0100 then
                code >= 0x00 && code <= 0xFF

            else
                (code >= 0x0100 && code <= 0x0344)
                    || (code >= 0x0346 && code <= 0x216F)
                    || (code >= 0x2180 && code <= 0x24CF)
                    || (code >= 0x24EA && code <= 0x000F0000)
           )
    )
        || (if code < 0xA7AE then
                if code < 0x1E9E then
                    if code < 0x024E then
                        (code >= 0x0137 && code <= 0x0138)
                            || (code >= 0x018C && code <= 0x018D)
                            || (code >= 0x0199 && code <= 0x019B)
                            || (code >= 0x01AA && code <= 0x01AB)
                            || (code >= 0x01B9 && code <= 0x01BA)
                            || (code >= 0x01BD && code <= 0x01BF)
                            || (code == 0x0221)
                            || (code >= 0x0233 && code <= 0x0239)

                    else
                        (code >= 0x024F && code <= 0x0293)
                            || (code >= 0x0295 && code <= 0x02AF)
                            || (code >= 0x03FB && code <= 0x03FC)
                            || (code >= 0x0560 && code <= 0x0588)
                            || (code >= 0x1D00 && code <= 0x1D2B)
                            || (code >= 0x1D6B && code <= 0x1D77)
                            || (code >= 0x1D79 && code <= 0x1D9A)
                            || (code >= 0x1E95 && code <= 0x1E9D)

                else if code < 0x2145 then
                    (code == 0x1E9F)
                        || (code == 0x210A)
                        || (code >= 0x210E && code <= 0x210F)
                        || (code == 0x2113)
                        || (code == 0x212F)
                        || (code == 0x2134)
                        || (code == 0x2139)
                        || (code >= 0x213C && code <= 0x213D)

                else
                    (code >= 0x2146 && code <= 0x2149)
                        || (code == 0x2C71)
                        || (code >= 0x2C73 && code <= 0x2C74)
                        || (code >= 0x2C76 && code <= 0x2C7B)
                        || (code >= 0x2CE3 && code <= 0x2CE4)
                        || (code >= 0xA72F && code <= 0xA731)
                        || (code >= 0xA771 && code <= 0xA778)
                        || (code == 0xA78E)
                        || (code >= 0xA793 && code <= 0xA795)

            else if code < 0x0001D5ED then
                if code < 0x0001D4B5 then
                    (code == 0xA7AF)
                        || (code == 0xA7FA)
                        || (code >= 0xAB30 && code <= 0xAB5A)
                        || (code >= 0xAB60 && code <= 0xAB68)
                        || (code >= 0x0001D41A && code <= 0x0001D433)
                        || (code >= 0x0001D44E && code <= 0x0001D454)
                        || (code >= 0x0001D456 && code <= 0x0001D467)
                        || (code >= 0x0001D482 && code <= 0x0001D49B)
                        || ((modBy 2 code == 1)
                                && (code >= 0xA7D3 && code <= 0xA7D5)
                           )

                else
                    (code >= 0x0001D4B6 && code <= 0x0001D4B9)
                        || (code == 0x0001D4BB)
                        || (code >= 0x0001D4BD && code <= 0x0001D4C3)
                        || (code >= 0x0001D4C5 && code <= 0x0001D4CF)
                        || (code >= 0x0001D4EA && code <= 0x0001D503)
                        || (code >= 0x0001D51E && code <= 0x0001D537)
                        || (code >= 0x0001D552 && code <= 0x0001D56B)
                        || (code >= 0x0001D586 && code <= 0x0001D59F)
                        || (code >= 0x0001D5BA && code <= 0x0001D5D3)

            else if code < 0x0001D735 then
                (code >= 0x0001D5EE && code <= 0x0001D607)
                    || (code >= 0x0001D622 && code <= 0x0001D63B)
                    || (code >= 0x0001D656 && code <= 0x0001D66F)
                    || (code >= 0x0001D68A && code <= 0x0001D6A5)
                    || (code >= 0x0001D6C2 && code <= 0x0001D6DA)
                    || (code >= 0x0001D6DC && code <= 0x0001D6E1)
                    || (code >= 0x0001D6FC && code <= 0x0001D714)
                    || (code >= 0x0001D716 && code <= 0x0001D71B)

            else
                (code >= 0x0001D736 && code <= 0x0001D74E)
                    || (code >= 0x0001D750 && code <= 0x0001D755)
                    || (code >= 0x0001D770 && code <= 0x0001D788)
                    || (code >= 0x0001D78A && code <= 0x0001D78F)
                    || (code >= 0x0001D7AA && code <= 0x0001D7C2)
                    || (code >= 0x0001D7C4 && code <= 0x0001D7C9)
                    || (code == 0x0001D7CB)
                    || (code >= 0x0001DF00 && code <= 0x0001DF09)
                    || (code >= 0x0001DF0B && code <= 0x0001DF1E)
           )


{-| Detect letters (Unicode categories Lu, Ll, Lt, Lm, Lo)
-}
isAlpha : Char -> Bool
isAlpha c =
    let
        code =
            Char.toCode c
    in
    if code < 0x0100 then
        (code >= 0x41 && code <= 0x5A)
            || (code >= 0x61 && code <= 0x7A)
            || (code == 0xAA)
            || (code == 0xB5)
            || (code == 0xBA)
            || (code >= 0xC0 && code <= 0xD6)
            || (code >= 0xD8 && code <= 0xF6)
            || (code >= 0xF8 && code <= 0xFF)

    else if code < 0xA9CE then
        if code < 0x106D then
            if code < 0x0ADF then
                if code < 0x085F then
                    if code < 0x061F then
                        if code < 0x038B then
                            (code >= 0x0100 && code <= 0x02C1)
                                || (code >= 0x02C6 && code <= 0x02D1)
                                || (code >= 0x02E0 && code <= 0x02E4)
                                || (code >= 0x0370 && code <= 0x0374)
                                || (code >= 0x0376 && code <= 0x0377)
                                || (code >= 0x037A && code <= 0x037D)
                                || (code == 0x037F)
                                || (code == 0x0386)
                                || (code >= 0x0388 && code <= 0x038A)
                                || ((modBy 2 code == 0)
                                        && (code >= 0x02EC && code <= 0x02EE)
                                   )

                        else
                            (code == 0x038C)
                                || (code >= 0x038E && code <= 0x03A1)
                                || (code >= 0x03A3 && code <= 0x03F5)
                                || (code >= 0x03F7 && code <= 0x0481)
                                || (code >= 0x048A && code <= 0x052F)
                                || (code >= 0x0531 && code <= 0x0556)
                                || (code == 0x0559)
                                || (code >= 0x0560 && code <= 0x0588)
                                || (code >= 0x05D0 && code <= 0x05EA)
                                || (code >= 0x05EF && code <= 0x05F2)

                    else if code < 0x074C then
                        (code >= 0x0620 && code <= 0x064A)
                            || (code >= 0x066E && code <= 0x066F)
                            || (code >= 0x0671 && code <= 0x06D3)
                            || (code == 0x06D5)
                            || (code >= 0x06E5 && code <= 0x06E6)
                            || (code >= 0x06EE && code <= 0x06EF)
                            || (code >= 0x06FA && code <= 0x06FC)
                            || (code == 0x06FF)
                            || (code == 0x0710)
                            || (code >= 0x0712 && code <= 0x072F)

                    else
                        (code >= 0x074D && code <= 0x07A5)
                            || (code == 0x07B1)
                            || (code >= 0x07CA && code <= 0x07EA)
                            || (code >= 0x07F4 && code <= 0x07F5)
                            || (code == 0x07FA)
                            || (code >= 0x0800 && code <= 0x0815)
                            || (code == 0x081A)
                            || (code == 0x0824)
                            || (code == 0x0828)
                            || (code >= 0x0840 && code <= 0x0858)

                else if code < 0x09EF then
                    if code < 0x0984 then
                        (code >= 0x0860 && code <= 0x086A)
                            || (code >= 0x0870 && code <= 0x0887)
                            || (code >= 0x0889 && code <= 0x088E)
                            || (code >= 0x08A0 && code <= 0x08C9)
                            || (code >= 0x0904 && code <= 0x0939)
                            || (code == 0x093D)
                            || (code == 0x0950)
                            || (code >= 0x0958 && code <= 0x0961)
                            || (code >= 0x0971 && code <= 0x0980)

                    else
                        (code >= 0x0985 && code <= 0x098C)
                            || (code >= 0x098F && code <= 0x0990)
                            || (code >= 0x0993 && code <= 0x09A8)
                            || (code >= 0x09AA && code <= 0x09B0)
                            || (code == 0x09B2)
                            || (code >= 0x09B6 && code <= 0x09B9)
                            || (code == 0x09BD)
                            || (code == 0x09CE)
                            || (code >= 0x09DC && code <= 0x09DD)
                            || (code >= 0x09DF && code <= 0x09E1)

                else if code < 0x0A5D then
                    (code >= 0x09F0 && code <= 0x09F1)
                        || (code == 0x09FC)
                        || (code >= 0x0A05 && code <= 0x0A0A)
                        || (code >= 0x0A0F && code <= 0x0A10)
                        || (code >= 0x0A13 && code <= 0x0A28)
                        || (code >= 0x0A2A && code <= 0x0A30)
                        || (code >= 0x0A32 && code <= 0x0A33)
                        || (code >= 0x0A35 && code <= 0x0A36)
                        || (code >= 0x0A38 && code <= 0x0A39)
                        || (code >= 0x0A59 && code <= 0x0A5C)

                else
                    (code == 0x0A5E)
                        || (code >= 0x0A72 && code <= 0x0A74)
                        || (code >= 0x0A85 && code <= 0x0A8D)
                        || (code >= 0x0A8F && code <= 0x0A91)
                        || (code >= 0x0A93 && code <= 0x0AA8)
                        || (code >= 0x0AAA && code <= 0x0AB0)
                        || (code >= 0x0AB2 && code <= 0x0AB3)
                        || (code >= 0x0AB5 && code <= 0x0AB9)
                        || (code == 0x0ABD)
                        || (code == 0x0AD0)

            else if code < 0x0CDF then
                if code < 0x0BA2 then
                    if code < 0x0B5B then
                        (code >= 0x0AE0 && code <= 0x0AE1)
                            || (code == 0x0AF9)
                            || (code >= 0x0B05 && code <= 0x0B0C)
                            || (code >= 0x0B0F && code <= 0x0B10)
                            || (code >= 0x0B13 && code <= 0x0B28)
                            || (code >= 0x0B2A && code <= 0x0B30)
                            || (code >= 0x0B32 && code <= 0x0B33)
                            || (code >= 0x0B35 && code <= 0x0B39)
                            || (code == 0x0B3D)

                    else
                        (code >= 0x0B5C && code <= 0x0B5D)
                            || (code >= 0x0B5F && code <= 0x0B61)
                            || (code == 0x0B71)
                            || (code == 0x0B83)
                            || (code >= 0x0B85 && code <= 0x0B8A)
                            || (code >= 0x0B8E && code <= 0x0B90)
                            || (code >= 0x0B92 && code <= 0x0B95)
                            || (code >= 0x0B99 && code <= 0x0B9A)
                            || (code == 0x0B9C)
                            || (code >= 0x0B9E && code <= 0x0B9F)

                else if code < 0x0C5C then
                    (code >= 0x0BA3 && code <= 0x0BA4)
                        || (code >= 0x0BA8 && code <= 0x0BAA)
                        || (code >= 0x0BAE && code <= 0x0BB9)
                        || (code == 0x0BD0)
                        || (code >= 0x0C05 && code <= 0x0C0C)
                        || (code >= 0x0C0E && code <= 0x0C10)
                        || (code >= 0x0C12 && code <= 0x0C28)
                        || (code >= 0x0C2A && code <= 0x0C39)
                        || (code == 0x0C3D)
                        || (code >= 0x0C58 && code <= 0x0C5A)

                else
                    (code == 0x0C5D)
                        || (code >= 0x0C60 && code <= 0x0C61)
                        || (code == 0x0C80)
                        || (code >= 0x0C85 && code <= 0x0C8C)
                        || (code >= 0x0C8E && code <= 0x0C90)
                        || (code >= 0x0C92 && code <= 0x0CA8)
                        || (code >= 0x0CAA && code <= 0x0CB3)
                        || (code >= 0x0CB5 && code <= 0x0CB9)
                        || (code == 0x0CBD)
                        || (code >= 0x0CDD && code <= 0x0CDE)

            else if code < 0x0E83 then
                if code < 0x0D79 then
                    (code >= 0x0CE0 && code <= 0x0CE1)
                        || (code >= 0x0CF1 && code <= 0x0CF2)
                        || (code >= 0x0D04 && code <= 0x0D0C)
                        || (code >= 0x0D0E && code <= 0x0D10)
                        || (code >= 0x0D12 && code <= 0x0D3A)
                        || (code == 0x0D3D)
                        || (code == 0x0D4E)
                        || (code >= 0x0D54 && code <= 0x0D56)
                        || (code >= 0x0D5F && code <= 0x0D61)

                else
                    (code >= 0x0D7A && code <= 0x0D7F)
                        || (code >= 0x0D85 && code <= 0x0D96)
                        || (code >= 0x0D9A && code <= 0x0DB1)
                        || (code >= 0x0DB3 && code <= 0x0DBB)
                        || (code == 0x0DBD)
                        || (code >= 0x0DC0 && code <= 0x0DC6)
                        || (code >= 0x0E01 && code <= 0x0E30)
                        || (code >= 0x0E32 && code <= 0x0E33)
                        || (code >= 0x0E40 && code <= 0x0E46)
                        || (code >= 0x0E81 && code <= 0x0E82)

            else if code < 0x0EFF then
                (code == 0x0E84)
                    || (code >= 0x0E86 && code <= 0x0E8A)
                    || (code >= 0x0E8C && code <= 0x0EA3)
                    || (code == 0x0EA5)
                    || (code >= 0x0EA7 && code <= 0x0EB0)
                    || (code >= 0x0EB2 && code <= 0x0EB3)
                    || (code == 0x0EBD)
                    || (code >= 0x0EC0 && code <= 0x0EC4)
                    || (code == 0x0EC6)
                    || (code >= 0x0EDC && code <= 0x0EDF)

            else
                (code == 0x0F00)
                    || (code >= 0x0F40 && code <= 0x0F47)
                    || (code >= 0x0F49 && code <= 0x0F6C)
                    || (code >= 0x0F88 && code <= 0x0F8C)
                    || (code >= 0x1000 && code <= 0x102A)
                    || (code == 0x103F)
                    || (code >= 0x1050 && code <= 0x1055)
                    || (code >= 0x105A && code <= 0x105D)
                    || (code == 0x1061)
                    || (code >= 0x1065 && code <= 0x1066)

        else if code < 0x1FC5 then
            if code < 0x181F then
                if code < 0x12C7 then
                    if code < 0x124F then
                        (code >= 0x106E && code <= 0x1070)
                            || (code >= 0x1075 && code <= 0x1081)
                            || (code == 0x108E)
                            || (code >= 0x10A0 && code <= 0x10C5)
                            || (code == 0x10C7)
                            || (code == 0x10CD)
                            || (code >= 0x10D0 && code <= 0x10FA)
                            || (code >= 0x10FC && code <= 0x1248)
                            || (code >= 0x124A && code <= 0x124D)

                    else
                        (code >= 0x1250 && code <= 0x1256)
                            || (code == 0x1258)
                            || (code >= 0x125A && code <= 0x125D)
                            || (code >= 0x1260 && code <= 0x1288)
                            || (code >= 0x128A && code <= 0x128D)
                            || (code >= 0x1290 && code <= 0x12B0)
                            || (code >= 0x12B2 && code <= 0x12B5)
                            || (code >= 0x12B8 && code <= 0x12BE)
                            || (code == 0x12C0)
                            || (code >= 0x12C2 && code <= 0x12C5)

                else if code < 0x169F then
                    (code >= 0x12C8 && code <= 0x12D6)
                        || (code >= 0x12D8 && code <= 0x1310)
                        || (code >= 0x1312 && code <= 0x1315)
                        || (code >= 0x1318 && code <= 0x135A)
                        || (code >= 0x1380 && code <= 0x138F)
                        || (code >= 0x13A0 && code <= 0x13F5)
                        || (code >= 0x13F8 && code <= 0x13FD)
                        || (code >= 0x1401 && code <= 0x166C)
                        || (code >= 0x166F && code <= 0x167F)
                        || (code >= 0x1681 && code <= 0x169A)

                else
                    (code >= 0x16A0 && code <= 0x16EA)
                        || (code >= 0x16F1 && code <= 0x16F8)
                        || (code >= 0x1700 && code <= 0x1711)
                        || (code >= 0x171F && code <= 0x1731)
                        || (code >= 0x1740 && code <= 0x1751)
                        || (code >= 0x1760 && code <= 0x176C)
                        || (code >= 0x176E && code <= 0x1770)
                        || (code >= 0x1780 && code <= 0x17B3)
                        || (code == 0x17D7)
                        || (code == 0x17DC)

            else if code < 0x1C4C then
                if code < 0x19AF then
                    (code >= 0x1820 && code <= 0x1878)
                        || (code >= 0x1880 && code <= 0x1884)
                        || (code >= 0x1887 && code <= 0x18A8)
                        || (code == 0x18AA)
                        || (code >= 0x18B0 && code <= 0x18F5)
                        || (code >= 0x1900 && code <= 0x191E)
                        || (code >= 0x1950 && code <= 0x196D)
                        || (code >= 0x1970 && code <= 0x1974)
                        || (code >= 0x1980 && code <= 0x19AB)

                else
                    (code >= 0x19B0 && code <= 0x19C9)
                        || (code >= 0x1A00 && code <= 0x1A16)
                        || (code >= 0x1A20 && code <= 0x1A54)
                        || (code == 0x1AA7)
                        || (code >= 0x1B05 && code <= 0x1B33)
                        || (code >= 0x1B45 && code <= 0x1B4C)
                        || (code >= 0x1B83 && code <= 0x1BA0)
                        || (code >= 0x1BAE && code <= 0x1BAF)
                        || (code >= 0x1BBA && code <= 0x1BE5)
                        || (code >= 0x1C00 && code <= 0x1C23)

            else if code < 0x1DFF then
                (code >= 0x1C4D && code <= 0x1C4F)
                    || (code >= 0x1C5A && code <= 0x1C7D)
                    || (code >= 0x1C80 && code <= 0x1C88)
                    || (code >= 0x1C90 && code <= 0x1CBA)
                    || (code >= 0x1CBD && code <= 0x1CBF)
                    || (code >= 0x1CE9 && code <= 0x1CEC)
                    || (code >= 0x1CEE && code <= 0x1CF3)
                    || (code >= 0x1CF5 && code <= 0x1CF6)
                    || (code == 0x1CFA)
                    || (code >= 0x1D00 && code <= 0x1DBF)

            else
                (code >= 0x1E00 && code <= 0x1F15)
                    || (code >= 0x1F18 && code <= 0x1F1D)
                    || (code >= 0x1F20 && code <= 0x1F45)
                    || (code >= 0x1F48 && code <= 0x1F4D)
                    || (code >= 0x1F50 && code <= 0x1F57)
                    || (code >= 0x1F60 && code <= 0x1F7D)
                    || (code >= 0x1F80 && code <= 0x1FB4)
                    || (code >= 0x1FB6 && code <= 0x1FBC)
                    || (code == 0x1FBE)
                    || (code >= 0x1FC2 && code <= 0x1FC4)
                    || ((modBy 2 code == 1)
                            && (code >= 0x1F59 && code <= 0x1F5F)
                       )

        else if code < 0x3030 then
            if code < 0x2182 then
                if code < 0x2101 then
                    (code >= 0x1FC6 && code <= 0x1FCC)
                        || (code >= 0x1FD0 && code <= 0x1FD3)
                        || (code >= 0x1FD6 && code <= 0x1FDB)
                        || (code >= 0x1FE0 && code <= 0x1FEC)
                        || (code >= 0x1FF2 && code <= 0x1FF4)
                        || (code >= 0x1FF6 && code <= 0x1FFC)
                        || (code == 0x2071)
                        || (code == 0x207F)
                        || (code >= 0x2090 && code <= 0x209C)

                else
                    (code == 0x2102)
                        || (code == 0x2107)
                        || (code >= 0x210A && code <= 0x2113)
                        || (code == 0x2115)
                        || (code >= 0x2119 && code <= 0x211D)
                        || (code >= 0x212A && code <= 0x212D)
                        || (code >= 0x212F && code <= 0x2139)
                        || (code >= 0x213C && code <= 0x213F)
                        || (code >= 0x2145 && code <= 0x2149)
                        || (code == 0x214E)
                        || ((modBy 2 code == 0)
                                && (code >= 0x2124 && code <= 0x2128)
                           )

            else if code < 0x2D9F then
                (code >= 0x2183 && code <= 0x2184)
                    || (code >= 0x2C00 && code <= 0x2CE4)
                    || (code >= 0x2CEB && code <= 0x2CEE)
                    || (code >= 0x2CF2 && code <= 0x2CF3)
                    || (code >= 0x2D00 && code <= 0x2D25)
                    || (code == 0x2D27)
                    || (code == 0x2D2D)
                    || (code >= 0x2D30 && code <= 0x2D67)
                    || (code == 0x2D6F)
                    || (code >= 0x2D80 && code <= 0x2D96)

            else
                (code >= 0x2DA0 && code <= 0x2DA6)
                    || (code >= 0x2DA8 && code <= 0x2DAE)
                    || (code >= 0x2DB0 && code <= 0x2DB6)
                    || (code >= 0x2DB8 && code <= 0x2DBE)
                    || (code >= 0x2DC0 && code <= 0x2DC6)
                    || (code >= 0x2DC8 && code <= 0x2DCE)
                    || (code >= 0x2DD0 && code <= 0x2DD6)
                    || (code >= 0x2DD8 && code <= 0x2DDE)
                    || (code == 0x2E2F)
                    || (code >= 0x3005 && code <= 0x3006)

        else if code < 0xA67E then
            if code < 0x31EF then
                (code >= 0x3031 && code <= 0x3035)
                    || (code >= 0x303B && code <= 0x303C)
                    || (code >= 0x3041 && code <= 0x3096)
                    || (code >= 0x309D && code <= 0x309F)
                    || (code >= 0x30A1 && code <= 0x30FA)
                    || (code >= 0x30FC && code <= 0x30FF)
                    || (code >= 0x3105 && code <= 0x312F)
                    || (code >= 0x3131 && code <= 0x318E)
                    || (code >= 0x31A0 && code <= 0x31BF)

            else
                (code >= 0x31F0 && code <= 0x31FF)
                    || (code == 0x3400)
                    || (code == 0x4DBF)
                    || (code == 0x4E00)
                    || (code >= 0x9FFF && code <= 0xA48C)
                    || (code >= 0xA4D0 && code <= 0xA4FD)
                    || (code >= 0xA500 && code <= 0xA60C)
                    || (code >= 0xA610 && code <= 0xA61F)
                    || (code >= 0xA62A && code <= 0xA62B)
                    || (code >= 0xA640 && code <= 0xA66E)

        else if code < 0xA80B then
            (code >= 0xA67F && code <= 0xA69D)
                || (code >= 0xA6A0 && code <= 0xA6E5)
                || (code >= 0xA717 && code <= 0xA71F)
                || (code >= 0xA722 && code <= 0xA788)
                || (code >= 0xA78B && code <= 0xA7CA)
                || (code >= 0xA7D0 && code <= 0xA7D1)
                || (code >= 0xA7D6 && code <= 0xA7D9)
                || (code >= 0xA7F2 && code <= 0xA801)
                || (code >= 0xA803 && code <= 0xA805)
                || (code >= 0xA807 && code <= 0xA80A)
                || ((modBy 2 code == 1) && (code >= 0xA7D3 && code <= 0xA7D5))

        else
            (code >= 0xA80C && code <= 0xA822)
                || (code >= 0xA840 && code <= 0xA873)
                || (code >= 0xA882 && code <= 0xA8B3)
                || (code >= 0xA8F2 && code <= 0xA8F7)
                || (code == 0xA8FB)
                || (code >= 0xA8FD && code <= 0xA8FE)
                || (code >= 0xA90A && code <= 0xA925)
                || (code >= 0xA930 && code <= 0xA946)
                || (code >= 0xA960 && code <= 0xA97C)
                || (code >= 0xA984 && code <= 0xA9B2)

    else if code < 0x00011446 then
        if code < 0x00010596 then
            if code < 0xFB45 then
                if code < 0xAB1F then
                    if code < 0xAA7D then
                        (code == 0xA9CF)
                            || (code >= 0xA9E0 && code <= 0xA9E4)
                            || (code >= 0xA9E6 && code <= 0xA9EF)
                            || (code >= 0xA9FA && code <= 0xA9FE)
                            || (code >= 0xAA00 && code <= 0xAA28)
                            || (code >= 0xAA40 && code <= 0xAA42)
                            || (code >= 0xAA44 && code <= 0xAA4B)
                            || (code >= 0xAA60 && code <= 0xAA76)
                            || (code == 0xAA7A)

                    else
                        (code >= 0xAA7E && code <= 0xAAAF)
                            || (code == 0xAAB1)
                            || (code >= 0xAAB5 && code <= 0xAAB6)
                            || (code >= 0xAAB9 && code <= 0xAABD)
                            || (code >= 0xAADB && code <= 0xAADD)
                            || (code >= 0xAAE0 && code <= 0xAAEA)
                            || (code >= 0xAAF2 && code <= 0xAAF4)
                            || (code >= 0xAB01 && code <= 0xAB06)
                            || (code >= 0xAB09 && code <= 0xAB0E)
                            || (code >= 0xAB11 && code <= 0xAB16)
                            || ((modBy 2 code == 0)
                                    && (code >= 0xAAC0 && code <= 0xAAC2)
                               )

                else if code < 0xFA6F then
                    (code >= 0xAB20 && code <= 0xAB26)
                        || (code >= 0xAB28 && code <= 0xAB2E)
                        || (code >= 0xAB30 && code <= 0xAB5A)
                        || (code >= 0xAB5C && code <= 0xAB69)
                        || (code >= 0xAB70 && code <= 0xABE2)
                        || (code == 0xAC00)
                        || (code == 0xD7A3)
                        || (code >= 0xD7B0 && code <= 0xD7C6)
                        || (code >= 0xD7CB && code <= 0xD7FB)
                        || (code >= 0xF900 && code <= 0xFA6D)

                else
                    (code >= 0xFA70 && code <= 0xFAD9)
                        || (code >= 0xFB00 && code <= 0xFB06)
                        || (code >= 0xFB13 && code <= 0xFB17)
                        || (code == 0xFB1D)
                        || (code >= 0xFB1F && code <= 0xFB28)
                        || (code >= 0xFB2A && code <= 0xFB36)
                        || (code >= 0xFB38 && code <= 0xFB3C)
                        || (code == 0xFB3E)
                        || (code >= 0xFB40 && code <= 0xFB41)
                        || (code >= 0xFB43 && code <= 0xFB44)

            else if code < 0x0001004F then
                if code < 0xFF65 then
                    (code >= 0xFB46 && code <= 0xFBB1)
                        || (code >= 0xFBD3 && code <= 0xFD3D)
                        || (code >= 0xFD50 && code <= 0xFD8F)
                        || (code >= 0xFD92 && code <= 0xFDC7)
                        || (code >= 0xFDF0 && code <= 0xFDFB)
                        || (code >= 0xFE70 && code <= 0xFE74)
                        || (code >= 0xFE76 && code <= 0xFEFC)
                        || (code >= 0xFF21 && code <= 0xFF3A)
                        || (code >= 0xFF41 && code <= 0xFF5A)

                else
                    (code >= 0xFF66 && code <= 0xFFBE)
                        || (code >= 0xFFC2 && code <= 0xFFC7)
                        || (code >= 0xFFCA && code <= 0xFFCF)
                        || (code >= 0xFFD2 && code <= 0xFFD7)
                        || (code >= 0xFFDA && code <= 0xFFDC)
                        || (code >= 0x00010000 && code <= 0x0001000B)
                        || (code >= 0x0001000D && code <= 0x00010026)
                        || (code >= 0x00010028 && code <= 0x0001003A)
                        || (code >= 0x0001003C && code <= 0x0001003D)
                        || (code >= 0x0001003F && code <= 0x0001004D)

            else if code < 0x000103C7 then
                (code >= 0x00010050 && code <= 0x0001005D)
                    || (code >= 0x00010080 && code <= 0x000100FA)
                    || (code >= 0x00010280 && code <= 0x0001029C)
                    || (code >= 0x000102A0 && code <= 0x000102D0)
                    || (code >= 0x00010300 && code <= 0x0001031F)
                    || (code >= 0x0001032D && code <= 0x00010340)
                    || (code >= 0x00010342 && code <= 0x00010349)
                    || (code >= 0x00010350 && code <= 0x00010375)
                    || (code >= 0x00010380 && code <= 0x0001039D)
                    || (code >= 0x000103A0 && code <= 0x000103C3)

            else
                (code >= 0x000103C8 && code <= 0x000103CF)
                    || (code >= 0x00010400 && code <= 0x0001049D)
                    || (code >= 0x000104B0 && code <= 0x000104D3)
                    || (code >= 0x000104D8 && code <= 0x000104FB)
                    || (code >= 0x00010500 && code <= 0x00010527)
                    || (code >= 0x00010530 && code <= 0x00010563)
                    || (code >= 0x00010570 && code <= 0x0001057A)
                    || (code >= 0x0001057C && code <= 0x0001058A)
                    || (code >= 0x0001058C && code <= 0x00010592)
                    || (code >= 0x00010594 && code <= 0x00010595)

        else if code < 0x00010CFF then
            if code < 0x000108F3 then
                if code < 0x000107B1 then
                    (code >= 0x00010597 && code <= 0x000105A1)
                        || (code >= 0x000105A3 && code <= 0x000105B1)
                        || (code >= 0x000105B3 && code <= 0x000105B9)
                        || (code >= 0x000105BB && code <= 0x000105BC)
                        || (code >= 0x00010600 && code <= 0x00010736)
                        || (code >= 0x00010740 && code <= 0x00010755)
                        || (code >= 0x00010760 && code <= 0x00010767)
                        || (code >= 0x00010780 && code <= 0x00010785)
                        || (code >= 0x00010787 && code <= 0x000107B0)

                else
                    (code >= 0x000107B2 && code <= 0x000107BA)
                        || (code >= 0x00010800 && code <= 0x00010805)
                        || (code == 0x00010808)
                        || (code >= 0x0001080A && code <= 0x00010835)
                        || (code >= 0x00010837 && code <= 0x00010838)
                        || (code == 0x0001083C)
                        || (code >= 0x0001083F && code <= 0x00010855)
                        || (code >= 0x00010860 && code <= 0x00010876)
                        || (code >= 0x00010880 && code <= 0x0001089E)
                        || (code >= 0x000108E0 && code <= 0x000108F2)

            else if code < 0x00010A7F then
                (code >= 0x000108F4 && code <= 0x000108F5)
                    || (code >= 0x00010900 && code <= 0x00010915)
                    || (code >= 0x00010920 && code <= 0x00010939)
                    || (code >= 0x00010980 && code <= 0x000109B7)
                    || (code >= 0x000109BE && code <= 0x000109BF)
                    || (code == 0x00010A00)
                    || (code >= 0x00010A10 && code <= 0x00010A13)
                    || (code >= 0x00010A15 && code <= 0x00010A17)
                    || (code >= 0x00010A19 && code <= 0x00010A35)
                    || (code >= 0x00010A60 && code <= 0x00010A7C)

            else
                (code >= 0x00010A80 && code <= 0x00010A9C)
                    || (code >= 0x00010AC0 && code <= 0x00010AC7)
                    || (code >= 0x00010AC9 && code <= 0x00010AE4)
                    || (code >= 0x00010B00 && code <= 0x00010B35)
                    || (code >= 0x00010B40 && code <= 0x00010B55)
                    || (code >= 0x00010B60 && code <= 0x00010B72)
                    || (code >= 0x00010B80 && code <= 0x00010B91)
                    || (code >= 0x00010C00 && code <= 0x00010C48)
                    || (code >= 0x00010C80 && code <= 0x00010CB2)
                    || (code >= 0x00010CC0 && code <= 0x00010CF2)

        else if code < 0x00011182 then
            if code < 0x00011002 then
                (code >= 0x00010D00 && code <= 0x00010D23)
                    || (code >= 0x00010E80 && code <= 0x00010EA9)
                    || (code >= 0x00010EB0 && code <= 0x00010EB1)
                    || (code >= 0x00010F00 && code <= 0x00010F1C)
                    || (code == 0x00010F27)
                    || (code >= 0x00010F30 && code <= 0x00010F45)
                    || (code >= 0x00010F70 && code <= 0x00010F81)
                    || (code >= 0x00010FB0 && code <= 0x00010FC4)
                    || (code >= 0x00010FE0 && code <= 0x00010FF6)

            else
                (code >= 0x00011003 && code <= 0x00011037)
                    || (code >= 0x00011071 && code <= 0x00011072)
                    || (code == 0x00011075)
                    || (code >= 0x00011083 && code <= 0x000110AF)
                    || (code >= 0x000110D0 && code <= 0x000110E8)
                    || (code >= 0x00011103 && code <= 0x00011126)
                    || (code == 0x00011144)
                    || (code == 0x00011147)
                    || (code >= 0x00011150 && code <= 0x00011172)
                    || (code == 0x00011176)

        else if code < 0x00011304 then
            (code >= 0x00011183 && code <= 0x000111B2)
                || (code >= 0x000111C1 && code <= 0x000111C4)
                || (code >= 0x00011200 && code <= 0x00011211)
                || (code >= 0x00011213 && code <= 0x0001122B)
                || (code >= 0x00011280 && code <= 0x00011286)
                || (code == 0x00011288)
                || (code >= 0x0001128A && code <= 0x0001128D)
                || (code >= 0x0001128F && code <= 0x0001129D)
                || (code >= 0x0001129F && code <= 0x000112A8)
                || (code >= 0x000112B0 && code <= 0x000112DE)
                || ((modBy 2 code == 0)
                        && (code >= 0x000111DA && code <= 0x000111DC)
                   )

        else
            (code >= 0x00011305 && code <= 0x0001130C)
                || (code >= 0x0001130F && code <= 0x00011310)
                || (code >= 0x00011313 && code <= 0x00011328)
                || (code >= 0x0001132A && code <= 0x00011330)
                || (code >= 0x00011332 && code <= 0x00011333)
                || (code >= 0x00011335 && code <= 0x00011339)
                || (code == 0x0001133D)
                || (code == 0x00011350)
                || (code >= 0x0001135D && code <= 0x00011361)
                || (code >= 0x00011400 && code <= 0x00011434)

    else if code < 0x0001D3FF then
        if code < 0x00011D69 then
            if code < 0x00011917 then
                if code < 0x0001167F then
                    (code >= 0x00011447 && code <= 0x0001144A)
                        || (code >= 0x0001145F && code <= 0x00011461)
                        || (code >= 0x00011480 && code <= 0x000114AF)
                        || (code >= 0x000114C4 && code <= 0x000114C5)
                        || (code == 0x000114C7)
                        || (code >= 0x00011580 && code <= 0x000115AE)
                        || (code >= 0x000115D8 && code <= 0x000115DB)
                        || (code >= 0x00011600 && code <= 0x0001162F)
                        || (code == 0x00011644)

                else
                    (code >= 0x00011680 && code <= 0x000116AA)
                        || (code == 0x000116B8)
                        || (code >= 0x00011700 && code <= 0x0001171A)
                        || (code >= 0x00011740 && code <= 0x00011746)
                        || (code >= 0x00011800 && code <= 0x0001182B)
                        || (code >= 0x000118A0 && code <= 0x000118DF)
                        || (code >= 0x000118FF && code <= 0x00011906)
                        || (code == 0x00011909)
                        || (code >= 0x0001190C && code <= 0x00011913)
                        || (code >= 0x00011915 && code <= 0x00011916)

            else if code < 0x00011BFF then
                (code >= 0x00011918 && code <= 0x0001192F)
                    || (code >= 0x000119A0 && code <= 0x000119A7)
                    || (code >= 0x000119AA && code <= 0x000119D0)
                    || (code == 0x00011A00)
                    || (code >= 0x00011A0B && code <= 0x00011A32)
                    || (code == 0x00011A3A)
                    || (code == 0x00011A50)
                    || (code >= 0x00011A5C && code <= 0x00011A89)
                    || (code == 0x00011A9D)
                    || (code >= 0x00011AB0 && code <= 0x00011AF8)
                    || ((modBy 2 code == 1)
                            && ((code >= 0x0001193F && code <= 0x00011941)
                                    || (code >= 0x000119E1 && code <= 0x000119E3)
                               )
                       )

            else
                (code >= 0x00011C00 && code <= 0x00011C08)
                    || (code >= 0x00011C0A && code <= 0x00011C2E)
                    || (code == 0x00011C40)
                    || (code >= 0x00011C72 && code <= 0x00011C8F)
                    || (code >= 0x00011D00 && code <= 0x00011D06)
                    || (code >= 0x00011D08 && code <= 0x00011D09)
                    || (code >= 0x00011D0B && code <= 0x00011D30)
                    || (code == 0x00011D46)
                    || (code >= 0x00011D60 && code <= 0x00011D65)
                    || (code >= 0x00011D67 && code <= 0x00011D68)

        else if code < 0x00016F4F then
            if code < 0x000167FF then
                (code >= 0x00011D6A && code <= 0x00011D89)
                    || (code == 0x00011D98)
                    || (code >= 0x00011EE0 && code <= 0x00011EF2)
                    || (code == 0x00011FB0)
                    || (code >= 0x00012000 && code <= 0x00012399)
                    || (code >= 0x00012480 && code <= 0x00012543)
                    || (code >= 0x00012F90 && code <= 0x00012FF0)
                    || (code >= 0x00013000 && code <= 0x0001342E)
                    || (code >= 0x00014400 && code <= 0x00014646)

            else
                (code >= 0x00016800 && code <= 0x00016A38)
                    || (code >= 0x00016A40 && code <= 0x00016A5E)
                    || (code >= 0x00016A70 && code <= 0x00016ABE)
                    || (code >= 0x00016AD0 && code <= 0x00016AED)
                    || (code >= 0x00016B00 && code <= 0x00016B2F)
                    || (code >= 0x00016B40 && code <= 0x00016B43)
                    || (code >= 0x00016B63 && code <= 0x00016B77)
                    || (code >= 0x00016B7D && code <= 0x00016B8F)
                    || (code >= 0x00016E40 && code <= 0x00016E7F)
                    || (code >= 0x00016F00 && code <= 0x00016F4A)

        else if code < 0x0001AFF4 then
            (code == 0x00016F50)
                || (code >= 0x00016F93 && code <= 0x00016F9F)
                || (code >= 0x00016FE0 && code <= 0x00016FE1)
                || (code == 0x00016FE3)
                || (code == 0x00017000)
                || (code == 0x000187F7)
                || (code >= 0x00018800 && code <= 0x00018CD5)
                || (code == 0x00018D00)
                || (code == 0x00018D08)
                || (code >= 0x0001AFF0 && code <= 0x0001AFF3)

        else
            (code >= 0x0001AFF5 && code <= 0x0001AFFB)
                || (code >= 0x0001AFFD && code <= 0x0001AFFE)
                || (code >= 0x0001B000 && code <= 0x0001B122)
                || (code >= 0x0001B150 && code <= 0x0001B152)
                || (code >= 0x0001B164 && code <= 0x0001B167)
                || (code >= 0x0001B170 && code <= 0x0001B2FB)
                || (code >= 0x0001BC00 && code <= 0x0001BC6A)
                || (code >= 0x0001BC70 && code <= 0x0001BC7C)
                || (code >= 0x0001BC80 && code <= 0x0001BC88)
                || (code >= 0x0001BC90 && code <= 0x0001BC99)

    else if code < 0x0001E7EF then
        if code < 0x0001D6A7 then
            if code < 0x0001D4C4 then
                (code >= 0x0001D400 && code <= 0x0001D454)
                    || (code >= 0x0001D456 && code <= 0x0001D49C)
                    || (code >= 0x0001D49E && code <= 0x0001D49F)
                    || (code == 0x0001D4A2)
                    || (code >= 0x0001D4A5 && code <= 0x0001D4A6)
                    || (code >= 0x0001D4A9 && code <= 0x0001D4AC)
                    || (code >= 0x0001D4AE && code <= 0x0001D4B9)
                    || (code == 0x0001D4BB)
                    || (code >= 0x0001D4BD && code <= 0x0001D4C3)

            else
                (code >= 0x0001D4C5 && code <= 0x0001D505)
                    || (code >= 0x0001D507 && code <= 0x0001D50A)
                    || (code >= 0x0001D50D && code <= 0x0001D514)
                    || (code >= 0x0001D516 && code <= 0x0001D51C)
                    || (code >= 0x0001D51E && code <= 0x0001D539)
                    || (code >= 0x0001D53B && code <= 0x0001D53E)
                    || (code >= 0x0001D540 && code <= 0x0001D544)
                    || (code == 0x0001D546)
                    || (code >= 0x0001D54A && code <= 0x0001D550)
                    || (code >= 0x0001D552 && code <= 0x0001D6A5)

        else if code < 0x0001D7C3 then
            (code >= 0x0001D6A8 && code <= 0x0001D6C0)
                || (code >= 0x0001D6C2 && code <= 0x0001D6DA)
                || (code >= 0x0001D6DC && code <= 0x0001D6FA)
                || (code >= 0x0001D6FC && code <= 0x0001D714)
                || (code >= 0x0001D716 && code <= 0x0001D734)
                || (code >= 0x0001D736 && code <= 0x0001D74E)
                || (code >= 0x0001D750 && code <= 0x0001D76E)
                || (code >= 0x0001D770 && code <= 0x0001D788)
                || (code >= 0x0001D78A && code <= 0x0001D7A8)
                || (code >= 0x0001D7AA && code <= 0x0001D7C2)

        else
            (code >= 0x0001D7C4 && code <= 0x0001D7CB)
                || (code >= 0x0001DF00 && code <= 0x0001DF1E)
                || (code >= 0x0001E100 && code <= 0x0001E12C)
                || (code >= 0x0001E137 && code <= 0x0001E13D)
                || (code == 0x0001E14E)
                || (code >= 0x0001E290 && code <= 0x0001E2AD)
                || (code >= 0x0001E2C0 && code <= 0x0001E2EB)
                || (code >= 0x0001E7E0 && code <= 0x0001E7E6)
                || (code >= 0x0001E7E8 && code <= 0x0001E7EB)
                || (code >= 0x0001E7ED && code <= 0x0001E7EE)

    else if code < 0x0001EE78 then
        if code < 0x0001EE33 then
            (code >= 0x0001E7F0 && code <= 0x0001E7FE)
                || (code >= 0x0001E800 && code <= 0x0001E8C4)
                || (code >= 0x0001E900 && code <= 0x0001E943)
                || (code == 0x0001E94B)
                || (code >= 0x0001EE00 && code <= 0x0001EE03)
                || (code >= 0x0001EE05 && code <= 0x0001EE1F)
                || (code >= 0x0001EE21 && code <= 0x0001EE22)
                || (code == 0x0001EE24)
                || (code == 0x0001EE27)
                || (code >= 0x0001EE29 && code <= 0x0001EE32)

        else
            (code >= 0x0001EE34 && code <= 0x0001EE37)
                || (code == 0x0001EE42)
                || (code >= 0x0001EE4D && code <= 0x0001EE4F)
                || (code >= 0x0001EE51 && code <= 0x0001EE52)
                || (code == 0x0001EE54)
                || (code >= 0x0001EE61 && code <= 0x0001EE62)
                || (code == 0x0001EE64)
                || (code >= 0x0001EE67 && code <= 0x0001EE6A)
                || (code >= 0x0001EE6C && code <= 0x0001EE72)
                || (code >= 0x0001EE74 && code <= 0x0001EE77)
                || ((modBy 2 code == 1)
                        && ((code >= 0x0001EE39 && code <= 0x0001EE3B)
                                || (code >= 0x0001EE47 && code <= 0x0001EE4B)
                                || (code >= 0x0001EE57 && code <= 0x0001EE5F)
                           )
                   )

    else if code < 0x0002B737 then
        (code >= 0x0001EE79 && code <= 0x0001EE7C)
            || (code == 0x0001EE7E)
            || (code >= 0x0001EE80 && code <= 0x0001EE89)
            || (code >= 0x0001EE8B && code <= 0x0001EE9B)
            || (code >= 0x0001EEA1 && code <= 0x0001EEA3)
            || (code >= 0x0001EEA5 && code <= 0x0001EEA9)
            || (code >= 0x0001EEAB && code <= 0x0001EEBB)
            || (code == 0x00020000)
            || (code == 0x0002A6DF)
            || (code == 0x0002A700)

    else
        (code == 0x0002B738)
            || (code == 0x0002B740)
            || (code == 0x0002B81D)
            || (code == 0x0002B820)
            || (code == 0x0002CEA1)
            || (code == 0x0002CEB0)
            || (code == 0x0002EBE0)
            || (code >= 0x0002F800 && code <= 0x0002FA1D)
            || (code == 0x00030000)
            || (code == 0x0003134A)


{-| Detect digits (Unicode categories Nd, Nl, No)
-}
isDigit : Char -> Bool
isDigit c =
    let
        code =
            Char.toCode c
    in
    if code < 0x0100 then
        (code >= 0x30 && code <= 0x39)
            || (code >= 0xB2 && code <= 0xB3)
            || (code == 0xB9)
            || (code >= 0xBC && code <= 0xBE)

    else if code < 0x00010189 then
        if code < 0x1BAF then
            if code < 0x0DE5 then
                if code < 0x0B65 then
                    (code >= 0x0660 && code <= 0x0669)
                        || (code >= 0x06F0 && code <= 0x06F9)
                        || (code >= 0x07C0 && code <= 0x07C9)
                        || (code >= 0x0966 && code <= 0x096F)
                        || (code >= 0x09E6 && code <= 0x09EF)
                        || (code >= 0x09F4 && code <= 0x09F9)
                        || (code >= 0x0A66 && code <= 0x0A6F)
                        || (code >= 0x0AE6 && code <= 0x0AEF)

                else
                    (code >= 0x0B66 && code <= 0x0B6F)
                        || (code >= 0x0B72 && code <= 0x0B77)
                        || (code >= 0x0BE6 && code <= 0x0BF2)
                        || (code >= 0x0C66 && code <= 0x0C6F)
                        || (code >= 0x0C78 && code <= 0x0C7E)
                        || (code >= 0x0CE6 && code <= 0x0CEF)
                        || (code >= 0x0D58 && code <= 0x0D5E)
                        || (code >= 0x0D66 && code <= 0x0D78)

            else if code < 0x17DF then
                (code >= 0x0DE6 && code <= 0x0DEF)
                    || (code >= 0x0E50 && code <= 0x0E59)
                    || (code >= 0x0ED0 && code <= 0x0ED9)
                    || (code >= 0x0F20 && code <= 0x0F33)
                    || (code >= 0x1040 && code <= 0x1049)
                    || (code >= 0x1090 && code <= 0x1099)
                    || (code >= 0x1369 && code <= 0x137C)
                    || (code >= 0x16EE && code <= 0x16F0)

            else
                (code >= 0x17E0 && code <= 0x17E9)
                    || (code >= 0x17F0 && code <= 0x17F9)
                    || (code >= 0x1810 && code <= 0x1819)
                    || (code >= 0x1946 && code <= 0x194F)
                    || (code >= 0x19D0 && code <= 0x19DA)
                    || (code >= 0x1A80 && code <= 0x1A89)
                    || (code >= 0x1A90 && code <= 0x1A99)
                    || (code >= 0x1B50 && code <= 0x1B59)

        else if code < 0x321F then
            if code < 0x245F then
                (code >= 0x1BB0 && code <= 0x1BB9)
                    || (code >= 0x1C40 && code <= 0x1C49)
                    || (code >= 0x1C50 && code <= 0x1C59)
                    || (code == 0x2070)
                    || (code >= 0x2074 && code <= 0x2079)
                    || (code >= 0x2080 && code <= 0x2089)
                    || (code >= 0x2150 && code <= 0x2182)
                    || (code >= 0x2185 && code <= 0x2189)

            else
                (code >= 0x2460 && code <= 0x249B)
                    || (code >= 0x24EA && code <= 0x24FF)
                    || (code >= 0x2776 && code <= 0x2793)
                    || (code == 0x2CFD)
                    || (code == 0x3007)
                    || (code >= 0x3021 && code <= 0x3029)
                    || (code >= 0x3038 && code <= 0x303A)
                    || (code >= 0x3192 && code <= 0x3195)

        else if code < 0xA8CF then
            (code >= 0x3220 && code <= 0x3229)
                || (code >= 0x3248 && code <= 0x324F)
                || (code >= 0x3251 && code <= 0x325F)
                || (code >= 0x3280 && code <= 0x3289)
                || (code >= 0x32B1 && code <= 0x32BF)
                || (code >= 0xA620 && code <= 0xA629)
                || (code >= 0xA6E6 && code <= 0xA6EF)
                || (code >= 0xA830 && code <= 0xA835)

        else
            (code >= 0xA8D0 && code <= 0xA8D9)
                || (code >= 0xA900 && code <= 0xA909)
                || (code >= 0xA9D0 && code <= 0xA9D9)
                || (code >= 0xA9F0 && code <= 0xA9F9)
                || (code >= 0xAA50 && code <= 0xAA59)
                || (code >= 0xABF0 && code <= 0xABF9)
                || (code >= 0xFF10 && code <= 0xFF19)
                || (code >= 0x00010107 && code <= 0x00010133)
                || (code >= 0x00010140 && code <= 0x00010178)

    else if code < 0x000111E0 then
        if code < 0x00010A7C then
            if code < 0x00010878 then
                (code >= 0x0001018A && code <= 0x0001018B)
                    || (code >= 0x000102E1 && code <= 0x000102FB)
                    || (code >= 0x00010320 && code <= 0x00010323)
                    || (code == 0x00010341)
                    || (code == 0x0001034A)
                    || (code >= 0x000103D1 && code <= 0x000103D5)
                    || (code >= 0x000104A0 && code <= 0x000104A9)
                    || (code >= 0x00010858 && code <= 0x0001085F)

            else
                (code >= 0x00010879 && code <= 0x0001087F)
                    || (code >= 0x000108A7 && code <= 0x000108AF)
                    || (code >= 0x000108FB && code <= 0x000108FF)
                    || (code >= 0x00010916 && code <= 0x0001091B)
                    || (code >= 0x000109BC && code <= 0x000109BD)
                    || (code >= 0x000109C0 && code <= 0x000109CF)
                    || (code >= 0x000109D2 && code <= 0x000109FF)
                    || (code >= 0x00010A40 && code <= 0x00010A48)

        else if code < 0x00010E5F then
            (code >= 0x00010A7D && code <= 0x00010A7E)
                || (code >= 0x00010A9D && code <= 0x00010A9F)
                || (code >= 0x00010AEB && code <= 0x00010AEF)
                || (code >= 0x00010B58 && code <= 0x00010B5F)
                || (code >= 0x00010B78 && code <= 0x00010B7F)
                || (code >= 0x00010BA9 && code <= 0x00010BAF)
                || (code >= 0x00010CFA && code <= 0x00010CFF)
                || (code >= 0x00010D30 && code <= 0x00010D39)

        else
            (code >= 0x00010E60 && code <= 0x00010E7E)
                || (code >= 0x00010F1D && code <= 0x00010F26)
                || (code >= 0x00010F51 && code <= 0x00010F54)
                || (code >= 0x00010FC5 && code <= 0x00010FCB)
                || (code >= 0x00011052 && code <= 0x0001106F)
                || (code >= 0x000110F0 && code <= 0x000110F9)
                || (code >= 0x00011136 && code <= 0x0001113F)
                || (code >= 0x000111D0 && code <= 0x000111D9)

    else if code < 0x00016B4F then
        if code < 0x0001194F then
            (code >= 0x000111E1 && code <= 0x000111F4)
                || (code >= 0x000112F0 && code <= 0x000112F9)
                || (code >= 0x00011450 && code <= 0x00011459)
                || (code >= 0x000114D0 && code <= 0x000114D9)
                || (code >= 0x00011650 && code <= 0x00011659)
                || (code >= 0x000116C0 && code <= 0x000116C9)
                || (code >= 0x00011730 && code <= 0x0001173B)
                || (code >= 0x000118E0 && code <= 0x000118F2)

        else
            (code >= 0x00011950 && code <= 0x00011959)
                || (code >= 0x00011C50 && code <= 0x00011C6C)
                || (code >= 0x00011D50 && code <= 0x00011D59)
                || (code >= 0x00011DA0 && code <= 0x00011DA9)
                || (code >= 0x00011FC0 && code <= 0x00011FD4)
                || (code >= 0x00012400 && code <= 0x0001246E)
                || (code >= 0x00016A60 && code <= 0x00016A69)
                || (code >= 0x00016AC0 && code <= 0x00016AC9)

    else if code < 0x0001E8C6 then
        (code >= 0x00016B50 && code <= 0x00016B59)
            || (code >= 0x00016B5B && code <= 0x00016B61)
            || (code >= 0x00016E80 && code <= 0x00016E96)
            || (code >= 0x0001D2E0 && code <= 0x0001D2F3)
            || (code >= 0x0001D360 && code <= 0x0001D378)
            || (code >= 0x0001D7CE && code <= 0x0001D7FF)
            || (code >= 0x0001E140 && code <= 0x0001E149)
            || (code >= 0x0001E2F0 && code <= 0x0001E2F9)

    else
        (code >= 0x0001E8C7 && code <= 0x0001E8CF)
            || (code >= 0x0001E950 && code <= 0x0001E959)
            || (code >= 0x0001EC71 && code <= 0x0001ECAB)
            || (code >= 0x0001ECAD && code <= 0x0001ECAF)
            || (code >= 0x0001ECB1 && code <= 0x0001ECB4)
            || (code >= 0x0001ED01 && code <= 0x0001ED2D)
            || (code >= 0x0001ED2F && code <= 0x0001ED3D)
            || (code >= 0x0001F100 && code <= 0x0001F10C)
            || (code >= 0x0001FBF0 && code <= 0x0001FBF9)


{-| Detect letters or digits (Unicode categories Lu, Ll, Lt, Lm, Lo, Nd, Nl, No)
-}
isAlphaNum : Char -> Bool
isAlphaNum c =
    let
        code =
            Char.toCode c
    in
    if code < 0x0100 then
        (code >= 0x30 && code <= 0x39)
            || (code >= 0x41 && code <= 0x5A)
            || (code >= 0x61 && code <= 0x7A)
            || (code == 0xAA)
            || (code >= 0xB2 && code <= 0xB3)
            || (code == 0xB5)
            || (code >= 0xB9 && code <= 0xBA)
            || (code >= 0xBC && code <= 0xBE)
            || (code >= 0xC0 && code <= 0xD6)
            || (code >= 0xD8 && code <= 0xF6)
            || (code >= 0xF8 && code <= 0xFF)

    else if code < 0xAA5F then
        if code < 0x10CF then
            if code < 0x0B31 then
                if code < 0x093C then
                    if code < 0x0670 then
                        if code < 0x03A2 then
                            (code >= 0x0100 && code <= 0x02C1)
                                || (code >= 0x02C6 && code <= 0x02D1)
                                || (code >= 0x02E0 && code <= 0x02E4)
                                || (code >= 0x0370 && code <= 0x0374)
                                || (code >= 0x0376 && code <= 0x0377)
                                || (code >= 0x037A && code <= 0x037D)
                                || (code == 0x037F)
                                || (code == 0x0386)
                                || (code >= 0x0388 && code <= 0x038A)
                                || (code == 0x038C)
                                || (code >= 0x038E && code <= 0x03A1)
                                || ((modBy 2 code == 0)
                                        && (code >= 0x02EC && code <= 0x02EE)
                                   )

                        else
                            (code >= 0x03A3 && code <= 0x03F5)
                                || (code >= 0x03F7 && code <= 0x0481)
                                || (code >= 0x048A && code <= 0x052F)
                                || (code >= 0x0531 && code <= 0x0556)
                                || (code == 0x0559)
                                || (code >= 0x0560 && code <= 0x0588)
                                || (code >= 0x05D0 && code <= 0x05EA)
                                || (code >= 0x05EF && code <= 0x05F2)
                                || (code >= 0x0620 && code <= 0x064A)
                                || (code >= 0x0660 && code <= 0x0669)
                                || (code >= 0x066E && code <= 0x066F)

                    else if code < 0x07F9 then
                        (code >= 0x0671 && code <= 0x06D3)
                            || (code == 0x06D5)
                            || (code >= 0x06E5 && code <= 0x06E6)
                            || (code >= 0x06EE && code <= 0x06FC)
                            || (code == 0x06FF)
                            || (code == 0x0710)
                            || (code >= 0x0712 && code <= 0x072F)
                            || (code >= 0x074D && code <= 0x07A5)
                            || (code == 0x07B1)
                            || (code >= 0x07C0 && code <= 0x07EA)
                            || (code >= 0x07F4 && code <= 0x07F5)

                    else
                        (code == 0x07FA)
                            || (code >= 0x0800 && code <= 0x0815)
                            || (code == 0x081A)
                            || (code == 0x0824)
                            || (code == 0x0828)
                            || (code >= 0x0840 && code <= 0x0858)
                            || (code >= 0x0860 && code <= 0x086A)
                            || (code >= 0x0870 && code <= 0x0887)
                            || (code >= 0x0889 && code <= 0x088E)
                            || (code >= 0x08A0 && code <= 0x08C9)
                            || (code >= 0x0904 && code <= 0x0939)

                else if code < 0x0A31 then
                    if code < 0x09BC then
                        (code == 0x093D)
                            || (code == 0x0950)
                            || (code >= 0x0958 && code <= 0x0961)
                            || (code >= 0x0966 && code <= 0x096F)
                            || (code >= 0x0971 && code <= 0x0980)
                            || (code >= 0x0985 && code <= 0x098C)
                            || (code >= 0x098F && code <= 0x0990)
                            || (code >= 0x0993 && code <= 0x09A8)
                            || (code >= 0x09AA && code <= 0x09B0)
                            || (code == 0x09B2)
                            || (code >= 0x09B6 && code <= 0x09B9)

                    else
                        (code == 0x09BD)
                            || (code == 0x09CE)
                            || (code >= 0x09DC && code <= 0x09DD)
                            || (code >= 0x09DF && code <= 0x09E1)
                            || (code >= 0x09E6 && code <= 0x09F1)
                            || (code >= 0x09F4 && code <= 0x09F9)
                            || (code == 0x09FC)
                            || (code >= 0x0A05 && code <= 0x0A0A)
                            || (code >= 0x0A0F && code <= 0x0A10)
                            || (code >= 0x0A13 && code <= 0x0A28)
                            || (code >= 0x0A2A && code <= 0x0A30)

                else if code < 0x0AB1 then
                    (code >= 0x0A32 && code <= 0x0A33)
                        || (code >= 0x0A35 && code <= 0x0A36)
                        || (code >= 0x0A38 && code <= 0x0A39)
                        || (code >= 0x0A59 && code <= 0x0A5C)
                        || (code == 0x0A5E)
                        || (code >= 0x0A66 && code <= 0x0A6F)
                        || (code >= 0x0A72 && code <= 0x0A74)
                        || (code >= 0x0A85 && code <= 0x0A8D)
                        || (code >= 0x0A8F && code <= 0x0A91)
                        || (code >= 0x0A93 && code <= 0x0AA8)
                        || (code >= 0x0AAA && code <= 0x0AB0)

                else
                    (code >= 0x0AB2 && code <= 0x0AB3)
                        || (code >= 0x0AB5 && code <= 0x0AB9)
                        || (code == 0x0ABD)
                        || (code == 0x0AD0)
                        || (code >= 0x0AE0 && code <= 0x0AE1)
                        || (code >= 0x0AE6 && code <= 0x0AEF)
                        || (code == 0x0AF9)
                        || (code >= 0x0B05 && code <= 0x0B0C)
                        || (code >= 0x0B0F && code <= 0x0B10)
                        || (code >= 0x0B13 && code <= 0x0B28)
                        || (code >= 0x0B2A && code <= 0x0B30)

            else if code < 0x0D4D then
                if code < 0x0C29 then
                    if code < 0x0B98 then
                        (code >= 0x0B32 && code <= 0x0B33)
                            || (code >= 0x0B35 && code <= 0x0B39)
                            || (code == 0x0B3D)
                            || (code >= 0x0B5C && code <= 0x0B5D)
                            || (code >= 0x0B5F && code <= 0x0B61)
                            || (code >= 0x0B66 && code <= 0x0B6F)
                            || (code >= 0x0B71 && code <= 0x0B77)
                            || (code == 0x0B83)
                            || (code >= 0x0B85 && code <= 0x0B8A)
                            || (code >= 0x0B8E && code <= 0x0B90)
                            || (code >= 0x0B92 && code <= 0x0B95)

                    else
                        (code >= 0x0B99 && code <= 0x0B9A)
                            || (code == 0x0B9C)
                            || (code >= 0x0B9E && code <= 0x0B9F)
                            || (code >= 0x0BA3 && code <= 0x0BA4)
                            || (code >= 0x0BA8 && code <= 0x0BAA)
                            || (code >= 0x0BAE && code <= 0x0BB9)
                            || (code == 0x0BD0)
                            || (code >= 0x0BE6 && code <= 0x0BF2)
                            || (code >= 0x0C05 && code <= 0x0C0C)
                            || (code >= 0x0C0E && code <= 0x0C10)
                            || (code >= 0x0C12 && code <= 0x0C28)

                else if code < 0x0CA9 then
                    (code >= 0x0C2A && code <= 0x0C39)
                        || (code == 0x0C3D)
                        || (code >= 0x0C58 && code <= 0x0C5A)
                        || (code == 0x0C5D)
                        || (code >= 0x0C60 && code <= 0x0C61)
                        || (code >= 0x0C66 && code <= 0x0C6F)
                        || (code >= 0x0C78 && code <= 0x0C7E)
                        || (code == 0x0C80)
                        || (code >= 0x0C85 && code <= 0x0C8C)
                        || (code >= 0x0C8E && code <= 0x0C90)
                        || (code >= 0x0C92 && code <= 0x0CA8)

                else
                    (code >= 0x0CAA && code <= 0x0CB3)
                        || (code >= 0x0CB5 && code <= 0x0CB9)
                        || (code == 0x0CBD)
                        || (code >= 0x0CDD && code <= 0x0CDE)
                        || (code >= 0x0CE0 && code <= 0x0CE1)
                        || (code >= 0x0CE6 && code <= 0x0CEF)
                        || (code >= 0x0CF1 && code <= 0x0CF2)
                        || (code >= 0x0D04 && code <= 0x0D0C)
                        || (code >= 0x0D0E && code <= 0x0D10)
                        || (code >= 0x0D12 && code <= 0x0D3A)
                        || (code == 0x0D3D)

            else if code < 0x0EBC then
                if code < 0x0E00 then
                    (code == 0x0D4E)
                        || (code >= 0x0D54 && code <= 0x0D56)
                        || (code >= 0x0D58 && code <= 0x0D61)
                        || (code >= 0x0D66 && code <= 0x0D78)
                        || (code >= 0x0D7A && code <= 0x0D7F)
                        || (code >= 0x0D85 && code <= 0x0D96)
                        || (code >= 0x0D9A && code <= 0x0DB1)
                        || (code >= 0x0DB3 && code <= 0x0DBB)
                        || (code == 0x0DBD)
                        || (code >= 0x0DC0 && code <= 0x0DC6)
                        || (code >= 0x0DE6 && code <= 0x0DEF)

                else
                    (code >= 0x0E01 && code <= 0x0E30)
                        || (code >= 0x0E32 && code <= 0x0E33)
                        || (code >= 0x0E40 && code <= 0x0E46)
                        || (code >= 0x0E50 && code <= 0x0E59)
                        || (code >= 0x0E81 && code <= 0x0E82)
                        || (code == 0x0E84)
                        || (code >= 0x0E86 && code <= 0x0E8A)
                        || (code >= 0x0E8C && code <= 0x0EA3)
                        || (code == 0x0EA5)
                        || (code >= 0x0EA7 && code <= 0x0EB0)
                        || (code >= 0x0EB2 && code <= 0x0EB3)

            else if code < 0x103E then
                (code == 0x0EBD)
                    || (code >= 0x0EC0 && code <= 0x0EC4)
                    || (code == 0x0EC6)
                    || (code >= 0x0ED0 && code <= 0x0ED9)
                    || (code >= 0x0EDC && code <= 0x0EDF)
                    || (code == 0x0F00)
                    || (code >= 0x0F20 && code <= 0x0F33)
                    || (code >= 0x0F40 && code <= 0x0F47)
                    || (code >= 0x0F49 && code <= 0x0F6C)
                    || (code >= 0x0F88 && code <= 0x0F8C)
                    || (code >= 0x1000 && code <= 0x102A)

            else
                (code >= 0x103F && code <= 0x1049)
                    || (code >= 0x1050 && code <= 0x1055)
                    || (code >= 0x105A && code <= 0x105D)
                    || (code == 0x1061)
                    || (code >= 0x1065 && code <= 0x1066)
                    || (code >= 0x106E && code <= 0x1070)
                    || (code >= 0x1075 && code <= 0x1081)
                    || (code == 0x108E)
                    || (code >= 0x1090 && code <= 0x1099)
                    || (code >= 0x10A0 && code <= 0x10C5)
                    || (code == 0x10C7)
                    || (code == 0x10CD)

        else if code < 0x208F then
            if code < 0x196F then
                if code < 0x166E then
                    if code < 0x12BF then
                        (code >= 0x10D0 && code <= 0x10FA)
                            || (code >= 0x10FC && code <= 0x1248)
                            || (code >= 0x124A && code <= 0x124D)
                            || (code >= 0x1250 && code <= 0x1256)
                            || (code == 0x1258)
                            || (code >= 0x125A && code <= 0x125D)
                            || (code >= 0x1260 && code <= 0x1288)
                            || (code >= 0x128A && code <= 0x128D)
                            || (code >= 0x1290 && code <= 0x12B0)
                            || (code >= 0x12B2 && code <= 0x12B5)
                            || (code >= 0x12B8 && code <= 0x12BE)

                    else
                        (code == 0x12C0)
                            || (code >= 0x12C2 && code <= 0x12C5)
                            || (code >= 0x12C8 && code <= 0x12D6)
                            || (code >= 0x12D8 && code <= 0x1310)
                            || (code >= 0x1312 && code <= 0x1315)
                            || (code >= 0x1318 && code <= 0x135A)
                            || (code >= 0x1369 && code <= 0x137C)
                            || (code >= 0x1380 && code <= 0x138F)
                            || (code >= 0x13A0 && code <= 0x13F5)
                            || (code >= 0x13F8 && code <= 0x13FD)
                            || (code >= 0x1401 && code <= 0x166C)

                else if code < 0x17DB then
                    (code >= 0x166F && code <= 0x167F)
                        || (code >= 0x1681 && code <= 0x169A)
                        || (code >= 0x16A0 && code <= 0x16EA)
                        || (code >= 0x16EE && code <= 0x16F8)
                        || (code >= 0x1700 && code <= 0x1711)
                        || (code >= 0x171F && code <= 0x1731)
                        || (code >= 0x1740 && code <= 0x1751)
                        || (code >= 0x1760 && code <= 0x176C)
                        || (code >= 0x176E && code <= 0x1770)
                        || (code >= 0x1780 && code <= 0x17B3)
                        || (code == 0x17D7)

                else
                    (code == 0x17DC)
                        || (code >= 0x17E0 && code <= 0x17E9)
                        || (code >= 0x17F0 && code <= 0x17F9)
                        || (code >= 0x1810 && code <= 0x1819)
                        || (code >= 0x1820 && code <= 0x1878)
                        || (code >= 0x1880 && code <= 0x1884)
                        || (code >= 0x1887 && code <= 0x18A8)
                        || (code == 0x18AA)
                        || (code >= 0x18B0 && code <= 0x18F5)
                        || (code >= 0x1900 && code <= 0x191E)
                        || (code >= 0x1946 && code <= 0x196D)

            else if code < 0x1CF4 then
                if code < 0x1B4F then
                    (code >= 0x1970 && code <= 0x1974)
                        || (code >= 0x1980 && code <= 0x19AB)
                        || (code >= 0x19B0 && code <= 0x19C9)
                        || (code >= 0x19D0 && code <= 0x19DA)
                        || (code >= 0x1A00 && code <= 0x1A16)
                        || (code >= 0x1A20 && code <= 0x1A54)
                        || (code >= 0x1A80 && code <= 0x1A89)
                        || (code >= 0x1A90 && code <= 0x1A99)
                        || (code == 0x1AA7)
                        || (code >= 0x1B05 && code <= 0x1B33)
                        || (code >= 0x1B45 && code <= 0x1B4C)

                else
                    (code >= 0x1B50 && code <= 0x1B59)
                        || (code >= 0x1B83 && code <= 0x1BA0)
                        || (code >= 0x1BAE && code <= 0x1BE5)
                        || (code >= 0x1C00 && code <= 0x1C23)
                        || (code >= 0x1C40 && code <= 0x1C49)
                        || (code >= 0x1C4D && code <= 0x1C7D)
                        || (code >= 0x1C80 && code <= 0x1C88)
                        || (code >= 0x1C90 && code <= 0x1CBA)
                        || (code >= 0x1CBD && code <= 0x1CBF)
                        || (code >= 0x1CE9 && code <= 0x1CEC)
                        || (code >= 0x1CEE && code <= 0x1CF3)

            else if code < 0x1FBD then
                (code >= 0x1CF5 && code <= 0x1CF6)
                    || (code == 0x1CFA)
                    || (code >= 0x1D00 && code <= 0x1DBF)
                    || (code >= 0x1E00 && code <= 0x1F15)
                    || (code >= 0x1F18 && code <= 0x1F1D)
                    || (code >= 0x1F20 && code <= 0x1F45)
                    || (code >= 0x1F48 && code <= 0x1F4D)
                    || (code >= 0x1F50 && code <= 0x1F57)
                    || (code >= 0x1F60 && code <= 0x1F7D)
                    || (code >= 0x1F80 && code <= 0x1FB4)
                    || (code >= 0x1FB6 && code <= 0x1FBC)
                    || ((modBy 2 code == 1)
                            && (code >= 0x1F59 && code <= 0x1F5F)
                       )

            else
                (code == 0x1FBE)
                    || (code >= 0x1FC2 && code <= 0x1FC4)
                    || (code >= 0x1FC6 && code <= 0x1FCC)
                    || (code >= 0x1FD0 && code <= 0x1FD3)
                    || (code >= 0x1FD6 && code <= 0x1FDB)
                    || (code >= 0x1FE0 && code <= 0x1FEC)
                    || (code >= 0x1FF2 && code <= 0x1FF4)
                    || (code >= 0x1FF6 && code <= 0x1FFC)
                    || (code >= 0x2070 && code <= 0x2071)
                    || (code >= 0x2074 && code <= 0x2079)
                    || (code >= 0x207F && code <= 0x2089)

        else if code < 0x3191 then
            if code < 0x2D2F then
                if code < 0x214F then
                    (code >= 0x2090 && code <= 0x209C)
                        || (code == 0x2102)
                        || (code == 0x2107)
                        || (code >= 0x210A && code <= 0x2113)
                        || (code == 0x2115)
                        || (code >= 0x2119 && code <= 0x211D)
                        || (code >= 0x212A && code <= 0x212D)
                        || (code >= 0x212F && code <= 0x2139)
                        || (code >= 0x213C && code <= 0x213F)
                        || (code >= 0x2145 && code <= 0x2149)
                        || (code == 0x214E)
                        || ((modBy 2 code == 0)
                                && (code >= 0x2124 && code <= 0x2128)
                           )

                else
                    (code >= 0x2150 && code <= 0x2189)
                        || (code >= 0x2460 && code <= 0x249B)
                        || (code >= 0x24EA && code <= 0x24FF)
                        || (code >= 0x2776 && code <= 0x2793)
                        || (code >= 0x2C00 && code <= 0x2CE4)
                        || (code >= 0x2CEB && code <= 0x2CEE)
                        || (code >= 0x2CF2 && code <= 0x2CF3)
                        || (code == 0x2CFD)
                        || (code >= 0x2D00 && code <= 0x2D25)
                        || (code == 0x2D27)
                        || (code == 0x2D2D)

            else if code < 0x2E2E then
                (code >= 0x2D30 && code <= 0x2D67)
                    || (code == 0x2D6F)
                    || (code >= 0x2D80 && code <= 0x2D96)
                    || (code >= 0x2DA0 && code <= 0x2DA6)
                    || (code >= 0x2DA8 && code <= 0x2DAE)
                    || (code >= 0x2DB0 && code <= 0x2DB6)
                    || (code >= 0x2DB8 && code <= 0x2DBE)
                    || (code >= 0x2DC0 && code <= 0x2DC6)
                    || (code >= 0x2DC8 && code <= 0x2DCE)
                    || (code >= 0x2DD0 && code <= 0x2DD6)
                    || (code >= 0x2DD8 && code <= 0x2DDE)

            else
                (code == 0x2E2F)
                    || (code >= 0x3005 && code <= 0x3007)
                    || (code >= 0x3021 && code <= 0x3029)
                    || (code >= 0x3031 && code <= 0x3035)
                    || (code >= 0x3038 && code <= 0x303C)
                    || (code >= 0x3041 && code <= 0x3096)
                    || (code >= 0x309D && code <= 0x309F)
                    || (code >= 0x30A1 && code <= 0x30FA)
                    || (code >= 0x30FC && code <= 0x30FF)
                    || (code >= 0x3105 && code <= 0x312F)
                    || (code >= 0x3131 && code <= 0x318E)

        else if code < 0xA7D5 then
            if code < 0x9FFE then
                (code >= 0x3192 && code <= 0x3195)
                    || (code >= 0x31A0 && code <= 0x31BF)
                    || (code >= 0x31F0 && code <= 0x31FF)
                    || (code >= 0x3220 && code <= 0x3229)
                    || (code >= 0x3248 && code <= 0x324F)
                    || (code >= 0x3251 && code <= 0x325F)
                    || (code >= 0x3280 && code <= 0x3289)
                    || (code >= 0x32B1 && code <= 0x32BF)
                    || (code == 0x3400)
                    || (code == 0x4DBF)
                    || (code == 0x4E00)

            else
                (code >= 0x9FFF && code <= 0xA48C)
                    || (code >= 0xA4D0 && code <= 0xA4FD)
                    || (code >= 0xA500 && code <= 0xA60C)
                    || (code >= 0xA610 && code <= 0xA62B)
                    || (code >= 0xA640 && code <= 0xA66E)
                    || (code >= 0xA67F && code <= 0xA69D)
                    || (code >= 0xA6A0 && code <= 0xA6EF)
                    || (code >= 0xA717 && code <= 0xA71F)
                    || (code >= 0xA722 && code <= 0xA788)
                    || (code >= 0xA78B && code <= 0xA7CA)
                    || (code >= 0xA7D0 && code <= 0xA7D1)
                    || ((modBy 2 code == 1)
                            && (code >= 0xA7D3 && code <= 0xA7D4)
                       )

        else if code < 0xA8FC then
            (code >= 0xA7D6 && code <= 0xA7D9)
                || (code >= 0xA7F2 && code <= 0xA801)
                || (code >= 0xA803 && code <= 0xA805)
                || (code >= 0xA807 && code <= 0xA80A)
                || (code >= 0xA80C && code <= 0xA822)
                || (code >= 0xA830 && code <= 0xA835)
                || (code >= 0xA840 && code <= 0xA873)
                || (code >= 0xA882 && code <= 0xA8B3)
                || (code >= 0xA8D0 && code <= 0xA8D9)
                || (code >= 0xA8F2 && code <= 0xA8F7)
                || (code == 0xA8FB)
                || ((modBy 2 code == 1) && (code == 0xA7D5))

        else
            (code >= 0xA8FD && code <= 0xA8FE)
                || (code >= 0xA900 && code <= 0xA925)
                || (code >= 0xA930 && code <= 0xA946)
                || (code >= 0xA960 && code <= 0xA97C)
                || (code >= 0xA984 && code <= 0xA9B2)
                || (code >= 0xA9CF && code <= 0xA9D9)
                || (code >= 0xA9E0 && code <= 0xA9E4)
                || (code >= 0xA9E6 && code <= 0xA9FE)
                || (code >= 0xAA00 && code <= 0xAA28)
                || (code >= 0xAA40 && code <= 0xAA42)
                || (code >= 0xAA44 && code <= 0xAA4B)
                || (code >= 0xAA50 && code <= 0xAA59)

    else if code < 0x000115D7 then
        if code < 0x000107FF then
            if code < 0xFFC1 then
                if code < 0xF8FF then
                    if code < 0xAB10 then
                        (code >= 0xAA60 && code <= 0xAA76)
                            || (code == 0xAA7A)
                            || (code >= 0xAA7E && code <= 0xAAAF)
                            || (code == 0xAAB1)
                            || (code >= 0xAAB5 && code <= 0xAAB6)
                            || (code >= 0xAAB9 && code <= 0xAABD)
                            || (code >= 0xAADB && code <= 0xAADD)
                            || (code >= 0xAAE0 && code <= 0xAAEA)
                            || (code >= 0xAAF2 && code <= 0xAAF4)
                            || (code >= 0xAB01 && code <= 0xAB06)
                            || (code >= 0xAB09 && code <= 0xAB0E)
                            || ((modBy 2 code == 0)
                                    && (code >= 0xAAC0 && code <= 0xAAC2)
                               )

                    else
                        (code >= 0xAB11 && code <= 0xAB16)
                            || (code >= 0xAB20 && code <= 0xAB26)
                            || (code >= 0xAB28 && code <= 0xAB2E)
                            || (code >= 0xAB30 && code <= 0xAB5A)
                            || (code >= 0xAB5C && code <= 0xAB69)
                            || (code >= 0xAB70 && code <= 0xABE2)
                            || (code >= 0xABF0 && code <= 0xABF9)
                            || (code == 0xAC00)
                            || (code == 0xD7A3)
                            || (code >= 0xD7B0 && code <= 0xD7C6)
                            || (code >= 0xD7CB && code <= 0xD7FB)

                else if code < 0xFB45 then
                    (code >= 0xF900 && code <= 0xFA6D)
                        || (code >= 0xFA70 && code <= 0xFAD9)
                        || (code >= 0xFB00 && code <= 0xFB06)
                        || (code >= 0xFB13 && code <= 0xFB17)
                        || (code == 0xFB1D)
                        || (code >= 0xFB1F && code <= 0xFB28)
                        || (code >= 0xFB2A && code <= 0xFB36)
                        || (code >= 0xFB38 && code <= 0xFB3C)
                        || (code == 0xFB3E)
                        || (code >= 0xFB40 && code <= 0xFB41)
                        || (code >= 0xFB43 && code <= 0xFB44)

                else
                    (code >= 0xFB46 && code <= 0xFBB1)
                        || (code >= 0xFBD3 && code <= 0xFD3D)
                        || (code >= 0xFD50 && code <= 0xFD8F)
                        || (code >= 0xFD92 && code <= 0xFDC7)
                        || (code >= 0xFDF0 && code <= 0xFDFB)
                        || (code >= 0xFE70 && code <= 0xFE74)
                        || (code >= 0xFE76 && code <= 0xFEFC)
                        || (code >= 0xFF10 && code <= 0xFF19)
                        || (code >= 0xFF21 && code <= 0xFF3A)
                        || (code >= 0xFF41 && code <= 0xFF5A)
                        || (code >= 0xFF66 && code <= 0xFFBE)

            else if code < 0x000103C7 then
                if code < 0x00010106 then
                    (code >= 0xFFC2 && code <= 0xFFC7)
                        || (code >= 0xFFCA && code <= 0xFFCF)
                        || (code >= 0xFFD2 && code <= 0xFFD7)
                        || (code >= 0xFFDA && code <= 0xFFDC)
                        || (code >= 0x00010000 && code <= 0x0001000B)
                        || (code >= 0x0001000D && code <= 0x00010026)
                        || (code >= 0x00010028 && code <= 0x0001003A)
                        || (code >= 0x0001003C && code <= 0x0001003D)
                        || (code >= 0x0001003F && code <= 0x0001004D)
                        || (code >= 0x00010050 && code <= 0x0001005D)
                        || (code >= 0x00010080 && code <= 0x000100FA)

                else
                    (code >= 0x00010107 && code <= 0x00010133)
                        || (code >= 0x00010140 && code <= 0x00010178)
                        || (code >= 0x0001018A && code <= 0x0001018B)
                        || (code >= 0x00010280 && code <= 0x0001029C)
                        || (code >= 0x000102A0 && code <= 0x000102D0)
                        || (code >= 0x000102E1 && code <= 0x000102FB)
                        || (code >= 0x00010300 && code <= 0x00010323)
                        || (code >= 0x0001032D && code <= 0x0001034A)
                        || (code >= 0x00010350 && code <= 0x00010375)
                        || (code >= 0x00010380 && code <= 0x0001039D)
                        || (code >= 0x000103A0 && code <= 0x000103C3)

            else if code < 0x00010593 then
                (code >= 0x000103C8 && code <= 0x000103CF)
                    || (code >= 0x000103D1 && code <= 0x000103D5)
                    || (code >= 0x00010400 && code <= 0x0001049D)
                    || (code >= 0x000104A0 && code <= 0x000104A9)
                    || (code >= 0x000104B0 && code <= 0x000104D3)
                    || (code >= 0x000104D8 && code <= 0x000104FB)
                    || (code >= 0x00010500 && code <= 0x00010527)
                    || (code >= 0x00010530 && code <= 0x00010563)
                    || (code >= 0x00010570 && code <= 0x0001057A)
                    || (code >= 0x0001057C && code <= 0x0001058A)
                    || (code >= 0x0001058C && code <= 0x00010592)

            else
                (code >= 0x00010594 && code <= 0x00010595)
                    || (code >= 0x00010597 && code <= 0x000105A1)
                    || (code >= 0x000105A3 && code <= 0x000105B1)
                    || (code >= 0x000105B3 && code <= 0x000105B9)
                    || (code >= 0x000105BB && code <= 0x000105BC)
                    || (code >= 0x00010600 && code <= 0x00010736)
                    || (code >= 0x00010740 && code <= 0x00010755)
                    || (code >= 0x00010760 && code <= 0x00010767)
                    || (code >= 0x00010780 && code <= 0x00010785)
                    || (code >= 0x00010787 && code <= 0x000107B0)
                    || (code >= 0x000107B2 && code <= 0x000107BA)

        else if code < 0x00011002 then
            if code < 0x00010ABF then
                if code < 0x000108FA then
                    (code >= 0x00010800 && code <= 0x00010805)
                        || (code == 0x00010808)
                        || (code >= 0x0001080A && code <= 0x00010835)
                        || (code >= 0x00010837 && code <= 0x00010838)
                        || (code == 0x0001083C)
                        || (code >= 0x0001083F && code <= 0x00010855)
                        || (code >= 0x00010858 && code <= 0x00010876)
                        || (code >= 0x00010879 && code <= 0x0001089E)
                        || (code >= 0x000108A7 && code <= 0x000108AF)
                        || (code >= 0x000108E0 && code <= 0x000108F2)
                        || (code >= 0x000108F4 && code <= 0x000108F5)

                else
                    (code >= 0x000108FB && code <= 0x0001091B)
                        || (code >= 0x00010920 && code <= 0x00010939)
                        || (code >= 0x00010980 && code <= 0x000109B7)
                        || (code >= 0x000109BC && code <= 0x000109CF)
                        || (code >= 0x000109D2 && code <= 0x00010A00)
                        || (code >= 0x00010A10 && code <= 0x00010A13)
                        || (code >= 0x00010A15 && code <= 0x00010A17)
                        || (code >= 0x00010A19 && code <= 0x00010A35)
                        || (code >= 0x00010A40 && code <= 0x00010A48)
                        || (code >= 0x00010A60 && code <= 0x00010A7E)
                        || (code >= 0x00010A80 && code <= 0x00010A9F)

            else if code < 0x00010CF9 then
                (code >= 0x00010AC0 && code <= 0x00010AC7)
                    || (code >= 0x00010AC9 && code <= 0x00010AE4)
                    || (code >= 0x00010AEB && code <= 0x00010AEF)
                    || (code >= 0x00010B00 && code <= 0x00010B35)
                    || (code >= 0x00010B40 && code <= 0x00010B55)
                    || (code >= 0x00010B58 && code <= 0x00010B72)
                    || (code >= 0x00010B78 && code <= 0x00010B91)
                    || (code >= 0x00010BA9 && code <= 0x00010BAF)
                    || (code >= 0x00010C00 && code <= 0x00010C48)
                    || (code >= 0x00010C80 && code <= 0x00010CB2)
                    || (code >= 0x00010CC0 && code <= 0x00010CF2)

            else
                (code >= 0x00010CFA && code <= 0x00010D23)
                    || (code >= 0x00010D30 && code <= 0x00010D39)
                    || (code >= 0x00010E60 && code <= 0x00010E7E)
                    || (code >= 0x00010E80 && code <= 0x00010EA9)
                    || (code >= 0x00010EB0 && code <= 0x00010EB1)
                    || (code >= 0x00010F00 && code <= 0x00010F27)
                    || (code >= 0x00010F30 && code <= 0x00010F45)
                    || (code >= 0x00010F51 && code <= 0x00010F54)
                    || (code >= 0x00010F70 && code <= 0x00010F81)
                    || (code >= 0x00010FB0 && code <= 0x00010FCB)
                    || (code >= 0x00010FE0 && code <= 0x00010FF6)

        else if code < 0x00011289 then
            if code < 0x0001114F then
                (code >= 0x00011003 && code <= 0x00011037)
                    || (code >= 0x00011052 && code <= 0x0001106F)
                    || (code >= 0x00011071 && code <= 0x00011072)
                    || (code == 0x00011075)
                    || (code >= 0x00011083 && code <= 0x000110AF)
                    || (code >= 0x000110D0 && code <= 0x000110E8)
                    || (code >= 0x000110F0 && code <= 0x000110F9)
                    || (code >= 0x00011103 && code <= 0x00011126)
                    || (code >= 0x00011136 && code <= 0x0001113F)
                    || (code == 0x00011144)
                    || (code == 0x00011147)

            else
                (code >= 0x00011150 && code <= 0x00011172)
                    || (code == 0x00011176)
                    || (code >= 0x00011183 && code <= 0x000111B2)
                    || (code >= 0x000111C1 && code <= 0x000111C4)
                    || (code >= 0x000111D0 && code <= 0x000111DA)
                    || (code == 0x000111DC)
                    || (code >= 0x000111E1 && code <= 0x000111F4)
                    || (code >= 0x00011200 && code <= 0x00011211)
                    || (code >= 0x00011213 && code <= 0x0001122B)
                    || (code >= 0x00011280 && code <= 0x00011286)
                    || (code == 0x00011288)

        else if code < 0x0001133C then
            (code >= 0x0001128A && code <= 0x0001128D)
                || (code >= 0x0001128F && code <= 0x0001129D)
                || (code >= 0x0001129F && code <= 0x000112A8)
                || (code >= 0x000112B0 && code <= 0x000112DE)
                || (code >= 0x000112F0 && code <= 0x000112F9)
                || (code >= 0x00011305 && code <= 0x0001130C)
                || (code >= 0x0001130F && code <= 0x00011310)
                || (code >= 0x00011313 && code <= 0x00011328)
                || (code >= 0x0001132A && code <= 0x00011330)
                || (code >= 0x00011332 && code <= 0x00011333)
                || (code >= 0x00011335 && code <= 0x00011339)

        else
            (code == 0x0001133D)
                || (code == 0x00011350)
                || (code >= 0x0001135D && code <= 0x00011361)
                || (code >= 0x00011400 && code <= 0x00011434)
                || (code >= 0x00011447 && code <= 0x0001144A)
                || (code >= 0x00011450 && code <= 0x00011459)
                || (code >= 0x0001145F && code <= 0x00011461)
                || (code >= 0x00011480 && code <= 0x000114AF)
                || (code >= 0x000114C4 && code <= 0x000114C5)
                || (code == 0x000114C7)
                || (code >= 0x000114D0 && code <= 0x000114D9)
                || (code >= 0x00011580 && code <= 0x000115AE)

    else if code < 0x0001D49D then
        if code < 0x00011FBF then
            if code < 0x00011A39 then
                if code < 0x0001189F then
                    (code >= 0x000115D8 && code <= 0x000115DB)
                        || (code >= 0x00011600 && code <= 0x0001162F)
                        || (code == 0x00011644)
                        || (code >= 0x00011650 && code <= 0x00011659)
                        || (code >= 0x00011680 && code <= 0x000116AA)
                        || (code == 0x000116B8)
                        || (code >= 0x000116C0 && code <= 0x000116C9)
                        || (code >= 0x00011700 && code <= 0x0001171A)
                        || (code >= 0x00011730 && code <= 0x0001173B)
                        || (code >= 0x00011740 && code <= 0x00011746)
                        || (code >= 0x00011800 && code <= 0x0001182B)

                else
                    (code >= 0x000118A0 && code <= 0x000118F2)
                        || (code >= 0x000118FF && code <= 0x00011906)
                        || (code == 0x00011909)
                        || (code >= 0x0001190C && code <= 0x00011913)
                        || (code >= 0x00011915 && code <= 0x00011916)
                        || (code >= 0x00011918 && code <= 0x0001192F)
                        || (code >= 0x00011950 && code <= 0x00011959)
                        || (code >= 0x000119A0 && code <= 0x000119A7)
                        || (code >= 0x000119AA && code <= 0x000119D0)
                        || (code == 0x00011A00)
                        || (code >= 0x00011A0B && code <= 0x00011A32)
                        || ((modBy 2 code == 1)
                                && ((code >= 0x0001193F && code <= 0x00011941)
                                        || (code
                                                >= 0x000119E1
                                                && code
                                                <= 0x000119E3
                                           )
                                   )
                           )

            else if code < 0x00011D07 then
                (code == 0x00011A3A)
                    || (code == 0x00011A50)
                    || (code >= 0x00011A5C && code <= 0x00011A89)
                    || (code == 0x00011A9D)
                    || (code >= 0x00011AB0 && code <= 0x00011AF8)
                    || (code >= 0x00011C00 && code <= 0x00011C08)
                    || (code >= 0x00011C0A && code <= 0x00011C2E)
                    || (code == 0x00011C40)
                    || (code >= 0x00011C50 && code <= 0x00011C6C)
                    || (code >= 0x00011C72 && code <= 0x00011C8F)
                    || (code >= 0x00011D00 && code <= 0x00011D06)

            else
                (code >= 0x00011D08 && code <= 0x00011D09)
                    || (code >= 0x00011D0B && code <= 0x00011D30)
                    || (code == 0x00011D46)
                    || (code >= 0x00011D50 && code <= 0x00011D59)
                    || (code >= 0x00011D60 && code <= 0x00011D65)
                    || (code >= 0x00011D67 && code <= 0x00011D68)
                    || (code >= 0x00011D6A && code <= 0x00011D89)
                    || (code == 0x00011D98)
                    || (code >= 0x00011DA0 && code <= 0x00011DA9)
                    || (code >= 0x00011EE0 && code <= 0x00011EF2)
                    || (code == 0x00011FB0)

        else if code < 0x00016F92 then
            if code < 0x00016ABF then
                (code >= 0x00011FC0 && code <= 0x00011FD4)
                    || (code >= 0x00012000 && code <= 0x00012399)
                    || (code >= 0x00012400 && code <= 0x0001246E)
                    || (code >= 0x00012480 && code <= 0x00012543)
                    || (code >= 0x00012F90 && code <= 0x00012FF0)
                    || (code >= 0x00013000 && code <= 0x0001342E)
                    || (code >= 0x00014400 && code <= 0x00014646)
                    || (code >= 0x00016800 && code <= 0x00016A38)
                    || (code >= 0x00016A40 && code <= 0x00016A5E)
                    || (code >= 0x00016A60 && code <= 0x00016A69)
                    || (code >= 0x00016A70 && code <= 0x00016ABE)

            else
                (code >= 0x00016AC0 && code <= 0x00016AC9)
                    || (code >= 0x00016AD0 && code <= 0x00016AED)
                    || (code >= 0x00016B00 && code <= 0x00016B2F)
                    || (code >= 0x00016B40 && code <= 0x00016B43)
                    || (code >= 0x00016B50 && code <= 0x00016B59)
                    || (code >= 0x00016B5B && code <= 0x00016B61)
                    || (code >= 0x00016B63 && code <= 0x00016B77)
                    || (code >= 0x00016B7D && code <= 0x00016B8F)
                    || (code >= 0x00016E40 && code <= 0x00016E96)
                    || (code >= 0x00016F00 && code <= 0x00016F4A)
                    || (code == 0x00016F50)

        else if code < 0x0001AFFF then
            (code >= 0x00016F93 && code <= 0x00016F9F)
                || (code >= 0x00016FE0 && code <= 0x00016FE1)
                || (code == 0x00016FE3)
                || (code == 0x00017000)
                || (code == 0x000187F7)
                || (code >= 0x00018800 && code <= 0x00018CD5)
                || (code == 0x00018D00)
                || (code == 0x00018D08)
                || (code >= 0x0001AFF0 && code <= 0x0001AFF3)
                || (code >= 0x0001AFF5 && code <= 0x0001AFFB)
                || (code >= 0x0001AFFD && code <= 0x0001AFFE)

        else
            (code >= 0x0001B000 && code <= 0x0001B122)
                || (code >= 0x0001B150 && code <= 0x0001B152)
                || (code >= 0x0001B164 && code <= 0x0001B167)
                || (code >= 0x0001B170 && code <= 0x0001B2FB)
                || (code >= 0x0001BC00 && code <= 0x0001BC6A)
                || (code >= 0x0001BC70 && code <= 0x0001BC7C)
                || (code >= 0x0001BC80 && code <= 0x0001BC88)
                || (code >= 0x0001BC90 && code <= 0x0001BC99)
                || (code >= 0x0001D2E0 && code <= 0x0001D2F3)
                || (code >= 0x0001D360 && code <= 0x0001D378)
                || (code >= 0x0001D400 && code <= 0x0001D454)
                || (code >= 0x0001D456 && code <= 0x0001D49C)

    else if code < 0x0001E94A then
        if code < 0x0001D735 then
            if code < 0x0001D51D then
                (code >= 0x0001D49E && code <= 0x0001D49F)
                    || (code == 0x0001D4A2)
                    || (code >= 0x0001D4A5 && code <= 0x0001D4A6)
                    || (code >= 0x0001D4A9 && code <= 0x0001D4AC)
                    || (code >= 0x0001D4AE && code <= 0x0001D4B9)
                    || (code == 0x0001D4BB)
                    || (code >= 0x0001D4BD && code <= 0x0001D4C3)
                    || (code >= 0x0001D4C5 && code <= 0x0001D505)
                    || (code >= 0x0001D507 && code <= 0x0001D50A)
                    || (code >= 0x0001D50D && code <= 0x0001D514)
                    || (code >= 0x0001D516 && code <= 0x0001D51C)

            else
                (code >= 0x0001D51E && code <= 0x0001D539)
                    || (code >= 0x0001D53B && code <= 0x0001D53E)
                    || (code >= 0x0001D540 && code <= 0x0001D544)
                    || (code == 0x0001D546)
                    || (code >= 0x0001D54A && code <= 0x0001D550)
                    || (code >= 0x0001D552 && code <= 0x0001D6A5)
                    || (code >= 0x0001D6A8 && code <= 0x0001D6C0)
                    || (code >= 0x0001D6C2 && code <= 0x0001D6DA)
                    || (code >= 0x0001D6DC && code <= 0x0001D6FA)
                    || (code >= 0x0001D6FC && code <= 0x0001D714)
                    || (code >= 0x0001D716 && code <= 0x0001D734)

        else if code < 0x0001E14D then
            (code >= 0x0001D736 && code <= 0x0001D74E)
                || (code >= 0x0001D750 && code <= 0x0001D76E)
                || (code >= 0x0001D770 && code <= 0x0001D788)
                || (code >= 0x0001D78A && code <= 0x0001D7A8)
                || (code >= 0x0001D7AA && code <= 0x0001D7C2)
                || (code >= 0x0001D7C4 && code <= 0x0001D7CB)
                || (code >= 0x0001D7CE && code <= 0x0001D7FF)
                || (code >= 0x0001DF00 && code <= 0x0001DF1E)
                || (code >= 0x0001E100 && code <= 0x0001E12C)
                || (code >= 0x0001E137 && code <= 0x0001E13D)
                || (code >= 0x0001E140 && code <= 0x0001E149)

        else
            (code == 0x0001E14E)
                || (code >= 0x0001E290 && code <= 0x0001E2AD)
                || (code >= 0x0001E2C0 && code <= 0x0001E2EB)
                || (code >= 0x0001E2F0 && code <= 0x0001E2F9)
                || (code >= 0x0001E7E0 && code <= 0x0001E7E6)
                || (code >= 0x0001E7E8 && code <= 0x0001E7EB)
                || (code >= 0x0001E7ED && code <= 0x0001E7EE)
                || (code >= 0x0001E7F0 && code <= 0x0001E7FE)
                || (code >= 0x0001E800 && code <= 0x0001E8C4)
                || (code >= 0x0001E8C7 && code <= 0x0001E8CF)
                || (code >= 0x0001E900 && code <= 0x0001E943)

    else if code < 0x0001EE73 then
        if code < 0x0001EE26 then
            (code == 0x0001E94B)
                || (code >= 0x0001E950 && code <= 0x0001E959)
                || (code >= 0x0001EC71 && code <= 0x0001ECAB)
                || (code >= 0x0001ECAD && code <= 0x0001ECAF)
                || (code >= 0x0001ECB1 && code <= 0x0001ECB4)
                || (code >= 0x0001ED01 && code <= 0x0001ED2D)
                || (code >= 0x0001ED2F && code <= 0x0001ED3D)
                || (code >= 0x0001EE00 && code <= 0x0001EE03)
                || (code >= 0x0001EE05 && code <= 0x0001EE1F)
                || (code >= 0x0001EE21 && code <= 0x0001EE22)
                || (code == 0x0001EE24)

        else
            (code == 0x0001EE27)
                || (code >= 0x0001EE29 && code <= 0x0001EE32)
                || (code >= 0x0001EE34 && code <= 0x0001EE37)
                || (code == 0x0001EE42)
                || (code >= 0x0001EE4D && code <= 0x0001EE4F)
                || (code >= 0x0001EE51 && code <= 0x0001EE52)
                || (code == 0x0001EE54)
                || (code >= 0x0001EE61 && code <= 0x0001EE62)
                || (code == 0x0001EE64)
                || (code >= 0x0001EE67 && code <= 0x0001EE6A)
                || (code >= 0x0001EE6C && code <= 0x0001EE72)
                || ((modBy 2 code == 1)
                        && ((code >= 0x0001EE39 && code <= 0x0001EE3B)
                                || (code >= 0x0001EE47 && code <= 0x0001EE4B)
                                || (code >= 0x0001EE57 && code <= 0x0001EE5F)
                           )
                   )

    else if code < 0x0002A6DE then
        (code >= 0x0001EE74 && code <= 0x0001EE77)
            || (code >= 0x0001EE79 && code <= 0x0001EE7C)
            || (code == 0x0001EE7E)
            || (code >= 0x0001EE80 && code <= 0x0001EE89)
            || (code >= 0x0001EE8B && code <= 0x0001EE9B)
            || (code >= 0x0001EEA1 && code <= 0x0001EEA3)
            || (code >= 0x0001EEA5 && code <= 0x0001EEA9)
            || (code >= 0x0001EEAB && code <= 0x0001EEBB)
            || (code >= 0x0001F100 && code <= 0x0001F10C)
            || (code >= 0x0001FBF0 && code <= 0x0001FBF9)
            || (code == 0x00020000)

    else
        (code == 0x0002A6DF)
            || (code == 0x0002A700)
            || (code == 0x0002B738)
            || (code == 0x0002B740)
            || (code == 0x0002B81D)
            || (code == 0x0002B820)
            || (code == 0x0002CEA1)
            || (code == 0x0002CEB0)
            || (code == 0x0002EBE0)
            || (code >= 0x0002F800 && code <= 0x0002FA1D)
            || (code == 0x00030000)
            || (code == 0x0003134A)


{-| A category as defined by the Unicode standard.
-}
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


{-| Get the Unicode category. Warning: this function is very big. You should usually use one of the `isXXX` ones instead.
-}
getCategory : Char.Char -> Maybe Category
getCategory c =
    let
        code =
            Char.toCode c
    in
    if code < 0x0100 then
        if code < 0xA0 then
            if code < 0x3B then
                if code < 0x29 then
                    if code >= 0x00 && code <= 0x1F then
                        Just OtherControl

                    else if code == 0x20 then
                        Just SeparatorSpace

                    else if
                        (code >= 0x21 && code <= 0x23)
                            || (code >= 0x25 && code <= 0x27)
                    then
                        Just PunctuationOther

                    else if code == 0x24 then
                        Just SymbolCurrency

                    else if code == 0x28 then
                        Just PunctuationOpen

                    else
                        Nothing

                else if code == 0x29 then
                    Just PunctuationClose

                else if
                    (code == 0x2A)
                        || (code == 0x2C)
                        || (code >= 0x2E && code <= 0x2F)
                        || (code == 0x3A)
                then
                    Just PunctuationOther

                else if code == 0x2B then
                    Just SymbolMath

                else if code == 0x2D then
                    Just PunctuationDash

                else if code >= 0x30 && code <= 0x39 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x5E then
                if
                    (code == 0x3B)
                        || (code >= 0x3F && code <= 0x40)
                        || (code == 0x5C)
                then
                    Just PunctuationOther

                else if code >= 0x3C && code <= 0x3E then
                    Just SymbolMath

                else if code >= 0x41 && code <= 0x5A then
                    Just LetterUppercase

                else if code == 0x5B then
                    Just PunctuationOpen

                else if code == 0x5D then
                    Just PunctuationClose

                else
                    Nothing

            else if (code == 0x5E) || (code == 0x60) then
                Just SymbolModifier

            else if code == 0x5F then
                Just PunctuationConnector

            else if code >= 0x61 && code <= 0x7A then
                Just LetterLowercase

            else if code == 0x7B then
                Just PunctuationOpen

            else if (code == 0x7C) || (code == 0x7E) then
                Just SymbolMath

            else if code == 0x7D then
                Just PunctuationClose

            else if code >= 0x7F && code <= 0x9F then
                Just OtherControl

            else
                Nothing

        else if code < 0xB1 then
            if code < 0xA9 then
                if code == 0xA0 then
                    Just SeparatorSpace

                else if (code == 0xA1) || (code == 0xA7) then
                    Just PunctuationOther

                else if code >= 0xA2 && code <= 0xA5 then
                    Just SymbolCurrency

                else if code == 0xA6 then
                    Just SymbolOther

                else if code == 0xA8 then
                    Just SymbolModifier

                else
                    Nothing

            else if (code == 0xA9) || (code == 0xAE) || (code == 0xB0) then
                Just SymbolOther

            else if code == 0xAA then
                Just LetterOther

            else if code == 0xAB then
                Just PunctuationInitialQuote

            else if code == 0xAC then
                Just SymbolMath

            else if code == 0xAD then
                Just OtherFormat

            else if code == 0xAF then
                Just SymbolModifier

            else
                Nothing

        else if code < 0xBA then
            if code == 0xB1 then
                Just SymbolMath

            else if (code >= 0xB2 && code <= 0xB3) || (code == 0xB9) then
                Just NumberOther

            else if (code == 0xB4) || (code == 0xB8) then
                Just SymbolModifier

            else if code == 0xB5 then
                Just LetterLowercase

            else if code >= 0xB6 && code <= 0xB7 then
                Just PunctuationOther

            else
                Nothing

        else if code == 0xBA then
            Just LetterOther

        else if code == 0xBB then
            Just PunctuationFinalQuote

        else if code >= 0xBC && code <= 0xBE then
            Just NumberOther

        else if code == 0xBF then
            Just PunctuationOther

        else if (code >= 0xC0 && code <= 0xD6) || (code >= 0xD8 && code <= 0xDE) then
            Just LetterUppercase

        else if (code == 0xD7) || (code == 0xF7) then
            Just SymbolMath

        else if (code >= 0xDF && code <= 0xF6) || (code >= 0xF8 && code <= 0xFF) then
            Just LetterLowercase

        else
            Nothing

    else if code < 0x231F then
        if code < 0x0C00 then
            if code < 0x048C then
                if code < 0x01E7 then
                    if code < 0x0164 then
                        if code < 0x0130 then
                            if code < 0x0117 then
                                if code < 0x010A then
                                    if
                                        (code == 0x0100)
                                            || (code == 0x0102)
                                            || (code == 0x0104)
                                            || (code == 0x0106)
                                            || (code == 0x0108)
                                    then
                                        Just LetterUppercase

                                    else if
                                        (code == 0x0101)
                                            || (code == 0x0103)
                                            || (code == 0x0105)
                                            || (code == 0x0107)
                                            || (code == 0x0109)
                                    then
                                        Just LetterLowercase

                                    else
                                        Nothing

                                else if code < 0x010F then
                                    if
                                        (code == 0x010A)
                                            || (code == 0x010C)
                                            || (code == 0x010E)
                                    then
                                        Just LetterUppercase

                                    else if (code == 0x010B) || (code == 0x010D) then
                                        Just LetterLowercase

                                    else
                                        Nothing

                                else if
                                    (code == 0x010F)
                                        || (code == 0x0111)
                                        || (code == 0x0113)
                                        || (code == 0x0115)
                                then
                                    Just LetterLowercase

                                else if
                                    (code == 0x0110)
                                        || (code == 0x0112)
                                        || (code == 0x0114)
                                        || (code == 0x0116)
                                then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code < 0x0122 then
                                if
                                    (code == 0x0117)
                                        || (code == 0x0119)
                                        || (code == 0x011B)
                                        || (code == 0x011D)
                                        || (code == 0x011F)
                                        || (code == 0x0121)
                                then
                                    Just LetterLowercase

                                else if
                                    (code == 0x0118)
                                        || (code == 0x011A)
                                        || (code == 0x011C)
                                        || (code == 0x011E)
                                        || (code == 0x0120)
                                then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code < 0x0128 then
                                if
                                    (code == 0x0122)
                                        || (code == 0x0124)
                                        || (code == 0x0126)
                                then
                                    Just LetterUppercase

                                else if
                                    (code == 0x0123)
                                        || (code == 0x0125)
                                        || (code == 0x0127)
                                then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if
                                (code == 0x0128)
                                    || (code == 0x012A)
                                    || (code == 0x012C)
                                    || (code == 0x012E)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x0129)
                                    || (code == 0x012B)
                                    || (code == 0x012D)
                                    || (code == 0x012F)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x014A then
                            if code < 0x013C then
                                if
                                    (code == 0x0130)
                                        || (code == 0x0132)
                                        || (code == 0x0134)
                                        || (code == 0x0136)
                                        || (code == 0x0139)
                                        || (code == 0x013B)
                                then
                                    Just LetterUppercase

                                else if
                                    (code == 0x0131)
                                        || (code == 0x0133)
                                        || (code == 0x0135)
                                        || (code >= 0x0137 && code <= 0x0138)
                                        || (code == 0x013A)
                                then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x0141 then
                                if
                                    (code == 0x013C)
                                        || (code == 0x013E)
                                        || (code == 0x0140)
                                then
                                    Just LetterLowercase

                                else if (code == 0x013D) || (code == 0x013F) then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if
                                (code == 0x0141)
                                    || (code == 0x0143)
                                    || (code == 0x0145)
                                    || (code == 0x0147)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x0142)
                                    || (code == 0x0144)
                                    || (code == 0x0146)
                                    || (code >= 0x0148 && code <= 0x0149)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x0156 then
                            if
                                (code == 0x014A)
                                    || (code == 0x014C)
                                    || (code == 0x014E)
                                    || (code == 0x0150)
                                    || (code == 0x0152)
                                    || (code == 0x0154)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x014B)
                                    || (code == 0x014D)
                                    || (code == 0x014F)
                                    || (code == 0x0151)
                                    || (code == 0x0153)
                                    || (code == 0x0155)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x015C then
                            if
                                (code == 0x0156)
                                    || (code == 0x0158)
                                    || (code == 0x015A)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x0157)
                                    || (code == 0x0159)
                                    || (code == 0x015B)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if
                            (code == 0x015C)
                                || (code == 0x015E)
                                || (code == 0x0160)
                                || (code == 0x0162)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x015D)
                                || (code == 0x015F)
                                || (code == 0x0161)
                                || (code == 0x0163)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x01A8 then
                        if code < 0x017C then
                            if code < 0x016E then
                                if
                                    (code == 0x0164)
                                        || (code == 0x0166)
                                        || (code == 0x0168)
                                        || (code == 0x016A)
                                        || (code == 0x016C)
                                then
                                    Just LetterUppercase

                                else if
                                    (code == 0x0165)
                                        || (code == 0x0167)
                                        || (code == 0x0169)
                                        || (code == 0x016B)
                                        || (code == 0x016D)
                                then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x0173 then
                                if
                                    (code == 0x016E)
                                        || (code == 0x0170)
                                        || (code == 0x0172)
                                then
                                    Just LetterUppercase

                                else if (code == 0x016F) || (code == 0x0171) then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if
                                (code == 0x0173)
                                    || (code == 0x0175)
                                    || (code == 0x0177)
                                    || (code == 0x017A)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x0174)
                                    || (code == 0x0176)
                                    || (code >= 0x0178 && code <= 0x0179)
                                    || (code == 0x017B)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x0192 then
                            if
                                (code == 0x017C)
                                    || (code >= 0x017E && code <= 0x0180)
                                    || (code == 0x0183)
                                    || (code == 0x0185)
                                    || (code == 0x0188)
                                    || (code >= 0x018C && code <= 0x018D)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x017D)
                                    || (code >= 0x0181 && code <= 0x0182)
                                    || (code == 0x0184)
                                    || (code >= 0x0186 && code <= 0x0187)
                                    || (code >= 0x0189 && code <= 0x018B)
                                    || (code >= 0x018E && code <= 0x0191)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x019E then
                            if
                                (code == 0x0192)
                                    || (code == 0x0195)
                                    || (code >= 0x0199 && code <= 0x019B)
                            then
                                Just LetterLowercase

                            else if
                                (code >= 0x0193 && code <= 0x0194)
                                    || (code >= 0x0196 && code <= 0x0198)
                                    || (code >= 0x019C && code <= 0x019D)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if
                            (code == 0x019E)
                                || (code == 0x01A1)
                                || (code == 0x01A3)
                                || (code == 0x01A5)
                        then
                            Just LetterLowercase

                        else if
                            (code >= 0x019F && code <= 0x01A0)
                                || (code == 0x01A2)
                                || (code == 0x01A4)
                                || (code >= 0x01A6 && code <= 0x01A7)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x01CB then
                        if code < 0x01B8 then
                            if
                                (code == 0x01A8)
                                    || (code >= 0x01AA && code <= 0x01AB)
                                    || (code == 0x01AD)
                                    || (code == 0x01B0)
                                    || (code == 0x01B4)
                                    || (code == 0x01B6)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x01A9)
                                    || (code == 0x01AC)
                                    || (code >= 0x01AE && code <= 0x01AF)
                                    || (code >= 0x01B1 && code <= 0x01B3)
                                    || (code == 0x01B5)
                                    || (code == 0x01B7)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x01C3 then
                            if (code == 0x01B8) || (code == 0x01BC) then
                                Just LetterUppercase

                            else if
                                (code >= 0x01B9 && code <= 0x01BA)
                                    || (code >= 0x01BD && code <= 0x01BF)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x01BB)
                                    || (code >= 0x01C0 && code <= 0x01C2)
                            then
                                Just LetterOther

                            else
                                Nothing

                        else if code == 0x01C3 then
                            Just LetterOther

                        else if
                            (code == 0x01C4)
                                || (code == 0x01C7)
                                || (code == 0x01CA)
                        then
                            Just LetterUppercase

                        else if (code == 0x01C5) || (code == 0x01C8) then
                            Just LetterTitlecase

                        else if (code == 0x01C6) || (code == 0x01C9) then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x01D7 then
                        if code == 0x01CB then
                            Just LetterTitlecase

                        else if
                            (code == 0x01CC)
                                || (code == 0x01CE)
                                || (code == 0x01D0)
                                || (code == 0x01D2)
                                || (code == 0x01D4)
                                || (code == 0x01D6)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x01CD)
                                || (code == 0x01CF)
                                || (code == 0x01D1)
                                || (code == 0x01D3)
                                || (code == 0x01D5)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x01DE then
                        if
                            (code == 0x01D7)
                                || (code == 0x01D9)
                                || (code == 0x01DB)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x01D8)
                                || (code == 0x01DA)
                                || (code >= 0x01DC && code <= 0x01DD)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x01DE)
                            || (code == 0x01E0)
                            || (code == 0x01E2)
                            || (code == 0x01E4)
                            || (code == 0x01E6)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x01DF)
                            || (code == 0x01E1)
                            || (code == 0x01E3)
                            || (code == 0x01E5)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x02ED then
                    if code < 0x021A then
                        if code < 0x0201 then
                            if code < 0x01F2 then
                                if
                                    (code == 0x01E7)
                                        || (code == 0x01E9)
                                        || (code == 0x01EB)
                                        || (code == 0x01ED)
                                        || (code >= 0x01EF && code <= 0x01F0)
                                then
                                    Just LetterLowercase

                                else if
                                    (code == 0x01E8)
                                        || (code == 0x01EA)
                                        || (code == 0x01EC)
                                        || (code == 0x01EE)
                                        || (code == 0x01F1)
                                then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code < 0x01F9 then
                                if code == 0x01F2 then
                                    Just LetterTitlecase

                                else if (code == 0x01F3) || (code == 0x01F5) then
                                    Just LetterLowercase

                                else if
                                    (code == 0x01F4)
                                        || (code >= 0x01F6 && code <= 0x01F8)
                                then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if
                                (code == 0x01F9)
                                    || (code == 0x01FB)
                                    || (code == 0x01FD)
                                    || (code == 0x01FF)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x01FA)
                                    || (code == 0x01FC)
                                    || (code == 0x01FE)
                                    || (code == 0x0200)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x020C then
                            if
                                (code == 0x0201)
                                    || (code == 0x0203)
                                    || (code == 0x0205)
                                    || (code == 0x0207)
                                    || (code == 0x0209)
                                    || (code == 0x020B)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x0202)
                                    || (code == 0x0204)
                                    || (code == 0x0206)
                                    || (code == 0x0208)
                                    || (code == 0x020A)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x0212 then
                            if
                                (code == 0x020C)
                                    || (code == 0x020E)
                                    || (code == 0x0210)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x020D)
                                    || (code == 0x020F)
                                    || (code == 0x0211)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if
                            (code == 0x0212)
                                || (code == 0x0214)
                                || (code == 0x0216)
                                || (code == 0x0218)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x0213)
                                || (code == 0x0215)
                                || (code == 0x0217)
                                || (code == 0x0219)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x0232 then
                        if code < 0x0225 then
                            if
                                (code == 0x021A)
                                    || (code == 0x021C)
                                    || (code == 0x021E)
                                    || (code == 0x0220)
                                    || (code == 0x0222)
                                    || (code == 0x0224)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x021B)
                                    || (code == 0x021D)
                                    || (code == 0x021F)
                                    || (code == 0x0221)
                                    || (code == 0x0223)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x022A then
                            if
                                (code == 0x0225)
                                    || (code == 0x0227)
                                    || (code == 0x0229)
                            then
                                Just LetterLowercase

                            else if (code == 0x0226) || (code == 0x0228) then
                                Just LetterUppercase

                            else
                                Nothing

                        else if
                            (code == 0x022A)
                                || (code == 0x022C)
                                || (code == 0x022E)
                                || (code == 0x0230)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x022B)
                                || (code == 0x022D)
                                || (code == 0x022F)
                                || (code == 0x0231)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x024A then
                        if
                            (code == 0x0232)
                                || (code >= 0x023A && code <= 0x023B)
                                || (code >= 0x023D && code <= 0x023E)
                                || (code == 0x0241)
                                || (code >= 0x0243 && code <= 0x0246)
                                || (code == 0x0248)
                        then
                            Just LetterUppercase

                        else if
                            (code >= 0x0233 && code <= 0x0239)
                                || (code == 0x023C)
                                || (code >= 0x023F && code <= 0x0240)
                                || (code == 0x0242)
                                || (code == 0x0247)
                                || (code == 0x0249)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x0294 then
                        if
                            (code == 0x024A)
                                || (code == 0x024C)
                                || (code == 0x024E)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x024B)
                                || (code == 0x024D)
                                || (code >= 0x024F && code <= 0x0293)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0x0294 then
                        Just LetterOther

                    else if code >= 0x0295 && code <= 0x02AF then
                        Just LetterLowercase

                    else if
                        (code >= 0x02B0 && code <= 0x02C1)
                            || (code >= 0x02C6 && code <= 0x02D1)
                            || (code >= 0x02E0 && code <= 0x02E4)
                            || (code == 0x02EC)
                    then
                        Just LetterModifier

                    else if
                        (code >= 0x02C2 && code <= 0x02C5)
                            || (code >= 0x02D2 && code <= 0x02DF)
                            || (code >= 0x02E5 && code <= 0x02EB)
                    then
                        Just SymbolModifier

                    else
                        Nothing

                else if code < 0x03EC then
                    if code < 0x03AB then
                        if code < 0x0379 then
                            if
                                (code == 0x02ED)
                                    || (code >= 0x02EF && code <= 0x02FF)
                                    || (code == 0x0375)
                            then
                                Just SymbolModifier

                            else if (code == 0x02EE) || (code == 0x0374) then
                                Just LetterModifier

                            else if code >= 0x0300 && code <= 0x036F then
                                Just MarkNonSpacing

                            else if
                                (code == 0x0370)
                                    || (code == 0x0372)
                                    || (code == 0x0376)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x0371)
                                    || (code == 0x0373)
                                    || (code == 0x0377)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x0386 then
                            if code == 0x037A then
                                Just LetterModifier

                            else if code >= 0x037B && code <= 0x037D then
                                Just LetterLowercase

                            else if code == 0x037E then
                                Just PunctuationOther

                            else if code == 0x037F then
                                Just LetterUppercase

                            else if code >= 0x0384 && code <= 0x0385 then
                                Just SymbolModifier

                            else
                                Nothing

                        else if
                            (code == 0x0386)
                                || (code >= 0x0388 && code <= 0x038A)
                                || (code == 0x038C)
                                || (code >= 0x038E && code <= 0x038F)
                                || (code >= 0x0391 && code <= 0x03A1)
                                || (code >= 0x03A3 && code <= 0x03AA)
                        then
                            Just LetterUppercase

                        else if code == 0x0387 then
                            Just PunctuationOther

                        else if code == 0x0390 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x03DE then
                        if
                            (code == 0x03AB)
                                || (code == 0x03CF)
                                || (code >= 0x03D2 && code <= 0x03D4)
                                || (code == 0x03D8)
                                || (code == 0x03DA)
                                || (code == 0x03DC)
                        then
                            Just LetterUppercase

                        else if
                            (code >= 0x03AC && code <= 0x03CE)
                                || (code >= 0x03D0 && code <= 0x03D1)
                                || (code >= 0x03D5 && code <= 0x03D7)
                                || (code == 0x03D9)
                                || (code == 0x03DB)
                                || (code == 0x03DD)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x03E4 then
                        if
                            (code == 0x03DE)
                                || (code == 0x03E0)
                                || (code == 0x03E2)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x03DF)
                                || (code == 0x03E1)
                                || (code == 0x03E3)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x03E4)
                            || (code == 0x03E6)
                            || (code == 0x03E8)
                            || (code == 0x03EA)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x03E5)
                            || (code == 0x03E7)
                            || (code == 0x03E9)
                            || (code == 0x03EB)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x046C then
                    if code < 0x042F then
                        if
                            (code == 0x03EC)
                                || (code == 0x03EE)
                                || (code == 0x03F4)
                                || (code == 0x03F7)
                                || (code >= 0x03F9 && code <= 0x03FA)
                                || (code >= 0x03FD && code <= 0x042E)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x03ED)
                                || (code >= 0x03EF && code <= 0x03F3)
                                || (code == 0x03F5)
                                || (code == 0x03F8)
                                || (code >= 0x03FB && code <= 0x03FC)
                        then
                            Just LetterLowercase

                        else if code == 0x03F6 then
                            Just SymbolMath

                        else
                            Nothing

                    else if code < 0x0464 then
                        if
                            (code == 0x042F)
                                || (code == 0x0460)
                                || (code == 0x0462)
                        then
                            Just LetterUppercase

                        else if
                            (code >= 0x0430 && code <= 0x045F)
                                || (code == 0x0461)
                                || (code == 0x0463)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x0464)
                            || (code == 0x0466)
                            || (code == 0x0468)
                            || (code == 0x046A)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x0465)
                            || (code == 0x0467)
                            || (code == 0x0469)
                            || (code == 0x046B)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x0478 then
                    if
                        (code == 0x046C)
                            || (code == 0x046E)
                            || (code == 0x0470)
                            || (code == 0x0472)
                            || (code == 0x0474)
                            || (code == 0x0476)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x046D)
                            || (code == 0x046F)
                            || (code == 0x0471)
                            || (code == 0x0473)
                            || (code == 0x0475)
                            || (code == 0x0477)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x047E then
                    if (code == 0x0478) || (code == 0x047A) || (code == 0x047C) then
                        Just LetterUppercase

                    else if (code == 0x0479) || (code == 0x047B) || (code == 0x047D) then
                        Just LetterLowercase

                    else
                        Nothing

                else if (code == 0x047E) || (code == 0x0480) || (code == 0x048A) then
                    Just LetterUppercase

                else if (code == 0x047F) || (code == 0x0481) || (code == 0x048B) then
                    Just LetterLowercase

                else if code == 0x0482 then
                    Just SymbolOther

                else if code >= 0x0483 && code <= 0x0487 then
                    Just MarkNonSpacing

                else if code >= 0x0488 && code <= 0x0489 then
                    Just MarkEnclosing

                else
                    Nothing

            else if code < 0x06D3 then
                if code < 0x04F1 then
                    if code < 0x04BC then
                        if code < 0x04A3 then
                            if code < 0x0496 then
                                if
                                    (code == 0x048C)
                                        || (code == 0x048E)
                                        || (code == 0x0490)
                                        || (code == 0x0492)
                                        || (code == 0x0494)
                                then
                                    Just LetterUppercase

                                else if
                                    (code == 0x048D)
                                        || (code == 0x048F)
                                        || (code == 0x0491)
                                        || (code == 0x0493)
                                        || (code == 0x0495)
                                then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x049B then
                                if
                                    (code == 0x0496)
                                        || (code == 0x0498)
                                        || (code == 0x049A)
                                then
                                    Just LetterUppercase

                                else if (code == 0x0497) || (code == 0x0499) then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if
                                (code == 0x049B)
                                    || (code == 0x049D)
                                    || (code == 0x049F)
                                    || (code == 0x04A1)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x049C)
                                    || (code == 0x049E)
                                    || (code == 0x04A0)
                                    || (code == 0x04A2)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x04AE then
                            if
                                (code == 0x04A3)
                                    || (code == 0x04A5)
                                    || (code == 0x04A7)
                                    || (code == 0x04A9)
                                    || (code == 0x04AB)
                                    || (code == 0x04AD)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x04A4)
                                    || (code == 0x04A6)
                                    || (code == 0x04A8)
                                    || (code == 0x04AA)
                                    || (code == 0x04AC)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x04B4 then
                            if
                                (code == 0x04AE)
                                    || (code == 0x04B0)
                                    || (code == 0x04B2)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x04AF)
                                    || (code == 0x04B1)
                                    || (code == 0x04B3)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if
                            (code == 0x04B4)
                                || (code == 0x04B6)
                                || (code == 0x04B8)
                                || (code == 0x04BA)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x04B5)
                                || (code == 0x04B7)
                                || (code == 0x04B9)
                                || (code == 0x04BB)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x04D6 then
                        if code < 0x04C8 then
                            if
                                (code == 0x04BC)
                                    || (code == 0x04BE)
                                    || (code >= 0x04C0 && code <= 0x04C1)
                                    || (code == 0x04C3)
                                    || (code == 0x04C5)
                                    || (code == 0x04C7)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x04BD)
                                    || (code == 0x04BF)
                                    || (code == 0x04C2)
                                    || (code == 0x04C4)
                                    || (code == 0x04C6)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x04CD then
                            if
                                (code == 0x04C8)
                                    || (code == 0x04CA)
                                    || (code == 0x04CC)
                            then
                                Just LetterLowercase

                            else if (code == 0x04C9) || (code == 0x04CB) then
                                Just LetterUppercase

                            else
                                Nothing

                        else if
                            (code == 0x04CD)
                                || (code == 0x04D0)
                                || (code == 0x04D2)
                                || (code == 0x04D4)
                        then
                            Just LetterUppercase

                        else if
                            (code >= 0x04CE && code <= 0x04CF)
                                || (code == 0x04D1)
                                || (code == 0x04D3)
                                || (code == 0x04D5)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x04E2 then
                        if
                            (code == 0x04D6)
                                || (code == 0x04D8)
                                || (code == 0x04DA)
                                || (code == 0x04DC)
                                || (code == 0x04DE)
                                || (code == 0x04E0)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x04D7)
                                || (code == 0x04D9)
                                || (code == 0x04DB)
                                || (code == 0x04DD)
                                || (code == 0x04DF)
                                || (code == 0x04E1)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x04E8 then
                        if
                            (code == 0x04E2)
                                || (code == 0x04E4)
                                || (code == 0x04E6)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x04E3)
                                || (code == 0x04E5)
                                || (code == 0x04E7)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x04E8)
                            || (code == 0x04EA)
                            || (code == 0x04EC)
                            || (code == 0x04EE)
                            || (code == 0x04F0)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x04E9)
                            || (code == 0x04EB)
                            || (code == 0x04ED)
                            || (code == 0x04EF)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x0523 then
                    if code < 0x0509 then
                        if code < 0x04FC then
                            if
                                (code == 0x04F1)
                                    || (code == 0x04F3)
                                    || (code == 0x04F5)
                                    || (code == 0x04F7)
                                    || (code == 0x04F9)
                                    || (code == 0x04FB)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x04F2)
                                    || (code == 0x04F4)
                                    || (code == 0x04F6)
                                    || (code == 0x04F8)
                                    || (code == 0x04FA)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x0501 then
                            if
                                (code == 0x04FC)
                                    || (code == 0x04FE)
                                    || (code == 0x0500)
                            then
                                Just LetterUppercase

                            else if (code == 0x04FD) || (code == 0x04FF) then
                                Just LetterLowercase

                            else
                                Nothing

                        else if
                            (code == 0x0501)
                                || (code == 0x0503)
                                || (code == 0x0505)
                                || (code == 0x0507)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x0502)
                                || (code == 0x0504)
                                || (code == 0x0506)
                                || (code == 0x0508)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x0515 then
                        if
                            (code == 0x0509)
                                || (code == 0x050B)
                                || (code == 0x050D)
                                || (code == 0x050F)
                                || (code == 0x0511)
                                || (code == 0x0513)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x050A)
                                || (code == 0x050C)
                                || (code == 0x050E)
                                || (code == 0x0510)
                                || (code == 0x0512)
                                || (code == 0x0514)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x051B then
                        if
                            (code == 0x0515)
                                || (code == 0x0517)
                                || (code == 0x0519)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x0516)
                                || (code == 0x0518)
                                || (code == 0x051A)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if
                        (code == 0x051B)
                            || (code == 0x051D)
                            || (code == 0x051F)
                            || (code == 0x0521)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x051C)
                            || (code == 0x051E)
                            || (code == 0x0520)
                            || (code == 0x0522)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x05C2 then
                    if code < 0x0530 then
                        if code < 0x0528 then
                            if
                                (code == 0x0523)
                                    || (code == 0x0525)
                                    || (code == 0x0527)
                            then
                                Just LetterLowercase

                            else if (code == 0x0524) || (code == 0x0526) then
                                Just LetterUppercase

                            else
                                Nothing

                        else if
                            (code == 0x0528)
                                || (code == 0x052A)
                                || (code == 0x052C)
                                || (code == 0x052E)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x0529)
                                || (code == 0x052B)
                                || (code == 0x052D)
                                || (code == 0x052F)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x058C then
                        if code >= 0x0531 && code <= 0x0556 then
                            Just LetterUppercase

                        else if code == 0x0559 then
                            Just LetterModifier

                        else if
                            (code >= 0x055A && code <= 0x055F)
                                || (code == 0x0589)
                        then
                            Just PunctuationOther

                        else if code >= 0x0560 && code <= 0x0588 then
                            Just LetterLowercase

                        else if code == 0x058A then
                            Just PunctuationDash

                        else
                            Nothing

                    else if code >= 0x058D && code <= 0x058E then
                        Just SymbolOther

                    else if code == 0x058F then
                        Just SymbolCurrency

                    else if
                        (code >= 0x0591 && code <= 0x05BD)
                            || (code == 0x05BF)
                            || (code == 0x05C1)
                    then
                        Just MarkNonSpacing

                    else if code == 0x05BE then
                        Just PunctuationDash

                    else if code == 0x05C0 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x060D then
                    if code < 0x05EE then
                        if
                            (code == 0x05C2)
                                || (code >= 0x05C4 && code <= 0x05C5)
                                || (code == 0x05C7)
                        then
                            Just MarkNonSpacing

                        else if (code == 0x05C3) || (code == 0x05C6) then
                            Just PunctuationOther

                        else if code >= 0x05D0 && code <= 0x05EA then
                            Just LetterOther

                        else
                            Nothing

                    else if code >= 0x05EF && code <= 0x05F2 then
                        Just LetterOther

                    else if
                        (code >= 0x05F3 && code <= 0x05F4)
                            || (code >= 0x0609 && code <= 0x060A)
                            || (code == 0x060C)
                    then
                        Just PunctuationOther

                    else if code >= 0x0600 && code <= 0x0605 then
                        Just OtherFormat

                    else if code >= 0x0606 && code <= 0x0608 then
                        Just SymbolMath

                    else if code == 0x060B then
                        Just SymbolCurrency

                    else
                        Nothing

                else if code < 0x063F then
                    if
                        (code == 0x060D)
                            || (code == 0x061B)
                            || (code >= 0x061D && code <= 0x061F)
                    then
                        Just PunctuationOther

                    else if code >= 0x060E && code <= 0x060F then
                        Just SymbolOther

                    else if code >= 0x0610 && code <= 0x061A then
                        Just MarkNonSpacing

                    else if code == 0x061C then
                        Just OtherFormat

                    else if code >= 0x0620 && code <= 0x063E then
                        Just LetterOther

                    else
                        Nothing

                else if
                    (code == 0x063F)
                        || (code >= 0x0641 && code <= 0x064A)
                        || (code >= 0x066E && code <= 0x066F)
                        || (code >= 0x0671 && code <= 0x06D2)
                then
                    Just LetterOther

                else if code == 0x0640 then
                    Just LetterModifier

                else if (code >= 0x064B && code <= 0x065F) || (code == 0x0670) then
                    Just MarkNonSpacing

                else if code >= 0x0660 && code <= 0x0669 then
                    Just NumberDecimalDigit

                else if code >= 0x066A && code <= 0x066D then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x09F9 then
                if code < 0x088F then
                    if code < 0x07BF then
                        if code < 0x06EF then
                            if
                                (code == 0x06D3)
                                    || (code == 0x06D5)
                                    || (code == 0x06EE)
                            then
                                Just LetterOther

                            else if code == 0x06D4 then
                                Just PunctuationOther

                            else if
                                (code >= 0x06D6 && code <= 0x06DC)
                                    || (code >= 0x06DF && code <= 0x06E4)
                                    || (code >= 0x06E7 && code <= 0x06E8)
                                    || (code >= 0x06EA && code <= 0x06ED)
                            then
                                Just MarkNonSpacing

                            else if code == 0x06DD then
                                Just OtherFormat

                            else if (code == 0x06DE) || (code == 0x06E9) then
                                Just SymbolOther

                            else if code >= 0x06E5 && code <= 0x06E6 then
                                Just LetterModifier

                            else
                                Nothing

                        else if code < 0x070F then
                            if
                                (code == 0x06EF)
                                    || (code >= 0x06FA && code <= 0x06FC)
                                    || (code == 0x06FF)
                            then
                                Just LetterOther

                            else if code >= 0x06F0 && code <= 0x06F9 then
                                Just NumberDecimalDigit

                            else if code >= 0x06FD && code <= 0x06FE then
                                Just SymbolOther

                            else if code >= 0x0700 && code <= 0x070D then
                                Just PunctuationOther

                            else
                                Nothing

                        else if code == 0x070F then
                            Just OtherFormat

                        else if
                            (code == 0x0710)
                                || (code >= 0x0712 && code <= 0x072F)
                                || (code >= 0x074D && code <= 0x07A5)
                                || (code == 0x07B1)
                        then
                            Just LetterOther

                        else if
                            (code == 0x0711)
                                || (code >= 0x0730 && code <= 0x074A)
                                || (code >= 0x07A6 && code <= 0x07B0)
                        then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code < 0x081A then
                        if code >= 0x07C0 && code <= 0x07C9 then
                            Just NumberDecimalDigit

                        else if
                            (code >= 0x07CA && code <= 0x07EA)
                                || (code >= 0x0800 && code <= 0x0815)
                        then
                            Just LetterOther

                        else if
                            (code >= 0x07EB && code <= 0x07F3)
                                || (code == 0x07FD)
                                || (code >= 0x0816 && code <= 0x0819)
                        then
                            Just MarkNonSpacing

                        else if
                            (code >= 0x07F4 && code <= 0x07F5)
                                || (code == 0x07FA)
                        then
                            Just LetterModifier

                        else if code == 0x07F6 then
                            Just SymbolOther

                        else if code >= 0x07F7 && code <= 0x07F9 then
                            Just PunctuationOther

                        else if code >= 0x07FE && code <= 0x07FF then
                            Just SymbolCurrency

                        else
                            Nothing

                    else if code < 0x083F then
                        if
                            (code == 0x081A)
                                || (code == 0x0824)
                                || (code == 0x0828)
                        then
                            Just LetterModifier

                        else if
                            (code >= 0x081B && code <= 0x0823)
                                || (code >= 0x0825 && code <= 0x0827)
                                || (code >= 0x0829 && code <= 0x082D)
                        then
                            Just MarkNonSpacing

                        else if code >= 0x0830 && code <= 0x083E then
                            Just PunctuationOther

                        else
                            Nothing

                    else if
                        (code >= 0x0840 && code <= 0x0858)
                            || (code >= 0x0860 && code <= 0x086A)
                            || (code >= 0x0870 && code <= 0x0887)
                            || (code >= 0x0889 && code <= 0x088E)
                    then
                        Just LetterOther

                    else if code >= 0x0859 && code <= 0x085B then
                        Just MarkNonSpacing

                    else if code == 0x085E then
                        Just PunctuationOther

                    else if code == 0x0888 then
                        Just SymbolModifier

                    else
                        Nothing

                else if code < 0x0970 then
                    if code < 0x093C then
                        if
                            (code >= 0x0890 && code <= 0x0891)
                                || (code == 0x08E2)
                        then
                            Just OtherFormat

                        else if
                            (code >= 0x0898 && code <= 0x089F)
                                || (code >= 0x08CA && code <= 0x08E1)
                                || (code >= 0x08E3 && code <= 0x0902)
                                || (code == 0x093A)
                        then
                            Just MarkNonSpacing

                        else if
                            (code >= 0x08A0 && code <= 0x08C8)
                                || (code >= 0x0904 && code <= 0x0939)
                        then
                            Just LetterOther

                        else if code == 0x08C9 then
                            Just LetterModifier

                        else if (code == 0x0903) || (code == 0x093B) then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x094D then
                        if
                            (code == 0x093C)
                                || (code >= 0x0941 && code <= 0x0948)
                        then
                            Just MarkNonSpacing

                        else if code == 0x093D then
                            Just LetterOther

                        else if
                            (code >= 0x093E && code <= 0x0940)
                                || (code >= 0x0949 && code <= 0x094C)
                        then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if
                        (code == 0x094D)
                            || (code >= 0x0951 && code <= 0x0957)
                            || (code >= 0x0962 && code <= 0x0963)
                    then
                        Just MarkNonSpacing

                    else if code >= 0x094E && code <= 0x094F then
                        Just MarkSpacingCombining

                    else if (code == 0x0950) || (code >= 0x0958 && code <= 0x0961) then
                        Just LetterOther

                    else if code >= 0x0964 && code <= 0x0965 then
                        Just PunctuationOther

                    else if code >= 0x0966 && code <= 0x096F then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code < 0x09BD then
                    if code == 0x0970 then
                        Just PunctuationOther

                    else if code == 0x0971 then
                        Just LetterModifier

                    else if
                        (code >= 0x0972 && code <= 0x0980)
                            || (code >= 0x0985 && code <= 0x098C)
                            || (code >= 0x098F && code <= 0x0990)
                            || (code >= 0x0993 && code <= 0x09A8)
                            || (code >= 0x09AA && code <= 0x09B0)
                            || (code == 0x09B2)
                            || (code >= 0x09B6 && code <= 0x09B9)
                    then
                        Just LetterOther

                    else if (code == 0x0981) || (code == 0x09BC) then
                        Just MarkNonSpacing

                    else if code >= 0x0982 && code <= 0x0983 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x09D6 then
                    if (code == 0x09BD) || (code == 0x09CE) then
                        Just LetterOther

                    else if
                        (code >= 0x09BE && code <= 0x09C0)
                            || (code >= 0x09C7 && code <= 0x09C8)
                            || (code >= 0x09CB && code <= 0x09CC)
                    then
                        Just MarkSpacingCombining

                    else if (code >= 0x09C1 && code <= 0x09C4) || (code == 0x09CD) then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x09D7 then
                    Just MarkSpacingCombining

                else if
                    (code >= 0x09DC && code <= 0x09DD)
                        || (code >= 0x09DF && code <= 0x09E1)
                        || (code >= 0x09F0 && code <= 0x09F1)
                then
                    Just LetterOther

                else if code >= 0x09E2 && code <= 0x09E3 then
                    Just MarkNonSpacing

                else if code >= 0x09E6 && code <= 0x09EF then
                    Just NumberDecimalDigit

                else if code >= 0x09F2 && code <= 0x09F3 then
                    Just SymbolCurrency

                else if code >= 0x09F4 && code <= 0x09F8 then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x0AF9 then
                if code < 0x0A71 then
                    if code < 0x0A31 then
                        if code == 0x09F9 then
                            Just NumberOther

                        else if code == 0x09FA then
                            Just SymbolOther

                        else if code == 0x09FB then
                            Just SymbolCurrency

                        else if
                            (code == 0x09FC)
                                || (code >= 0x0A05 && code <= 0x0A0A)
                                || (code >= 0x0A0F && code <= 0x0A10)
                                || (code >= 0x0A13 && code <= 0x0A28)
                                || (code >= 0x0A2A && code <= 0x0A30)
                        then
                            Just LetterOther

                        else if code == 0x09FD then
                            Just PunctuationOther

                        else if
                            (code == 0x09FE)
                                || (code >= 0x0A01 && code <= 0x0A02)
                        then
                            Just MarkNonSpacing

                        else if code == 0x0A03 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x0A46 then
                        if
                            (code >= 0x0A32 && code <= 0x0A33)
                                || (code >= 0x0A35 && code <= 0x0A36)
                                || (code >= 0x0A38 && code <= 0x0A39)
                        then
                            Just LetterOther

                        else if
                            (code == 0x0A3C)
                                || (code >= 0x0A41 && code <= 0x0A42)
                        then
                            Just MarkNonSpacing

                        else if code >= 0x0A3E && code <= 0x0A40 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if
                        (code >= 0x0A47 && code <= 0x0A48)
                            || (code >= 0x0A4B && code <= 0x0A4D)
                            || (code == 0x0A51)
                            || (code == 0x0A70)
                    then
                        Just MarkNonSpacing

                    else if (code >= 0x0A59 && code <= 0x0A5C) || (code == 0x0A5E) then
                        Just LetterOther

                    else if code >= 0x0A66 && code <= 0x0A6F then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code < 0x0ABC then
                    if
                        (code == 0x0A71)
                            || (code == 0x0A75)
                            || (code >= 0x0A81 && code <= 0x0A82)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x0A72 && code <= 0x0A74)
                            || (code >= 0x0A85 && code <= 0x0A8D)
                            || (code >= 0x0A8F && code <= 0x0A91)
                            || (code >= 0x0A93 && code <= 0x0AA8)
                            || (code >= 0x0AAA && code <= 0x0AB0)
                            || (code >= 0x0AB2 && code <= 0x0AB3)
                            || (code >= 0x0AB5 && code <= 0x0AB9)
                    then
                        Just LetterOther

                    else if code == 0x0A76 then
                        Just PunctuationOther

                    else if code == 0x0A83 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x0ACC then
                    if
                        (code == 0x0ABC)
                            || (code >= 0x0AC1 && code <= 0x0AC5)
                            || (code >= 0x0AC7 && code <= 0x0AC8)
                    then
                        Just MarkNonSpacing

                    else if code == 0x0ABD then
                        Just LetterOther

                    else if
                        (code >= 0x0ABE && code <= 0x0AC0)
                            || (code == 0x0AC9)
                            || (code == 0x0ACB)
                    then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code == 0x0ACC then
                    Just MarkSpacingCombining

                else if (code == 0x0ACD) || (code >= 0x0AE2 && code <= 0x0AE3) then
                    Just MarkNonSpacing

                else if (code == 0x0AD0) || (code >= 0x0AE0 && code <= 0x0AE1) then
                    Just LetterOther

                else if code >= 0x0AE6 && code <= 0x0AEF then
                    Just NumberDecimalDigit

                else if code == 0x0AF0 then
                    Just PunctuationOther

                else if code == 0x0AF1 then
                    Just SymbolCurrency

                else
                    Nothing

            else if code < 0x0B70 then
                if code < 0x0B3D then
                    if
                        (code == 0x0AF9)
                            || (code >= 0x0B05 && code <= 0x0B0C)
                            || (code >= 0x0B0F && code <= 0x0B10)
                            || (code >= 0x0B13 && code <= 0x0B28)
                            || (code >= 0x0B2A && code <= 0x0B30)
                            || (code >= 0x0B32 && code <= 0x0B33)
                            || (code >= 0x0B35 && code <= 0x0B39)
                    then
                        Just LetterOther

                    else if
                        (code >= 0x0AFA && code <= 0x0AFF)
                            || (code == 0x0B01)
                            || (code == 0x0B3C)
                    then
                        Just MarkNonSpacing

                    else if code >= 0x0B02 && code <= 0x0B03 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x0B4C then
                    if code == 0x0B3D then
                        Just LetterOther

                    else if
                        (code == 0x0B3E)
                            || (code == 0x0B40)
                            || (code >= 0x0B47 && code <= 0x0B48)
                            || (code == 0x0B4B)
                    then
                        Just MarkSpacingCombining

                    else if (code == 0x0B3F) || (code >= 0x0B41 && code <= 0x0B44) then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if (code == 0x0B4C) || (code == 0x0B57) then
                    Just MarkSpacingCombining

                else if
                    (code == 0x0B4D)
                        || (code >= 0x0B55 && code <= 0x0B56)
                        || (code >= 0x0B62 && code <= 0x0B63)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0x0B5C && code <= 0x0B5D)
                        || (code >= 0x0B5F && code <= 0x0B61)
                then
                    Just LetterOther

                else if code >= 0x0B66 && code <= 0x0B6F then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x0BAD then
                if code < 0x0B8D then
                    if code == 0x0B70 then
                        Just SymbolOther

                    else if
                        (code == 0x0B71)
                            || (code == 0x0B83)
                            || (code >= 0x0B85 && code <= 0x0B8A)
                    then
                        Just LetterOther

                    else if code >= 0x0B72 && code <= 0x0B77 then
                        Just NumberOther

                    else if code == 0x0B82 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if
                    (code >= 0x0B8E && code <= 0x0B90)
                        || (code >= 0x0B92 && code <= 0x0B95)
                        || (code >= 0x0B99 && code <= 0x0B9A)
                        || (code == 0x0B9C)
                        || (code >= 0x0B9E && code <= 0x0B9F)
                        || (code >= 0x0BA3 && code <= 0x0BA4)
                        || (code >= 0x0BA8 && code <= 0x0BAA)
                then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x0BCF then
                if code >= 0x0BAE && code <= 0x0BB9 then
                    Just LetterOther

                else if
                    (code >= 0x0BBE && code <= 0x0BBF)
                        || (code >= 0x0BC1 && code <= 0x0BC2)
                        || (code >= 0x0BC6 && code <= 0x0BC8)
                        || (code >= 0x0BCA && code <= 0x0BCC)
                then
                    Just MarkSpacingCombining

                else if (code == 0x0BC0) || (code == 0x0BCD) then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code == 0x0BD0 then
                Just LetterOther

            else if code == 0x0BD7 then
                Just MarkSpacingCombining

            else if code >= 0x0BE6 && code <= 0x0BEF then
                Just NumberDecimalDigit

            else if code >= 0x0BF0 && code <= 0x0BF2 then
                Just NumberOther

            else if (code >= 0x0BF3 && code <= 0x0BF8) || (code == 0x0BFA) then
                Just SymbolOther

            else if code == 0x0BF9 then
                Just SymbolCurrency

            else
                Nothing

        else if code < 0x1E0B then
            if code < 0x12BF then
                if code < 0x0EA6 then
                    if code < 0x0D11 then
                        if code < 0x0C83 then
                            if code < 0x0C49 then
                                if
                                    (code == 0x0C00)
                                        || (code == 0x0C04)
                                        || (code == 0x0C3C)
                                        || (code >= 0x0C3E && code <= 0x0C40)
                                        || (code >= 0x0C46 && code <= 0x0C48)
                                then
                                    Just MarkNonSpacing

                                else if
                                    (code >= 0x0C01 && code <= 0x0C03)
                                        || (code >= 0x0C41 && code <= 0x0C44)
                                then
                                    Just MarkSpacingCombining

                                else if
                                    (code >= 0x0C05 && code <= 0x0C0C)
                                        || (code >= 0x0C0E && code <= 0x0C10)
                                        || (code >= 0x0C12 && code <= 0x0C28)
                                        || (code >= 0x0C2A && code <= 0x0C39)
                                        || (code == 0x0C3D)
                                then
                                    Just LetterOther

                                else
                                    Nothing

                            else if code < 0x0C65 then
                                if
                                    (code >= 0x0C4A && code <= 0x0C4D)
                                        || (code >= 0x0C55 && code <= 0x0C56)
                                        || (code >= 0x0C62 && code <= 0x0C63)
                                then
                                    Just MarkNonSpacing

                                else if
                                    (code >= 0x0C58 && code <= 0x0C5A)
                                        || (code == 0x0C5D)
                                        || (code >= 0x0C60 && code <= 0x0C61)
                                then
                                    Just LetterOther

                                else
                                    Nothing

                            else if code >= 0x0C66 && code <= 0x0C6F then
                                Just NumberDecimalDigit

                            else if code == 0x0C77 then
                                Just PunctuationOther

                            else if code >= 0x0C78 && code <= 0x0C7E then
                                Just NumberOther

                            else if code == 0x0C7F then
                                Just SymbolOther

                            else if code == 0x0C80 then
                                Just LetterOther

                            else if code == 0x0C81 then
                                Just MarkNonSpacing

                            else if code == 0x0C82 then
                                Just MarkSpacingCombining

                            else
                                Nothing

                        else if code < 0x0CC6 then
                            if
                                (code == 0x0C83)
                                    || (code == 0x0CBE)
                                    || (code >= 0x0CC0 && code <= 0x0CC4)
                            then
                                Just MarkSpacingCombining

                            else if code == 0x0C84 then
                                Just PunctuationOther

                            else if
                                (code >= 0x0C85 && code <= 0x0C8C)
                                    || (code >= 0x0C8E && code <= 0x0C90)
                                    || (code >= 0x0C92 && code <= 0x0CA8)
                                    || (code >= 0x0CAA && code <= 0x0CB3)
                                    || (code >= 0x0CB5 && code <= 0x0CB9)
                                    || (code == 0x0CBD)
                            then
                                Just LetterOther

                            else if (code == 0x0CBC) || (code == 0x0CBF) then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if code < 0x0CE1 then
                            if
                                (code == 0x0CC6)
                                    || (code >= 0x0CCC && code <= 0x0CCD)
                            then
                                Just MarkNonSpacing

                            else if
                                (code >= 0x0CC7 && code <= 0x0CC8)
                                    || (code >= 0x0CCA && code <= 0x0CCB)
                                    || (code >= 0x0CD5 && code <= 0x0CD6)
                            then
                                Just MarkSpacingCombining

                            else if
                                (code >= 0x0CDD && code <= 0x0CDE)
                                    || (code == 0x0CE0)
                            then
                                Just LetterOther

                            else
                                Nothing

                        else if
                            (code == 0x0CE1)
                                || (code >= 0x0CF1 && code <= 0x0CF2)
                                || (code >= 0x0D04 && code <= 0x0D0C)
                                || (code >= 0x0D0E && code <= 0x0D10)
                        then
                            Just LetterOther

                        else if
                            (code >= 0x0CE2 && code <= 0x0CE3)
                                || (code >= 0x0D00 && code <= 0x0D01)
                        then
                            Just MarkNonSpacing

                        else if code >= 0x0CE6 && code <= 0x0CEF then
                            Just NumberDecimalDigit

                        else if code >= 0x0D02 && code <= 0x0D03 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x0DBF then
                        if code < 0x0D57 then
                            if
                                (code >= 0x0D12 && code <= 0x0D3A)
                                    || (code == 0x0D3D)
                                    || (code == 0x0D4E)
                                    || (code >= 0x0D54 && code <= 0x0D56)
                            then
                                Just LetterOther

                            else if
                                (code >= 0x0D3B && code <= 0x0D3C)
                                    || (code >= 0x0D41 && code <= 0x0D44)
                                    || (code == 0x0D4D)
                            then
                                Just MarkNonSpacing

                            else if
                                (code >= 0x0D3E && code <= 0x0D40)
                                    || (code >= 0x0D46 && code <= 0x0D48)
                                    || (code >= 0x0D4A && code <= 0x0D4C)
                            then
                                Just MarkSpacingCombining

                            else if code == 0x0D4F then
                                Just SymbolOther

                            else
                                Nothing

                        else if code < 0x0D79 then
                            if code == 0x0D57 then
                                Just MarkSpacingCombining

                            else if
                                (code >= 0x0D58 && code <= 0x0D5E)
                                    || (code >= 0x0D70 && code <= 0x0D78)
                            then
                                Just NumberOther

                            else if code >= 0x0D5F && code <= 0x0D61 then
                                Just LetterOther

                            else if code >= 0x0D62 && code <= 0x0D63 then
                                Just MarkNonSpacing

                            else if code >= 0x0D66 && code <= 0x0D6F then
                                Just NumberDecimalDigit

                            else
                                Nothing

                        else if code == 0x0D79 then
                            Just SymbolOther

                        else if
                            (code >= 0x0D7A && code <= 0x0D7F)
                                || (code >= 0x0D85 && code <= 0x0D96)
                                || (code >= 0x0D9A && code <= 0x0DB1)
                                || (code >= 0x0DB3 && code <= 0x0DBB)
                                || (code == 0x0DBD)
                        then
                            Just LetterOther

                        else if code == 0x0D81 then
                            Just MarkNonSpacing

                        else if code >= 0x0D82 && code <= 0x0D83 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x0E33 then
                        if
                            (code >= 0x0DC0 && code <= 0x0DC6)
                                || (code >= 0x0E01 && code <= 0x0E30)
                                || (code == 0x0E32)
                        then
                            Just LetterOther

                        else if
                            (code == 0x0DCA)
                                || (code >= 0x0DD2 && code <= 0x0DD4)
                                || (code == 0x0DD6)
                                || (code == 0x0E31)
                        then
                            Just MarkNonSpacing

                        else if
                            (code >= 0x0DCF && code <= 0x0DD1)
                                || (code >= 0x0DD8 && code <= 0x0DDF)
                                || (code >= 0x0DF2 && code <= 0x0DF3)
                        then
                            Just MarkSpacingCombining

                        else if code >= 0x0DE6 && code <= 0x0DEF then
                            Just NumberDecimalDigit

                        else if code == 0x0DF4 then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x0E4F then
                        if
                            (code == 0x0E33)
                                || (code >= 0x0E40 && code <= 0x0E45)
                        then
                            Just LetterOther

                        else if
                            (code >= 0x0E34 && code <= 0x0E3A)
                                || (code >= 0x0E47 && code <= 0x0E4E)
                        then
                            Just MarkNonSpacing

                        else if code == 0x0E3F then
                            Just SymbolCurrency

                        else if code == 0x0E46 then
                            Just LetterModifier

                        else
                            Nothing

                    else if (code == 0x0E4F) || (code >= 0x0E5A && code <= 0x0E5B) then
                        Just PunctuationOther

                    else if code >= 0x0E50 && code <= 0x0E59 then
                        Just NumberDecimalDigit

                    else if
                        (code >= 0x0E81 && code <= 0x0E82)
                            || (code == 0x0E84)
                            || (code >= 0x0E86 && code <= 0x0E8A)
                            || (code >= 0x0E8C && code <= 0x0EA3)
                            || (code == 0x0EA5)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x102C then
                    if code < 0x0F38 then
                        if code < 0x0F03 then
                            if
                                (code >= 0x0EA7 && code <= 0x0EB0)
                                    || (code >= 0x0EB2 && code <= 0x0EB3)
                                    || (code == 0x0EBD)
                                    || (code >= 0x0EC0 && code <= 0x0EC4)
                                    || (code >= 0x0EDC && code <= 0x0EDF)
                                    || (code == 0x0F00)
                            then
                                Just LetterOther

                            else if
                                (code == 0x0EB1)
                                    || (code >= 0x0EB4 && code <= 0x0EBC)
                                    || (code >= 0x0EC8 && code <= 0x0ECD)
                            then
                                Just MarkNonSpacing

                            else if code == 0x0EC6 then
                                Just LetterModifier

                            else if code >= 0x0ED0 && code <= 0x0ED9 then
                                Just NumberDecimalDigit

                            else if code >= 0x0F01 && code <= 0x0F02 then
                                Just SymbolOther

                            else
                                Nothing

                        else if code < 0x0F19 then
                            if
                                (code == 0x0F03)
                                    || (code == 0x0F13)
                                    || (code >= 0x0F15 && code <= 0x0F17)
                            then
                                Just SymbolOther

                            else if
                                (code >= 0x0F04 && code <= 0x0F12)
                                    || (code == 0x0F14)
                            then
                                Just PunctuationOther

                            else if code == 0x0F18 then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if
                            (code == 0x0F19)
                                || (code == 0x0F35)
                                || (code == 0x0F37)
                        then
                            Just MarkNonSpacing

                        else if
                            (code >= 0x0F1A && code <= 0x0F1F)
                                || (code == 0x0F34)
                                || (code == 0x0F36)
                        then
                            Just SymbolOther

                        else if code >= 0x0F20 && code <= 0x0F29 then
                            Just NumberDecimalDigit

                        else if code >= 0x0F2A && code <= 0x0F33 then
                            Just NumberOther

                        else
                            Nothing

                    else if code < 0x0F85 then
                        if code == 0x0F38 then
                            Just SymbolOther

                        else if
                            (code == 0x0F39)
                                || (code >= 0x0F71 && code <= 0x0F7E)
                                || (code >= 0x0F80 && code <= 0x0F84)
                        then
                            Just MarkNonSpacing

                        else if (code == 0x0F3A) || (code == 0x0F3C) then
                            Just PunctuationOpen

                        else if (code == 0x0F3B) || (code == 0x0F3D) then
                            Just PunctuationClose

                        else if
                            (code >= 0x0F3E && code <= 0x0F3F)
                                || (code == 0x0F7F)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x0F40 && code <= 0x0F47)
                                || (code >= 0x0F49 && code <= 0x0F6C)
                        then
                            Just LetterOther

                        else
                            Nothing

                    else if code < 0x0FC6 then
                        if code == 0x0F85 then
                            Just PunctuationOther

                        else if
                            (code >= 0x0F86 && code <= 0x0F87)
                                || (code >= 0x0F8D && code <= 0x0F97)
                                || (code >= 0x0F99 && code <= 0x0FBC)
                        then
                            Just MarkNonSpacing

                        else if code >= 0x0F88 && code <= 0x0F8C then
                            Just LetterOther

                        else if code >= 0x0FBE && code <= 0x0FC5 then
                            Just SymbolOther

                        else
                            Nothing

                    else if code == 0x0FC6 then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x0FC7 && code <= 0x0FCC)
                            || (code >= 0x0FCE && code <= 0x0FCF)
                            || (code >= 0x0FD5 && code <= 0x0FD8)
                    then
                        Just SymbolOther

                    else if
                        (code >= 0x0FD0 && code <= 0x0FD4)
                            || (code >= 0x0FD9 && code <= 0x0FDA)
                    then
                        Just PunctuationOther

                    else if code >= 0x1000 && code <= 0x102A then
                        Just LetterOther

                    else if code == 0x102B then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x1084 then
                    if code < 0x1055 then
                        if
                            (code == 0x102C)
                                || (code == 0x1031)
                                || (code == 0x1038)
                                || (code >= 0x103B && code <= 0x103C)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x102D && code <= 0x1030)
                                || (code >= 0x1032 && code <= 0x1037)
                                || (code >= 0x1039 && code <= 0x103A)
                                || (code >= 0x103D && code <= 0x103E)
                        then
                            Just MarkNonSpacing

                        else if
                            (code == 0x103F)
                                || (code >= 0x1050 && code <= 0x1054)
                        then
                            Just LetterOther

                        else if code >= 0x1040 && code <= 0x1049 then
                            Just NumberDecimalDigit

                        else if code >= 0x104A && code <= 0x104F then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x1064 then
                        if
                            (code == 0x1055)
                                || (code >= 0x105A && code <= 0x105D)
                                || (code == 0x1061)
                        then
                            Just LetterOther

                        else if
                            (code >= 0x1056 && code <= 0x1057)
                                || (code >= 0x1062 && code <= 0x1063)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x1058 && code <= 0x1059)
                                || (code >= 0x105E && code <= 0x1060)
                        then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if
                        (code == 0x1064)
                            || (code >= 0x1067 && code <= 0x106D)
                            || (code == 0x1083)
                    then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x1065 && code <= 0x1066)
                            || (code >= 0x106E && code <= 0x1070)
                            || (code >= 0x1075 && code <= 0x1081)
                    then
                        Just LetterOther

                    else if (code >= 0x1071 && code <= 0x1074) || (code == 0x1082) then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x10CF then
                    if code < 0x108F then
                        if
                            (code == 0x1084)
                                || (code >= 0x1087 && code <= 0x108C)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x1085 && code <= 0x1086)
                                || (code == 0x108D)
                        then
                            Just MarkNonSpacing

                        else if code == 0x108E then
                            Just LetterOther

                        else
                            Nothing

                    else if (code == 0x108F) || (code >= 0x109A && code <= 0x109C) then
                        Just MarkSpacingCombining

                    else if code >= 0x1090 && code <= 0x1099 then
                        Just NumberDecimalDigit

                    else if code == 0x109D then
                        Just MarkNonSpacing

                    else if code >= 0x109E && code <= 0x109F then
                        Just SymbolOther

                    else if
                        (code >= 0x10A0 && code <= 0x10C5)
                            || (code == 0x10C7)
                            || (code == 0x10CD)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1257 then
                    if
                        (code >= 0x10D0 && code <= 0x10FA)
                            || (code >= 0x10FD && code <= 0x10FF)
                    then
                        Just LetterLowercase

                    else if code == 0x10FB then
                        Just PunctuationOther

                    else if code == 0x10FC then
                        Just LetterModifier

                    else if
                        (code >= 0x1100 && code <= 0x1248)
                            || (code >= 0x124A && code <= 0x124D)
                            || (code >= 0x1250 && code <= 0x1256)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if
                    (code == 0x1258)
                        || (code >= 0x125A && code <= 0x125D)
                        || (code >= 0x1260 && code <= 0x1288)
                        || (code >= 0x128A && code <= 0x128D)
                        || (code >= 0x1290 && code <= 0x12B0)
                        || (code >= 0x12B2 && code <= 0x12B5)
                        || (code >= 0x12B8 && code <= 0x12BE)
                then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x1A60 then
                if code < 0x17DA then
                    if code < 0x16ED then
                        if code < 0x13F7 then
                            if
                                (code == 0x12C0)
                                    || (code >= 0x12C2 && code <= 0x12C5)
                                    || (code >= 0x12C8 && code <= 0x12D6)
                                    || (code >= 0x12D8 && code <= 0x1310)
                                    || (code >= 0x1312 && code <= 0x1315)
                                    || (code >= 0x1318 && code <= 0x135A)
                                    || (code >= 0x1380 && code <= 0x138F)
                            then
                                Just LetterOther

                            else if code >= 0x135D && code <= 0x135F then
                                Just MarkNonSpacing

                            else if code >= 0x1360 && code <= 0x1368 then
                                Just PunctuationOther

                            else if code >= 0x1369 && code <= 0x137C then
                                Just NumberOther

                            else if code >= 0x1390 && code <= 0x1399 then
                                Just SymbolOther

                            else if code >= 0x13A0 && code <= 0x13F5 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code >= 0x13F8 && code <= 0x13FD then
                            Just LetterLowercase

                        else if code == 0x1400 then
                            Just PunctuationDash

                        else if
                            (code >= 0x1401 && code <= 0x166C)
                                || (code >= 0x166F && code <= 0x167F)
                                || (code >= 0x1681 && code <= 0x169A)
                                || (code >= 0x16A0 && code <= 0x16EA)
                        then
                            Just LetterOther

                        else if code == 0x166D then
                            Just SymbolOther

                        else if
                            (code == 0x166E)
                                || (code >= 0x16EB && code <= 0x16EC)
                        then
                            Just PunctuationOther

                        else if code == 0x1680 then
                            Just SeparatorSpace

                        else if code == 0x169B then
                            Just PunctuationOpen

                        else if code == 0x169C then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code < 0x176D then
                        if code < 0x171E then
                            if code == 0x16ED then
                                Just PunctuationOther

                            else if code >= 0x16EE && code <= 0x16F0 then
                                Just NumberLetter

                            else if
                                (code >= 0x16F1 && code <= 0x16F8)
                                    || (code >= 0x1700 && code <= 0x1711)
                            then
                                Just LetterOther

                            else if code >= 0x1712 && code <= 0x1714 then
                                Just MarkNonSpacing

                            else if code == 0x1715 then
                                Just MarkSpacingCombining

                            else
                                Nothing

                        else if
                            (code >= 0x171F && code <= 0x1731)
                                || (code >= 0x1740 && code <= 0x1751)
                                || (code >= 0x1760 && code <= 0x176C)
                        then
                            Just LetterOther

                        else if
                            (code >= 0x1732 && code <= 0x1733)
                                || (code >= 0x1752 && code <= 0x1753)
                        then
                            Just MarkNonSpacing

                        else if code == 0x1734 then
                            Just MarkSpacingCombining

                        else if code >= 0x1735 && code <= 0x1736 then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x17BD then
                        if
                            (code >= 0x176E && code <= 0x1770)
                                || (code >= 0x1780 && code <= 0x17B3)
                        then
                            Just LetterOther

                        else if
                            (code >= 0x1772 && code <= 0x1773)
                                || (code >= 0x17B4 && code <= 0x17B5)
                                || (code >= 0x17B7 && code <= 0x17BC)
                        then
                            Just MarkNonSpacing

                        else if code == 0x17B6 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if
                        (code == 0x17BD)
                            || (code == 0x17C6)
                            || (code >= 0x17C9 && code <= 0x17D3)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x17BE && code <= 0x17C5)
                            || (code >= 0x17C7 && code <= 0x17C8)
                    then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x17D4 && code <= 0x17D6)
                            || (code >= 0x17D8 && code <= 0x17D9)
                    then
                        Just PunctuationOther

                    else if code == 0x17D7 then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0x1926 then
                    if code < 0x180F then
                        if
                            (code == 0x17DA)
                                || (code >= 0x1800 && code <= 0x1805)
                                || (code >= 0x1807 && code <= 0x180A)
                        then
                            Just PunctuationOther

                        else if code == 0x17DB then
                            Just SymbolCurrency

                        else if code == 0x17DC then
                            Just LetterOther

                        else if
                            (code == 0x17DD)
                                || (code >= 0x180B && code <= 0x180D)
                        then
                            Just MarkNonSpacing

                        else if code >= 0x17E0 && code <= 0x17E9 then
                            Just NumberDecimalDigit

                        else if code >= 0x17F0 && code <= 0x17F9 then
                            Just NumberOther

                        else if code == 0x1806 then
                            Just PunctuationDash

                        else if code == 0x180E then
                            Just OtherFormat

                        else
                            Nothing

                    else if code < 0x1886 then
                        if (code == 0x180F) || (code == 0x1885) then
                            Just MarkNonSpacing

                        else if code >= 0x1810 && code <= 0x1819 then
                            Just NumberDecimalDigit

                        else if
                            (code >= 0x1820 && code <= 0x1842)
                                || (code >= 0x1844 && code <= 0x1878)
                                || (code >= 0x1880 && code <= 0x1884)
                        then
                            Just LetterOther

                        else if code == 0x1843 then
                            Just LetterModifier

                        else
                            Nothing

                    else if
                        (code == 0x1886)
                            || (code == 0x18A9)
                            || (code >= 0x1920 && code <= 0x1922)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x1887 && code <= 0x18A8)
                            || (code == 0x18AA)
                            || (code >= 0x18B0 && code <= 0x18F5)
                            || (code >= 0x1900 && code <= 0x191E)
                    then
                        Just LetterOther

                    else if code >= 0x1923 && code <= 0x1925 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x19AF then
                    if code < 0x1938 then
                        if
                            (code == 0x1926)
                                || (code >= 0x1929 && code <= 0x192B)
                                || (code >= 0x1930 && code <= 0x1931)
                                || (code >= 0x1933 && code <= 0x1937)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x1927 && code <= 0x1928)
                                || (code == 0x1932)
                        then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x1938 then
                        Just MarkSpacingCombining

                    else if code >= 0x1939 && code <= 0x193B then
                        Just MarkNonSpacing

                    else if code == 0x1940 then
                        Just SymbolOther

                    else if code >= 0x1944 && code <= 0x1945 then
                        Just PunctuationOther

                    else if code >= 0x1946 && code <= 0x194F then
                        Just NumberDecimalDigit

                    else if
                        (code >= 0x1950 && code <= 0x196D)
                            || (code >= 0x1970 && code <= 0x1974)
                            || (code >= 0x1980 && code <= 0x19AB)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x1A1A then
                    if
                        (code >= 0x19B0 && code <= 0x19C9)
                            || (code >= 0x1A00 && code <= 0x1A16)
                    then
                        Just LetterOther

                    else if code >= 0x19D0 && code <= 0x19D9 then
                        Just NumberDecimalDigit

                    else if code == 0x19DA then
                        Just NumberOther

                    else if code >= 0x19DE && code <= 0x19FF then
                        Just SymbolOther

                    else if code >= 0x1A17 && code <= 0x1A18 then
                        Just MarkNonSpacing

                    else if code == 0x1A19 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if (code == 0x1A1A) || (code == 0x1A55) || (code == 0x1A57) then
                    Just MarkSpacingCombining

                else if
                    (code == 0x1A1B)
                        || (code == 0x1A56)
                        || (code >= 0x1A58 && code <= 0x1A5E)
                then
                    Just MarkNonSpacing

                else if code >= 0x1A1E && code <= 0x1A1F then
                    Just PunctuationOther

                else if code >= 0x1A20 && code <= 0x1A54 then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x1BED then
                if code < 0x1B41 then
                    if code < 0x1AA7 then
                        if
                            (code == 0x1A60)
                                || (code == 0x1A62)
                                || (code >= 0x1A65 && code <= 0x1A6C)
                                || (code >= 0x1A73 && code <= 0x1A7C)
                                || (code == 0x1A7F)
                        then
                            Just MarkNonSpacing

                        else if
                            (code == 0x1A61)
                                || (code >= 0x1A63 && code <= 0x1A64)
                                || (code >= 0x1A6D && code <= 0x1A72)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x1A80 && code <= 0x1A89)
                                || (code >= 0x1A90 && code <= 0x1A99)
                        then
                            Just NumberDecimalDigit

                        else if code >= 0x1AA0 && code <= 0x1AA6 then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x1B04 then
                        if code == 0x1AA7 then
                            Just LetterModifier

                        else if code >= 0x1AA8 && code <= 0x1AAD then
                            Just PunctuationOther

                        else if
                            (code >= 0x1AB0 && code <= 0x1ABD)
                                || (code >= 0x1ABF && code <= 0x1ACE)
                                || (code >= 0x1B00 && code <= 0x1B03)
                        then
                            Just MarkNonSpacing

                        else if code == 0x1ABE then
                            Just MarkEnclosing

                        else
                            Nothing

                    else if
                        (code == 0x1B04)
                            || (code == 0x1B35)
                            || (code == 0x1B3B)
                            || (code >= 0x1B3D && code <= 0x1B40)
                    then
                        Just MarkSpacingCombining

                    else if code >= 0x1B05 && code <= 0x1B33 then
                        Just LetterOther

                    else if
                        (code == 0x1B34)
                            || (code >= 0x1B36 && code <= 0x1B3A)
                            || (code == 0x1B3C)
                    then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x1BA0 then
                    if code < 0x1B60 then
                        if
                            (code == 0x1B41)
                                || (code >= 0x1B43 && code <= 0x1B44)
                        then
                            Just MarkSpacingCombining

                        else if code == 0x1B42 then
                            Just MarkNonSpacing

                        else if code >= 0x1B45 && code <= 0x1B4C then
                            Just LetterOther

                        else if code >= 0x1B50 && code <= 0x1B59 then
                            Just NumberDecimalDigit

                        else if code >= 0x1B5A && code <= 0x1B5F then
                            Just PunctuationOther

                        else
                            Nothing

                    else if (code == 0x1B60) || (code >= 0x1B7D && code <= 0x1B7E) then
                        Just PunctuationOther

                    else if
                        (code >= 0x1B61 && code <= 0x1B6A)
                            || (code >= 0x1B74 && code <= 0x1B7C)
                    then
                        Just SymbolOther

                    else if
                        (code >= 0x1B6B && code <= 0x1B73)
                            || (code >= 0x1B80 && code <= 0x1B81)
                    then
                        Just MarkNonSpacing

                    else if code == 0x1B82 then
                        Just MarkSpacingCombining

                    else if code >= 0x1B83 && code <= 0x1B9F then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x1BAD then
                    if code == 0x1BA0 then
                        Just LetterOther

                    else if
                        (code == 0x1BA1)
                            || (code >= 0x1BA6 && code <= 0x1BA7)
                            || (code == 0x1BAA)
                    then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x1BA2 && code <= 0x1BA5)
                            || (code >= 0x1BA8 && code <= 0x1BA9)
                            || (code >= 0x1BAB && code <= 0x1BAC)
                    then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if
                    (code == 0x1BAD)
                        || (code == 0x1BE6)
                        || (code >= 0x1BE8 && code <= 0x1BE9)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0x1BAE && code <= 0x1BAF)
                        || (code >= 0x1BBA && code <= 0x1BE5)
                then
                    Just LetterOther

                else if code >= 0x1BB0 && code <= 0x1BB9 then
                    Just NumberDecimalDigit

                else if (code == 0x1BE7) || (code >= 0x1BEA && code <= 0x1BEC) then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x1CE8 then
                if code < 0x1C4F then
                    if code < 0x1C23 then
                        if
                            (code == 0x1BED)
                                || (code >= 0x1BEF && code <= 0x1BF1)
                        then
                            Just MarkNonSpacing

                        else if
                            (code == 0x1BEE)
                                || (code >= 0x1BF2 && code <= 0x1BF3)
                        then
                            Just MarkSpacingCombining

                        else if code >= 0x1BFC && code <= 0x1BFF then
                            Just PunctuationOther

                        else if code >= 0x1C00 && code <= 0x1C22 then
                            Just LetterOther

                        else
                            Nothing

                    else if (code == 0x1C23) || (code >= 0x1C4D && code <= 0x1C4E) then
                        Just LetterOther

                    else if
                        (code >= 0x1C24 && code <= 0x1C2B)
                            || (code >= 0x1C34 && code <= 0x1C35)
                    then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x1C2C && code <= 0x1C33)
                            || (code >= 0x1C36 && code <= 0x1C37)
                    then
                        Just MarkNonSpacing

                    else if code >= 0x1C3B && code <= 0x1C3F then
                        Just PunctuationOther

                    else if code >= 0x1C40 && code <= 0x1C49 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code < 0x1CBC then
                    if (code == 0x1C4F) || (code >= 0x1C5A && code <= 0x1C77) then
                        Just LetterOther

                    else if code >= 0x1C50 && code <= 0x1C59 then
                        Just NumberDecimalDigit

                    else if code >= 0x1C78 && code <= 0x1C7D then
                        Just LetterModifier

                    else if code >= 0x1C7E && code <= 0x1C7F then
                        Just PunctuationOther

                    else if code >= 0x1C80 && code <= 0x1C88 then
                        Just LetterLowercase

                    else if code >= 0x1C90 && code <= 0x1CBA then
                        Just LetterUppercase

                    else
                        Nothing

                else if code >= 0x1CBD && code <= 0x1CBF then
                    Just LetterUppercase

                else if (code >= 0x1CC0 && code <= 0x1CC7) || (code == 0x1CD3) then
                    Just PunctuationOther

                else if
                    (code >= 0x1CD0 && code <= 0x1CD2)
                        || (code >= 0x1CD4 && code <= 0x1CE0)
                        || (code >= 0x1CE2 && code <= 0x1CE7)
                then
                    Just MarkNonSpacing

                else if code == 0x1CE1 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x1D78 then
                if
                    (code == 0x1CE8)
                        || (code == 0x1CED)
                        || (code == 0x1CF4)
                        || (code >= 0x1CF8 && code <= 0x1CF9)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0x1CE9 && code <= 0x1CEC)
                        || (code >= 0x1CEE && code <= 0x1CF3)
                        || (code >= 0x1CF5 && code <= 0x1CF6)
                        || (code == 0x1CFA)
                then
                    Just LetterOther

                else if code == 0x1CF7 then
                    Just MarkSpacingCombining

                else if
                    (code >= 0x1D00 && code <= 0x1D2B)
                        || (code >= 0x1D6B && code <= 0x1D77)
                then
                    Just LetterLowercase

                else if code >= 0x1D2C && code <= 0x1D6A then
                    Just LetterModifier

                else
                    Nothing

            else if code < 0x1E02 then
                if (code == 0x1D78) || (code >= 0x1D9B && code <= 0x1DBF) then
                    Just LetterModifier

                else if (code >= 0x1D79 && code <= 0x1D9A) || (code == 0x1E01) then
                    Just LetterLowercase

                else if code >= 0x1DC0 && code <= 0x1DFF then
                    Just MarkNonSpacing

                else if code == 0x1E00 then
                    Just LetterUppercase

                else
                    Nothing

            else if
                (code == 0x1E02)
                    || (code == 0x1E04)
                    || (code == 0x1E06)
                    || (code == 0x1E08)
                    || (code == 0x1E0A)
            then
                Just LetterUppercase

            else if
                (code == 0x1E03)
                    || (code == 0x1E05)
                    || (code == 0x1E07)
                    || (code == 0x1E09)
            then
                Just LetterLowercase

            else
                Nothing

        else if code < 0x1EDB then
            if code < 0x1E6E then
                if code < 0x1E3B then
                    if code < 0x1E22 then
                        if code < 0x1E15 then
                            if
                                (code == 0x1E0B)
                                    || (code == 0x1E0D)
                                    || (code == 0x1E0F)
                                    || (code == 0x1E11)
                                    || (code == 0x1E13)
                            then
                                Just LetterLowercase

                            else if
                                (code == 0x1E0C)
                                    || (code == 0x1E0E)
                                    || (code == 0x1E10)
                                    || (code == 0x1E12)
                                    || (code == 0x1E14)
                            then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x1E1A then
                            if
                                (code == 0x1E15)
                                    || (code == 0x1E17)
                                    || (code == 0x1E19)
                            then
                                Just LetterLowercase

                            else if (code == 0x1E16) || (code == 0x1E18) then
                                Just LetterUppercase

                            else
                                Nothing

                        else if
                            (code == 0x1E1A)
                                || (code == 0x1E1C)
                                || (code == 0x1E1E)
                                || (code == 0x1E20)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x1E1B)
                                || (code == 0x1E1D)
                                || (code == 0x1E1F)
                                || (code == 0x1E21)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x1E2D then
                        if
                            (code == 0x1E22)
                                || (code == 0x1E24)
                                || (code == 0x1E26)
                                || (code == 0x1E28)
                                || (code == 0x1E2A)
                                || (code == 0x1E2C)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x1E23)
                                || (code == 0x1E25)
                                || (code == 0x1E27)
                                || (code == 0x1E29)
                                || (code == 0x1E2B)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x1E33 then
                        if
                            (code == 0x1E2D)
                                || (code == 0x1E2F)
                                || (code == 0x1E31)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x1E2E)
                                || (code == 0x1E30)
                                || (code == 0x1E32)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if
                        (code == 0x1E33)
                            || (code == 0x1E35)
                            || (code == 0x1E37)
                            || (code == 0x1E39)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1E34)
                            || (code == 0x1E36)
                            || (code == 0x1E38)
                            || (code == 0x1E3A)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1E53 then
                    if code < 0x1E46 then
                        if
                            (code == 0x1E3B)
                                || (code == 0x1E3D)
                                || (code == 0x1E3F)
                                || (code == 0x1E41)
                                || (code == 0x1E43)
                                || (code == 0x1E45)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x1E3C)
                                || (code == 0x1E3E)
                                || (code == 0x1E40)
                                || (code == 0x1E42)
                                || (code == 0x1E44)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x1E4B then
                        if
                            (code == 0x1E46)
                                || (code == 0x1E48)
                                || (code == 0x1E4A)
                        then
                            Just LetterUppercase

                        else if (code == 0x1E47) || (code == 0x1E49) then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x1E4B)
                            || (code == 0x1E4D)
                            || (code == 0x1E4F)
                            || (code == 0x1E51)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1E4C)
                            || (code == 0x1E4E)
                            || (code == 0x1E50)
                            || (code == 0x1E52)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1E5F then
                    if
                        (code == 0x1E53)
                            || (code == 0x1E55)
                            || (code == 0x1E57)
                            || (code == 0x1E59)
                            || (code == 0x1E5B)
                            || (code == 0x1E5D)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1E54)
                            || (code == 0x1E56)
                            || (code == 0x1E58)
                            || (code == 0x1E5A)
                            || (code == 0x1E5C)
                            || (code == 0x1E5E)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1E65 then
                    if (code == 0x1E5F) || (code == 0x1E61) || (code == 0x1E63) then
                        Just LetterLowercase

                    else if (code == 0x1E60) || (code == 0x1E62) || (code == 0x1E64) then
                        Just LetterUppercase

                    else
                        Nothing

                else if
                    (code == 0x1E65)
                        || (code == 0x1E67)
                        || (code == 0x1E69)
                        || (code == 0x1E6B)
                        || (code == 0x1E6D)
                then
                    Just LetterLowercase

                else if
                    (code == 0x1E66)
                        || (code == 0x1E68)
                        || (code == 0x1E6A)
                        || (code == 0x1E6C)
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x1EA7 then
                if code < 0x1E85 then
                    if code < 0x1E78 then
                        if
                            (code == 0x1E6E)
                                || (code == 0x1E70)
                                || (code == 0x1E72)
                                || (code == 0x1E74)
                                || (code == 0x1E76)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x1E6F)
                                || (code == 0x1E71)
                                || (code == 0x1E73)
                                || (code == 0x1E75)
                                || (code == 0x1E77)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x1E7D then
                        if
                            (code == 0x1E78)
                                || (code == 0x1E7A)
                                || (code == 0x1E7C)
                        then
                            Just LetterUppercase

                        else if (code == 0x1E79) || (code == 0x1E7B) then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x1E7D)
                            || (code == 0x1E7F)
                            || (code == 0x1E81)
                            || (code == 0x1E83)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1E7E)
                            || (code == 0x1E80)
                            || (code == 0x1E82)
                            || (code == 0x1E84)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1E91 then
                    if
                        (code == 0x1E85)
                            || (code == 0x1E87)
                            || (code == 0x1E89)
                            || (code == 0x1E8B)
                            || (code == 0x1E8D)
                            || (code == 0x1E8F)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1E86)
                            || (code == 0x1E88)
                            || (code == 0x1E8A)
                            || (code == 0x1E8C)
                            || (code == 0x1E8E)
                            || (code == 0x1E90)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1E9F then
                    if
                        (code == 0x1E91)
                            || (code == 0x1E93)
                            || (code >= 0x1E95 && code <= 0x1E9D)
                    then
                        Just LetterLowercase

                    else if (code == 0x1E92) || (code == 0x1E94) || (code == 0x1E9E) then
                        Just LetterUppercase

                    else
                        Nothing

                else if
                    (code == 0x1E9F)
                        || (code == 0x1EA1)
                        || (code == 0x1EA3)
                        || (code == 0x1EA5)
                then
                    Just LetterLowercase

                else if
                    (code == 0x1EA0)
                        || (code == 0x1EA2)
                        || (code == 0x1EA4)
                        || (code == 0x1EA6)
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x1EC0 then
                if code < 0x1EB2 then
                    if
                        (code == 0x1EA7)
                            || (code == 0x1EA9)
                            || (code == 0x1EAB)
                            || (code == 0x1EAD)
                            || (code == 0x1EAF)
                            || (code == 0x1EB1)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1EA8)
                            || (code == 0x1EAA)
                            || (code == 0x1EAC)
                            || (code == 0x1EAE)
                            || (code == 0x1EB0)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1EB8 then
                    if (code == 0x1EB2) || (code == 0x1EB4) || (code == 0x1EB6) then
                        Just LetterUppercase

                    else if (code == 0x1EB3) || (code == 0x1EB5) || (code == 0x1EB7) then
                        Just LetterLowercase

                    else
                        Nothing

                else if
                    (code == 0x1EB8)
                        || (code == 0x1EBA)
                        || (code == 0x1EBC)
                        || (code == 0x1EBE)
                then
                    Just LetterUppercase

                else if
                    (code == 0x1EB9)
                        || (code == 0x1EBB)
                        || (code == 0x1EBD)
                        || (code == 0x1EBF)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x1ECC then
                if
                    (code == 0x1EC0)
                        || (code == 0x1EC2)
                        || (code == 0x1EC4)
                        || (code == 0x1EC6)
                        || (code == 0x1EC8)
                        || (code == 0x1ECA)
                then
                    Just LetterUppercase

                else if
                    (code == 0x1EC1)
                        || (code == 0x1EC3)
                        || (code == 0x1EC5)
                        || (code == 0x1EC7)
                        || (code == 0x1EC9)
                        || (code == 0x1ECB)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x1ED2 then
                if (code == 0x1ECC) || (code == 0x1ECE) || (code == 0x1ED0) then
                    Just LetterUppercase

                else if (code == 0x1ECD) || (code == 0x1ECF) || (code == 0x1ED1) then
                    Just LetterLowercase

                else
                    Nothing

            else if
                (code == 0x1ED2)
                    || (code == 0x1ED4)
                    || (code == 0x1ED6)
                    || (code == 0x1ED8)
                    || (code == 0x1EDA)
            then
                Just LetterUppercase

            else if
                (code == 0x1ED3)
                    || (code == 0x1ED5)
                    || (code == 0x1ED7)
                    || (code == 0x1ED9)
            then
                Just LetterLowercase

            else
                Nothing

        else if code < 0x2040 then
            if code < 0x1F7F then
                if code < 0x1EF3 then
                    if code < 0x1EE6 then
                        if
                            (code == 0x1EDB)
                                || (code == 0x1EDD)
                                || (code == 0x1EDF)
                                || (code == 0x1EE1)
                                || (code == 0x1EE3)
                                || (code == 0x1EE5)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x1EDC)
                                || (code == 0x1EDE)
                                || (code == 0x1EE0)
                                || (code == 0x1EE2)
                                || (code == 0x1EE4)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x1EEB then
                        if
                            (code == 0x1EE6)
                                || (code == 0x1EE8)
                                || (code == 0x1EEA)
                        then
                            Just LetterUppercase

                        else if (code == 0x1EE7) || (code == 0x1EE9) then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x1EEB)
                            || (code == 0x1EED)
                            || (code == 0x1EEF)
                            || (code == 0x1EF1)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x1EEC)
                            || (code == 0x1EEE)
                            || (code == 0x1EF0)
                            || (code == 0x1EF2)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1F07 then
                    if code < 0x1EF8 then
                        if
                            (code == 0x1EF3)
                                || (code == 0x1EF5)
                                || (code == 0x1EF7)
                        then
                            Just LetterLowercase

                        else if (code == 0x1EF4) || (code == 0x1EF6) then
                            Just LetterUppercase

                        else
                            Nothing

                    else if
                        (code == 0x1EF8)
                            || (code == 0x1EFA)
                            || (code == 0x1EFC)
                            || (code == 0x1EFE)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x1EF9)
                            || (code == 0x1EFB)
                            || (code == 0x1EFD)
                            || (code >= 0x1EFF && code <= 0x1F06)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x1F37 then
                    if
                        (code == 0x1F07)
                            || (code >= 0x1F10 && code <= 0x1F15)
                            || (code >= 0x1F20 && code <= 0x1F27)
                            || (code >= 0x1F30 && code <= 0x1F36)
                    then
                        Just LetterLowercase

                    else if
                        (code >= 0x1F08 && code <= 0x1F0F)
                            || (code >= 0x1F18 && code <= 0x1F1D)
                            || (code >= 0x1F28 && code <= 0x1F2F)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if
                    (code == 0x1F37)
                        || (code >= 0x1F40 && code <= 0x1F45)
                        || (code >= 0x1F50 && code <= 0x1F57)
                        || (code >= 0x1F60 && code <= 0x1F67)
                        || (code >= 0x1F70 && code <= 0x1F7D)
                then
                    Just LetterLowercase

                else if
                    (code >= 0x1F38 && code <= 0x1F3F)
                        || (code >= 0x1F48 && code <= 0x1F4D)
                        || (code >= 0x1F68 && code <= 0x1F6F)
                        || ((modBy 2 code == 1)
                                && (code >= 0x1F59 && code <= 0x1F5F)
                           )
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x1FF1 then
                if code < 0x1FBE then
                    if
                        (code >= 0x1F80 && code <= 0x1F87)
                            || (code >= 0x1F90 && code <= 0x1F97)
                            || (code >= 0x1FA0 && code <= 0x1FA7)
                            || (code >= 0x1FB0 && code <= 0x1FB4)
                            || (code >= 0x1FB6 && code <= 0x1FB7)
                    then
                        Just LetterLowercase

                    else if
                        (code >= 0x1F88 && code <= 0x1F8F)
                            || (code >= 0x1F98 && code <= 0x1F9F)
                            || (code >= 0x1FA8 && code <= 0x1FAF)
                            || (code == 0x1FBC)
                    then
                        Just LetterTitlecase

                    else if code >= 0x1FB8 && code <= 0x1FBB then
                        Just LetterUppercase

                    else if code == 0x1FBD then
                        Just SymbolModifier

                    else
                        Nothing

                else if code < 0x1FCF then
                    if
                        (code == 0x1FBE)
                            || (code >= 0x1FC2 && code <= 0x1FC4)
                            || (code >= 0x1FC6 && code <= 0x1FC7)
                    then
                        Just LetterLowercase

                    else if
                        (code >= 0x1FBF && code <= 0x1FC1)
                            || (code >= 0x1FCD && code <= 0x1FCE)
                    then
                        Just SymbolModifier

                    else if code >= 0x1FC8 && code <= 0x1FCB then
                        Just LetterUppercase

                    else if code == 0x1FCC then
                        Just LetterTitlecase

                    else
                        Nothing

                else if
                    (code == 0x1FCF)
                        || (code >= 0x1FDD && code <= 0x1FDF)
                        || (code >= 0x1FED && code <= 0x1FEF)
                then
                    Just SymbolModifier

                else if
                    (code >= 0x1FD0 && code <= 0x1FD3)
                        || (code >= 0x1FD6 && code <= 0x1FD7)
                        || (code >= 0x1FE0 && code <= 0x1FE7)
                then
                    Just LetterLowercase

                else if
                    (code >= 0x1FD8 && code <= 0x1FDB)
                        || (code >= 0x1FE8 && code <= 0x1FEC)
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x201C then
                if code < 0x200A then
                    if
                        (code >= 0x1FF2 && code <= 0x1FF4)
                            || (code >= 0x1FF6 && code <= 0x1FF7)
                    then
                        Just LetterLowercase

                    else if code >= 0x1FF8 && code <= 0x1FFB then
                        Just LetterUppercase

                    else if code == 0x1FFC then
                        Just LetterTitlecase

                    else if code >= 0x1FFD && code <= 0x1FFE then
                        Just SymbolModifier

                    else if code >= 0x2000 && code <= 0x2009 then
                        Just SeparatorSpace

                    else
                        Nothing

                else if code == 0x200A then
                    Just SeparatorSpace

                else if code >= 0x200B && code <= 0x200F then
                    Just OtherFormat

                else if code >= 0x2010 && code <= 0x2015 then
                    Just PunctuationDash

                else if code >= 0x2016 && code <= 0x2017 then
                    Just PunctuationOther

                else if (code == 0x2018) || (code == 0x201B) then
                    Just PunctuationInitialQuote

                else if code == 0x2019 then
                    Just PunctuationFinalQuote

                else if code == 0x201A then
                    Just PunctuationOpen

                else
                    Nothing

            else if code < 0x2029 then
                if (code == 0x201C) || (code == 0x201F) then
                    Just PunctuationInitialQuote

                else if code == 0x201D then
                    Just PunctuationFinalQuote

                else if code == 0x201E then
                    Just PunctuationOpen

                else if code >= 0x2020 && code <= 0x2027 then
                    Just PunctuationOther

                else if code == 0x2028 then
                    Just SeparatorLine

                else
                    Nothing

            else if code == 0x2029 then
                Just SeparatorParagraph

            else if code >= 0x202A && code <= 0x202E then
                Just OtherFormat

            else if code == 0x202F then
                Just SeparatorSpace

            else if
                (code >= 0x2030 && code <= 0x2038)
                    || (code >= 0x203B && code <= 0x203E)
            then
                Just PunctuationOther

            else if code == 0x2039 then
                Just PunctuationInitialQuote

            else if code == 0x203A then
                Just PunctuationFinalQuote

            else if code == 0x203F then
                Just PunctuationConnector

            else
                Nothing

        else if code < 0x2127 then
            if code < 0x209F then
                if code < 0x2065 then
                    if (code == 0x2040) || (code == 0x2054) then
                        Just PunctuationConnector

                    else if
                        (code >= 0x2041 && code <= 0x2043)
                            || (code >= 0x2047 && code <= 0x2051)
                            || (code == 0x2053)
                            || (code >= 0x2055 && code <= 0x205E)
                    then
                        Just PunctuationOther

                    else if (code == 0x2044) || (code == 0x2052) then
                        Just SymbolMath

                    else if code == 0x2045 then
                        Just PunctuationOpen

                    else if code == 0x2046 then
                        Just PunctuationClose

                    else if code == 0x205F then
                        Just SeparatorSpace

                    else if code >= 0x2060 && code <= 0x2064 then
                        Just OtherFormat

                    else
                        Nothing

                else if code < 0x207D then
                    if code >= 0x2066 && code <= 0x206F then
                        Just OtherFormat

                    else if (code == 0x2070) || (code >= 0x2074 && code <= 0x2079) then
                        Just NumberOther

                    else if code == 0x2071 then
                        Just LetterModifier

                    else if code >= 0x207A && code <= 0x207C then
                        Just SymbolMath

                    else
                        Nothing

                else if (code == 0x207D) || (code == 0x208D) then
                    Just PunctuationOpen

                else if (code == 0x207E) || (code == 0x208E) then
                    Just PunctuationClose

                else if (code == 0x207F) || (code >= 0x2090 && code <= 0x209C) then
                    Just LetterModifier

                else if code >= 0x2080 && code <= 0x2089 then
                    Just NumberOther

                else if code >= 0x208A && code <= 0x208C then
                    Just SymbolMath

                else
                    Nothing

            else if code < 0x210A then
                if code >= 0x20A0 && code <= 0x20C0 then
                    Just SymbolCurrency

                else if
                    (code >= 0x20D0 && code <= 0x20DC)
                        || (code == 0x20E1)
                        || (code >= 0x20E5 && code <= 0x20F0)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0x20DD && code <= 0x20E0)
                        || (code >= 0x20E2 && code <= 0x20E4)
                then
                    Just MarkEnclosing

                else if
                    (code >= 0x2100 && code <= 0x2101)
                        || (code >= 0x2103 && code <= 0x2106)
                        || (code >= 0x2108 && code <= 0x2109)
                then
                    Just SymbolOther

                else if (code == 0x2102) || (code == 0x2107) then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x2115 then
                if
                    (code == 0x210A)
                        || (code >= 0x210E && code <= 0x210F)
                        || (code == 0x2113)
                then
                    Just LetterLowercase

                else if
                    (code >= 0x210B && code <= 0x210D)
                        || (code >= 0x2110 && code <= 0x2112)
                then
                    Just LetterUppercase

                else if code == 0x2114 then
                    Just SymbolOther

                else
                    Nothing

            else if
                (code == 0x2115)
                    || (code >= 0x2119 && code <= 0x211D)
                    || (code == 0x2124)
                    || (code == 0x2126)
            then
                Just LetterUppercase

            else if
                (code >= 0x2116 && code <= 0x2117)
                    || (code >= 0x211E && code <= 0x2123)
                    || (code == 0x2125)
            then
                Just SymbolOther

            else if code == 0x2118 then
                Just SymbolMath

            else
                Nothing

        else if code < 0x2188 then
            if code < 0x213F then
                if code < 0x212F then
                    if (code == 0x2127) || (code == 0x2129) || (code == 0x212E) then
                        Just SymbolOther

                    else if (code == 0x2128) || (code >= 0x212A && code <= 0x212D) then
                        Just LetterUppercase

                    else
                        Nothing

                else if
                    (code == 0x212F)
                        || (code == 0x2134)
                        || (code == 0x2139)
                        || (code >= 0x213C && code <= 0x213D)
                then
                    Just LetterLowercase

                else if (code >= 0x2130 && code <= 0x2133) || (code == 0x213E) then
                    Just LetterUppercase

                else if code >= 0x2135 && code <= 0x2138 then
                    Just LetterOther

                else if code >= 0x213A && code <= 0x213B then
                    Just SymbolOther

                else
                    Nothing

            else if code < 0x214D then
                if (code == 0x213F) || (code == 0x2145) then
                    Just LetterUppercase

                else if (code >= 0x2140 && code <= 0x2144) || (code == 0x214B) then
                    Just SymbolMath

                else if code >= 0x2146 && code <= 0x2149 then
                    Just LetterLowercase

                else if (code == 0x214A) || (code == 0x214C) then
                    Just SymbolOther

                else
                    Nothing

            else if (code == 0x214D) || (code == 0x214F) then
                Just SymbolOther

            else if (code == 0x214E) || (code == 0x2184) then
                Just LetterLowercase

            else if code >= 0x2150 && code <= 0x215F then
                Just NumberOther

            else if
                (code >= 0x2160 && code <= 0x2182)
                    || (code >= 0x2185 && code <= 0x2187)
            then
                Just NumberLetter

            else if code == 0x2183 then
                Just LetterUppercase

            else
                Nothing

        else if code < 0x21AE then
            if code < 0x219B then
                if code == 0x2188 then
                    Just NumberLetter

                else if code == 0x2189 then
                    Just NumberOther

                else if
                    (code >= 0x218A && code <= 0x218B)
                        || (code >= 0x2195 && code <= 0x2199)
                then
                    Just SymbolOther

                else if (code >= 0x2190 && code <= 0x2194) || (code == 0x219A) then
                    Just SymbolMath

                else
                    Nothing

            else if
                (code == 0x219B)
                    || (code == 0x21A0)
                    || (code == 0x21A3)
                    || (code == 0x21A6)
            then
                Just SymbolMath

            else if
                (code >= 0x219C && code <= 0x219F)
                    || (code >= 0x21A1 && code <= 0x21A2)
                    || (code >= 0x21A4 && code <= 0x21A5)
                    || (code >= 0x21A7 && code <= 0x21AD)
            then
                Just SymbolOther

            else
                Nothing

        else if code < 0x21D4 then
            if
                (code == 0x21AE)
                    || (code >= 0x21CE && code <= 0x21CF)
                    || (code == 0x21D2)
            then
                Just SymbolMath

            else if
                (code >= 0x21AF && code <= 0x21CD)
                    || (code >= 0x21D0 && code <= 0x21D1)
                    || (code == 0x21D3)
            then
                Just SymbolOther

            else
                Nothing

        else if (code == 0x21D4) || (code >= 0x21F4 && code <= 0x22FF) then
            Just SymbolMath

        else if
            (code >= 0x21D5 && code <= 0x21F3)
                || (code >= 0x2300 && code <= 0x2307)
                || (code >= 0x230C && code <= 0x231E)
        then
            Just SymbolOther

        else if (code == 0x2308) || (code == 0x230A) then
            Just PunctuationOpen

        else if (code == 0x2309) || (code == 0x230B) then
            Just PunctuationClose

        else
            Nothing

    else if code < 0xFE3A then
        if code < 0xA652 then
            if code < 0x2CD4 then
                if code < 0x2C60 then
                    if code < 0x27E9 then
                        if code < 0x266F then
                            if code < 0x23E1 then
                                if
                                    (code == 0x231F)
                                        || (code >= 0x2322 && code <= 0x2328)
                                        || (code >= 0x232B && code <= 0x237B)
                                        || (code >= 0x237D && code <= 0x239A)
                                        || (code >= 0x23B4 && code <= 0x23DB)
                                then
                                    Just SymbolOther

                                else if
                                    (code >= 0x2320 && code <= 0x2321)
                                        || (code == 0x237C)
                                        || (code >= 0x239B && code <= 0x23B3)
                                        || (code >= 0x23DC && code <= 0x23E0)
                                then
                                    Just SymbolMath

                                else if code == 0x2329 then
                                    Just PunctuationOpen

                                else if code == 0x232A then
                                    Just PunctuationClose

                                else
                                    Nothing

                            else if code < 0x24FF then
                                if code == 0x23E1 then
                                    Just SymbolMath

                                else if
                                    (code >= 0x23E2 && code <= 0x2426)
                                        || (code >= 0x2440 && code <= 0x244A)
                                        || (code >= 0x249C && code <= 0x24E9)
                                then
                                    Just SymbolOther

                                else if
                                    (code >= 0x2460 && code <= 0x249B)
                                        || (code >= 0x24EA && code <= 0x24FE)
                                then
                                    Just NumberOther

                                else
                                    Nothing

                            else if code == 0x24FF then
                                Just NumberOther

                            else if
                                (code >= 0x2500 && code <= 0x25B6)
                                    || (code >= 0x25B8 && code <= 0x25C0)
                                    || (code >= 0x25C2 && code <= 0x25F7)
                                    || (code >= 0x2600 && code <= 0x266E)
                            then
                                Just SymbolOther

                            else if
                                (code == 0x25B7)
                                    || (code == 0x25C1)
                                    || (code >= 0x25F8 && code <= 0x25FF)
                            then
                                Just SymbolMath

                            else
                                Nothing

                        else if code < 0x2771 then
                            if code == 0x266F then
                                Just SymbolMath

                            else if code >= 0x2670 && code <= 0x2767 then
                                Just SymbolOther

                            else if
                                (code == 0x2768)
                                    || (code == 0x276A)
                                    || (code == 0x276C)
                                    || (code == 0x276E)
                                    || (code == 0x2770)
                            then
                                Just PunctuationOpen

                            else if
                                (code == 0x2769)
                                    || (code == 0x276B)
                                    || (code == 0x276D)
                                    || (code == 0x276F)
                            then
                                Just PunctuationClose

                            else
                                Nothing

                        else if code < 0x27BF then
                            if
                                (code == 0x2771)
                                    || (code == 0x2773)
                                    || (code == 0x2775)
                            then
                                Just PunctuationClose

                            else if (code == 0x2772) || (code == 0x2774) then
                                Just PunctuationOpen

                            else if code >= 0x2776 && code <= 0x2793 then
                                Just NumberOther

                            else if code >= 0x2794 && code <= 0x27BE then
                                Just SymbolOther

                            else
                                Nothing

                        else if code == 0x27BF then
                            Just SymbolOther

                        else if
                            (code >= 0x27C0 && code <= 0x27C4)
                                || (code >= 0x27C7 && code <= 0x27E5)
                        then
                            Just SymbolMath

                        else if
                            (code == 0x27C5)
                                || (code == 0x27E6)
                                || (code == 0x27E8)
                        then
                            Just PunctuationOpen

                        else if (code == 0x27C6) || (code == 0x27E7) then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code < 0x2991 then
                        if code < 0x2984 then
                            if
                                (code == 0x27E9)
                                    || (code == 0x27EB)
                                    || (code == 0x27ED)
                                    || (code == 0x27EF)
                            then
                                Just PunctuationClose

                            else if
                                (code == 0x27EA)
                                    || (code == 0x27EC)
                                    || (code == 0x27EE)
                                    || (code == 0x2983)
                            then
                                Just PunctuationOpen

                            else if
                                (code >= 0x27F0 && code <= 0x27FF)
                                    || (code >= 0x2900 && code <= 0x2982)
                            then
                                Just SymbolMath

                            else if code >= 0x2800 && code <= 0x28FF then
                                Just SymbolOther

                            else
                                Nothing

                        else if code < 0x2989 then
                            if
                                (code == 0x2984)
                                    || (code == 0x2986)
                                    || (code == 0x2988)
                            then
                                Just PunctuationClose

                            else if (code == 0x2985) || (code == 0x2987) then
                                Just PunctuationOpen

                            else
                                Nothing

                        else if
                            (code == 0x2989)
                                || (code == 0x298B)
                                || (code == 0x298D)
                                || (code == 0x298F)
                        then
                            Just PunctuationOpen

                        else if
                            (code == 0x298A)
                                || (code == 0x298C)
                                || (code == 0x298E)
                                || (code == 0x2990)
                        then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code < 0x29DB then
                        if
                            (code == 0x2991)
                                || (code == 0x2993)
                                || (code == 0x2995)
                                || (code == 0x2997)
                                || (code == 0x29D8)
                                || (code == 0x29DA)
                        then
                            Just PunctuationOpen

                        else if
                            (code == 0x2992)
                                || (code == 0x2994)
                                || (code == 0x2996)
                                || (code == 0x2998)
                                || (code == 0x29D9)
                        then
                            Just PunctuationClose

                        else if code >= 0x2999 && code <= 0x29D7 then
                            Just SymbolMath

                        else
                            Nothing

                    else if code < 0x2B44 then
                        if (code == 0x29DB) || (code == 0x29FD) then
                            Just PunctuationClose

                        else if
                            (code >= 0x29DC && code <= 0x29FB)
                                || (code >= 0x29FE && code <= 0x2AFF)
                                || (code >= 0x2B30 && code <= 0x2B43)
                        then
                            Just SymbolMath

                        else if code == 0x29FC then
                            Just PunctuationOpen

                        else if code >= 0x2B00 && code <= 0x2B2F then
                            Just SymbolOther

                        else
                            Nothing

                    else if (code == 0x2B44) || (code >= 0x2B47 && code <= 0x2B4C) then
                        Just SymbolMath

                    else if
                        (code >= 0x2B45 && code <= 0x2B46)
                            || (code >= 0x2B4D && code <= 0x2B73)
                            || (code >= 0x2B76 && code <= 0x2B95)
                            || (code >= 0x2B97 && code <= 0x2BFF)
                    then
                        Just SymbolOther

                    else if code >= 0x2C00 && code <= 0x2C2F then
                        Just LetterUppercase

                    else if code >= 0x2C30 && code <= 0x2C5F then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x2CA0 then
                    if code < 0x2C86 then
                        if code < 0x2C70 then
                            if
                                (code == 0x2C60)
                                    || (code >= 0x2C62 && code <= 0x2C64)
                                    || (code == 0x2C67)
                                    || (code == 0x2C69)
                                    || (code == 0x2C6B)
                                    || (code >= 0x2C6D && code <= 0x2C6F)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x2C61)
                                    || (code >= 0x2C65 && code <= 0x2C66)
                                    || (code == 0x2C68)
                                    || (code == 0x2C6A)
                                    || (code == 0x2C6C)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x2C7B then
                            if
                                (code == 0x2C70)
                                    || (code == 0x2C72)
                                    || (code == 0x2C75)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x2C71)
                                    || (code >= 0x2C73 && code <= 0x2C74)
                                    || (code >= 0x2C76 && code <= 0x2C7A)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if
                            (code == 0x2C7B)
                                || (code == 0x2C81)
                                || (code == 0x2C83)
                                || (code == 0x2C85)
                        then
                            Just LetterLowercase

                        else if code >= 0x2C7C && code <= 0x2C7D then
                            Just LetterModifier

                        else if
                            (code >= 0x2C7E && code <= 0x2C80)
                                || (code == 0x2C82)
                                || (code == 0x2C84)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x2C92 then
                        if
                            (code == 0x2C86)
                                || (code == 0x2C88)
                                || (code == 0x2C8A)
                                || (code == 0x2C8C)
                                || (code == 0x2C8E)
                                || (code == 0x2C90)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x2C87)
                                || (code == 0x2C89)
                                || (code == 0x2C8B)
                                || (code == 0x2C8D)
                                || (code == 0x2C8F)
                                || (code == 0x2C91)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x2C98 then
                        if
                            (code == 0x2C92)
                                || (code == 0x2C94)
                                || (code == 0x2C96)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x2C93)
                                || (code == 0x2C95)
                                || (code == 0x2C97)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0x2C98)
                            || (code == 0x2C9A)
                            || (code == 0x2C9C)
                            || (code == 0x2C9E)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0x2C99)
                            || (code == 0x2C9B)
                            || (code == 0x2C9D)
                            || (code == 0x2C9F)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x2CB9 then
                    if code < 0x2CAB then
                        if
                            (code == 0x2CA0)
                                || (code == 0x2CA2)
                                || (code == 0x2CA4)
                                || (code == 0x2CA6)
                                || (code == 0x2CA8)
                                || (code == 0x2CAA)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x2CA1)
                                || (code == 0x2CA3)
                                || (code == 0x2CA5)
                                || (code == 0x2CA7)
                                || (code == 0x2CA9)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x2CB1 then
                        if
                            (code == 0x2CAB)
                                || (code == 0x2CAD)
                                || (code == 0x2CAF)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0x2CAC)
                                || (code == 0x2CAE)
                                || (code == 0x2CB0)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if
                        (code == 0x2CB1)
                            || (code == 0x2CB3)
                            || (code == 0x2CB5)
                            || (code == 0x2CB7)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x2CB2)
                            || (code == 0x2CB4)
                            || (code == 0x2CB6)
                            || (code == 0x2CB8)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x2CC5 then
                    if
                        (code == 0x2CB9)
                            || (code == 0x2CBB)
                            || (code == 0x2CBD)
                            || (code == 0x2CBF)
                            || (code == 0x2CC1)
                            || (code == 0x2CC3)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x2CBA)
                            || (code == 0x2CBC)
                            || (code == 0x2CBE)
                            || (code == 0x2CC0)
                            || (code == 0x2CC2)
                            || (code == 0x2CC4)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x2CCB then
                    if (code == 0x2CC5) || (code == 0x2CC7) || (code == 0x2CC9) then
                        Just LetterLowercase

                    else if (code == 0x2CC6) || (code == 0x2CC8) || (code == 0x2CCA) then
                        Just LetterUppercase

                    else
                        Nothing

                else if
                    (code == 0x2CCB)
                        || (code == 0x2CCD)
                        || (code == 0x2CCF)
                        || (code == 0x2CD1)
                        || (code == 0x2CD3)
                then
                    Just LetterLowercase

                else if
                    (code == 0x2CCC)
                        || (code == 0x2CCE)
                        || (code == 0x2CD0)
                        || (code == 0x2CD2)
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x3004 then
                if code < 0x2E05 then
                    if code < 0x2CF8 then
                        if code < 0x2CDF then
                            if
                                (code == 0x2CD4)
                                    || (code == 0x2CD6)
                                    || (code == 0x2CD8)
                                    || (code == 0x2CDA)
                                    || (code == 0x2CDC)
                                    || (code == 0x2CDE)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0x2CD5)
                                    || (code == 0x2CD7)
                                    || (code == 0x2CD9)
                                    || (code == 0x2CDB)
                                    || (code == 0x2CDD)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x2CEA then
                            if
                                (code == 0x2CDF)
                                    || (code == 0x2CE1)
                                    || (code >= 0x2CE3 && code <= 0x2CE4)
                            then
                                Just LetterLowercase

                            else if (code == 0x2CE0) || (code == 0x2CE2) then
                                Just LetterUppercase

                            else if code >= 0x2CE5 && code <= 0x2CE9 then
                                Just SymbolOther

                            else
                                Nothing

                        else if code == 0x2CEA then
                            Just SymbolOther

                        else if
                            (code == 0x2CEB)
                                || (code == 0x2CED)
                                || (code == 0x2CF2)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0x2CEC)
                                || (code == 0x2CEE)
                                || (code == 0x2CF3)
                        then
                            Just LetterLowercase

                        else if code >= 0x2CEF && code <= 0x2CF1 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code < 0x2DA7 then
                        if
                            (code >= 0x2CF9 && code <= 0x2CFC)
                                || (code >= 0x2CFE && code <= 0x2CFF)
                                || (code == 0x2D70)
                        then
                            Just PunctuationOther

                        else if code == 0x2CFD then
                            Just NumberOther

                        else if
                            (code >= 0x2D00 && code <= 0x2D25)
                                || (code == 0x2D27)
                                || (code == 0x2D2D)
                        then
                            Just LetterLowercase

                        else if
                            (code >= 0x2D30 && code <= 0x2D67)
                                || (code >= 0x2D80 && code <= 0x2D96)
                                || (code >= 0x2DA0 && code <= 0x2DA6)
                        then
                            Just LetterOther

                        else if code == 0x2D6F then
                            Just LetterModifier

                        else if code == 0x2D7F then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if
                        (code >= 0x2DA8 && code <= 0x2DAE)
                            || (code >= 0x2DB0 && code <= 0x2DB6)
                            || (code >= 0x2DB8 && code <= 0x2DBE)
                            || (code >= 0x2DC0 && code <= 0x2DC6)
                            || (code >= 0x2DC8 && code <= 0x2DCE)
                            || (code >= 0x2DD0 && code <= 0x2DD6)
                            || (code >= 0x2DD8 && code <= 0x2DDE)
                    then
                        Just LetterOther

                    else if code >= 0x2DE0 && code <= 0x2DFF then
                        Just MarkNonSpacing

                    else if code >= 0x2E00 && code <= 0x2E01 then
                        Just PunctuationOther

                    else if (code == 0x2E02) || (code == 0x2E04) then
                        Just PunctuationInitialQuote

                    else if code == 0x2E03 then
                        Just PunctuationFinalQuote

                    else
                        Nothing

                else if code < 0x2E29 then
                    if code < 0x2E1B then
                        if
                            (code == 0x2E05)
                                || (code == 0x2E0A)
                                || (code == 0x2E0D)
                        then
                            Just PunctuationFinalQuote

                        else if
                            (code >= 0x2E06 && code <= 0x2E08)
                                || (code == 0x2E0B)
                                || (code >= 0x2E0E && code <= 0x2E16)
                                || (code >= 0x2E18 && code <= 0x2E19)
                        then
                            Just PunctuationOther

                        else if (code == 0x2E09) || (code == 0x2E0C) then
                            Just PunctuationInitialQuote

                        else if (code == 0x2E17) || (code == 0x2E1A) then
                            Just PunctuationDash

                        else
                            Nothing

                    else if code < 0x2E21 then
                        if
                            (code == 0x2E1B)
                                || (code >= 0x2E1E && code <= 0x2E1F)
                        then
                            Just PunctuationOther

                        else if (code == 0x2E1C) || (code == 0x2E20) then
                            Just PunctuationInitialQuote

                        else if code == 0x2E1D then
                            Just PunctuationFinalQuote

                        else
                            Nothing

                    else if code == 0x2E21 then
                        Just PunctuationFinalQuote

                    else if
                        (code == 0x2E22)
                            || (code == 0x2E24)
                            || (code == 0x2E26)
                            || (code == 0x2E28)
                    then
                        Just PunctuationOpen

                    else if (code == 0x2E23) || (code == 0x2E25) || (code == 0x2E27) then
                        Just PunctuationClose

                    else
                        Nothing

                else if code < 0x2E55 then
                    if code == 0x2E29 then
                        Just PunctuationClose

                    else if
                        (code >= 0x2E2A && code <= 0x2E2E)
                            || (code >= 0x2E30 && code <= 0x2E39)
                            || (code >= 0x2E3C && code <= 0x2E3F)
                            || (code == 0x2E41)
                            || (code >= 0x2E43 && code <= 0x2E4F)
                            || (code >= 0x2E52 && code <= 0x2E54)
                    then
                        Just PunctuationOther

                    else if code == 0x2E2F then
                        Just LetterModifier

                    else if (code >= 0x2E3A && code <= 0x2E3B) || (code == 0x2E40) then
                        Just PunctuationDash

                    else if code == 0x2E42 then
                        Just PunctuationOpen

                    else if code >= 0x2E50 && code <= 0x2E51 then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0x2E5B then
                    if (code == 0x2E55) || (code == 0x2E57) || (code == 0x2E59) then
                        Just PunctuationOpen

                    else if (code == 0x2E56) || (code == 0x2E58) || (code == 0x2E5A) then
                        Just PunctuationClose

                    else
                        Nothing

                else if code == 0x2E5B then
                    Just PunctuationOpen

                else if code == 0x2E5C then
                    Just PunctuationClose

                else if code == 0x2E5D then
                    Just PunctuationDash

                else if
                    (code >= 0x2E80 && code <= 0x2E99)
                        || (code >= 0x2E9B && code <= 0x2EF3)
                        || (code >= 0x2F00 && code <= 0x2FD5)
                        || (code >= 0x2FF0 && code <= 0x2FFB)
                then
                    Just SymbolOther

                else if code == 0x3000 then
                    Just SeparatorSpace

                else if code >= 0x3001 && code <= 0x3003 then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x3191 then
                if code < 0x301D then
                    if code < 0x300F then
                        if code == 0x3004 then
                            Just SymbolOther

                        else if code == 0x3005 then
                            Just LetterModifier

                        else if code == 0x3006 then
                            Just LetterOther

                        else if code == 0x3007 then
                            Just NumberLetter

                        else if
                            (code == 0x3008)
                                || (code == 0x300A)
                                || (code == 0x300C)
                                || (code == 0x300E)
                        then
                            Just PunctuationOpen

                        else if
                            (code == 0x3009)
                                || (code == 0x300B)
                                || (code == 0x300D)
                        then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code < 0x3015 then
                        if (code == 0x300F) || (code == 0x3011) then
                            Just PunctuationClose

                        else if (code == 0x3010) || (code == 0x3014) then
                            Just PunctuationOpen

                        else if code >= 0x3012 && code <= 0x3013 then
                            Just SymbolOther

                        else
                            Nothing

                    else if
                        (code == 0x3015)
                            || (code == 0x3017)
                            || (code == 0x3019)
                            || (code == 0x301B)
                    then
                        Just PunctuationClose

                    else if (code == 0x3016) || (code == 0x3018) || (code == 0x301A) then
                        Just PunctuationOpen

                    else if code == 0x301C then
                        Just PunctuationDash

                    else
                        Nothing

                else if code < 0x303D then
                    if code == 0x301D then
                        Just PunctuationOpen

                    else if code >= 0x301E && code <= 0x301F then
                        Just PunctuationClose

                    else if (code == 0x3020) || (code >= 0x3036 && code <= 0x3037) then
                        Just SymbolOther

                    else if
                        (code >= 0x3021 && code <= 0x3029)
                            || (code >= 0x3038 && code <= 0x303A)
                    then
                        Just NumberLetter

                    else if code >= 0x302A && code <= 0x302D then
                        Just MarkNonSpacing

                    else if code >= 0x302E && code <= 0x302F then
                        Just MarkSpacingCombining

                    else if code == 0x3030 then
                        Just PunctuationDash

                    else if (code >= 0x3031 && code <= 0x3035) || (code == 0x303B) then
                        Just LetterModifier

                    else if code == 0x303C then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x309F then
                    if code == 0x303D then
                        Just PunctuationOther

                    else if code >= 0x303E && code <= 0x303F then
                        Just SymbolOther

                    else if code >= 0x3041 && code <= 0x3096 then
                        Just LetterOther

                    else if code >= 0x3099 && code <= 0x309A then
                        Just MarkNonSpacing

                    else if code >= 0x309B && code <= 0x309C then
                        Just SymbolModifier

                    else if code >= 0x309D && code <= 0x309E then
                        Just LetterModifier

                    else
                        Nothing

                else if
                    (code == 0x309F)
                        || (code >= 0x30A1 && code <= 0x30FA)
                        || (code == 0x30FF)
                        || (code >= 0x3105 && code <= 0x312F)
                        || (code >= 0x3131 && code <= 0x318E)
                then
                    Just LetterOther

                else if code == 0x30A0 then
                    Just PunctuationDash

                else if code == 0x30FB then
                    Just PunctuationOther

                else if code >= 0x30FC && code <= 0x30FE then
                    Just LetterModifier

                else if code == 0x3190 then
                    Just SymbolOther

                else
                    Nothing

            else if code < 0xA4F7 then
                if code < 0x327F then
                    if code < 0x31FF then
                        if
                            (code == 0x3191)
                                || (code >= 0x3196 && code <= 0x319F)
                                || (code >= 0x31C0 && code <= 0x31E3)
                        then
                            Just SymbolOther

                        else if code >= 0x3192 && code <= 0x3195 then
                            Just NumberOther

                        else if
                            (code >= 0x31A0 && code <= 0x31BF)
                                || (code >= 0x31F0 && code <= 0x31FE)
                        then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x31FF then
                        Just LetterOther

                    else if
                        (code >= 0x3200 && code <= 0x321E)
                            || (code >= 0x322A && code <= 0x3247)
                            || (code == 0x3250)
                            || (code >= 0x3260 && code <= 0x327E)
                    then
                        Just SymbolOther

                    else if
                        (code >= 0x3220 && code <= 0x3229)
                            || (code >= 0x3248 && code <= 0x324F)
                            || (code >= 0x3251 && code <= 0x325F)
                    then
                        Just NumberOther

                    else
                        Nothing

                else if code < 0x4DBF then
                    if
                        (code == 0x327F)
                            || (code >= 0x328A && code <= 0x32B0)
                            || (code >= 0x32C0 && code <= 0x33FF)
                    then
                        Just SymbolOther

                    else if
                        (code >= 0x3280 && code <= 0x3289)
                            || (code >= 0x32B1 && code <= 0x32BF)
                    then
                        Just NumberOther

                    else if code == 0x3400 then
                        Just LetterOther

                    else
                        Nothing

                else if
                    (code == 0x4DBF)
                        || (code == 0x4E00)
                        || (code >= 0x9FFF && code <= 0xA014)
                        || (code >= 0xA016 && code <= 0xA48C)
                        || (code >= 0xA4D0 && code <= 0xA4F6)
                then
                    Just LetterOther

                else if
                    (code >= 0x4DC0 && code <= 0x4DFF)
                        || (code >= 0xA490 && code <= 0xA4C6)
                then
                    Just SymbolOther

                else if code == 0xA015 then
                    Just LetterModifier

                else
                    Nothing

            else if code < 0xA643 then
                if
                    (code == 0xA4F7)
                        || (code >= 0xA500 && code <= 0xA60B)
                        || (code >= 0xA610 && code <= 0xA61F)
                        || (code >= 0xA62A && code <= 0xA62B)
                then
                    Just LetterOther

                else if (code >= 0xA4F8 && code <= 0xA4FD) || (code == 0xA60C) then
                    Just LetterModifier

                else if
                    (code >= 0xA4FE && code <= 0xA4FF)
                        || (code >= 0xA60D && code <= 0xA60F)
                then
                    Just PunctuationOther

                else if code >= 0xA620 && code <= 0xA629 then
                    Just NumberDecimalDigit

                else if (code == 0xA640) || (code == 0xA642) then
                    Just LetterUppercase

                else if code == 0xA641 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xA649 then
                if (code == 0xA643) || (code == 0xA645) || (code == 0xA647) then
                    Just LetterLowercase

                else if (code == 0xA644) || (code == 0xA646) || (code == 0xA648) then
                    Just LetterUppercase

                else
                    Nothing

            else if
                (code == 0xA649)
                    || (code == 0xA64B)
                    || (code == 0xA64D)
                    || (code == 0xA64F)
                    || (code == 0xA651)
            then
                Just LetterLowercase

            else if
                (code == 0xA64A)
                    || (code == 0xA64C)
                    || (code == 0xA64E)
                    || (code == 0xA650)
            then
                Just LetterUppercase

            else
                Nothing

        else if code < 0xA7B7 then
            if code < 0xA73F then
                if code < 0xA68D then
                    if code < 0xA669 then
                        if code < 0xA65C then
                            if
                                (code == 0xA652)
                                    || (code == 0xA654)
                                    || (code == 0xA656)
                                    || (code == 0xA658)
                                    || (code == 0xA65A)
                            then
                                Just LetterUppercase

                            else if
                                (code == 0xA653)
                                    || (code == 0xA655)
                                    || (code == 0xA657)
                                    || (code == 0xA659)
                                    || (code == 0xA65B)
                            then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0xA661 then
                            if
                                (code == 0xA65C)
                                    || (code == 0xA65E)
                                    || (code == 0xA660)
                            then
                                Just LetterUppercase

                            else if (code == 0xA65D) || (code == 0xA65F) then
                                Just LetterLowercase

                            else
                                Nothing

                        else if
                            (code == 0xA661)
                                || (code == 0xA663)
                                || (code == 0xA665)
                                || (code == 0xA667)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0xA662)
                                || (code == 0xA664)
                                || (code == 0xA666)
                                || (code == 0xA668)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0xA67F then
                        if
                            (code == 0xA669)
                                || (code == 0xA66B)
                                || (code == 0xA66D)
                        then
                            Just LetterLowercase

                        else if (code == 0xA66A) || (code == 0xA66C) then
                            Just LetterUppercase

                        else if code == 0xA66E then
                            Just LetterOther

                        else if
                            (code == 0xA66F)
                                || (code >= 0xA674 && code <= 0xA67D)
                        then
                            Just MarkNonSpacing

                        else if code >= 0xA670 && code <= 0xA672 then
                            Just MarkEnclosing

                        else if (code == 0xA673) || (code == 0xA67E) then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0xA685 then
                        if code == 0xA67F then
                            Just LetterModifier

                        else if
                            (code == 0xA680)
                                || (code == 0xA682)
                                || (code == 0xA684)
                        then
                            Just LetterUppercase

                        else if (code == 0xA681) || (code == 0xA683) then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0xA685)
                            || (code == 0xA687)
                            || (code == 0xA689)
                            || (code == 0xA68B)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0xA686)
                            || (code == 0xA688)
                            || (code == 0xA68A)
                            || (code == 0xA68C)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xA722 then
                    if code < 0xA698 then
                        if
                            (code == 0xA68D)
                                || (code == 0xA68F)
                                || (code == 0xA691)
                                || (code == 0xA693)
                                || (code == 0xA695)
                                || (code == 0xA697)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0xA68E)
                                || (code == 0xA690)
                                || (code == 0xA692)
                                || (code == 0xA694)
                                || (code == 0xA696)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0xA69F then
                        if (code == 0xA698) || (code == 0xA69A) then
                            Just LetterUppercase

                        else if (code == 0xA699) || (code == 0xA69B) then
                            Just LetterLowercase

                        else if code >= 0xA69C && code <= 0xA69D then
                            Just LetterModifier

                        else if code == 0xA69E then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if (code == 0xA69F) || (code >= 0xA6F0 && code <= 0xA6F1) then
                        Just MarkNonSpacing

                    else if code >= 0xA6A0 && code <= 0xA6E5 then
                        Just LetterOther

                    else if code >= 0xA6E6 && code <= 0xA6EF then
                        Just NumberLetter

                    else if code >= 0xA6F2 && code <= 0xA6F7 then
                        Just PunctuationOther

                    else if
                        (code >= 0xA700 && code <= 0xA716)
                            || (code >= 0xA720 && code <= 0xA721)
                    then
                        Just SymbolModifier

                    else if code >= 0xA717 && code <= 0xA71F then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0xA72E then
                    if
                        (code == 0xA722)
                            || (code == 0xA724)
                            || (code == 0xA726)
                            || (code == 0xA728)
                            || (code == 0xA72A)
                            || (code == 0xA72C)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0xA723)
                            || (code == 0xA725)
                            || (code == 0xA727)
                            || (code == 0xA729)
                            || (code == 0xA72B)
                            || (code == 0xA72D)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0xA736 then
                    if (code == 0xA72E) || (code == 0xA732) || (code == 0xA734) then
                        Just LetterUppercase

                    else if
                        (code >= 0xA72F && code <= 0xA731)
                            || (code == 0xA733)
                            || (code == 0xA735)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if
                    (code == 0xA736)
                        || (code == 0xA738)
                        || (code == 0xA73A)
                        || (code == 0xA73C)
                        || (code == 0xA73E)
                then
                    Just LetterUppercase

                else if
                    (code == 0xA737)
                        || (code == 0xA739)
                        || (code == 0xA73B)
                        || (code == 0xA73D)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xA770 then
                if code < 0xA756 then
                    if code < 0xA749 then
                        if
                            (code == 0xA73F)
                                || (code == 0xA741)
                                || (code == 0xA743)
                                || (code == 0xA745)
                                || (code == 0xA747)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0xA740)
                                || (code == 0xA742)
                                || (code == 0xA744)
                                || (code == 0xA746)
                                || (code == 0xA748)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0xA74E then
                        if
                            (code == 0xA749)
                                || (code == 0xA74B)
                                || (code == 0xA74D)
                        then
                            Just LetterLowercase

                        else if (code == 0xA74A) || (code == 0xA74C) then
                            Just LetterUppercase

                        else
                            Nothing

                    else if
                        (code == 0xA74E)
                            || (code == 0xA750)
                            || (code == 0xA752)
                            || (code == 0xA754)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0xA74F)
                            || (code == 0xA751)
                            || (code == 0xA753)
                            || (code == 0xA755)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0xA762 then
                    if
                        (code == 0xA756)
                            || (code == 0xA758)
                            || (code == 0xA75A)
                            || (code == 0xA75C)
                            || (code == 0xA75E)
                            || (code == 0xA760)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0xA757)
                            || (code == 0xA759)
                            || (code == 0xA75B)
                            || (code == 0xA75D)
                            || (code == 0xA75F)
                            || (code == 0xA761)
                    then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0xA768 then
                    if (code == 0xA762) || (code == 0xA764) || (code == 0xA766) then
                        Just LetterUppercase

                    else if (code == 0xA763) || (code == 0xA765) || (code == 0xA767) then
                        Just LetterLowercase

                    else
                        Nothing

                else if
                    (code == 0xA768)
                        || (code == 0xA76A)
                        || (code == 0xA76C)
                        || (code == 0xA76E)
                then
                    Just LetterUppercase

                else if
                    (code == 0xA769)
                        || (code == 0xA76B)
                        || (code == 0xA76D)
                        || (code == 0xA76F)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xA792 then
                if code < 0xA783 then
                    if code == 0xA770 then
                        Just LetterModifier

                    else if
                        (code >= 0xA771 && code <= 0xA778)
                            || (code == 0xA77A)
                            || (code == 0xA77C)
                            || (code == 0xA77F)
                            || (code == 0xA781)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0xA779)
                            || (code == 0xA77B)
                            || (code >= 0xA77D && code <= 0xA77E)
                            || (code == 0xA780)
                            || (code == 0xA782)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xA78A then
                    if (code == 0xA783) || (code == 0xA785) || (code == 0xA787) then
                        Just LetterLowercase

                    else if (code == 0xA784) || (code == 0xA786) then
                        Just LetterUppercase

                    else if code == 0xA788 then
                        Just LetterModifier

                    else if code == 0xA789 then
                        Just SymbolModifier

                    else
                        Nothing

                else if code == 0xA78A then
                    Just SymbolModifier

                else if (code == 0xA78B) || (code == 0xA78D) || (code == 0xA790) then
                    Just LetterUppercase

                else if (code == 0xA78C) || (code == 0xA78E) || (code == 0xA791) then
                    Just LetterLowercase

                else if code == 0xA78F then
                    Just LetterOther

                else
                    Nothing

            else if code < 0xA7A0 then
                if
                    (code == 0xA792)
                        || (code == 0xA796)
                        || (code == 0xA798)
                        || (code == 0xA79A)
                        || (code == 0xA79C)
                        || (code == 0xA79E)
                then
                    Just LetterUppercase

                else if
                    (code >= 0xA793 && code <= 0xA795)
                        || (code == 0xA797)
                        || (code == 0xA799)
                        || (code == 0xA79B)
                        || (code == 0xA79D)
                        || (code == 0xA79F)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xA7A6 then
                if (code == 0xA7A0) || (code == 0xA7A2) || (code == 0xA7A4) then
                    Just LetterUppercase

                else if (code == 0xA7A1) || (code == 0xA7A3) || (code == 0xA7A5) then
                    Just LetterLowercase

                else
                    Nothing

            else if
                (code == 0xA7A6)
                    || (code == 0xA7A8)
                    || (code >= 0xA7AA && code <= 0xA7AE)
                    || (code >= 0xA7B0 && code <= 0xA7B4)
                    || (code == 0xA7B6)
            then
                Just LetterUppercase

            else if
                (code == 0xA7A7)
                    || (code == 0xA7A9)
                    || (code == 0xA7AF)
                    || (code == 0xA7B5)
            then
                Just LetterLowercase

            else
                Nothing

        else if code < 0xAA6F then
            if code < 0xA8CD then
                if code < 0xA7F6 then
                    if code < 0xA7C2 then
                        if
                            (code == 0xA7B7)
                                || (code == 0xA7B9)
                                || (code == 0xA7BB)
                                || (code == 0xA7BD)
                                || (code == 0xA7BF)
                                || (code == 0xA7C1)
                        then
                            Just LetterLowercase

                        else if
                            (code == 0xA7B8)
                                || (code == 0xA7BA)
                                || (code == 0xA7BC)
                                || (code == 0xA7BE)
                                || (code == 0xA7C0)
                        then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0xA7CF then
                        if
                            (code == 0xA7C2)
                                || (code >= 0xA7C4 && code <= 0xA7C7)
                                || (code == 0xA7C9)
                        then
                            Just LetterUppercase

                        else if
                            (code == 0xA7C3)
                                || (code == 0xA7C8)
                                || (code == 0xA7CA)
                        then
                            Just LetterLowercase

                        else
                            Nothing

                    else if
                        (code == 0xA7D0)
                            || (code == 0xA7D6)
                            || (code == 0xA7D8)
                            || (code == 0xA7F5)
                    then
                        Just LetterUppercase

                    else if
                        (code == 0xA7D7)
                            || (code == 0xA7D9)
                            || ((modBy 2 code == 1)
                                    && (code >= 0xA7D1 && code <= 0xA7D5)
                               )
                    then
                        Just LetterLowercase

                    else if code >= 0xA7F2 && code <= 0xA7F4 then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0xA826 then
                    if code < 0xA802 then
                        if (code == 0xA7F6) || (code == 0xA7FA) then
                            Just LetterLowercase

                        else if
                            (code == 0xA7F7)
                                || (code >= 0xA7FB && code <= 0xA801)
                        then
                            Just LetterOther

                        else if code >= 0xA7F8 && code <= 0xA7F9 then
                            Just LetterModifier

                        else
                            Nothing

                    else if
                        (code == 0xA802)
                            || (code == 0xA806)
                            || (code == 0xA80B)
                            || (code == 0xA825)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0xA803 && code <= 0xA805)
                            || (code >= 0xA807 && code <= 0xA80A)
                            || (code >= 0xA80C && code <= 0xA822)
                    then
                        Just LetterOther

                    else if code >= 0xA823 && code <= 0xA824 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0xA838 then
                    if (code == 0xA826) || (code == 0xA82C) then
                        Just MarkNonSpacing

                    else if code == 0xA827 then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0xA828 && code <= 0xA82B)
                            || (code >= 0xA836 && code <= 0xA837)
                    then
                        Just SymbolOther

                    else if code >= 0xA830 && code <= 0xA835 then
                        Just NumberOther

                    else
                        Nothing

                else if code == 0xA838 then
                    Just SymbolCurrency

                else if code == 0xA839 then
                    Just SymbolOther

                else if
                    (code >= 0xA840 && code <= 0xA873)
                        || (code >= 0xA882 && code <= 0xA8B3)
                then
                    Just LetterOther

                else if code >= 0xA874 && code <= 0xA877 then
                    Just PunctuationOther

                else if
                    (code >= 0xA880 && code <= 0xA881)
                        || (code >= 0xA8B4 && code <= 0xA8C3)
                then
                    Just MarkSpacingCombining

                else if code >= 0xA8C4 && code <= 0xA8C5 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0xA9BB then
                if code < 0xA92D then
                    if
                        (code >= 0xA8CE && code <= 0xA8CF)
                            || (code >= 0xA8F8 && code <= 0xA8FA)
                            || (code == 0xA8FC)
                    then
                        Just PunctuationOther

                    else if
                        (code >= 0xA8D0 && code <= 0xA8D9)
                            || (code >= 0xA900 && code <= 0xA909)
                    then
                        Just NumberDecimalDigit

                    else if
                        (code >= 0xA8E0 && code <= 0xA8F1)
                            || (code == 0xA8FF)
                            || (code >= 0xA926 && code <= 0xA92C)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0xA8F2 && code <= 0xA8F7)
                            || (code == 0xA8FB)
                            || (code >= 0xA8FD && code <= 0xA8FE)
                            || (code >= 0xA90A && code <= 0xA925)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0xA97F then
                    if (code == 0xA92D) || (code >= 0xA947 && code <= 0xA951) then
                        Just MarkNonSpacing

                    else if (code >= 0xA92E && code <= 0xA92F) || (code == 0xA95F) then
                        Just PunctuationOther

                    else if
                        (code >= 0xA930 && code <= 0xA946)
                            || (code >= 0xA960 && code <= 0xA97C)
                    then
                        Just LetterOther

                    else if code >= 0xA952 && code <= 0xA953 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if
                    (code >= 0xA980 && code <= 0xA982)
                        || (code == 0xA9B3)
                        || (code >= 0xA9B6 && code <= 0xA9B9)
                then
                    Just MarkNonSpacing

                else if
                    (code == 0xA983)
                        || (code >= 0xA9B4 && code <= 0xA9B5)
                        || (code == 0xA9BA)
                then
                    Just MarkSpacingCombining

                else if code >= 0xA984 && code <= 0xA9B2 then
                    Just LetterOther

                else
                    Nothing

            else if code < 0xA9FF then
                if code < 0xA9DD then
                    if (code == 0xA9BB) || (code >= 0xA9BE && code <= 0xA9C0) then
                        Just MarkSpacingCombining

                    else if code >= 0xA9BC && code <= 0xA9BD then
                        Just MarkNonSpacing

                    else if code >= 0xA9C1 && code <= 0xA9CD then
                        Just PunctuationOther

                    else if code == 0xA9CF then
                        Just LetterModifier

                    else if code >= 0xA9D0 && code <= 0xA9D9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code >= 0xA9DE && code <= 0xA9DF then
                    Just PunctuationOther

                else if
                    (code >= 0xA9E0 && code <= 0xA9E4)
                        || (code >= 0xA9E7 && code <= 0xA9EF)
                        || (code >= 0xA9FA && code <= 0xA9FE)
                then
                    Just LetterOther

                else if code == 0xA9E5 then
                    Just MarkNonSpacing

                else if code == 0xA9E6 then
                    Just LetterModifier

                else if code >= 0xA9F0 && code <= 0xA9F9 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0xAA42 then
                if
                    (code >= 0xAA00 && code <= 0xAA28)
                        || (code >= 0xAA40 && code <= 0xAA41)
                then
                    Just LetterOther

                else if
                    (code >= 0xAA29 && code <= 0xAA2E)
                        || (code >= 0xAA31 && code <= 0xAA32)
                        || (code >= 0xAA35 && code <= 0xAA36)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0xAA2F && code <= 0xAA30)
                        || (code >= 0xAA33 && code <= 0xAA34)
                then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if
                (code == 0xAA42)
                    || (code >= 0xAA44 && code <= 0xAA4B)
                    || (code >= 0xAA60 && code <= 0xAA6E)
            then
                Just LetterOther

            else if (code == 0xAA43) || (code == 0xAA4C) then
                Just MarkNonSpacing

            else if code == 0xAA4D then
                Just MarkSpacingCombining

            else if code >= 0xAA50 && code <= 0xAA59 then
                Just NumberDecimalDigit

            else if code >= 0xAA5C && code <= 0xAA5F then
                Just PunctuationOther

            else
                Nothing

        else if code < 0xABEC then
            if code < 0xAAED then
                if code < 0xAAB4 then
                    if
                        (code == 0xAA6F)
                            || (code >= 0xAA71 && code <= 0xAA76)
                            || (code == 0xAA7A)
                            || (code >= 0xAA7E && code <= 0xAAAF)
                            || (code == 0xAAB1)
                    then
                        Just LetterOther

                    else if code == 0xAA70 then
                        Just LetterModifier

                    else if code >= 0xAA77 && code <= 0xAA79 then
                        Just SymbolOther

                    else if (code == 0xAA7B) || (code == 0xAA7D) then
                        Just MarkSpacingCombining

                    else if
                        (code == 0xAA7C)
                            || (code == 0xAAB0)
                            || (code >= 0xAAB2 && code <= 0xAAB3)
                    then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0xAAC1 then
                    if
                        (code == 0xAAB4)
                            || (code >= 0xAAB7 && code <= 0xAAB8)
                            || (code >= 0xAABE && code <= 0xAABF)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0xAAB5 && code <= 0xAAB6)
                            || (code >= 0xAAB9 && code <= 0xAABD)
                            || (code == 0xAAC0)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if (code == 0xAAC1) || (code == 0xAAEC) then
                    Just MarkNonSpacing

                else if
                    (code == 0xAAC2)
                        || (code >= 0xAADB && code <= 0xAADC)
                        || (code >= 0xAAE0 && code <= 0xAAEA)
                then
                    Just LetterOther

                else if code == 0xAADD then
                    Just LetterModifier

                else if code >= 0xAADE && code <= 0xAADF then
                    Just PunctuationOther

                else if code == 0xAAEB then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0xAB5A then
                if code < 0xAAF5 then
                    if code == 0xAAED then
                        Just MarkNonSpacing

                    else if code >= 0xAAEE && code <= 0xAAEF then
                        Just MarkSpacingCombining

                    else if code >= 0xAAF0 && code <= 0xAAF1 then
                        Just PunctuationOther

                    else if code == 0xAAF2 then
                        Just LetterOther

                    else if code >= 0xAAF3 && code <= 0xAAF4 then
                        Just LetterModifier

                    else
                        Nothing

                else if code == 0xAAF5 then
                    Just MarkSpacingCombining

                else if code == 0xAAF6 then
                    Just MarkNonSpacing

                else if
                    (code >= 0xAB01 && code <= 0xAB06)
                        || (code >= 0xAB09 && code <= 0xAB0E)
                        || (code >= 0xAB11 && code <= 0xAB16)
                        || (code >= 0xAB20 && code <= 0xAB26)
                        || (code >= 0xAB28 && code <= 0xAB2E)
                then
                    Just LetterOther

                else if code >= 0xAB30 && code <= 0xAB59 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xABBF then
                if
                    (code == 0xAB5A)
                        || (code >= 0xAB60 && code <= 0xAB68)
                        || (code >= 0xAB70 && code <= 0xABBE)
                then
                    Just LetterLowercase

                else if (code == 0xAB5B) || (code >= 0xAB6A && code <= 0xAB6B) then
                    Just SymbolModifier

                else if (code >= 0xAB5C && code <= 0xAB5F) || (code == 0xAB69) then
                    Just LetterModifier

                else
                    Nothing

            else if code == 0xABBF then
                Just LetterLowercase

            else if code >= 0xABC0 && code <= 0xABE2 then
                Just LetterOther

            else if
                (code >= 0xABE3 && code <= 0xABE4)
                    || (code >= 0xABE6 && code <= 0xABE7)
                    || (code >= 0xABE9 && code <= 0xABEA)
            then
                Just MarkSpacingCombining

            else if (code == 0xABE5) || (code == 0xABE8) then
                Just MarkNonSpacing

            else if code == 0xABEB then
                Just PunctuationOther

            else
                Nothing

        else if code < 0xFB45 then
            if code < 0xF8FF then
                if code == 0xABEC then
                    Just MarkSpacingCombining

                else if code == 0xABED then
                    Just MarkNonSpacing

                else if code >= 0xABF0 && code <= 0xABF9 then
                    Just NumberDecimalDigit

                else if
                    (code == 0xAC00)
                        || (code == 0xD7A3)
                        || (code >= 0xD7B0 && code <= 0xD7C6)
                        || (code >= 0xD7CB && code <= 0xD7FB)
                then
                    Just LetterOther

                else if
                    (code == 0xD800)
                        || (code >= 0xDB7F && code <= 0xDB80)
                        || (code >= 0xDBFF && code <= 0xDC00)
                        || (code == 0xDFFF)
                then
                    Just OtherSurrogate

                else if code == 0xE000 then
                    Just OtherPrivateUse

                else
                    Nothing

            else if code < 0xFB1E then
                if code == 0xF8FF then
                    Just OtherPrivateUse

                else if
                    (code >= 0xF900 && code <= 0xFA6D)
                        || (code >= 0xFA70 && code <= 0xFAD9)
                        || (code == 0xFB1D)
                then
                    Just LetterOther

                else if
                    (code >= 0xFB00 && code <= 0xFB06)
                        || (code >= 0xFB13 && code <= 0xFB17)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code == 0xFB1E then
                Just MarkNonSpacing

            else if
                (code >= 0xFB1F && code <= 0xFB28)
                    || (code >= 0xFB2A && code <= 0xFB36)
                    || (code >= 0xFB38 && code <= 0xFB3C)
                    || (code == 0xFB3E)
                    || (code >= 0xFB40 && code <= 0xFB41)
                    || (code >= 0xFB43 && code <= 0xFB44)
            then
                Just LetterOther

            else if code == 0xFB29 then
                Just SymbolMath

            else
                Nothing

        else if code < 0xFE0F then
            if code < 0xFD4F then
                if
                    (code >= 0xFB46 && code <= 0xFBB1)
                        || (code >= 0xFBD3 && code <= 0xFD3D)
                then
                    Just LetterOther

                else if code >= 0xFBB2 && code <= 0xFBC2 then
                    Just SymbolModifier

                else if code == 0xFD3E then
                    Just PunctuationClose

                else if code == 0xFD3F then
                    Just PunctuationOpen

                else if code >= 0xFD40 && code <= 0xFD4E then
                    Just SymbolOther

                else
                    Nothing

            else if
                (code == 0xFD4F)
                    || (code == 0xFDCF)
                    || (code >= 0xFDFD && code <= 0xFDFF)
            then
                Just SymbolOther

            else if
                (code >= 0xFD50 && code <= 0xFD8F)
                    || (code >= 0xFD92 && code <= 0xFDC7)
                    || (code >= 0xFDF0 && code <= 0xFDFB)
            then
                Just LetterOther

            else if code == 0xFDFC then
                Just SymbolCurrency

            else if code >= 0xFE00 && code <= 0xFE0E then
                Just MarkNonSpacing

            else
                Nothing

        else if code < 0xFE30 then
            if (code == 0xFE0F) || (code >= 0xFE20 && code <= 0xFE2F) then
                Just MarkNonSpacing

            else if (code >= 0xFE10 && code <= 0xFE16) || (code == 0xFE19) then
                Just PunctuationOther

            else if code == 0xFE17 then
                Just PunctuationOpen

            else if code == 0xFE18 then
                Just PunctuationClose

            else
                Nothing

        else if code == 0xFE30 then
            Just PunctuationOther

        else if code >= 0xFE31 && code <= 0xFE32 then
            Just PunctuationDash

        else if code >= 0xFE33 && code <= 0xFE34 then
            Just PunctuationConnector

        else if (code == 0xFE35) || (code == 0xFE37) || (code == 0xFE39) then
            Just PunctuationOpen

        else if (code == 0xFE36) || (code == 0xFE38) then
            Just PunctuationClose

        else
            Nothing

    else if code < 0x00011838 then
        if code < 0x00010B7F then
            if code < 0x0001018B then
                if code < 0xFF3A then
                    if code < 0xFE5E then
                        if code < 0xFE46 then
                            if
                                (code == 0xFE3A)
                                    || (code == 0xFE3C)
                                    || (code == 0xFE3E)
                                    || (code == 0xFE40)
                                    || (code == 0xFE42)
                                    || (code == 0xFE44)
                            then
                                Just PunctuationClose

                            else if
                                (code == 0xFE3B)
                                    || (code == 0xFE3D)
                                    || (code == 0xFE3F)
                                    || (code == 0xFE41)
                                    || (code == 0xFE43)
                            then
                                Just PunctuationOpen

                            else if code == 0xFE45 then
                                Just PunctuationOther

                            else
                                Nothing

                        else if code < 0xFE53 then
                            if
                                (code == 0xFE46)
                                    || (code >= 0xFE49 && code <= 0xFE4C)
                                    || (code >= 0xFE50 && code <= 0xFE52)
                            then
                                Just PunctuationOther

                            else if code == 0xFE47 then
                                Just PunctuationOpen

                            else if code == 0xFE48 then
                                Just PunctuationClose

                            else if code >= 0xFE4D && code <= 0xFE4F then
                                Just PunctuationConnector

                            else
                                Nothing

                        else if code >= 0xFE54 && code <= 0xFE57 then
                            Just PunctuationOther

                        else if code == 0xFE58 then
                            Just PunctuationDash

                        else if
                            (code == 0xFE59)
                                || (code == 0xFE5B)
                                || (code == 0xFE5D)
                        then
                            Just PunctuationOpen

                        else if (code == 0xFE5A) || (code == 0xFE5C) then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code < 0xFF04 then
                        if code == 0xFE5E then
                            Just PunctuationClose

                        else if
                            (code >= 0xFE5F && code <= 0xFE61)
                                || (code == 0xFE68)
                                || (code >= 0xFE6A && code <= 0xFE6B)
                                || (code >= 0xFF01 && code <= 0xFF03)
                        then
                            Just PunctuationOther

                        else if
                            (code == 0xFE62)
                                || (code >= 0xFE64 && code <= 0xFE66)
                        then
                            Just SymbolMath

                        else if code == 0xFE63 then
                            Just PunctuationDash

                        else if code == 0xFE69 then
                            Just SymbolCurrency

                        else if
                            (code >= 0xFE70 && code <= 0xFE74)
                                || (code >= 0xFE76 && code <= 0xFEFC)
                        then
                            Just LetterOther

                        else if code == 0xFEFF then
                            Just OtherFormat

                        else
                            Nothing

                    else if code < 0xFF0C then
                        if code == 0xFF04 then
                            Just SymbolCurrency

                        else if
                            (code >= 0xFF05 && code <= 0xFF07)
                                || (code == 0xFF0A)
                        then
                            Just PunctuationOther

                        else if code == 0xFF08 then
                            Just PunctuationOpen

                        else if code == 0xFF09 then
                            Just PunctuationClose

                        else if code == 0xFF0B then
                            Just SymbolMath

                        else
                            Nothing

                    else if
                        (code == 0xFF0C)
                            || (code >= 0xFF0E && code <= 0xFF0F)
                            || (code >= 0xFF1A && code <= 0xFF1B)
                            || (code >= 0xFF1F && code <= 0xFF20)
                    then
                        Just PunctuationOther

                    else if code == 0xFF0D then
                        Just PunctuationDash

                    else if code >= 0xFF10 && code <= 0xFF19 then
                        Just NumberDecimalDigit

                    else if code >= 0xFF1C && code <= 0xFF1E then
                        Just SymbolMath

                    else if code >= 0xFF21 && code <= 0xFF39 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xFFD1 then
                    if code < 0xFF5E then
                        if code == 0xFF3A then
                            Just LetterUppercase

                        else if (code == 0xFF3B) || (code == 0xFF5B) then
                            Just PunctuationOpen

                        else if code == 0xFF3C then
                            Just PunctuationOther

                        else if (code == 0xFF3D) || (code == 0xFF5D) then
                            Just PunctuationClose

                        else if (code == 0xFF3E) || (code == 0xFF40) then
                            Just SymbolModifier

                        else if code == 0xFF3F then
                            Just PunctuationConnector

                        else if code >= 0xFF41 && code <= 0xFF5A then
                            Just LetterLowercase

                        else if code == 0xFF5C then
                            Just SymbolMath

                        else
                            Nothing

                    else if code < 0xFF65 then
                        if code == 0xFF5E then
                            Just SymbolMath

                        else if (code == 0xFF5F) || (code == 0xFF62) then
                            Just PunctuationOpen

                        else if (code == 0xFF60) || (code == 0xFF63) then
                            Just PunctuationClose

                        else if (code == 0xFF61) || (code == 0xFF64) then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code == 0xFF65 then
                        Just PunctuationOther

                    else if
                        (code >= 0xFF66 && code <= 0xFF6F)
                            || (code >= 0xFF71 && code <= 0xFF9D)
                            || (code >= 0xFFA0 && code <= 0xFFBE)
                            || (code >= 0xFFC2 && code <= 0xFFC7)
                            || (code >= 0xFFCA && code <= 0xFFCF)
                    then
                        Just LetterOther

                    else if (code == 0xFF70) || (code >= 0xFF9E && code <= 0xFF9F) then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0x0001000C then
                    if code < 0xFFE4 then
                        if
                            (code >= 0xFFD2 && code <= 0xFFD7)
                                || (code >= 0xFFDA && code <= 0xFFDC)
                        then
                            Just LetterOther

                        else if code >= 0xFFE0 && code <= 0xFFE1 then
                            Just SymbolCurrency

                        else if code == 0xFFE2 then
                            Just SymbolMath

                        else if code == 0xFFE3 then
                            Just SymbolModifier

                        else
                            Nothing

                    else if
                        (code == 0xFFE4)
                            || (code == 0xFFE8)
                            || (code >= 0xFFED && code <= 0xFFEE)
                            || (code >= 0xFFFC && code <= 0xFFFD)
                    then
                        Just SymbolOther

                    else if code >= 0xFFE5 && code <= 0xFFE6 then
                        Just SymbolCurrency

                    else if code >= 0xFFE9 && code <= 0xFFEC then
                        Just SymbolMath

                    else if code >= 0xFFF9 && code <= 0xFFFB then
                        Just OtherFormat

                    else if code >= 0x00010000 && code <= 0x0001000B then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x000100FF then
                    if
                        (code >= 0x0001000D && code <= 0x00010026)
                            || (code >= 0x00010028 && code <= 0x0001003A)
                            || (code >= 0x0001003C && code <= 0x0001003D)
                            || (code >= 0x0001003F && code <= 0x0001004D)
                            || (code >= 0x00010050 && code <= 0x0001005D)
                            || (code >= 0x00010080 && code <= 0x000100FA)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if code >= 0x00010100 && code <= 0x00010102 then
                    Just PunctuationOther

                else if
                    (code >= 0x00010107 && code <= 0x00010133)
                        || (code >= 0x00010175 && code <= 0x00010178)
                        || (code == 0x0001018A)
                then
                    Just NumberOther

                else if
                    (code >= 0x00010137 && code <= 0x0001013F)
                        || (code >= 0x00010179 && code <= 0x00010189)
                then
                    Just SymbolOther

                else if code >= 0x00010140 && code <= 0x00010174 then
                    Just NumberLetter

                else
                    Nothing

            else if code < 0x00010836 then
                if code < 0x00010427 then
                    if code < 0x0001032C then
                        if
                            (code == 0x0001018B)
                                || (code >= 0x000102E1 && code <= 0x000102FB)
                                || (code >= 0x00010320 && code <= 0x00010323)
                        then
                            Just NumberOther

                        else if
                            (code >= 0x0001018C && code <= 0x0001018E)
                                || (code >= 0x00010190 && code <= 0x0001019C)
                                || (code == 0x000101A0)
                                || (code >= 0x000101D0 && code <= 0x000101FC)
                        then
                            Just SymbolOther

                        else if (code == 0x000101FD) || (code == 0x000102E0) then
                            Just MarkNonSpacing

                        else if
                            (code >= 0x00010280 && code <= 0x0001029C)
                                || (code >= 0x000102A0 && code <= 0x000102D0)
                                || (code >= 0x00010300 && code <= 0x0001031F)
                        then
                            Just LetterOther

                        else
                            Nothing

                    else if code < 0x0001037F then
                        if
                            (code >= 0x0001032D && code <= 0x00010340)
                                || (code >= 0x00010342 && code <= 0x00010349)
                                || (code >= 0x00010350 && code <= 0x00010375)
                        then
                            Just LetterOther

                        else if (code == 0x00010341) || (code == 0x0001034A) then
                            Just NumberLetter

                        else if code >= 0x00010376 && code <= 0x0001037A then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if
                        (code >= 0x00010380 && code <= 0x0001039D)
                            || (code >= 0x000103A0 && code <= 0x000103C3)
                            || (code >= 0x000103C8 && code <= 0x000103CF)
                    then
                        Just LetterOther

                    else if (code == 0x0001039F) || (code == 0x000103D0) then
                        Just PunctuationOther

                    else if code >= 0x000103D1 && code <= 0x000103D5 then
                        Just NumberLetter

                    else if code >= 0x00010400 && code <= 0x00010426 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x00010596 then
                    if code < 0x000104FF then
                        if
                            (code == 0x00010427)
                                || (code >= 0x000104B0 && code <= 0x000104D3)
                        then
                            Just LetterUppercase

                        else if
                            (code >= 0x00010428 && code <= 0x0001044F)
                                || (code >= 0x000104D8 && code <= 0x000104FB)
                        then
                            Just LetterLowercase

                        else if code >= 0x00010450 && code <= 0x0001049D then
                            Just LetterOther

                        else if code >= 0x000104A0 && code <= 0x000104A9 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if
                        (code >= 0x00010500 && code <= 0x00010527)
                            || (code >= 0x00010530 && code <= 0x00010563)
                    then
                        Just LetterOther

                    else if code == 0x0001056F then
                        Just PunctuationOther

                    else if
                        (code >= 0x00010570 && code <= 0x0001057A)
                            || (code >= 0x0001057C && code <= 0x0001058A)
                            || (code >= 0x0001058C && code <= 0x00010592)
                            || (code >= 0x00010594 && code <= 0x00010595)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x0001075F then
                    if
                        (code >= 0x00010597 && code <= 0x000105A1)
                            || (code >= 0x000105A3 && code <= 0x000105B1)
                            || (code >= 0x000105B3 && code <= 0x000105B9)
                            || (code >= 0x000105BB && code <= 0x000105BC)
                    then
                        Just LetterLowercase

                    else if
                        (code >= 0x00010600 && code <= 0x00010736)
                            || (code >= 0x00010740 && code <= 0x00010755)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if
                    (code >= 0x00010760 && code <= 0x00010767)
                        || (code >= 0x00010800 && code <= 0x00010805)
                        || (code == 0x00010808)
                        || (code >= 0x0001080A && code <= 0x00010835)
                then
                    Just LetterOther

                else if
                    (code >= 0x00010780 && code <= 0x00010785)
                        || (code >= 0x00010787 && code <= 0x000107B0)
                        || (code >= 0x000107B2 && code <= 0x000107BA)
                then
                    Just LetterModifier

                else
                    Nothing

            else if code < 0x00010A04 then
                if code < 0x000108FA then
                    if
                        (code >= 0x00010837 && code <= 0x00010838)
                            || (code == 0x0001083C)
                            || (code >= 0x0001083F && code <= 0x00010855)
                            || (code >= 0x00010860 && code <= 0x00010876)
                            || (code >= 0x00010880 && code <= 0x0001089E)
                            || (code >= 0x000108E0 && code <= 0x000108F2)
                            || (code >= 0x000108F4 && code <= 0x000108F5)
                    then
                        Just LetterOther

                    else if code == 0x00010857 then
                        Just PunctuationOther

                    else if
                        (code >= 0x00010858 && code <= 0x0001085F)
                            || (code >= 0x00010879 && code <= 0x0001087F)
                            || (code >= 0x000108A7 && code <= 0x000108AF)
                    then
                        Just NumberOther

                    else if code >= 0x00010877 && code <= 0x00010878 then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0x0001097F then
                    if
                        (code >= 0x000108FB && code <= 0x000108FF)
                            || (code >= 0x00010916 && code <= 0x0001091B)
                    then
                        Just NumberOther

                    else if
                        (code >= 0x00010900 && code <= 0x00010915)
                            || (code >= 0x00010920 && code <= 0x00010939)
                    then
                        Just LetterOther

                    else if (code == 0x0001091F) || (code == 0x0001093F) then
                        Just PunctuationOther

                    else
                        Nothing

                else if
                    (code >= 0x00010980 && code <= 0x000109B7)
                        || (code >= 0x000109BE && code <= 0x000109BF)
                        || (code == 0x00010A00)
                then
                    Just LetterOther

                else if
                    (code >= 0x000109BC && code <= 0x000109BD)
                        || (code >= 0x000109C0 && code <= 0x000109CF)
                        || (code >= 0x000109D2 && code <= 0x000109FF)
                then
                    Just NumberOther

                else if code >= 0x00010A01 && code <= 0x00010A03 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x00010A9C then
                if code < 0x00010A3E then
                    if
                        (code >= 0x00010A05 && code <= 0x00010A06)
                            || (code >= 0x00010A0C && code <= 0x00010A0F)
                            || (code >= 0x00010A38 && code <= 0x00010A3A)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x00010A10 && code <= 0x00010A13)
                            || (code >= 0x00010A15 && code <= 0x00010A17)
                            || (code >= 0x00010A19 && code <= 0x00010A35)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if code == 0x00010A3F then
                    Just MarkNonSpacing

                else if
                    (code >= 0x00010A40 && code <= 0x00010A48)
                        || (code >= 0x00010A7D && code <= 0x00010A7E)
                then
                    Just NumberOther

                else if
                    (code >= 0x00010A50 && code <= 0x00010A58)
                        || (code == 0x00010A7F)
                then
                    Just PunctuationOther

                else if
                    (code >= 0x00010A60 && code <= 0x00010A7C)
                        || (code >= 0x00010A80 && code <= 0x00010A9B)
                then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x00010AEF then
                if
                    (code == 0x00010A9C)
                        || (code >= 0x00010AC0 && code <= 0x00010AC7)
                        || (code >= 0x00010AC9 && code <= 0x00010AE4)
                then
                    Just LetterOther

                else if
                    (code >= 0x00010A9D && code <= 0x00010A9F)
                        || (code >= 0x00010AEB && code <= 0x00010AEE)
                then
                    Just NumberOther

                else if code == 0x00010AC8 then
                    Just SymbolOther

                else if code >= 0x00010AE5 && code <= 0x00010AE6 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if
                (code == 0x00010AEF)
                    || (code >= 0x00010B58 && code <= 0x00010B5F)
                    || (code >= 0x00010B78 && code <= 0x00010B7E)
            then
                Just NumberOther

            else if
                (code >= 0x00010AF0 && code <= 0x00010AF6)
                    || (code >= 0x00010B39 && code <= 0x00010B3F)
            then
                Just PunctuationOther

            else if
                (code >= 0x00010B00 && code <= 0x00010B35)
                    || (code >= 0x00010B40 && code <= 0x00010B55)
                    || (code >= 0x00010B60 && code <= 0x00010B72)
            then
                Just LetterOther

            else
                Nothing

        else if code < 0x0001129E then
            if code < 0x000110BD then
                if code < 0x00010F81 then
                    if code < 0x00010E7F then
                        if
                            (code == 0x00010B7F)
                                || (code >= 0x00010BA9 && code <= 0x00010BAF)
                                || (code >= 0x00010CFA && code <= 0x00010CFF)
                                || (code >= 0x00010E60 && code <= 0x00010E7E)
                        then
                            Just NumberOther

                        else if
                            (code >= 0x00010B80 && code <= 0x00010B91)
                                || (code >= 0x00010C00 && code <= 0x00010C48)
                                || (code >= 0x00010D00 && code <= 0x00010D23)
                        then
                            Just LetterOther

                        else if code >= 0x00010B99 && code <= 0x00010B9C then
                            Just PunctuationOther

                        else if code >= 0x00010C80 && code <= 0x00010CB2 then
                            Just LetterUppercase

                        else if code >= 0x00010CC0 && code <= 0x00010CF2 then
                            Just LetterLowercase

                        else if code >= 0x00010D24 && code <= 0x00010D27 then
                            Just MarkNonSpacing

                        else if code >= 0x00010D30 && code <= 0x00010D39 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if
                        (code >= 0x00010E80 && code <= 0x00010EA9)
                            || (code >= 0x00010EB0 && code <= 0x00010EB1)
                            || (code >= 0x00010F00 && code <= 0x00010F1C)
                            || (code == 0x00010F27)
                            || (code >= 0x00010F30 && code <= 0x00010F45)
                            || (code >= 0x00010F70 && code <= 0x00010F80)
                    then
                        Just LetterOther

                    else if
                        (code >= 0x00010EAB && code <= 0x00010EAC)
                            || (code >= 0x00010F46 && code <= 0x00010F50)
                    then
                        Just MarkNonSpacing

                    else if code == 0x00010EAD then
                        Just PunctuationDash

                    else if
                        (code >= 0x00010F1D && code <= 0x00010F26)
                            || (code >= 0x00010F51 && code <= 0x00010F54)
                    then
                        Just NumberOther

                    else if code >= 0x00010F55 && code <= 0x00010F59 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x00011065 then
                    if code < 0x00010FFF then
                        if
                            (code == 0x00010F81)
                                || (code >= 0x00010FB0 && code <= 0x00010FC4)
                                || (code >= 0x00010FE0 && code <= 0x00010FF6)
                        then
                            Just LetterOther

                        else if code >= 0x00010F82 && code <= 0x00010F85 then
                            Just MarkNonSpacing

                        else if code >= 0x00010F86 && code <= 0x00010F89 then
                            Just PunctuationOther

                        else if code >= 0x00010FC5 && code <= 0x00010FCB then
                            Just NumberOther

                        else
                            Nothing

                    else if (code == 0x00011000) || (code == 0x00011002) then
                        Just MarkSpacingCombining

                    else if
                        (code == 0x00011001)
                            || (code >= 0x00011038 && code <= 0x00011046)
                    then
                        Just MarkNonSpacing

                    else if code >= 0x00011003 && code <= 0x00011037 then
                        Just LetterOther

                    else if code >= 0x00011047 && code <= 0x0001104D then
                        Just PunctuationOther

                    else if code >= 0x00011052 && code <= 0x00011064 then
                        Just NumberOther

                    else
                        Nothing

                else if code < 0x00011081 then
                    if code == 0x00011065 then
                        Just NumberOther

                    else if code >= 0x00011066 && code <= 0x0001106F then
                        Just NumberDecimalDigit

                    else if
                        (code == 0x00011070)
                            || (code >= 0x00011073 && code <= 0x00011074)
                            || (code >= 0x0001107F && code <= 0x00011080)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x00011071 && code <= 0x00011072)
                            || (code == 0x00011075)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if
                    (code == 0x00011081)
                        || (code >= 0x000110B3 && code <= 0x000110B6)
                        || (code >= 0x000110B9 && code <= 0x000110BA)
                then
                    Just MarkNonSpacing

                else if
                    (code == 0x00011082)
                        || (code >= 0x000110B0 && code <= 0x000110B2)
                        || (code >= 0x000110B7 && code <= 0x000110B8)
                then
                    Just MarkSpacingCombining

                else if code >= 0x00011083 && code <= 0x000110AF then
                    Just LetterOther

                else if code >= 0x000110BB && code <= 0x000110BC then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x000111C0 then
                if code < 0x00011143 then
                    if code < 0x000110FF then
                        if (code == 0x000110BD) || (code == 0x000110CD) then
                            Just OtherFormat

                        else if code >= 0x000110BE && code <= 0x000110C1 then
                            Just PunctuationOther

                        else if code == 0x000110C2 then
                            Just MarkNonSpacing

                        else if code >= 0x000110D0 && code <= 0x000110E8 then
                            Just LetterOther

                        else if code >= 0x000110F0 && code <= 0x000110F9 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if
                        (code >= 0x00011100 && code <= 0x00011102)
                            || (code >= 0x00011127 && code <= 0x0001112B)
                            || (code >= 0x0001112D && code <= 0x00011134)
                    then
                        Just MarkNonSpacing

                    else if code >= 0x00011103 && code <= 0x00011126 then
                        Just LetterOther

                    else if code == 0x0001112C then
                        Just MarkSpacingCombining

                    else if code >= 0x00011136 && code <= 0x0001113F then
                        Just NumberDecimalDigit

                    else if code >= 0x00011140 && code <= 0x00011142 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x00011175 then
                    if (code == 0x00011143) || (code == 0x00011174) then
                        Just PunctuationOther

                    else if
                        (code == 0x00011144)
                            || (code == 0x00011147)
                            || (code >= 0x00011150 && code <= 0x00011172)
                    then
                        Just LetterOther

                    else if code >= 0x00011145 && code <= 0x00011146 then
                        Just MarkSpacingCombining

                    else if code == 0x00011173 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x00011175 then
                    Just PunctuationOther

                else if
                    (code == 0x00011176)
                        || (code >= 0x00011183 && code <= 0x000111B2)
                then
                    Just LetterOther

                else if
                    (code >= 0x00011180 && code <= 0x00011181)
                        || (code >= 0x000111B6 && code <= 0x000111BE)
                then
                    Just MarkNonSpacing

                else if
                    (code == 0x00011182)
                        || (code >= 0x000111B3 && code <= 0x000111B5)
                        || (code == 0x000111BF)
                then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x000111FF then
                if code < 0x000111CE then
                    if code == 0x000111C0 then
                        Just MarkSpacingCombining

                    else if code >= 0x000111C1 && code <= 0x000111C4 then
                        Just LetterOther

                    else if
                        (code >= 0x000111C5 && code <= 0x000111C8)
                            || (code == 0x000111CD)
                    then
                        Just PunctuationOther

                    else if code >= 0x000111C9 && code <= 0x000111CC then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x000111CE then
                    Just MarkSpacingCombining

                else if code == 0x000111CF then
                    Just MarkNonSpacing

                else if code >= 0x000111D0 && code <= 0x000111D9 then
                    Just NumberDecimalDigit

                else if (code == 0x000111DA) || (code == 0x000111DC) then
                    Just LetterOther

                else if
                    (code == 0x000111DB)
                        || (code >= 0x000111DD && code <= 0x000111DF)
                then
                    Just PunctuationOther

                else if code >= 0x000111E1 && code <= 0x000111F4 then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x00011235 then
                if
                    (code >= 0x00011200 && code <= 0x00011211)
                        || (code >= 0x00011213 && code <= 0x0001122B)
                then
                    Just LetterOther

                else if
                    (code >= 0x0001122C && code <= 0x0001122E)
                        || (code >= 0x00011232 && code <= 0x00011233)
                then
                    Just MarkSpacingCombining

                else if
                    (code >= 0x0001122F && code <= 0x00011231)
                        || (code == 0x00011234)
                then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code == 0x00011235 then
                Just MarkSpacingCombining

            else if
                (code >= 0x00011236 && code <= 0x00011237)
                    || (code == 0x0001123E)
            then
                Just MarkNonSpacing

            else if code >= 0x00011238 && code <= 0x0001123D then
                Just PunctuationOther

            else if
                (code >= 0x00011280 && code <= 0x00011286)
                    || (code == 0x00011288)
                    || (code >= 0x0001128A && code <= 0x0001128D)
                    || (code >= 0x0001128F && code <= 0x0001129D)
            then
                Just LetterOther

            else
                Nothing

        else if code < 0x000114C1 then
            if code < 0x0001135C then
                if code < 0x00011329 then
                    if
                        (code >= 0x0001129F && code <= 0x000112A8)
                            || (code >= 0x000112B0 && code <= 0x000112DE)
                            || (code >= 0x00011305 && code <= 0x0001130C)
                            || (code >= 0x0001130F && code <= 0x00011310)
                            || (code >= 0x00011313 && code <= 0x00011328)
                    then
                        Just LetterOther

                    else if code == 0x000112A9 then
                        Just PunctuationOther

                    else if
                        (code == 0x000112DF)
                            || (code >= 0x000112E3 && code <= 0x000112EA)
                            || (code >= 0x00011300 && code <= 0x00011301)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x000112E0 && code <= 0x000112E2)
                            || (code >= 0x00011302 && code <= 0x00011303)
                    then
                        Just MarkSpacingCombining

                    else if code >= 0x000112F0 && code <= 0x000112F9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if
                    (code >= 0x0001132A && code <= 0x00011330)
                        || (code >= 0x00011332 && code <= 0x00011333)
                        || (code >= 0x00011335 && code <= 0x00011339)
                        || (code == 0x0001133D)
                        || (code == 0x00011350)
                then
                    Just LetterOther

                else if
                    (code >= 0x0001133B && code <= 0x0001133C)
                        || (code == 0x00011340)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0x0001133E && code <= 0x0001133F)
                        || (code >= 0x00011341 && code <= 0x00011344)
                        || (code >= 0x00011347 && code <= 0x00011348)
                        || (code >= 0x0001134B && code <= 0x0001134D)
                        || (code == 0x00011357)
                then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x0001144A then
                if
                    (code >= 0x0001135D && code <= 0x00011361)
                        || (code >= 0x00011400 && code <= 0x00011434)
                        || (code >= 0x00011447 && code <= 0x00011449)
                then
                    Just LetterOther

                else if
                    (code >= 0x00011362 && code <= 0x00011363)
                        || (code >= 0x00011435 && code <= 0x00011437)
                        || (code >= 0x00011440 && code <= 0x00011441)
                        || (code == 0x00011445)
                then
                    Just MarkSpacingCombining

                else if
                    (code >= 0x00011366 && code <= 0x0001136C)
                        || (code >= 0x00011370 && code <= 0x00011374)
                        || (code >= 0x00011438 && code <= 0x0001143F)
                        || (code >= 0x00011442 && code <= 0x00011444)
                        || (code == 0x00011446)
                then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x0001147F then
                if
                    (code == 0x0001144A)
                        || (code >= 0x0001145F && code <= 0x00011461)
                then
                    Just LetterOther

                else if
                    (code >= 0x0001144B && code <= 0x0001144F)
                        || (code >= 0x0001145A && code <= 0x0001145B)
                        || (code == 0x0001145D)
                then
                    Just PunctuationOther

                else if code >= 0x00011450 && code <= 0x00011459 then
                    Just NumberDecimalDigit

                else if code == 0x0001145E then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code >= 0x00011480 && code <= 0x000114AF then
                Just LetterOther

            else if
                (code >= 0x000114B0 && code <= 0x000114B2)
                    || (code == 0x000114B9)
                    || (code >= 0x000114BB && code <= 0x000114BE)
            then
                Just MarkSpacingCombining

            else if
                (code >= 0x000114B3 && code <= 0x000114B8)
                    || (code == 0x000114BA)
                    || (code >= 0x000114BF && code <= 0x000114C0)
            then
                Just MarkNonSpacing

            else
                Nothing

        else if code < 0x0001165F then
            if code < 0x000115C0 then
                if code < 0x0001157F then
                    if code == 0x000114C1 then
                        Just MarkSpacingCombining

                    else if code >= 0x000114C2 && code <= 0x000114C3 then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x000114C4 && code <= 0x000114C5)
                            || (code == 0x000114C7)
                    then
                        Just LetterOther

                    else if code == 0x000114C6 then
                        Just PunctuationOther

                    else if code >= 0x000114D0 && code <= 0x000114D9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code >= 0x00011580 && code <= 0x000115AE then
                    Just LetterOther

                else if
                    (code >= 0x000115AF && code <= 0x000115B1)
                        || (code >= 0x000115B8 && code <= 0x000115BB)
                        || (code == 0x000115BE)
                then
                    Just MarkSpacingCombining

                else if
                    (code >= 0x000115B2 && code <= 0x000115B5)
                        || (code >= 0x000115BC && code <= 0x000115BD)
                        || (code == 0x000115BF)
                then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x0001163A then
                if
                    (code == 0x000115C0)
                        || (code >= 0x000115DC && code <= 0x000115DD)
                        || (code >= 0x00011633 && code <= 0x00011639)
                then
                    Just MarkNonSpacing

                else if code >= 0x000115C1 && code <= 0x000115D7 then
                    Just PunctuationOther

                else if
                    (code >= 0x000115D8 && code <= 0x000115DB)
                        || (code >= 0x00011600 && code <= 0x0001162F)
                then
                    Just LetterOther

                else if code >= 0x00011630 && code <= 0x00011632 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if
                (code == 0x0001163A)
                    || (code == 0x0001163D)
                    || (code >= 0x0001163F && code <= 0x00011640)
            then
                Just MarkNonSpacing

            else if
                (code >= 0x0001163B && code <= 0x0001163C)
                    || (code == 0x0001163E)
            then
                Just MarkSpacingCombining

            else if code >= 0x00011641 && code <= 0x00011643 then
                Just PunctuationOther

            else if code == 0x00011644 then
                Just LetterOther

            else if code >= 0x00011650 && code <= 0x00011659 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code < 0x0001171C then
            if code < 0x000116AF then
                if code >= 0x00011660 && code <= 0x0001166C then
                    Just PunctuationOther

                else if code >= 0x00011680 && code <= 0x000116AA then
                    Just LetterOther

                else if (code == 0x000116AB) || (code == 0x000116AD) then
                    Just MarkNonSpacing

                else if (code == 0x000116AC) || (code == 0x000116AE) then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if (code == 0x000116AF) || (code == 0x000116B6) then
                Just MarkSpacingCombining

            else if
                (code >= 0x000116B0 && code <= 0x000116B5)
                    || (code == 0x000116B7)
            then
                Just MarkNonSpacing

            else if
                (code == 0x000116B8)
                    || (code >= 0x00011700 && code <= 0x0001171A)
            then
                Just LetterOther

            else if code == 0x000116B9 then
                Just PunctuationOther

            else if code >= 0x000116C0 && code <= 0x000116C9 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code < 0x00011739 then
            if
                (code >= 0x0001171D && code <= 0x0001171F)
                    || (code >= 0x00011722 && code <= 0x00011725)
                    || (code >= 0x00011727 && code <= 0x0001172B)
            then
                Just MarkNonSpacing

            else if
                (code >= 0x00011720 && code <= 0x00011721)
                    || (code == 0x00011726)
            then
                Just MarkSpacingCombining

            else if code >= 0x00011730 && code <= 0x00011738 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code == 0x00011739 then
            Just NumberDecimalDigit

        else if code >= 0x0001173A && code <= 0x0001173B then
            Just NumberOther

        else if code >= 0x0001173C && code <= 0x0001173E then
            Just PunctuationOther

        else if code == 0x0001173F then
            Just SymbolOther

        else if
            (code >= 0x00011740 && code <= 0x00011746)
                || (code >= 0x00011800 && code <= 0x0001182B)
        then
            Just LetterOther

        else if code >= 0x0001182C && code <= 0x0001182E then
            Just MarkSpacingCombining

        else if code >= 0x0001182F && code <= 0x00011837 then
            Just MarkNonSpacing

        else
            Nothing

    else if code < 0x0001D49B then
        if code < 0x00011EF6 then
            if code < 0x00011A96 then
                if code < 0x000119A9 then
                    if code < 0x0001192F then
                        if code == 0x00011838 then
                            Just MarkSpacingCombining

                        else if code >= 0x00011839 && code <= 0x0001183A then
                            Just MarkNonSpacing

                        else if code == 0x0001183B then
                            Just PunctuationOther

                        else if code >= 0x000118A0 && code <= 0x000118BF then
                            Just LetterUppercase

                        else if code >= 0x000118C0 && code <= 0x000118DF then
                            Just LetterLowercase

                        else if code >= 0x000118E0 && code <= 0x000118E9 then
                            Just NumberDecimalDigit

                        else if code >= 0x000118EA && code <= 0x000118F2 then
                            Just NumberOther

                        else if
                            (code >= 0x000118FF && code <= 0x00011906)
                                || (code == 0x00011909)
                                || (code >= 0x0001190C && code <= 0x00011913)
                                || (code >= 0x00011915 && code <= 0x00011916)
                                || (code >= 0x00011918 && code <= 0x0001192E)
                        then
                            Just LetterOther

                        else
                            Nothing

                    else if code < 0x0001193F then
                        if code == 0x0001192F then
                            Just LetterOther

                        else if
                            (code >= 0x00011930 && code <= 0x00011935)
                                || (code >= 0x00011937 && code <= 0x00011938)
                                || (code == 0x0001193D)
                        then
                            Just MarkSpacingCombining

                        else if
                            (code >= 0x0001193B && code <= 0x0001193C)
                                || (code == 0x0001193E)
                        then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if
                        (code == 0x0001193F)
                            || (code == 0x00011941)
                            || (code >= 0x000119A0 && code <= 0x000119A7)
                    then
                        Just LetterOther

                    else if (code == 0x00011940) || (code == 0x00011942) then
                        Just MarkSpacingCombining

                    else if code == 0x00011943 then
                        Just MarkNonSpacing

                    else if code >= 0x00011944 && code <= 0x00011946 then
                        Just PunctuationOther

                    else if code >= 0x00011950 && code <= 0x00011959 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code < 0x00011A0A then
                    if
                        (code >= 0x000119AA && code <= 0x000119D0)
                            || (code == 0x000119E1)
                            || (code == 0x000119E3)
                            || (code == 0x00011A00)
                    then
                        Just LetterOther

                    else if
                        (code >= 0x000119D1 && code <= 0x000119D3)
                            || (code >= 0x000119DC && code <= 0x000119DF)
                            || (code == 0x000119E4)
                    then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x000119D4 && code <= 0x000119D7)
                            || (code >= 0x000119DA && code <= 0x000119DB)
                            || (code == 0x000119E0)
                            || (code >= 0x00011A01 && code <= 0x00011A09)
                    then
                        Just MarkNonSpacing

                    else if code == 0x000119E2 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x00011A46 then
                    if
                        (code == 0x00011A0A)
                            || (code >= 0x00011A33 && code <= 0x00011A38)
                            || (code >= 0x00011A3B && code <= 0x00011A3E)
                    then
                        Just MarkNonSpacing

                    else if
                        (code >= 0x00011A0B && code <= 0x00011A32)
                            || (code == 0x00011A3A)
                    then
                        Just LetterOther

                    else if code == 0x00011A39 then
                        Just MarkSpacingCombining

                    else if code >= 0x00011A3F && code <= 0x00011A45 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code == 0x00011A46 then
                    Just PunctuationOther

                else if
                    (code == 0x00011A47)
                        || (code >= 0x00011A51 && code <= 0x00011A56)
                        || (code >= 0x00011A59 && code <= 0x00011A5B)
                        || (code >= 0x00011A8A && code <= 0x00011A95)
                then
                    Just MarkNonSpacing

                else if
                    (code == 0x00011A50)
                        || (code >= 0x00011A5C && code <= 0x00011A89)
                then
                    Just LetterOther

                else if code >= 0x00011A57 && code <= 0x00011A58 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x00011CB3 then
                if code < 0x00011C3D then
                    if
                        (code == 0x00011A96)
                            || (code >= 0x00011A98 && code <= 0x00011A99)
                            || (code >= 0x00011C30 && code <= 0x00011C36)
                            || (code >= 0x00011C38 && code <= 0x00011C3C)
                    then
                        Just MarkNonSpacing

                    else if (code == 0x00011A97) || (code == 0x00011C2F) then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x00011A9A && code <= 0x00011A9C)
                            || (code >= 0x00011A9E && code <= 0x00011AA2)
                    then
                        Just PunctuationOther

                    else if
                        (code == 0x00011A9D)
                            || (code >= 0x00011AB0 && code <= 0x00011AF8)
                            || (code >= 0x00011C00 && code <= 0x00011C08)
                            || (code >= 0x00011C0A && code <= 0x00011C2E)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x00011C6F then
                    if (code == 0x00011C3D) || (code == 0x00011C3F) then
                        Just MarkNonSpacing

                    else if code == 0x00011C3E then
                        Just MarkSpacingCombining

                    else if code == 0x00011C40 then
                        Just LetterOther

                    else if code >= 0x00011C41 && code <= 0x00011C45 then
                        Just PunctuationOther

                    else if code >= 0x00011C50 && code <= 0x00011C59 then
                        Just NumberDecimalDigit

                    else if code >= 0x00011C5A && code <= 0x00011C6C then
                        Just NumberOther

                    else
                        Nothing

                else if code >= 0x00011C70 && code <= 0x00011C71 then
                    Just PunctuationOther

                else if code >= 0x00011C72 && code <= 0x00011C8F then
                    Just LetterOther

                else if
                    (code >= 0x00011C92 && code <= 0x00011CA7)
                        || (code >= 0x00011CAA && code <= 0x00011CB0)
                        || (code == 0x00011CB2)
                then
                    Just MarkNonSpacing

                else if (code == 0x00011CA9) || (code == 0x00011CB1) then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x00011D5F then
                if code < 0x00011D30 then
                    if
                        (code == 0x00011CB3)
                            || (code >= 0x00011CB5 && code <= 0x00011CB6)
                    then
                        Just MarkNonSpacing

                    else if code == 0x00011CB4 then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x00011D00 && code <= 0x00011D06)
                            || (code >= 0x00011D08 && code <= 0x00011D09)
                            || (code >= 0x00011D0B && code <= 0x00011D2F)
                    then
                        Just LetterOther

                    else
                        Nothing

                else if (code == 0x00011D30) || (code == 0x00011D46) then
                    Just LetterOther

                else if
                    (code >= 0x00011D31 && code <= 0x00011D36)
                        || (code == 0x00011D3A)
                        || (code >= 0x00011D3C && code <= 0x00011D3D)
                        || (code >= 0x00011D3F && code <= 0x00011D45)
                        || (code == 0x00011D47)
                then
                    Just MarkNonSpacing

                else if code >= 0x00011D50 && code <= 0x00011D59 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x00011D95 then
                if
                    (code >= 0x00011D60 && code <= 0x00011D65)
                        || (code >= 0x00011D67 && code <= 0x00011D68)
                        || (code >= 0x00011D6A && code <= 0x00011D89)
                then
                    Just LetterOther

                else if
                    (code >= 0x00011D8A && code <= 0x00011D8E)
                        || (code >= 0x00011D93 && code <= 0x00011D94)
                then
                    Just MarkSpacingCombining

                else if code >= 0x00011D90 && code <= 0x00011D91 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if
                (code == 0x00011D95)
                    || (code == 0x00011D97)
                    || (code >= 0x00011EF3 && code <= 0x00011EF4)
            then
                Just MarkNonSpacing

            else if (code == 0x00011D96) || (code == 0x00011EF5) then
                Just MarkSpacingCombining

            else if
                (code == 0x00011D98)
                    || (code >= 0x00011EE0 && code <= 0x00011EF2)
            then
                Just LetterOther

            else if code >= 0x00011DA0 && code <= 0x00011DA9 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code < 0x00016FEF then
            if code < 0x00016AF4 then
                if code < 0x00012F8F then
                    if code == 0x00011EF6 then
                        Just MarkSpacingCombining

                    else if
                        (code >= 0x00011EF7 && code <= 0x00011EF8)
                            || (code == 0x00011FFF)
                            || (code >= 0x00012470 && code <= 0x00012474)
                    then
                        Just PunctuationOther

                    else if
                        (code == 0x00011FB0)
                            || (code >= 0x00012000 && code <= 0x00012399)
                            || (code >= 0x00012480 && code <= 0x00012543)
                    then
                        Just LetterOther

                    else if code >= 0x00011FC0 && code <= 0x00011FD4 then
                        Just NumberOther

                    else if
                        (code >= 0x00011FD5 && code <= 0x00011FDC)
                            || (code >= 0x00011FE1 && code <= 0x00011FF1)
                    then
                        Just SymbolOther

                    else if code >= 0x00011FDD && code <= 0x00011FE0 then
                        Just SymbolCurrency

                    else if code >= 0x00012400 && code <= 0x0001246E then
                        Just NumberLetter

                    else
                        Nothing

                else if code < 0x00016A3F then
                    if
                        (code >= 0x00012F90 && code <= 0x00012FF0)
                            || (code >= 0x00013000 && code <= 0x0001342E)
                            || (code >= 0x00014400 && code <= 0x00014646)
                            || (code >= 0x00016800 && code <= 0x00016A38)
                    then
                        Just LetterOther

                    else if code >= 0x00012FF1 && code <= 0x00012FF2 then
                        Just PunctuationOther

                    else if code >= 0x00013430 && code <= 0x00013438 then
                        Just OtherFormat

                    else
                        Nothing

                else if
                    (code >= 0x00016A40 && code <= 0x00016A5E)
                        || (code >= 0x00016A70 && code <= 0x00016ABE)
                        || (code >= 0x00016AD0 && code <= 0x00016AED)
                then
                    Just LetterOther

                else if
                    (code >= 0x00016A60 && code <= 0x00016A69)
                        || (code >= 0x00016AC0 && code <= 0x00016AC9)
                then
                    Just NumberDecimalDigit

                else if code >= 0x00016A6E && code <= 0x00016A6F then
                    Just PunctuationOther

                else if code >= 0x00016AF0 && code <= 0x00016AF3 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x00016E3F then
                if code < 0x00016B3F then
                    if
                        (code == 0x00016AF4)
                            || (code >= 0x00016B30 && code <= 0x00016B36)
                    then
                        Just MarkNonSpacing

                    else if
                        (code == 0x00016AF5)
                            || (code >= 0x00016B37 && code <= 0x00016B3B)
                    then
                        Just PunctuationOther

                    else if code >= 0x00016B00 && code <= 0x00016B2F then
                        Just LetterOther

                    else if code >= 0x00016B3C && code <= 0x00016B3E then
                        Just SymbolOther

                    else
                        Nothing

                else if (code == 0x00016B3F) || (code == 0x00016B45) then
                    Just SymbolOther

                else if code >= 0x00016B40 && code <= 0x00016B43 then
                    Just LetterModifier

                else if code == 0x00016B44 then
                    Just PunctuationOther

                else if code >= 0x00016B50 && code <= 0x00016B59 then
                    Just NumberDecimalDigit

                else if code >= 0x00016B5B && code <= 0x00016B61 then
                    Just NumberOther

                else if
                    (code >= 0x00016B63 && code <= 0x00016B77)
                        || (code >= 0x00016B7D && code <= 0x00016B8F)
                then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x00016F50 then
                if code >= 0x00016E40 && code <= 0x00016E5F then
                    Just LetterUppercase

                else if code >= 0x00016E60 && code <= 0x00016E7F then
                    Just LetterLowercase

                else if code >= 0x00016E80 && code <= 0x00016E96 then
                    Just NumberOther

                else if code >= 0x00016E97 && code <= 0x00016E9A then
                    Just PunctuationOther

                else if code >= 0x00016F00 && code <= 0x00016F4A then
                    Just LetterOther

                else if code == 0x00016F4F then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code == 0x00016F50 then
                Just LetterOther

            else if code >= 0x00016F51 && code <= 0x00016F87 then
                Just MarkSpacingCombining

            else if
                (code >= 0x00016F8F && code <= 0x00016F92)
                    || (code == 0x00016FE4)
            then
                Just MarkNonSpacing

            else if
                (code >= 0x00016F93 && code <= 0x00016F9F)
                    || (code >= 0x00016FE0 && code <= 0x00016FE1)
                    || (code == 0x00016FE3)
            then
                Just LetterModifier

            else if code == 0x00016FE2 then
                Just PunctuationOther

            else
                Nothing

        else if code < 0x0001D0FF then
            if code < 0x0001B16F then
                if code >= 0x00016FF0 && code <= 0x00016FF1 then
                    Just MarkSpacingCombining

                else if
                    (code == 0x00017000)
                        || (code == 0x000187F7)
                        || (code >= 0x00018800 && code <= 0x00018CD5)
                        || (code == 0x00018D00)
                        || (code == 0x00018D08)
                        || (code >= 0x0001B000 && code <= 0x0001B122)
                        || (code >= 0x0001B150 && code <= 0x0001B152)
                        || (code >= 0x0001B164 && code <= 0x0001B167)
                then
                    Just LetterOther

                else if
                    (code >= 0x0001AFF0 && code <= 0x0001AFF3)
                        || (code >= 0x0001AFF5 && code <= 0x0001AFFB)
                        || (code >= 0x0001AFFD && code <= 0x0001AFFE)
                then
                    Just LetterModifier

                else
                    Nothing

            else if code < 0x0001BC9C then
                if
                    (code >= 0x0001B170 && code <= 0x0001B2FB)
                        || (code >= 0x0001BC00 && code <= 0x0001BC6A)
                        || (code >= 0x0001BC70 && code <= 0x0001BC7C)
                        || (code >= 0x0001BC80 && code <= 0x0001BC88)
                        || (code >= 0x0001BC90 && code <= 0x0001BC99)
                then
                    Just LetterOther

                else
                    Nothing

            else if
                (code == 0x0001BC9C)
                    || (code >= 0x0001CF50 && code <= 0x0001CFC3)
                    || (code >= 0x0001D000 && code <= 0x0001D0F5)
            then
                Just SymbolOther

            else if
                (code >= 0x0001BC9D && code <= 0x0001BC9E)
                    || (code >= 0x0001CF00 && code <= 0x0001CF2D)
                    || (code >= 0x0001CF30 && code <= 0x0001CF46)
            then
                Just MarkNonSpacing

            else if code == 0x0001BC9F then
                Just PunctuationOther

            else if code >= 0x0001BCA0 && code <= 0x0001BCA3 then
                Just OtherFormat

            else
                Nothing

        else if code < 0x0001D1FF then
            if code < 0x0001D172 then
                if
                    (code >= 0x0001D100 && code <= 0x0001D126)
                        || (code >= 0x0001D129 && code <= 0x0001D164)
                        || (code >= 0x0001D16A && code <= 0x0001D16C)
                then
                    Just SymbolOther

                else if
                    (code >= 0x0001D165 && code <= 0x0001D166)
                        || (code >= 0x0001D16D && code <= 0x0001D171)
                then
                    Just MarkSpacingCombining

                else if code >= 0x0001D167 && code <= 0x0001D169 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code == 0x0001D172 then
                Just MarkSpacingCombining

            else if code >= 0x0001D173 && code <= 0x0001D17A then
                Just OtherFormat

            else if
                (code >= 0x0001D17B && code <= 0x0001D182)
                    || (code >= 0x0001D185 && code <= 0x0001D18B)
                    || (code >= 0x0001D1AA && code <= 0x0001D1AD)
            then
                Just MarkNonSpacing

            else if
                (code >= 0x0001D183 && code <= 0x0001D184)
                    || (code >= 0x0001D18C && code <= 0x0001D1A9)
                    || (code >= 0x0001D1AE && code <= 0x0001D1EA)
            then
                Just SymbolOther

            else
                Nothing

        else if code < 0x0001D3FF then
            if
                (code >= 0x0001D200 && code <= 0x0001D241)
                    || (code == 0x0001D245)
                    || (code >= 0x0001D300 && code <= 0x0001D356)
            then
                Just SymbolOther

            else if code >= 0x0001D242 && code <= 0x0001D244 then
                Just MarkNonSpacing

            else if
                (code >= 0x0001D2E0 && code <= 0x0001D2F3)
                    || (code >= 0x0001D360 && code <= 0x0001D378)
            then
                Just NumberOther

            else
                Nothing

        else if
            (code >= 0x0001D400 && code <= 0x0001D419)
                || (code >= 0x0001D434 && code <= 0x0001D44D)
                || (code >= 0x0001D468 && code <= 0x0001D481)
        then
            Just LetterUppercase

        else if
            (code >= 0x0001D41A && code <= 0x0001D433)
                || (code >= 0x0001D44E && code <= 0x0001D454)
                || (code >= 0x0001D456 && code <= 0x0001D467)
                || (code >= 0x0001D482 && code <= 0x0001D49A)
        then
            Just LetterLowercase

        else
            Nothing

    else if code < 0x0001E8C6 then
        if code < 0x0001D74F then
            if code < 0x0001D56B then
                if code < 0x0001D4E9 then
                    if
                        (code == 0x0001D49B)
                            || (code >= 0x0001D4B6 && code <= 0x0001D4B9)
                            || (code == 0x0001D4BB)
                            || (code >= 0x0001D4BD && code <= 0x0001D4C3)
                            || (code >= 0x0001D4C5 && code <= 0x0001D4CF)
                    then
                        Just LetterLowercase

                    else if
                        (code == 0x0001D49C)
                            || (code >= 0x0001D49E && code <= 0x0001D49F)
                            || (code == 0x0001D4A2)
                            || (code >= 0x0001D4A5 && code <= 0x0001D4A6)
                            || (code >= 0x0001D4A9 && code <= 0x0001D4AC)
                            || (code >= 0x0001D4AE && code <= 0x0001D4B5)
                            || (code >= 0x0001D4D0 && code <= 0x0001D4E8)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x0001D51D then
                    if
                        (code == 0x0001D4E9)
                            || (code >= 0x0001D504 && code <= 0x0001D505)
                            || (code >= 0x0001D507 && code <= 0x0001D50A)
                            || (code >= 0x0001D50D && code <= 0x0001D514)
                            || (code >= 0x0001D516 && code <= 0x0001D51C)
                    then
                        Just LetterUppercase

                    else if code >= 0x0001D4EA && code <= 0x0001D503 then
                        Just LetterLowercase

                    else
                        Nothing

                else if
                    (code >= 0x0001D51E && code <= 0x0001D537)
                        || (code >= 0x0001D552 && code <= 0x0001D56A)
                then
                    Just LetterLowercase

                else if
                    (code >= 0x0001D538 && code <= 0x0001D539)
                        || (code >= 0x0001D53B && code <= 0x0001D53E)
                        || (code >= 0x0001D540 && code <= 0x0001D544)
                        || (code == 0x0001D546)
                        || (code >= 0x0001D54A && code <= 0x0001D550)
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x0001D6A7 then
                if code < 0x0001D5ED then
                    if
                        (code == 0x0001D56B)
                            || (code >= 0x0001D586 && code <= 0x0001D59F)
                            || (code >= 0x0001D5BA && code <= 0x0001D5D3)
                    then
                        Just LetterLowercase

                    else if
                        (code >= 0x0001D56C && code <= 0x0001D585)
                            || (code >= 0x0001D5A0 && code <= 0x0001D5B9)
                            || (code >= 0x0001D5D4 && code <= 0x0001D5EC)
                    then
                        Just LetterUppercase

                    else
                        Nothing

                else if
                    (code == 0x0001D5ED)
                        || (code >= 0x0001D608 && code <= 0x0001D621)
                        || (code >= 0x0001D63C && code <= 0x0001D655)
                        || (code >= 0x0001D670 && code <= 0x0001D689)
                then
                    Just LetterUppercase

                else if
                    (code >= 0x0001D5EE && code <= 0x0001D607)
                        || (code >= 0x0001D622 && code <= 0x0001D63B)
                        || (code >= 0x0001D656 && code <= 0x0001D66F)
                        || (code >= 0x0001D68A && code <= 0x0001D6A5)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x0001D6FA then
                if
                    (code >= 0x0001D6A8 && code <= 0x0001D6C0)
                        || (code >= 0x0001D6E2 && code <= 0x0001D6F9)
                then
                    Just LetterUppercase

                else if (code == 0x0001D6C1) || (code == 0x0001D6DB) then
                    Just SymbolMath

                else if
                    (code >= 0x0001D6C2 && code <= 0x0001D6DA)
                        || (code >= 0x0001D6DC && code <= 0x0001D6E1)
                then
                    Just LetterLowercase

                else
                    Nothing

            else if
                (code == 0x0001D6FA)
                    || (code >= 0x0001D71C && code <= 0x0001D734)
            then
                Just LetterUppercase

            else if
                (code == 0x0001D6FB)
                    || (code == 0x0001D715)
                    || (code == 0x0001D735)
            then
                Just SymbolMath

            else if
                (code >= 0x0001D6FC && code <= 0x0001D714)
                    || (code >= 0x0001D716 && code <= 0x0001D71B)
                    || (code >= 0x0001D736 && code <= 0x0001D74E)
            then
                Just LetterLowercase

            else
                Nothing

        else if code < 0x0001DAA0 then
            if code < 0x0001D7CA then
                if
                    (code == 0x0001D74F)
                        || (code == 0x0001D76F)
                        || (code == 0x0001D789)
                        || (code == 0x0001D7A9)
                        || (code == 0x0001D7C3)
                then
                    Just SymbolMath

                else if
                    (code >= 0x0001D750 && code <= 0x0001D755)
                        || (code >= 0x0001D770 && code <= 0x0001D788)
                        || (code >= 0x0001D78A && code <= 0x0001D78F)
                        || (code >= 0x0001D7AA && code <= 0x0001D7C2)
                        || (code >= 0x0001D7C4 && code <= 0x0001D7C9)
                then
                    Just LetterLowercase

                else if
                    (code >= 0x0001D756 && code <= 0x0001D76E)
                        || (code >= 0x0001D790 && code <= 0x0001D7A8)
                then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x0001DA6C then
                if code == 0x0001D7CA then
                    Just LetterUppercase

                else if code == 0x0001D7CB then
                    Just LetterLowercase

                else if code >= 0x0001D7CE && code <= 0x0001D7FF then
                    Just NumberDecimalDigit

                else if
                    (code >= 0x0001D800 && code <= 0x0001D9FF)
                        || (code >= 0x0001DA37 && code <= 0x0001DA3A)
                then
                    Just SymbolOther

                else if
                    (code >= 0x0001DA00 && code <= 0x0001DA36)
                        || (code >= 0x0001DA3B && code <= 0x0001DA6B)
                then
                    Just MarkNonSpacing

                else
                    Nothing

            else if
                (code == 0x0001DA6C)
                    || (code == 0x0001DA75)
                    || (code == 0x0001DA84)
                    || (code >= 0x0001DA9B && code <= 0x0001DA9F)
            then
                Just MarkNonSpacing

            else if
                (code >= 0x0001DA6D && code <= 0x0001DA74)
                    || (code >= 0x0001DA76 && code <= 0x0001DA83)
                    || (code >= 0x0001DA85 && code <= 0x0001DA86)
            then
                Just SymbolOther

            else if code >= 0x0001DA87 && code <= 0x0001DA8B then
                Just PunctuationOther

            else
                Nothing

        else if code < 0x0001E14D then
            if code < 0x0001E01A then
                if
                    (code >= 0x0001DAA1 && code <= 0x0001DAAF)
                        || (code >= 0x0001E000 && code <= 0x0001E006)
                        || (code >= 0x0001E008 && code <= 0x0001E018)
                then
                    Just MarkNonSpacing

                else if
                    (code >= 0x0001DF00 && code <= 0x0001DF09)
                        || (code >= 0x0001DF0B && code <= 0x0001DF1E)
                then
                    Just LetterLowercase

                else if code == 0x0001DF0A then
                    Just LetterOther

                else
                    Nothing

            else if
                (code >= 0x0001E01B && code <= 0x0001E021)
                    || (code >= 0x0001E023 && code <= 0x0001E024)
                    || (code >= 0x0001E026 && code <= 0x0001E02A)
                    || (code >= 0x0001E130 && code <= 0x0001E136)
            then
                Just MarkNonSpacing

            else if code >= 0x0001E100 && code <= 0x0001E12C then
                Just LetterOther

            else if code >= 0x0001E137 && code <= 0x0001E13D then
                Just LetterModifier

            else if code >= 0x0001E140 && code <= 0x0001E149 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code < 0x0001E2EF then
            if
                (code == 0x0001E14E)
                    || (code >= 0x0001E290 && code <= 0x0001E2AD)
                    || (code >= 0x0001E2C0 && code <= 0x0001E2EB)
            then
                Just LetterOther

            else if code == 0x0001E14F then
                Just SymbolOther

            else if
                (code == 0x0001E2AE)
                    || (code >= 0x0001E2EC && code <= 0x0001E2EE)
            then
                Just MarkNonSpacing

            else
                Nothing

        else if code == 0x0001E2EF then
            Just MarkNonSpacing

        else if code >= 0x0001E2F0 && code <= 0x0001E2F9 then
            Just NumberDecimalDigit

        else if code == 0x0001E2FF then
            Just SymbolCurrency

        else if
            (code >= 0x0001E7E0 && code <= 0x0001E7E6)
                || (code >= 0x0001E7E8 && code <= 0x0001E7EB)
                || (code >= 0x0001E7ED && code <= 0x0001E7EE)
                || (code >= 0x0001E7F0 && code <= 0x0001E7FE)
                || (code >= 0x0001E800 && code <= 0x0001E8C4)
        then
            Just LetterOther

        else
            Nothing

    else if code < 0x0001F24F then
        if code < 0x0001EE50 then
            if code < 0x0001ECB0 then
                if
                    (code >= 0x0001E8C7 && code <= 0x0001E8CF)
                        || (code >= 0x0001EC71 && code <= 0x0001ECAB)
                        || (code >= 0x0001ECAD && code <= 0x0001ECAF)
                then
                    Just NumberOther

                else if
                    (code >= 0x0001E8D0 && code <= 0x0001E8D6)
                        || (code >= 0x0001E944 && code <= 0x0001E94A)
                then
                    Just MarkNonSpacing

                else if code >= 0x0001E900 && code <= 0x0001E921 then
                    Just LetterUppercase

                else if code >= 0x0001E922 && code <= 0x0001E943 then
                    Just LetterLowercase

                else if code == 0x0001E94B then
                    Just LetterModifier

                else if code >= 0x0001E950 && code <= 0x0001E959 then
                    Just NumberDecimalDigit

                else if code >= 0x0001E95E && code <= 0x0001E95F then
                    Just PunctuationOther

                else if code == 0x0001ECAC then
                    Just SymbolOther

                else
                    Nothing

            else if code < 0x0001EE20 then
                if code == 0x0001ECB0 then
                    Just SymbolCurrency

                else if
                    (code >= 0x0001ECB1 && code <= 0x0001ECB4)
                        || (code >= 0x0001ED01 && code <= 0x0001ED2D)
                        || (code >= 0x0001ED2F && code <= 0x0001ED3D)
                then
                    Just NumberOther

                else if code == 0x0001ED2E then
                    Just SymbolOther

                else if
                    (code >= 0x0001EE00 && code <= 0x0001EE03)
                        || (code >= 0x0001EE05 && code <= 0x0001EE1F)
                then
                    Just LetterOther

                else
                    Nothing

            else if
                (code >= 0x0001EE21 && code <= 0x0001EE22)
                    || (code == 0x0001EE24)
                    || (code == 0x0001EE27)
                    || (code >= 0x0001EE29 && code <= 0x0001EE32)
                    || (code >= 0x0001EE34 && code <= 0x0001EE37)
                    || (code == 0x0001EE42)
                    || (code >= 0x0001EE4D && code <= 0x0001EE4F)
                    || ((modBy 2 code == 1)
                            && ((code >= 0x0001EE39 && code <= 0x0001EE3B)
                                    || (code >= 0x0001EE47 && code <= 0x0001EE4B)
                               )
                       )
            then
                Just LetterOther

            else
                Nothing

        else if code < 0x0001EEAA then
            if code < 0x0001EE73 then
                if
                    (code >= 0x0001EE51 && code <= 0x0001EE52)
                        || (code == 0x0001EE54)
                        || (code >= 0x0001EE61 && code <= 0x0001EE62)
                        || (code == 0x0001EE64)
                        || (code >= 0x0001EE67 && code <= 0x0001EE6A)
                        || (code >= 0x0001EE6C && code <= 0x0001EE72)
                        || ((modBy 2 code == 1)
                                && (code >= 0x0001EE57 && code <= 0x0001EE5F)
                           )
                then
                    Just LetterOther

                else
                    Nothing

            else if
                (code >= 0x0001EE74 && code <= 0x0001EE77)
                    || (code >= 0x0001EE79 && code <= 0x0001EE7C)
                    || (code == 0x0001EE7E)
                    || (code >= 0x0001EE80 && code <= 0x0001EE89)
                    || (code >= 0x0001EE8B && code <= 0x0001EE9B)
                    || (code >= 0x0001EEA1 && code <= 0x0001EEA3)
                    || (code >= 0x0001EEA5 && code <= 0x0001EEA9)
            then
                Just LetterOther

            else
                Nothing

        else if code < 0x0001F0C0 then
            if code >= 0x0001EEAB && code <= 0x0001EEBB then
                Just LetterOther

            else if code >= 0x0001EEF0 && code <= 0x0001EEF1 then
                Just SymbolMath

            else if
                (code >= 0x0001F000 && code <= 0x0001F02B)
                    || (code >= 0x0001F030 && code <= 0x0001F093)
                    || (code >= 0x0001F0A0 && code <= 0x0001F0AE)
                    || (code >= 0x0001F0B1 && code <= 0x0001F0BF)
            then
                Just SymbolOther

            else
                Nothing

        else if
            (code >= 0x0001F0C1 && code <= 0x0001F0CF)
                || (code >= 0x0001F0D1 && code <= 0x0001F0F5)
                || (code >= 0x0001F10D && code <= 0x0001F1AD)
                || (code >= 0x0001F1E6 && code <= 0x0001F202)
                || (code >= 0x0001F210 && code <= 0x0001F23B)
                || (code >= 0x0001F240 && code <= 0x0001F248)
        then
            Just SymbolOther

        else if code >= 0x0001F100 && code <= 0x0001F10C then
            Just NumberOther

        else
            Nothing

    else if code < 0x0001FACF then
        if code < 0x0001F80F then
            if
                (code >= 0x0001F250 && code <= 0x0001F251)
                    || (code >= 0x0001F260 && code <= 0x0001F265)
                    || (code >= 0x0001F300 && code <= 0x0001F3FA)
                    || (code >= 0x0001F400 && code <= 0x0001F6D7)
                    || (code >= 0x0001F6DD && code <= 0x0001F6EC)
                    || (code >= 0x0001F6F0 && code <= 0x0001F6FC)
                    || (code >= 0x0001F700 && code <= 0x0001F773)
                    || (code >= 0x0001F780 && code <= 0x0001F7D8)
                    || (code >= 0x0001F7E0 && code <= 0x0001F7EB)
                    || (code == 0x0001F7F0)
                    || (code >= 0x0001F800 && code <= 0x0001F80B)
            then
                Just SymbolOther

            else if code >= 0x0001F3FB && code <= 0x0001F3FF then
                Just SymbolModifier

            else
                Nothing

        else if code < 0x0001FA5F then
            if
                (code >= 0x0001F810 && code <= 0x0001F847)
                    || (code >= 0x0001F850 && code <= 0x0001F859)
                    || (code >= 0x0001F860 && code <= 0x0001F887)
                    || (code >= 0x0001F890 && code <= 0x0001F8AD)
                    || (code >= 0x0001F8B0 && code <= 0x0001F8B1)
                    || (code >= 0x0001F900 && code <= 0x0001FA53)
            then
                Just SymbolOther

            else
                Nothing

        else if
            (code >= 0x0001FA60 && code <= 0x0001FA6D)
                || (code >= 0x0001FA70 && code <= 0x0001FA74)
                || (code >= 0x0001FA78 && code <= 0x0001FA7C)
                || (code >= 0x0001FA80 && code <= 0x0001FA86)
                || (code >= 0x0001FA90 && code <= 0x0001FAAC)
                || (code >= 0x0001FAB0 && code <= 0x0001FABA)
                || (code >= 0x0001FAC0 && code <= 0x0001FAC5)
        then
            Just SymbolOther

        else
            Nothing

    else if code < 0x0002CEA0 then
        if code < 0x0001FFFF then
            if
                (code >= 0x0001FAD0 && code <= 0x0001FAD9)
                    || (code >= 0x0001FAE0 && code <= 0x0001FAE7)
                    || (code >= 0x0001FAF0 && code <= 0x0001FAF6)
                    || (code >= 0x0001FB00 && code <= 0x0001FB92)
                    || (code >= 0x0001FB94 && code <= 0x0001FBCA)
            then
                Just SymbolOther

            else if code >= 0x0001FBF0 && code <= 0x0001FBF9 then
                Just NumberDecimalDigit

            else
                Nothing

        else if
            (code == 0x00020000)
                || (code == 0x0002A6DF)
                || (code == 0x0002A700)
                || (code == 0x0002B738)
                || (code == 0x0002B740)
                || (code == 0x0002B81D)
                || (code == 0x0002B820)
        then
            Just LetterOther

        else
            Nothing

    else if code < 0x000E0000 then
        if
            (code == 0x0002CEA1)
                || (code == 0x0002CEB0)
                || (code == 0x0002EBE0)
                || (code >= 0x0002F800 && code <= 0x0002FA1D)
                || (code == 0x00030000)
                || (code == 0x0003134A)
        then
            Just LetterOther

        else
            Nothing

    else if (code == 0x000E0001) || (code >= 0x000E0020 && code <= 0x000E007F) then
        Just OtherFormat

    else if code >= 0x000E0100 && code <= 0x000E01EF then
        Just MarkNonSpacing

    else if
        (code == 0x000F0000)
            || (code == 0x000FFFFD)
            || (code == 0x00100000)
            || (code == 0x0010FFFD)
    then
        Just OtherPrivateUse

    else
        Nothing


{-| Parses a category name (Lu, Ll, Lt, ...).
-}
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


{-| Convert a category to its short category name (Lu, Ll, Lt, ...).
-}
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


{-| Converts a category to its English description. Mostly useful for debugging purposes.
-}
categoryToDescription : Category -> String
categoryToDescription generalCategory =
    case generalCategory of
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
