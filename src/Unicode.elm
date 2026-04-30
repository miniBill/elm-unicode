module Unicode exposing
    ( isUpper, isLower, isAlpha, isAlphaNum
    , isDigit
    , Category(..), getCategory, categoryFromString, categoryToString, categoryToDescription
    , isSpace, isSeparator
    )

{-| Unicode aware functions for working with characters.


## Letters

@docs isUpper, isLower, isAlpha, isAlphaNum


## Digits

@docs isDigit


## Categories

@docs Category, getCategory, categoryFromString, categoryToString, categoryToDescription


## Separators

@docs isSpace, isSeparator

-}


{-| Detect upper case characters (Unicode category Lu)
-}
isUpper : Char -> Bool
isUpper c =
    let
        code =
            Char.toCode c

        simple =
            Char.toUpper c == c && Char.toLower c /= c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if simple then
        code <= 0x215F || 0x2170 <= code && code <= 0x24B5 || 0x24D0 <= code && code <= 0x000F0000

    else if code < 0x0001D4AD then
        if code < 0x213D then
            0x03D2 <= code && code <= 0x03D4 || code == 0x2102 || code == 0x2107 || 0x210B <= code && code <= 0x210D || 0x2110 <= code && code <= 0x2112 || code == 0x2115 || 0x2119 <= code && code <= 0x211D || code == 0x2124 || code == 0x2128 || 0x212A <= code && code <= 0x212D || 0x2130 <= code && code <= 0x2133

        else
            0x213E <= code && code <= 0x213F || code == 0x2145 || code == 0xA7CE || 0x00016EA0 <= code && code <= 0x00016EB8 || 0x0001D400 <= code && code <= 0x0001D419 || 0x0001D434 <= code && code <= 0x0001D44D || 0x0001D468 <= code && code <= 0x0001D481 || code == 0x0001D49C || 0x0001D49E <= code && code <= 0x0001D49F || code == 0x0001D4A2 || 0x0001D4A5 <= code && code <= 0x0001D4A6 || 0x0001D4A9 <= code && code <= 0x0001D4AC || modBy 2 code == 0 && 0xA7D2 <= code && code <= 0xA7D4

    else if code < 0x0001D56B then
        0x0001D4AE <= code && code <= 0x0001D4B5 || 0x0001D4D0 <= code && code <= 0x0001D4E9 || 0x0001D504 <= code && code <= 0x0001D505 || 0x0001D507 <= code && code <= 0x0001D50A || 0x0001D50D <= code && code <= 0x0001D514 || 0x0001D516 <= code && code <= 0x0001D51C || 0x0001D538 <= code && code <= 0x0001D539 || 0x0001D53B <= code && code <= 0x0001D53E || 0x0001D540 <= code && code <= 0x0001D544 || code == 0x0001D546 || 0x0001D54A <= code && code <= 0x0001D550

    else
        0x0001D56C <= code && code <= 0x0001D585 || 0x0001D5A0 <= code && code <= 0x0001D5B9 || 0x0001D5D4 <= code && code <= 0x0001D5ED || 0x0001D608 <= code && code <= 0x0001D621 || 0x0001D63C <= code && code <= 0x0001D655 || 0x0001D670 <= code && code <= 0x0001D689 || 0x0001D6A8 <= code && code <= 0x0001D6C0 || 0x0001D6E2 <= code && code <= 0x0001D6FA || 0x0001D71C <= code && code <= 0x0001D734 || 0x0001D756 <= code && code <= 0x0001D76E || 0x0001D790 <= code && code <= 0x0001D7A8 || code == 0x0001D7CA


{-| Detect lower case characters (Unicode category Ll)
-}
isLower : Char -> Bool
isLower c =
    let
        code =
            Char.toCode c

        simple =
            Char.toLower c == c && Char.toUpper c /= c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if simple then
        code <= 0x0344 || 0x0346 <= code && code <= 0x216F || 0x2180 <= code && code <= 0x24CF || 0x24EA <= code && code <= 0x000F0000

    else if code < 0xA7F9 then
        if code < 0x210D then
            if code < 0x0295 then
                0x0137 <= code && code <= 0x0138 || 0x018C <= code && code <= 0x018D || 0x01AA <= code && code <= 0x01AB || 0x01B9 <= code && code <= 0x01BA || 0x01BD <= code && code <= 0x01BF || code == 0x0221 || 0x0233 <= code && code <= 0x0239 || 0x024F <= code && code <= 0x0293

            else
                0x0296 <= code && code <= 0x02AF || 0x03FB <= code && code <= 0x03FC || 0x0560 <= code && code <= 0x0588 || 0x1D00 <= code && code <= 0x1D2B || 0x1D6B <= code && code <= 0x1D77 || 0x1D79 <= code && code <= 0x1D9A || 0x1E95 <= code && code <= 0x1E9D || code == 0x1E9F || code == 0x210A

        else if code < 0x2C72 then
            0x210E <= code && code <= 0x210F || code == 0x2113 || code == 0x212F || code == 0x2134 || code == 0x2139 || 0x213C <= code && code <= 0x213D || 0x2146 <= code && code <= 0x2149 || code == 0x2C71

        else
            0x2C73 <= code && code <= 0x2C74 || 0x2C76 <= code && code <= 0x2C7B || 0x2CE3 <= code && code <= 0x2CE4 || 0xA72F <= code && code <= 0xA731 || 0xA771 <= code && code <= 0xA778 || code == 0xA78E || 0xA793 <= code && code <= 0xA795 || code == 0xA7AF || code == 0xA7CF || modBy 2 code == 1 && 0xA7D3 <= code && code <= 0xA7D5

    else if code < 0x0001D5ED then
        if code < 0x0001D4B5 then
            code == 0xA7FA || 0xAB30 <= code && code <= 0xAB5A || 0xAB60 <= code && code <= 0xAB68 || 0x00016EBB <= code && code <= 0x00016ED3 || 0x0001D41A <= code && code <= 0x0001D433 || 0x0001D44E <= code && code <= 0x0001D454 || 0x0001D456 <= code && code <= 0x0001D467 || 0x0001D482 <= code && code <= 0x0001D49B

        else
            0x0001D4B6 <= code && code <= 0x0001D4B9 || code == 0x0001D4BB || 0x0001D4BD <= code && code <= 0x0001D4C3 || 0x0001D4C5 <= code && code <= 0x0001D4CF || 0x0001D4EA <= code && code <= 0x0001D503 || 0x0001D51E <= code && code <= 0x0001D537 || 0x0001D552 <= code && code <= 0x0001D56B || 0x0001D586 <= code && code <= 0x0001D59F || 0x0001D5BA <= code && code <= 0x0001D5D3

    else if code < 0x0001D74F then
        0x0001D5EE <= code && code <= 0x0001D607 || 0x0001D622 <= code && code <= 0x0001D63B || 0x0001D656 <= code && code <= 0x0001D66F || 0x0001D68A <= code && code <= 0x0001D6A5 || 0x0001D6C2 <= code && code <= 0x0001D6DA || 0x0001D6DC <= code && code <= 0x0001D6E1 || 0x0001D6FC <= code && code <= 0x0001D714 || 0x0001D716 <= code && code <= 0x0001D71B || 0x0001D736 <= code && code <= 0x0001D74E

    else
        0x0001D750 <= code && code <= 0x0001D755 || 0x0001D770 <= code && code <= 0x0001D788 || 0x0001D78A <= code && code <= 0x0001D78F || 0x0001D7AA <= code && code <= 0x0001D7C2 || 0x0001D7C4 <= code && code <= 0x0001D7C9 || code == 0x0001D7CB || 0x0001DF00 <= code && code <= 0x0001DF09 || 0x0001DF0B <= code && code <= 0x0001DF1E || 0x0001DF25 <= code && code <= 0x0001DF2A


{-| Detect letters (Unicode categories Lu, Ll, Lt, Lm, Lo)
-}
isAlpha : Char -> Bool
isAlpha c =
    let
        code =
            Char.toCode c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if code < 0x0100 then
        0x41 <= code && code <= 0x5A || 0x61 <= code && code <= 0x7A || code == 0xAA || code == 0xB5 || code == 0xBA || 0xC0 <= code && code <= 0xD6 || 0xD8 <= code && code <= 0xF6 || 0xF8 <= code && code <= 0xFF

    else if code < 0xAB00 then
        if code < 0x10CF then
            if code < 0x0B0E then
                if code < 0x086F then
                    if code < 0x066D then
                        if code < 0x038D then
                            0x0100 <= code && code <= 0x02C1 || 0x02C6 <= code && code <= 0x02D1 || 0x02E0 <= code && code <= 0x02E4 || 0x0370 <= code && code <= 0x0374 || 0x0376 <= code && code <= 0x0377 || 0x037A <= code && code <= 0x037D || code == 0x037F || code == 0x0386 || 0x0388 <= code && code <= 0x038A || code == 0x038C || modBy 2 code == 0 && 0x02EC <= code && code <= 0x02EE

                        else
                            0x038E <= code && code <= 0x03A1 || 0x03A3 <= code && code <= 0x03F5 || 0x03F7 <= code && code <= 0x0481 || 0x048A <= code && code <= 0x052F || 0x0531 <= code && code <= 0x0556 || code == 0x0559 || 0x0560 <= code && code <= 0x0588 || 0x05D0 <= code && code <= 0x05EA || 0x05EF <= code && code <= 0x05F2 || 0x0620 <= code && code <= 0x064A

                    else if code < 0x07B0 then
                        0x066E <= code && code <= 0x066F || 0x0671 <= code && code <= 0x06D3 || code == 0x06D5 || 0x06E5 <= code && code <= 0x06E6 || 0x06EE <= code && code <= 0x06EF || 0x06FA <= code && code <= 0x06FC || code == 0x06FF || code == 0x0710 || 0x0712 <= code && code <= 0x072F || 0x074D <= code && code <= 0x07A5

                    else
                        code == 0x07B1 || 0x07CA <= code && code <= 0x07EA || 0x07F4 <= code && code <= 0x07F5 || code == 0x07FA || 0x0800 <= code && code <= 0x0815 || code == 0x081A || code == 0x0824 || code == 0x0828 || 0x0840 <= code && code <= 0x0858 || 0x0860 <= code && code <= 0x086A

                else if code < 0x0A04 then
                    if code < 0x0992 then
                        0x0870 <= code && code <= 0x0887 || 0x0889 <= code && code <= 0x088F || 0x08A0 <= code && code <= 0x08C9 || 0x0904 <= code && code <= 0x0939 || code == 0x093D || code == 0x0950 || 0x0958 <= code && code <= 0x0961 || 0x0971 <= code && code <= 0x0980 || 0x0985 <= code && code <= 0x098C || 0x098F <= code && code <= 0x0990

                    else
                        0x0993 <= code && code <= 0x09A8 || 0x09AA <= code && code <= 0x09B0 || code == 0x09B2 || 0x09B6 <= code && code <= 0x09B9 || code == 0x09BD || code == 0x09CE || 0x09DC <= code && code <= 0x09DD || 0x09DF <= code && code <= 0x09E1 || 0x09F0 <= code && code <= 0x09F1 || code == 0x09FC

                else if code < 0x0A84 then
                    0x0A05 <= code && code <= 0x0A0A || 0x0A0F <= code && code <= 0x0A10 || 0x0A13 <= code && code <= 0x0A28 || 0x0A2A <= code && code <= 0x0A30 || 0x0A32 <= code && code <= 0x0A33 || 0x0A35 <= code && code <= 0x0A36 || 0x0A38 <= code && code <= 0x0A39 || 0x0A59 <= code && code <= 0x0A5C || code == 0x0A5E || 0x0A72 <= code && code <= 0x0A74

                else
                    0x0A85 <= code && code <= 0x0A8D || 0x0A8F <= code && code <= 0x0A91 || 0x0A93 <= code && code <= 0x0AA8 || 0x0AAA <= code && code <= 0x0AB0 || 0x0AB2 <= code && code <= 0x0AB3 || 0x0AB5 <= code && code <= 0x0AB9 || code == 0x0ABD || code == 0x0AD0 || 0x0AE0 <= code && code <= 0x0AE1 || code == 0x0AF9 || 0x0B05 <= code && code <= 0x0B0C

            else if code < 0x0D11 then
                if code < 0x0C04 then
                    if code < 0x0B84 then
                        0x0B0F <= code && code <= 0x0B10 || 0x0B13 <= code && code <= 0x0B28 || 0x0B2A <= code && code <= 0x0B30 || 0x0B32 <= code && code <= 0x0B33 || 0x0B35 <= code && code <= 0x0B39 || code == 0x0B3D || 0x0B5C <= code && code <= 0x0B5D || 0x0B5F <= code && code <= 0x0B61 || code == 0x0B71 || code == 0x0B83

                    else
                        0x0B85 <= code && code <= 0x0B8A || 0x0B8E <= code && code <= 0x0B90 || 0x0B92 <= code && code <= 0x0B95 || 0x0B99 <= code && code <= 0x0B9A || code == 0x0B9C || 0x0B9E <= code && code <= 0x0B9F || 0x0BA3 <= code && code <= 0x0BA4 || 0x0BA8 <= code && code <= 0x0BAA || 0x0BAE <= code && code <= 0x0BB9 || code == 0x0BD0

                else if code < 0x0C8D then
                    0x0C05 <= code && code <= 0x0C0C || 0x0C0E <= code && code <= 0x0C10 || 0x0C12 <= code && code <= 0x0C28 || 0x0C2A <= code && code <= 0x0C39 || code == 0x0C3D || 0x0C58 <= code && code <= 0x0C5A || 0x0C5C <= code && code <= 0x0C5D || 0x0C60 <= code && code <= 0x0C61 || code == 0x0C80 || 0x0C85 <= code && code <= 0x0C8C

                else
                    0x0C8E <= code && code <= 0x0C90 || 0x0C92 <= code && code <= 0x0CA8 || 0x0CAA <= code && code <= 0x0CB3 || 0x0CB5 <= code && code <= 0x0CB9 || code == 0x0CBD || 0x0CDC <= code && code <= 0x0CDE || 0x0CE0 <= code && code <= 0x0CE1 || 0x0CF1 <= code && code <= 0x0CF2 || 0x0D04 <= code && code <= 0x0D0C || 0x0D0E <= code && code <= 0x0D10

            else if code < 0x0EB1 then
                if code < 0x0DBF then
                    0x0D12 <= code && code <= 0x0D3A || code == 0x0D3D || code == 0x0D4E || 0x0D54 <= code && code <= 0x0D56 || 0x0D5F <= code && code <= 0x0D61 || 0x0D7A <= code && code <= 0x0D7F || 0x0D85 <= code && code <= 0x0D96 || 0x0D9A <= code && code <= 0x0DB1 || 0x0DB3 <= code && code <= 0x0DBB || code == 0x0DBD

                else
                    0x0DC0 <= code && code <= 0x0DC6 || 0x0E01 <= code && code <= 0x0E30 || 0x0E32 <= code && code <= 0x0E33 || 0x0E40 <= code && code <= 0x0E46 || 0x0E81 <= code && code <= 0x0E82 || code == 0x0E84 || 0x0E86 <= code && code <= 0x0E8A || 0x0E8C <= code && code <= 0x0EA3 || code == 0x0EA5 || 0x0EA7 <= code && code <= 0x0EB0

            else if code < 0x103E then
                0x0EB2 <= code && code <= 0x0EB3 || code == 0x0EBD || 0x0EC0 <= code && code <= 0x0EC4 || code == 0x0EC6 || 0x0EDC <= code && code <= 0x0EDF || code == 0x0F00 || 0x0F40 <= code && code <= 0x0F47 || 0x0F49 <= code && code <= 0x0F6C || 0x0F88 <= code && code <= 0x0F8C || 0x1000 <= code && code <= 0x102A

            else
                code == 0x103F || 0x1050 <= code && code <= 0x1055 || 0x105A <= code && code <= 0x105D || code == 0x1061 || 0x1065 <= code && code <= 0x1066 || 0x106E <= code && code <= 0x1070 || 0x1075 <= code && code <= 0x1081 || code == 0x108E || 0x10A0 <= code && code <= 0x10C5 || code == 0x10C7 || code == 0x10CD

        else if code < 0x2101 then
            if code < 0x196F then
                if code < 0x1400 then
                    if code < 0x12B7 then
                        0x10D0 <= code && code <= 0x10FA || 0x10FC <= code && code <= 0x1248 || 0x124A <= code && code <= 0x124D || 0x1250 <= code && code <= 0x1256 || code == 0x1258 || 0x125A <= code && code <= 0x125D || 0x1260 <= code && code <= 0x1288 || 0x128A <= code && code <= 0x128D || 0x1290 <= code && code <= 0x12B0 || 0x12B2 <= code && code <= 0x12B5

                    else
                        0x12B8 <= code && code <= 0x12BE || code == 0x12C0 || 0x12C2 <= code && code <= 0x12C5 || 0x12C8 <= code && code <= 0x12D6 || 0x12D8 <= code && code <= 0x1310 || 0x1312 <= code && code <= 0x1315 || 0x1318 <= code && code <= 0x135A || 0x1380 <= code && code <= 0x138F || 0x13A0 <= code && code <= 0x13F5 || 0x13F8 <= code && code <= 0x13FD

                else if code < 0x177F then
                    0x1401 <= code && code <= 0x166C || 0x166F <= code && code <= 0x167F || 0x1681 <= code && code <= 0x169A || 0x16A0 <= code && code <= 0x16EA || 0x16F1 <= code && code <= 0x16F8 || 0x1700 <= code && code <= 0x1711 || 0x171F <= code && code <= 0x1731 || 0x1740 <= code && code <= 0x1751 || 0x1760 <= code && code <= 0x176C || 0x176E <= code && code <= 0x1770

                else
                    0x1780 <= code && code <= 0x17B3 || code == 0x17D7 || code == 0x17DC || 0x1820 <= code && code <= 0x1878 || 0x1880 <= code && code <= 0x1884 || 0x1887 <= code && code <= 0x18A8 || code == 0x18AA || 0x18B0 <= code && code <= 0x18F5 || 0x1900 <= code && code <= 0x191E || 0x1950 <= code && code <= 0x196D

            else if code < 0x1CF9 then
                if code < 0x1BB9 then
                    0x1970 <= code && code <= 0x1974 || 0x1980 <= code && code <= 0x19AB || 0x19B0 <= code && code <= 0x19C9 || 0x1A00 <= code && code <= 0x1A16 || 0x1A20 <= code && code <= 0x1A54 || code == 0x1AA7 || 0x1B05 <= code && code <= 0x1B33 || 0x1B45 <= code && code <= 0x1B4C || 0x1B83 <= code && code <= 0x1BA0 || 0x1BAE <= code && code <= 0x1BAF

                else
                    0x1BBA <= code && code <= 0x1BE5 || 0x1C00 <= code && code <= 0x1C23 || 0x1C4D <= code && code <= 0x1C4F || 0x1C5A <= code && code <= 0x1C7D || 0x1C80 <= code && code <= 0x1C8A || 0x1C90 <= code && code <= 0x1CBA || 0x1CBD <= code && code <= 0x1CBF || 0x1CE9 <= code && code <= 0x1CEC || 0x1CEE <= code && code <= 0x1CF3 || 0x1CF5 <= code && code <= 0x1CF6

            else if code < 0x1FBD then
                code == 0x1CFA || 0x1D00 <= code && code <= 0x1DBF || 0x1E00 <= code && code <= 0x1F15 || 0x1F18 <= code && code <= 0x1F1D || 0x1F20 <= code && code <= 0x1F45 || 0x1F48 <= code && code <= 0x1F4D || 0x1F50 <= code && code <= 0x1F57 || 0x1F60 <= code && code <= 0x1F7D || 0x1F80 <= code && code <= 0x1FB4 || 0x1FB6 <= code && code <= 0x1FBC || modBy 2 code == 1 && 0x1F59 <= code && code <= 0x1F5F

            else
                code == 0x1FBE || 0x1FC2 <= code && code <= 0x1FC4 || 0x1FC6 <= code && code <= 0x1FCC || 0x1FD0 <= code && code <= 0x1FD3 || 0x1FD6 <= code && code <= 0x1FDB || 0x1FE0 <= code && code <= 0x1FEC || 0x1FF2 <= code && code <= 0x1FF4 || 0x1FF6 <= code && code <= 0x1FFC || code == 0x2071 || code == 0x207F || 0x2090 <= code && code <= 0x209C

        else if code < 0x33FF then
            if code < 0x2D9F then
                if code < 0x2182 then
                    code == 0x2102 || code == 0x2107 || 0x210A <= code && code <= 0x2113 || code == 0x2115 || 0x2119 <= code && code <= 0x211D || 0x212A <= code && code <= 0x212D || 0x212F <= code && code <= 0x2139 || 0x213C <= code && code <= 0x213F || 0x2145 <= code && code <= 0x2149 || code == 0x214E || modBy 2 code == 0 && 0x2124 <= code && code <= 0x2128

                else
                    0x2183 <= code && code <= 0x2184 || 0x2C00 <= code && code <= 0x2CE4 || 0x2CEB <= code && code <= 0x2CEE || 0x2CF2 <= code && code <= 0x2CF3 || 0x2D00 <= code && code <= 0x2D25 || code == 0x2D27 || code == 0x2D2D || 0x2D30 <= code && code <= 0x2D67 || code == 0x2D6F || 0x2D80 <= code && code <= 0x2D96

            else if code < 0x3030 then
                0x2DA0 <= code && code <= 0x2DA6 || 0x2DA8 <= code && code <= 0x2DAE || 0x2DB0 <= code && code <= 0x2DB6 || 0x2DB8 <= code && code <= 0x2DBE || 0x2DC0 <= code && code <= 0x2DC6 || 0x2DC8 <= code && code <= 0x2DCE || 0x2DD0 <= code && code <= 0x2DD6 || 0x2DD8 <= code && code <= 0x2DDE || code == 0x2E2F || 0x3005 <= code && code <= 0x3006

            else
                0x3031 <= code && code <= 0x3035 || 0x303B <= code && code <= 0x303C || 0x3041 <= code && code <= 0x3096 || 0x309D <= code && code <= 0x309F || 0x30A1 <= code && code <= 0x30FA || 0x30FC <= code && code <= 0x30FF || 0x3105 <= code && code <= 0x312F || 0x3131 <= code && code <= 0x318E || 0x31A0 <= code && code <= 0x31BF || 0x31F0 <= code && code <= 0x31FF

        else if code < 0xA8FC then
            if code < 0xA721 then
                0x3400 <= code && code <= 0x4DBF || 0x4E00 <= code && code <= 0xA48C || 0xA4D0 <= code && code <= 0xA4FD || 0xA500 <= code && code <= 0xA60C || 0xA610 <= code && code <= 0xA61F || 0xA62A <= code && code <= 0xA62B || 0xA640 <= code && code <= 0xA66E || 0xA67F <= code && code <= 0xA69D || 0xA6A0 <= code && code <= 0xA6E5 || 0xA717 <= code && code <= 0xA71F

            else
                0xA722 <= code && code <= 0xA788 || 0xA78B <= code && code <= 0xA7DC || 0xA7F1 <= code && code <= 0xA801 || 0xA803 <= code && code <= 0xA805 || 0xA807 <= code && code <= 0xA80A || 0xA80C <= code && code <= 0xA822 || 0xA840 <= code && code <= 0xA873 || 0xA882 <= code && code <= 0xA8B3 || 0xA8F2 <= code && code <= 0xA8F7 || code == 0xA8FB

        else if code < 0xAA3F then
            0xA8FD <= code && code <= 0xA8FE || 0xA90A <= code && code <= 0xA925 || 0xA930 <= code && code <= 0xA946 || 0xA960 <= code && code <= 0xA97C || 0xA984 <= code && code <= 0xA9B2 || code == 0xA9CF || 0xA9E0 <= code && code <= 0xA9E4 || 0xA9E6 <= code && code <= 0xA9EF || 0xA9FA <= code && code <= 0xA9FE || 0xAA00 <= code && code <= 0xAA28

        else
            0xAA40 <= code && code <= 0xAA42 || 0xAA44 <= code && code <= 0xAA4B || 0xAA60 <= code && code <= 0xAA76 || code == 0xAA7A || 0xAA7E <= code && code <= 0xAAAF || code == 0xAAB1 || 0xAAB5 <= code && code <= 0xAAB6 || 0xAAB9 <= code && code <= 0xAABD || 0xAADB <= code && code <= 0xAADD || 0xAAE0 <= code && code <= 0xAAEA || 0xAAF2 <= code && code <= 0xAAF4 || modBy 2 code == 0 && 0xAAC0 <= code && code <= 0xAAC2

    else if code < 0x0001173F then
        if code < 0x000108F3 then
            if code < 0x0001003E then
                if code < 0xFB3F then
                    if code < 0xD7CA then
                        0xAB01 <= code && code <= 0xAB06 || 0xAB09 <= code && code <= 0xAB0E || 0xAB11 <= code && code <= 0xAB16 || 0xAB20 <= code && code <= 0xAB26 || 0xAB28 <= code && code <= 0xAB2E || 0xAB30 <= code && code <= 0xAB5A || 0xAB5C <= code && code <= 0xAB69 || 0xAB70 <= code && code <= 0xABE2 || 0xAC00 <= code && code <= 0xD7A3 || 0xD7B0 <= code && code <= 0xD7C6

                    else
                        0xD7CB <= code && code <= 0xD7FB || 0xF900 <= code && code <= 0xFA6D || 0xFA70 <= code && code <= 0xFAD9 || 0xFB00 <= code && code <= 0xFB06 || 0xFB13 <= code && code <= 0xFB17 || code == 0xFB1D || 0xFB1F <= code && code <= 0xFB28 || 0xFB2A <= code && code <= 0xFB36 || 0xFB38 <= code && code <= 0xFB3C || code == 0xFB3E

                else if code < 0xFF40 then
                    0xFB40 <= code && code <= 0xFB41 || 0xFB43 <= code && code <= 0xFB44 || 0xFB46 <= code && code <= 0xFBB1 || 0xFBD3 <= code && code <= 0xFD3D || 0xFD50 <= code && code <= 0xFD8F || 0xFD92 <= code && code <= 0xFDC7 || 0xFDF0 <= code && code <= 0xFDFB || 0xFE70 <= code && code <= 0xFE74 || 0xFE76 <= code && code <= 0xFEFC || 0xFF21 <= code && code <= 0xFF3A

                else
                    0xFF41 <= code && code <= 0xFF5A || 0xFF66 <= code && code <= 0xFFBE || 0xFFC2 <= code && code <= 0xFFC7 || 0xFFCA <= code && code <= 0xFFCF || 0xFFD2 <= code && code <= 0xFFD7 || 0xFFDA <= code && code <= 0xFFDC || 0x00010000 <= code && code <= 0x0001000B || 0x0001000D <= code && code <= 0x00010026 || 0x00010028 <= code && code <= 0x0001003A || 0x0001003C <= code && code <= 0x0001003D

            else if code < 0x00010593 then
                if code < 0x0001039F then
                    0x0001003F <= code && code <= 0x0001004D || 0x00010050 <= code && code <= 0x0001005D || 0x00010080 <= code && code <= 0x000100FA || 0x00010280 <= code && code <= 0x0001029C || 0x000102A0 <= code && code <= 0x000102D0 || 0x00010300 <= code && code <= 0x0001031F || 0x0001032D <= code && code <= 0x00010340 || 0x00010342 <= code && code <= 0x00010349 || 0x00010350 <= code && code <= 0x00010375 || 0x00010380 <= code && code <= 0x0001039D

                else
                    0x000103A0 <= code && code <= 0x000103C3 || 0x000103C8 <= code && code <= 0x000103CF || 0x00010400 <= code && code <= 0x0001049D || 0x000104B0 <= code && code <= 0x000104D3 || 0x000104D8 <= code && code <= 0x000104FB || 0x00010500 <= code && code <= 0x00010527 || 0x00010530 <= code && code <= 0x00010563 || 0x00010570 <= code && code <= 0x0001057A || 0x0001057C <= code && code <= 0x0001058A || 0x0001058C <= code && code <= 0x00010592

            else if code < 0x00010786 then
                0x00010594 <= code && code <= 0x00010595 || 0x00010597 <= code && code <= 0x000105A1 || 0x000105A3 <= code && code <= 0x000105B1 || 0x000105B3 <= code && code <= 0x000105B9 || 0x000105BB <= code && code <= 0x000105BC || 0x000105C0 <= code && code <= 0x000105F3 || 0x00010600 <= code && code <= 0x00010736 || 0x00010740 <= code && code <= 0x00010755 || 0x00010760 <= code && code <= 0x00010767 || 0x00010780 <= code && code <= 0x00010785

            else
                0x00010787 <= code && code <= 0x000107B0 || 0x000107B2 <= code && code <= 0x000107BA || 0x00010800 <= code && code <= 0x00010805 || code == 0x00010808 || 0x0001080A <= code && code <= 0x00010835 || 0x00010837 <= code && code <= 0x00010838 || code == 0x0001083C || 0x0001083F <= code && code <= 0x00010855 || 0x00010860 <= code && code <= 0x00010876 || 0x00010880 <= code && code <= 0x0001089E || 0x000108E0 <= code && code <= 0x000108F2

        else if code < 0x00011146 then
            if code < 0x00010CBF then
                if code < 0x00010A5F then
                    0x000108F4 <= code && code <= 0x000108F5 || 0x00010900 <= code && code <= 0x00010915 || 0x00010920 <= code && code <= 0x00010939 || 0x00010940 <= code && code <= 0x00010959 || 0x00010980 <= code && code <= 0x000109B7 || 0x000109BE <= code && code <= 0x000109BF || code == 0x00010A00 || 0x00010A10 <= code && code <= 0x00010A13 || 0x00010A15 <= code && code <= 0x00010A17 || 0x00010A19 <= code && code <= 0x00010A35

                else
                    0x00010A60 <= code && code <= 0x00010A7C || 0x00010A80 <= code && code <= 0x00010A9C || 0x00010AC0 <= code && code <= 0x00010AC7 || 0x00010AC9 <= code && code <= 0x00010AE4 || 0x00010B00 <= code && code <= 0x00010B35 || 0x00010B40 <= code && code <= 0x00010B55 || 0x00010B60 <= code && code <= 0x00010B72 || 0x00010B80 <= code && code <= 0x00010B91 || 0x00010C00 <= code && code <= 0x00010C48 || 0x00010C80 <= code && code <= 0x00010CB2

            else if code < 0x00010F6F then
                0x00010CC0 <= code && code <= 0x00010CF2 || 0x00010D00 <= code && code <= 0x00010D23 || 0x00010D4A <= code && code <= 0x00010D65 || 0x00010D6F <= code && code <= 0x00010D85 || 0x00010E80 <= code && code <= 0x00010EA9 || 0x00010EB0 <= code && code <= 0x00010EB1 || 0x00010EC2 <= code && code <= 0x00010EC7 || 0x00010F00 <= code && code <= 0x00010F1C || code == 0x00010F27 || 0x00010F30 <= code && code <= 0x00010F45

            else
                0x00010F70 <= code && code <= 0x00010F81 || 0x00010FB0 <= code && code <= 0x00010FC4 || 0x00010FE0 <= code && code <= 0x00010FF6 || 0x00011003 <= code && code <= 0x00011037 || 0x00011071 <= code && code <= 0x00011072 || code == 0x00011075 || 0x00011083 <= code && code <= 0x000110AF || 0x000110D0 <= code && code <= 0x000110E8 || 0x00011103 <= code && code <= 0x00011126 || code == 0x00011144

        else if code < 0x0001133C then
            if code < 0x00011289 then
                code == 0x00011147 || 0x00011150 <= code && code <= 0x00011172 || code == 0x00011176 || 0x00011183 <= code && code <= 0x000111B2 || 0x000111C1 <= code && code <= 0x000111C4 || 0x00011200 <= code && code <= 0x00011211 || 0x00011213 <= code && code <= 0x0001122B || 0x0001123F <= code && code <= 0x00011240 || 0x00011280 <= code && code <= 0x00011286 || code == 0x00011288 || modBy 2 code == 0 && 0x000111DA <= code && code <= 0x000111DC

            else
                0x0001128A <= code && code <= 0x0001128D || 0x0001128F <= code && code <= 0x0001129D || 0x0001129F <= code && code <= 0x000112A8 || 0x000112B0 <= code && code <= 0x000112DE || 0x00011305 <= code && code <= 0x0001130C || 0x0001130F <= code && code <= 0x00011310 || 0x00011313 <= code && code <= 0x00011328 || 0x0001132A <= code && code <= 0x00011330 || 0x00011332 <= code && code <= 0x00011333 || 0x00011335 <= code && code <= 0x00011339

        else if code < 0x0001145E then
            code == 0x0001133D || code == 0x00011350 || 0x0001135D <= code && code <= 0x00011361 || 0x00011380 <= code && code <= 0x00011389 || code == 0x0001138B || code == 0x0001138E || 0x00011390 <= code && code <= 0x000113B5 || code == 0x000113B7 || 0x00011400 <= code && code <= 0x00011434 || 0x00011447 <= code && code <= 0x0001144A || modBy 2 code == 1 && 0x000113D1 <= code && code <= 0x000113D3

        else
            0x0001145F <= code && code <= 0x00011461 || 0x00011480 <= code && code <= 0x000114AF || 0x000114C4 <= code && code <= 0x000114C5 || code == 0x000114C7 || 0x00011580 <= code && code <= 0x000115AE || 0x000115D8 <= code && code <= 0x000115DB || 0x00011600 <= code && code <= 0x0001162F || code == 0x00011644 || 0x00011680 <= code && code <= 0x000116AA || code == 0x000116B8 || 0x00011700 <= code && code <= 0x0001171A

    else if code < 0x0001D4A1 then
        if code < 0x00013440 then
            if code < 0x00011C3F then
                if code < 0x000119FF then
                    0x00011740 <= code && code <= 0x00011746 || 0x00011800 <= code && code <= 0x0001182B || 0x000118A0 <= code && code <= 0x000118DF || 0x000118FF <= code && code <= 0x00011906 || code == 0x00011909 || 0x0001190C <= code && code <= 0x00011913 || 0x00011915 <= code && code <= 0x00011916 || 0x00011918 <= code && code <= 0x0001192F || 0x000119A0 <= code && code <= 0x000119A7 || 0x000119AA <= code && code <= 0x000119D0 || modBy 2 code == 1 && (0x0001193F <= code && code <= 0x00011941 || 0x000119E1 <= code && code <= 0x000119E3)

                else
                    code == 0x00011A00 || 0x00011A0B <= code && code <= 0x00011A32 || code == 0x00011A3A || code == 0x00011A50 || 0x00011A5C <= code && code <= 0x00011A89 || code == 0x00011A9D || 0x00011AB0 <= code && code <= 0x00011AF8 || 0x00011BC0 <= code && code <= 0x00011BE0 || 0x00011C00 <= code && code <= 0x00011C08 || 0x00011C0A <= code && code <= 0x00011C2E

            else if code < 0x00011DAF then
                code == 0x00011C40 || 0x00011C72 <= code && code <= 0x00011C8F || 0x00011D00 <= code && code <= 0x00011D06 || 0x00011D08 <= code && code <= 0x00011D09 || 0x00011D0B <= code && code <= 0x00011D30 || code == 0x00011D46 || 0x00011D60 <= code && code <= 0x00011D65 || 0x00011D67 <= code && code <= 0x00011D68 || 0x00011D6A <= code && code <= 0x00011D89 || code == 0x00011D98

            else
                0x00011DB0 <= code && code <= 0x00011DDB || 0x00011EE0 <= code && code <= 0x00011EF2 || code == 0x00011F02 || 0x00011F04 <= code && code <= 0x00011F10 || 0x00011F12 <= code && code <= 0x00011F33 || code == 0x00011FB0 || 0x00012000 <= code && code <= 0x00012399 || 0x00012480 <= code && code <= 0x00012543 || 0x00012F90 <= code && code <= 0x00012FF0 || 0x00013000 <= code && code <= 0x0001342F

        else if code < 0x00016FE2 then
            if code < 0x00016B62 then
                0x00013441 <= code && code <= 0x00013446 || 0x00013460 <= code && code <= 0x000143FA || 0x00014400 <= code && code <= 0x00014646 || 0x00016100 <= code && code <= 0x0001611D || 0x00016800 <= code && code <= 0x00016A38 || 0x00016A40 <= code && code <= 0x00016A5E || 0x00016A70 <= code && code <= 0x00016ABE || 0x00016AD0 <= code && code <= 0x00016AED || 0x00016B00 <= code && code <= 0x00016B2F || 0x00016B40 <= code && code <= 0x00016B43

            else
                0x00016B63 <= code && code <= 0x00016B77 || 0x00016B7D <= code && code <= 0x00016B8F || 0x00016D40 <= code && code <= 0x00016D6C || 0x00016E40 <= code && code <= 0x00016E7F || 0x00016EA0 <= code && code <= 0x00016EB8 || 0x00016EBB <= code && code <= 0x00016ED3 || 0x00016F00 <= code && code <= 0x00016F4A || code == 0x00016F50 || 0x00016F93 <= code && code <= 0x00016F9F || 0x00016FE0 <= code && code <= 0x00016FE1

        else if code < 0x0001B14F then
            code == 0x00016FE3 || 0x00016FF2 <= code && code <= 0x00016FF3 || 0x00017000 <= code && code <= 0x00018CD5 || 0x00018CFF <= code && code <= 0x00018D1E || 0x00018D80 <= code && code <= 0x00018DF2 || 0x0001AFF0 <= code && code <= 0x0001AFF3 || 0x0001AFF5 <= code && code <= 0x0001AFFB || 0x0001AFFD <= code && code <= 0x0001AFFE || 0x0001B000 <= code && code <= 0x0001B122 || code == 0x0001B132

        else
            0x0001B150 <= code && code <= 0x0001B152 || code == 0x0001B155 || 0x0001B164 <= code && code <= 0x0001B167 || 0x0001B170 <= code && code <= 0x0001B2FB || 0x0001BC00 <= code && code <= 0x0001BC6A || 0x0001BC70 <= code && code <= 0x0001BC7C || 0x0001BC80 <= code && code <= 0x0001BC88 || 0x0001BC90 <= code && code <= 0x0001BC99 || 0x0001D400 <= code && code <= 0x0001D454 || 0x0001D456 <= code && code <= 0x0001D49C || 0x0001D49E <= code && code <= 0x0001D49F

    else if code < 0x0001E6E6 then
        if code < 0x0001D715 then
            if code < 0x0001D51D then
                code == 0x0001D4A2 || 0x0001D4A5 <= code && code <= 0x0001D4A6 || 0x0001D4A9 <= code && code <= 0x0001D4AC || 0x0001D4AE <= code && code <= 0x0001D4B9 || code == 0x0001D4BB || 0x0001D4BD <= code && code <= 0x0001D4C3 || 0x0001D4C5 <= code && code <= 0x0001D505 || 0x0001D507 <= code && code <= 0x0001D50A || 0x0001D50D <= code && code <= 0x0001D514 || 0x0001D516 <= code && code <= 0x0001D51C

            else
                0x0001D51E <= code && code <= 0x0001D539 || 0x0001D53B <= code && code <= 0x0001D53E || 0x0001D540 <= code && code <= 0x0001D544 || code == 0x0001D546 || 0x0001D54A <= code && code <= 0x0001D550 || 0x0001D552 <= code && code <= 0x0001D6A5 || 0x0001D6A8 <= code && code <= 0x0001D6C0 || 0x0001D6C2 <= code && code <= 0x0001D6DA || 0x0001D6DC <= code && code <= 0x0001D6FA || 0x0001D6FC <= code && code <= 0x0001D714

        else if code < 0x0001E0FF then
            0x0001D716 <= code && code <= 0x0001D734 || 0x0001D736 <= code && code <= 0x0001D74E || 0x0001D750 <= code && code <= 0x0001D76E || 0x0001D770 <= code && code <= 0x0001D788 || 0x0001D78A <= code && code <= 0x0001D7A8 || 0x0001D7AA <= code && code <= 0x0001D7C2 || 0x0001D7C4 <= code && code <= 0x0001D7CB || 0x0001DF00 <= code && code <= 0x0001DF1E || 0x0001DF25 <= code && code <= 0x0001DF2A || 0x0001E030 <= code && code <= 0x0001E06D

        else
            0x0001E100 <= code && code <= 0x0001E12C || 0x0001E137 <= code && code <= 0x0001E13D || code == 0x0001E14E || 0x0001E290 <= code && code <= 0x0001E2AD || 0x0001E2C0 <= code && code <= 0x0001E2EB || 0x0001E4D0 <= code && code <= 0x0001E4EB || 0x0001E5D0 <= code && code <= 0x0001E5ED || code == 0x0001E5F0 || 0x0001E6C0 <= code && code <= 0x0001E6DE || 0x0001E6E0 <= code && code <= 0x0001E6E2 || 0x0001E6E4 <= code && code <= 0x0001E6E5

    else if code < 0x0001EE53 then
        if code < 0x0001EDFF then
            0x0001E6E7 <= code && code <= 0x0001E6ED || 0x0001E6F0 <= code && code <= 0x0001E6F4 || 0x0001E6FE <= code && code <= 0x0001E6FF || 0x0001E7E0 <= code && code <= 0x0001E7E6 || 0x0001E7E8 <= code && code <= 0x0001E7EB || 0x0001E7ED <= code && code <= 0x0001E7EE || 0x0001E7F0 <= code && code <= 0x0001E7FE || 0x0001E800 <= code && code <= 0x0001E8C4 || 0x0001E900 <= code && code <= 0x0001E943 || code == 0x0001E94B

        else
            0x0001EE00 <= code && code <= 0x0001EE03 || 0x0001EE05 <= code && code <= 0x0001EE1F || 0x0001EE21 <= code && code <= 0x0001EE22 || code == 0x0001EE24 || code == 0x0001EE27 || 0x0001EE29 <= code && code <= 0x0001EE32 || 0x0001EE34 <= code && code <= 0x0001EE37 || code == 0x0001EE42 || 0x0001EE4D <= code && code <= 0x0001EE4F || 0x0001EE51 <= code && code <= 0x0001EE52 || modBy 2 code == 1 && (0x0001EE39 <= code && code <= 0x0001EE3B || 0x0001EE47 <= code && code <= 0x0001EE4B)

    else if code < 0x0001EEA0 then
        code == 0x0001EE54 || 0x0001EE61 <= code && code <= 0x0001EE62 || code == 0x0001EE64 || 0x0001EE67 <= code && code <= 0x0001EE6A || 0x0001EE6C <= code && code <= 0x0001EE72 || 0x0001EE74 <= code && code <= 0x0001EE77 || 0x0001EE79 <= code && code <= 0x0001EE7C || code == 0x0001EE7E || 0x0001EE80 <= code && code <= 0x0001EE89 || 0x0001EE8B <= code && code <= 0x0001EE9B || modBy 2 code == 1 && 0x0001EE57 <= code && code <= 0x0001EE5F

    else
        0x0001EEA1 <= code && code <= 0x0001EEA3 || 0x0001EEA5 <= code && code <= 0x0001EEA9 || 0x0001EEAB <= code && code <= 0x0001EEBB || 0x00020000 <= code && code <= 0x0002A6DF || 0x0002A700 <= code && code <= 0x0002B81D || 0x0002B820 <= code && code <= 0x0002CEAD || 0x0002CEB0 <= code && code <= 0x0002EBE0 || 0x0002EBF0 <= code && code <= 0x0002EE5D || 0x0002F800 <= code && code <= 0x0002FA1D || 0x00030000 <= code && code <= 0x0003134A || 0x00031350 <= code && code <= 0x00033479


{-| Detect letters or digits (Unicode categories Lu, Ll, Lt, Lm, Lo, Nd, Nl, No)
-}
isAlphaNum : Char -> Bool
isAlphaNum c =
    let
        code =
            Char.toCode c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if code < 0x0100 then
        0x30 <= code && code <= 0x39 || 0x41 <= code && code <= 0x5A || 0x61 <= code && code <= 0x7A || code == 0xAA || 0xB2 <= code && code <= 0xB3 || code == 0xB5 || 0xB9 <= code && code <= 0xBA || 0xBC <= code && code <= 0xBE || 0xC0 <= code && code <= 0xD6 || 0xD8 <= code && code <= 0xF6 || 0xF8 <= code && code <= 0xFF

    else if code < 0xF8FF then
        if code < 0x128F then
            if code < 0x0B5E then
                if code < 0x0957 then
                    if code < 0x06D4 then
                        if code < 0x03A2 then
                            0x0100 <= code && code <= 0x02C1 || 0x02C6 <= code && code <= 0x02D1 || 0x02E0 <= code && code <= 0x02E4 || 0x0370 <= code && code <= 0x0374 || 0x0376 <= code && code <= 0x0377 || 0x037A <= code && code <= 0x037D || code == 0x037F || code == 0x0386 || 0x0388 <= code && code <= 0x038A || code == 0x038C || 0x038E <= code && code <= 0x03A1 || modBy 2 code == 0 && 0x02EC <= code && code <= 0x02EE

                        else
                            0x03A3 <= code && code <= 0x03F5 || 0x03F7 <= code && code <= 0x0481 || 0x048A <= code && code <= 0x052F || 0x0531 <= code && code <= 0x0556 || code == 0x0559 || 0x0560 <= code && code <= 0x0588 || 0x05D0 <= code && code <= 0x05EA || 0x05EF <= code && code <= 0x05F2 || 0x0620 <= code && code <= 0x064A || 0x0660 <= code && code <= 0x0669 || 0x066E <= code && code <= 0x066F || 0x0671 <= code && code <= 0x06D3

                    else if code < 0x07FF then
                        code == 0x06D5 || 0x06E5 <= code && code <= 0x06E6 || 0x06EE <= code && code <= 0x06FC || code == 0x06FF || code == 0x0710 || 0x0712 <= code && code <= 0x072F || 0x074D <= code && code <= 0x07A5 || code == 0x07B1 || 0x07C0 <= code && code <= 0x07EA || 0x07F4 <= code && code <= 0x07F5 || code == 0x07FA

                    else
                        0x0800 <= code && code <= 0x0815 || code == 0x081A || code == 0x0824 || code == 0x0828 || 0x0840 <= code && code <= 0x0858 || 0x0860 <= code && code <= 0x086A || 0x0870 <= code && code <= 0x0887 || 0x0889 <= code && code <= 0x088F || 0x08A0 <= code && code <= 0x08C9 || 0x0904 <= code && code <= 0x0939 || code == 0x093D || code == 0x0950

                else if code < 0x0A58 then
                    if code < 0x09DB then
                        0x0958 <= code && code <= 0x0961 || 0x0966 <= code && code <= 0x096F || 0x0971 <= code && code <= 0x0980 || 0x0985 <= code && code <= 0x098C || 0x098F <= code && code <= 0x0990 || 0x0993 <= code && code <= 0x09A8 || 0x09AA <= code && code <= 0x09B0 || code == 0x09B2 || 0x09B6 <= code && code <= 0x09B9 || code == 0x09BD || code == 0x09CE

                    else
                        0x09DC <= code && code <= 0x09DD || 0x09DF <= code && code <= 0x09E1 || 0x09E6 <= code && code <= 0x09F1 || 0x09F4 <= code && code <= 0x09F9 || code == 0x09FC || 0x0A05 <= code && code <= 0x0A0A || 0x0A0F <= code && code <= 0x0A10 || 0x0A13 <= code && code <= 0x0A28 || 0x0A2A <= code && code <= 0x0A30 || 0x0A32 <= code && code <= 0x0A33 || 0x0A35 <= code && code <= 0x0A36 || 0x0A38 <= code && code <= 0x0A39

                else if code < 0x0ACF then
                    0x0A59 <= code && code <= 0x0A5C || code == 0x0A5E || 0x0A66 <= code && code <= 0x0A6F || 0x0A72 <= code && code <= 0x0A74 || 0x0A85 <= code && code <= 0x0A8D || 0x0A8F <= code && code <= 0x0A91 || 0x0A93 <= code && code <= 0x0AA8 || 0x0AAA <= code && code <= 0x0AB0 || 0x0AB2 <= code && code <= 0x0AB3 || 0x0AB5 <= code && code <= 0x0AB9 || code == 0x0ABD

                else
                    code == 0x0AD0 || 0x0AE0 <= code && code <= 0x0AE1 || 0x0AE6 <= code && code <= 0x0AEF || code == 0x0AF9 || 0x0B05 <= code && code <= 0x0B0C || 0x0B0F <= code && code <= 0x0B10 || 0x0B13 <= code && code <= 0x0B28 || 0x0B2A <= code && code <= 0x0B30 || 0x0B32 <= code && code <= 0x0B33 || 0x0B35 <= code && code <= 0x0B39 || code == 0x0B3D || 0x0B5C <= code && code <= 0x0B5D

            else if code < 0x0D99 then
                if code < 0x0C65 then
                    if code < 0x0BA7 then
                        0x0B5F <= code && code <= 0x0B61 || 0x0B66 <= code && code <= 0x0B6F || 0x0B71 <= code && code <= 0x0B77 || code == 0x0B83 || 0x0B85 <= code && code <= 0x0B8A || 0x0B8E <= code && code <= 0x0B90 || 0x0B92 <= code && code <= 0x0B95 || 0x0B99 <= code && code <= 0x0B9A || code == 0x0B9C || 0x0B9E <= code && code <= 0x0B9F || 0x0BA3 <= code && code <= 0x0BA4

                    else
                        0x0BA8 <= code && code <= 0x0BAA || 0x0BAE <= code && code <= 0x0BB9 || code == 0x0BD0 || 0x0BE6 <= code && code <= 0x0BF2 || 0x0C05 <= code && code <= 0x0C0C || 0x0C0E <= code && code <= 0x0C10 || 0x0C12 <= code && code <= 0x0C28 || 0x0C2A <= code && code <= 0x0C39 || code == 0x0C3D || 0x0C58 <= code && code <= 0x0C5A || 0x0C5C <= code && code <= 0x0C5D || 0x0C60 <= code && code <= 0x0C61

                else if code < 0x0CE5 then
                    0x0C66 <= code && code <= 0x0C6F || 0x0C78 <= code && code <= 0x0C7E || code == 0x0C80 || 0x0C85 <= code && code <= 0x0C8C || 0x0C8E <= code && code <= 0x0C90 || 0x0C92 <= code && code <= 0x0CA8 || 0x0CAA <= code && code <= 0x0CB3 || 0x0CB5 <= code && code <= 0x0CB9 || code == 0x0CBD || 0x0CDC <= code && code <= 0x0CDE || 0x0CE0 <= code && code <= 0x0CE1

                else
                    0x0CE6 <= code && code <= 0x0CEF || 0x0CF1 <= code && code <= 0x0CF2 || 0x0D04 <= code && code <= 0x0D0C || 0x0D0E <= code && code <= 0x0D10 || 0x0D12 <= code && code <= 0x0D3A || code == 0x0D3D || code == 0x0D4E || 0x0D54 <= code && code <= 0x0D56 || 0x0D58 <= code && code <= 0x0D61 || 0x0D66 <= code && code <= 0x0D78 || 0x0D7A <= code && code <= 0x0D7F || 0x0D85 <= code && code <= 0x0D96

            else if code < 0x0F3F then
                if code < 0x0E85 then
                    0x0D9A <= code && code <= 0x0DB1 || 0x0DB3 <= code && code <= 0x0DBB || code == 0x0DBD || 0x0DC0 <= code && code <= 0x0DC6 || 0x0DE6 <= code && code <= 0x0DEF || 0x0E01 <= code && code <= 0x0E30 || 0x0E32 <= code && code <= 0x0E33 || 0x0E40 <= code && code <= 0x0E46 || 0x0E50 <= code && code <= 0x0E59 || 0x0E81 <= code && code <= 0x0E82 || code == 0x0E84

                else
                    0x0E86 <= code && code <= 0x0E8A || 0x0E8C <= code && code <= 0x0EA3 || code == 0x0EA5 || 0x0EA7 <= code && code <= 0x0EB0 || 0x0EB2 <= code && code <= 0x0EB3 || code == 0x0EBD || 0x0EC0 <= code && code <= 0x0EC4 || code == 0x0EC6 || 0x0ED0 <= code && code <= 0x0ED9 || 0x0EDC <= code && code <= 0x0EDF || code == 0x0F00 || 0x0F20 <= code && code <= 0x0F33

            else if code < 0x108F then
                0x0F40 <= code && code <= 0x0F47 || 0x0F49 <= code && code <= 0x0F6C || 0x0F88 <= code && code <= 0x0F8C || 0x1000 <= code && code <= 0x102A || 0x103F <= code && code <= 0x1049 || 0x1050 <= code && code <= 0x1055 || 0x105A <= code && code <= 0x105D || code == 0x1061 || 0x1065 <= code && code <= 0x1066 || 0x106E <= code && code <= 0x1070 || 0x1075 <= code && code <= 0x1081 || code == 0x108E

            else
                0x1090 <= code && code <= 0x1099 || 0x10A0 <= code && code <= 0x10C5 || code == 0x10C7 || code == 0x10CD || 0x10D0 <= code && code <= 0x10FA || 0x10FC <= code && code <= 0x1248 || 0x124A <= code && code <= 0x124D || 0x1250 <= code && code <= 0x1256 || code == 0x1258 || 0x125A <= code && code <= 0x125D || 0x1260 <= code && code <= 0x1288 || 0x128A <= code && code <= 0x128D

        else if code < 0x24E9 then
            if code < 0x1B44 then
                if code < 0x177F then
                    if code < 0x139F then
                        0x1290 <= code && code <= 0x12B0 || 0x12B2 <= code && code <= 0x12B5 || 0x12B8 <= code && code <= 0x12BE || code == 0x12C0 || 0x12C2 <= code && code <= 0x12C5 || 0x12C8 <= code && code <= 0x12D6 || 0x12D8 <= code && code <= 0x1310 || 0x1312 <= code && code <= 0x1315 || 0x1318 <= code && code <= 0x135A || 0x1369 <= code && code <= 0x137C || 0x1380 <= code && code <= 0x138F

                    else
                        0x13A0 <= code && code <= 0x13F5 || 0x13F8 <= code && code <= 0x13FD || 0x1401 <= code && code <= 0x166C || 0x166F <= code && code <= 0x167F || 0x1681 <= code && code <= 0x169A || 0x16A0 <= code && code <= 0x16EA || 0x16EE <= code && code <= 0x16F8 || 0x1700 <= code && code <= 0x1711 || 0x171F <= code && code <= 0x1731 || 0x1740 <= code && code <= 0x1751 || 0x1760 <= code && code <= 0x176C || 0x176E <= code && code <= 0x1770

                else if code < 0x18FF then
                    0x1780 <= code && code <= 0x17B3 || code == 0x17D7 || code == 0x17DC || 0x17E0 <= code && code <= 0x17E9 || 0x17F0 <= code && code <= 0x17F9 || 0x1810 <= code && code <= 0x1819 || 0x1820 <= code && code <= 0x1878 || 0x1880 <= code && code <= 0x1884 || 0x1887 <= code && code <= 0x18A8 || code == 0x18AA || 0x18B0 <= code && code <= 0x18F5

                else
                    0x1900 <= code && code <= 0x191E || 0x1946 <= code && code <= 0x196D || 0x1970 <= code && code <= 0x1974 || 0x1980 <= code && code <= 0x19AB || 0x19B0 <= code && code <= 0x19C9 || 0x19D0 <= code && code <= 0x19DA || 0x1A00 <= code && code <= 0x1A16 || 0x1A20 <= code && code <= 0x1A54 || 0x1A80 <= code && code <= 0x1A89 || 0x1A90 <= code && code <= 0x1A99 || code == 0x1AA7 || 0x1B05 <= code && code <= 0x1B33

            else if code < 0x1FBD then
                if code < 0x1CED then
                    0x1B45 <= code && code <= 0x1B4C || 0x1B50 <= code && code <= 0x1B59 || 0x1B83 <= code && code <= 0x1BA0 || 0x1BAE <= code && code <= 0x1BE5 || 0x1C00 <= code && code <= 0x1C23 || 0x1C40 <= code && code <= 0x1C49 || 0x1C4D <= code && code <= 0x1C7D || 0x1C80 <= code && code <= 0x1C8A || 0x1C90 <= code && code <= 0x1CBA || 0x1CBD <= code && code <= 0x1CBF || 0x1CE9 <= code && code <= 0x1CEC

                else
                    0x1CEE <= code && code <= 0x1CF3 || 0x1CF5 <= code && code <= 0x1CF6 || code == 0x1CFA || 0x1D00 <= code && code <= 0x1DBF || 0x1E00 <= code && code <= 0x1F15 || 0x1F18 <= code && code <= 0x1F1D || 0x1F20 <= code && code <= 0x1F45 || 0x1F48 <= code && code <= 0x1F4D || 0x1F50 <= code && code <= 0x1F57 || 0x1F60 <= code && code <= 0x1F7D || 0x1F80 <= code && code <= 0x1FB4 || 0x1FB6 <= code && code <= 0x1FBC || modBy 2 code == 1 && 0x1F59 <= code && code <= 0x1F5F

            else if code < 0x2101 then
                code == 0x1FBE || 0x1FC2 <= code && code <= 0x1FC4 || 0x1FC6 <= code && code <= 0x1FCC || 0x1FD0 <= code && code <= 0x1FD3 || 0x1FD6 <= code && code <= 0x1FDB || 0x1FE0 <= code && code <= 0x1FEC || 0x1FF2 <= code && code <= 0x1FF4 || 0x1FF6 <= code && code <= 0x1FFC || 0x2070 <= code && code <= 0x2071 || 0x2074 <= code && code <= 0x2079 || 0x207F <= code && code <= 0x2089 || 0x2090 <= code && code <= 0x209C

            else
                code == 0x2102 || code == 0x2107 || 0x210A <= code && code <= 0x2113 || code == 0x2115 || 0x2119 <= code && code <= 0x211D || 0x212A <= code && code <= 0x212D || 0x212F <= code && code <= 0x2139 || 0x213C <= code && code <= 0x213F || 0x2145 <= code && code <= 0x2149 || code == 0x214E || 0x2150 <= code && code <= 0x2189 || 0x2460 <= code && code <= 0x249B || modBy 2 code == 0 && 0x2124 <= code && code <= 0x2128

        else if code < 0xA69F then
            if code < 0x3030 then
                if code < 0x2D7F then
                    0x24EA <= code && code <= 0x24FF || 0x2776 <= code && code <= 0x2793 || 0x2C00 <= code && code <= 0x2CE4 || 0x2CEB <= code && code <= 0x2CEE || 0x2CF2 <= code && code <= 0x2CF3 || code == 0x2CFD || 0x2D00 <= code && code <= 0x2D25 || code == 0x2D27 || code == 0x2D2D || 0x2D30 <= code && code <= 0x2D67 || code == 0x2D6F

                else
                    0x2D80 <= code && code <= 0x2D96 || 0x2DA0 <= code && code <= 0x2DA6 || 0x2DA8 <= code && code <= 0x2DAE || 0x2DB0 <= code && code <= 0x2DB6 || 0x2DB8 <= code && code <= 0x2DBE || 0x2DC0 <= code && code <= 0x2DC6 || 0x2DC8 <= code && code <= 0x2DCE || 0x2DD0 <= code && code <= 0x2DD6 || 0x2DD8 <= code && code <= 0x2DDE || code == 0x2E2F || 0x3005 <= code && code <= 0x3007 || 0x3021 <= code && code <= 0x3029

            else if code < 0x321F then
                0x3031 <= code && code <= 0x3035 || 0x3038 <= code && code <= 0x303C || 0x3041 <= code && code <= 0x3096 || 0x309D <= code && code <= 0x309F || 0x30A1 <= code && code <= 0x30FA || 0x30FC <= code && code <= 0x30FF || 0x3105 <= code && code <= 0x312F || 0x3131 <= code && code <= 0x318E || 0x3192 <= code && code <= 0x3195 || 0x31A0 <= code && code <= 0x31BF || 0x31F0 <= code && code <= 0x31FF

            else
                0x3220 <= code && code <= 0x3229 || 0x3248 <= code && code <= 0x324F || 0x3251 <= code && code <= 0x325F || 0x3280 <= code && code <= 0x3289 || 0x32B1 <= code && code <= 0x32BF || 0x3400 <= code && code <= 0x4DBF || 0x4E00 <= code && code <= 0xA48C || 0xA4D0 <= code && code <= 0xA4FD || 0xA500 <= code && code <= 0xA60C || 0xA610 <= code && code <= 0xA62B || 0xA640 <= code && code <= 0xA66E || 0xA67F <= code && code <= 0xA69D

        else if code < 0xAA3F then
            if code < 0xA8CF then
                0xA6A0 <= code && code <= 0xA6EF || 0xA717 <= code && code <= 0xA71F || 0xA722 <= code && code <= 0xA788 || 0xA78B <= code && code <= 0xA7DC || 0xA7F1 <= code && code <= 0xA801 || 0xA803 <= code && code <= 0xA805 || 0xA807 <= code && code <= 0xA80A || 0xA80C <= code && code <= 0xA822 || 0xA830 <= code && code <= 0xA835 || 0xA840 <= code && code <= 0xA873 || 0xA882 <= code && code <= 0xA8B3

            else
                0xA8D0 <= code && code <= 0xA8D9 || 0xA8F2 <= code && code <= 0xA8F7 || code == 0xA8FB || 0xA8FD <= code && code <= 0xA8FE || 0xA900 <= code && code <= 0xA925 || 0xA930 <= code && code <= 0xA946 || 0xA960 <= code && code <= 0xA97C || 0xA984 <= code && code <= 0xA9B2 || 0xA9CF <= code && code <= 0xA9D9 || 0xA9E0 <= code && code <= 0xA9E4 || 0xA9E6 <= code && code <= 0xA9FE || 0xAA00 <= code && code <= 0xAA28

        else if code < 0xAB00 then
            0xAA40 <= code && code <= 0xAA42 || 0xAA44 <= code && code <= 0xAA4B || 0xAA50 <= code && code <= 0xAA59 || 0xAA60 <= code && code <= 0xAA76 || code == 0xAA7A || 0xAA7E <= code && code <= 0xAAAF || code == 0xAAB1 || 0xAAB5 <= code && code <= 0xAAB6 || 0xAAB9 <= code && code <= 0xAABD || 0xAADB <= code && code <= 0xAADD || 0xAAE0 <= code && code <= 0xAAEA || 0xAAF2 <= code && code <= 0xAAF4 || modBy 2 code == 0 && 0xAAC0 <= code && code <= 0xAAC2

        else
            0xAB01 <= code && code <= 0xAB06 || 0xAB09 <= code && code <= 0xAB0E || 0xAB11 <= code && code <= 0xAB16 || 0xAB20 <= code && code <= 0xAB26 || 0xAB28 <= code && code <= 0xAB2E || 0xAB30 <= code && code <= 0xAB5A || 0xAB5C <= code && code <= 0xAB69 || 0xAB70 <= code && code <= 0xABE2 || 0xABF0 <= code && code <= 0xABF9 || 0xAC00 <= code && code <= 0xD7A3 || 0xD7B0 <= code && code <= 0xD7C6 || 0xD7CB <= code && code <= 0xD7FB

    else if code < 0x0001199F then
        if code < 0x00010AEA then
            if code < 0x000103FF then
                if code < 0xFFC9 then
                    if code < 0xFB45 then
                        0xF900 <= code && code <= 0xFA6D || 0xFA70 <= code && code <= 0xFAD9 || 0xFB00 <= code && code <= 0xFB06 || 0xFB13 <= code && code <= 0xFB17 || code == 0xFB1D || 0xFB1F <= code && code <= 0xFB28 || 0xFB2A <= code && code <= 0xFB36 || 0xFB38 <= code && code <= 0xFB3C || code == 0xFB3E || 0xFB40 <= code && code <= 0xFB41 || 0xFB43 <= code && code <= 0xFB44

                    else
                        0xFB46 <= code && code <= 0xFBB1 || 0xFBD3 <= code && code <= 0xFD3D || 0xFD50 <= code && code <= 0xFD8F || 0xFD92 <= code && code <= 0xFDC7 || 0xFDF0 <= code && code <= 0xFDFB || 0xFE70 <= code && code <= 0xFE74 || 0xFE76 <= code && code <= 0xFEFC || 0xFF10 <= code && code <= 0xFF19 || 0xFF21 <= code && code <= 0xFF3A || 0xFF41 <= code && code <= 0xFF5A || 0xFF66 <= code && code <= 0xFFBE || 0xFFC2 <= code && code <= 0xFFC7

                else if code < 0x0001013F then
                    0xFFCA <= code && code <= 0xFFCF || 0xFFD2 <= code && code <= 0xFFD7 || 0xFFDA <= code && code <= 0xFFDC || 0x00010000 <= code && code <= 0x0001000B || 0x0001000D <= code && code <= 0x00010026 || 0x00010028 <= code && code <= 0x0001003A || 0x0001003C <= code && code <= 0x0001003D || 0x0001003F <= code && code <= 0x0001004D || 0x00010050 <= code && code <= 0x0001005D || 0x00010080 <= code && code <= 0x000100FA || 0x00010107 <= code && code <= 0x00010133

                else
                    0x00010140 <= code && code <= 0x00010178 || 0x0001018A <= code && code <= 0x0001018B || 0x00010280 <= code && code <= 0x0001029C || 0x000102A0 <= code && code <= 0x000102D0 || 0x000102E1 <= code && code <= 0x000102FB || 0x00010300 <= code && code <= 0x00010323 || 0x0001032D <= code && code <= 0x0001034A || 0x00010350 <= code && code <= 0x00010375 || 0x00010380 <= code && code <= 0x0001039D || 0x000103A0 <= code && code <= 0x000103C3 || 0x000103C8 <= code && code <= 0x000103CF || 0x000103D1 <= code && code <= 0x000103D5

            else if code < 0x00010809 then
                if code < 0x000105A2 then
                    0x00010400 <= code && code <= 0x0001049D || 0x000104A0 <= code && code <= 0x000104A9 || 0x000104B0 <= code && code <= 0x000104D3 || 0x000104D8 <= code && code <= 0x000104FB || 0x00010500 <= code && code <= 0x00010527 || 0x00010530 <= code && code <= 0x00010563 || 0x00010570 <= code && code <= 0x0001057A || 0x0001057C <= code && code <= 0x0001058A || 0x0001058C <= code && code <= 0x00010592 || 0x00010594 <= code && code <= 0x00010595 || 0x00010597 <= code && code <= 0x000105A1

                else
                    0x000105A3 <= code && code <= 0x000105B1 || 0x000105B3 <= code && code <= 0x000105B9 || 0x000105BB <= code && code <= 0x000105BC || 0x000105C0 <= code && code <= 0x000105F3 || 0x00010600 <= code && code <= 0x00010736 || 0x00010740 <= code && code <= 0x00010755 || 0x00010760 <= code && code <= 0x00010767 || 0x00010780 <= code && code <= 0x00010785 || 0x00010787 <= code && code <= 0x000107B0 || 0x000107B2 <= code && code <= 0x000107BA || 0x00010800 <= code && code <= 0x00010805 || code == 0x00010808

            else if code < 0x0001093F then
                0x0001080A <= code && code <= 0x00010835 || 0x00010837 <= code && code <= 0x00010838 || code == 0x0001083C || 0x0001083F <= code && code <= 0x00010855 || 0x00010858 <= code && code <= 0x00010876 || 0x00010879 <= code && code <= 0x0001089E || 0x000108A7 <= code && code <= 0x000108AF || 0x000108E0 <= code && code <= 0x000108F2 || 0x000108F4 <= code && code <= 0x000108F5 || 0x000108FB <= code && code <= 0x0001091B || 0x00010920 <= code && code <= 0x00010939

            else
                0x00010940 <= code && code <= 0x00010959 || 0x00010980 <= code && code <= 0x000109B7 || 0x000109BC <= code && code <= 0x000109CF || 0x000109D2 <= code && code <= 0x00010A00 || 0x00010A10 <= code && code <= 0x00010A13 || 0x00010A15 <= code && code <= 0x00010A17 || 0x00010A19 <= code && code <= 0x00010A35 || 0x00010A40 <= code && code <= 0x00010A48 || 0x00010A60 <= code && code <= 0x00010A7E || 0x00010A80 <= code && code <= 0x00010A9F || 0x00010AC0 <= code && code <= 0x00010AC7 || 0x00010AC9 <= code && code <= 0x00010AE4

        else if code < 0x00011289 then
            if code < 0x00011002 then
                if code < 0x00010D3F then
                    0x00010AEB <= code && code <= 0x00010AEF || 0x00010B00 <= code && code <= 0x00010B35 || 0x00010B40 <= code && code <= 0x00010B55 || 0x00010B58 <= code && code <= 0x00010B72 || 0x00010B78 <= code && code <= 0x00010B91 || 0x00010BA9 <= code && code <= 0x00010BAF || 0x00010C00 <= code && code <= 0x00010C48 || 0x00010C80 <= code && code <= 0x00010CB2 || 0x00010CC0 <= code && code <= 0x00010CF2 || 0x00010CFA <= code && code <= 0x00010D23 || 0x00010D30 <= code && code <= 0x00010D39

                else
                    0x00010D40 <= code && code <= 0x00010D65 || 0x00010D6F <= code && code <= 0x00010D85 || 0x00010E60 <= code && code <= 0x00010E7E || 0x00010E80 <= code && code <= 0x00010EA9 || 0x00010EB0 <= code && code <= 0x00010EB1 || 0x00010EC2 <= code && code <= 0x00010EC7 || 0x00010F00 <= code && code <= 0x00010F27 || 0x00010F30 <= code && code <= 0x00010F45 || 0x00010F51 <= code && code <= 0x00010F54 || 0x00010F70 <= code && code <= 0x00010F81 || 0x00010FB0 <= code && code <= 0x00010FCB || 0x00010FE0 <= code && code <= 0x00010FF6

            else if code < 0x0001114F then
                0x00011003 <= code && code <= 0x00011037 || 0x00011052 <= code && code <= 0x0001106F || 0x00011071 <= code && code <= 0x00011072 || code == 0x00011075 || 0x00011083 <= code && code <= 0x000110AF || 0x000110D0 <= code && code <= 0x000110E8 || 0x000110F0 <= code && code <= 0x000110F9 || 0x00011103 <= code && code <= 0x00011126 || 0x00011136 <= code && code <= 0x0001113F || code == 0x00011144 || code == 0x00011147

            else
                0x00011150 <= code && code <= 0x00011172 || code == 0x00011176 || 0x00011183 <= code && code <= 0x000111B2 || 0x000111C1 <= code && code <= 0x000111C4 || 0x000111D0 <= code && code <= 0x000111DA || code == 0x000111DC || 0x000111E1 <= code && code <= 0x000111F4 || 0x00011200 <= code && code <= 0x00011211 || 0x00011213 <= code && code <= 0x0001122B || 0x0001123F <= code && code <= 0x00011240 || 0x00011280 <= code && code <= 0x00011286 || code == 0x00011288

        else if code < 0x0001147F then
            if code < 0x0001133C then
                0x0001128A <= code && code <= 0x0001128D || 0x0001128F <= code && code <= 0x0001129D || 0x0001129F <= code && code <= 0x000112A8 || 0x000112B0 <= code && code <= 0x000112DE || 0x000112F0 <= code && code <= 0x000112F9 || 0x00011305 <= code && code <= 0x0001130C || 0x0001130F <= code && code <= 0x00011310 || 0x00011313 <= code && code <= 0x00011328 || 0x0001132A <= code && code <= 0x00011330 || 0x00011332 <= code && code <= 0x00011333 || 0x00011335 <= code && code <= 0x00011339

            else
                code == 0x0001133D || code == 0x00011350 || 0x0001135D <= code && code <= 0x00011361 || 0x00011380 <= code && code <= 0x00011389 || code == 0x0001138B || code == 0x0001138E || 0x00011390 <= code && code <= 0x000113B5 || code == 0x000113B7 || 0x00011400 <= code && code <= 0x00011434 || 0x00011447 <= code && code <= 0x0001144A || 0x00011450 <= code && code <= 0x00011459 || 0x0001145F <= code && code <= 0x00011461 || modBy 2 code == 1 && 0x000113D1 <= code && code <= 0x000113D3

        else if code < 0x000116CF then
            0x00011480 <= code && code <= 0x000114AF || 0x000114C4 <= code && code <= 0x000114C5 || code == 0x000114C7 || 0x000114D0 <= code && code <= 0x000114D9 || 0x00011580 <= code && code <= 0x000115AE || 0x000115D8 <= code && code <= 0x000115DB || 0x00011600 <= code && code <= 0x0001162F || code == 0x00011644 || 0x00011650 <= code && code <= 0x00011659 || 0x00011680 <= code && code <= 0x000116AA || code == 0x000116B8 || 0x000116C0 <= code && code <= 0x000116C9

        else
            0x000116D0 <= code && code <= 0x000116E3 || 0x00011700 <= code && code <= 0x0001171A || 0x00011730 <= code && code <= 0x0001173B || 0x00011740 <= code && code <= 0x00011746 || 0x00011800 <= code && code <= 0x0001182B || 0x000118A0 <= code && code <= 0x000118F2 || 0x000118FF <= code && code <= 0x00011906 || code == 0x00011909 || 0x0001190C <= code && code <= 0x00011913 || 0x00011915 <= code && code <= 0x00011916 || 0x00011918 <= code && code <= 0x0001192F || 0x00011950 <= code && code <= 0x00011959 || modBy 2 code == 1 && 0x0001193F <= code && code <= 0x00011941

    else if code < 0x0001D4A8 then
        if code < 0x00016A3F then
            if code < 0x00011D69 then
                if code < 0x00011BFF then
                    0x000119A0 <= code && code <= 0x000119A7 || 0x000119AA <= code && code <= 0x000119D0 || code == 0x00011A00 || 0x00011A0B <= code && code <= 0x00011A32 || code == 0x00011A3A || code == 0x00011A50 || 0x00011A5C <= code && code <= 0x00011A89 || code == 0x00011A9D || 0x00011AB0 <= code && code <= 0x00011AF8 || 0x00011BC0 <= code && code <= 0x00011BE0 || 0x00011BF0 <= code && code <= 0x00011BF9 || modBy 2 code == 1 && 0x000119E1 <= code && code <= 0x000119E3

                else
                    0x00011C00 <= code && code <= 0x00011C08 || 0x00011C0A <= code && code <= 0x00011C2E || code == 0x00011C40 || 0x00011C50 <= code && code <= 0x00011C6C || 0x00011C72 <= code && code <= 0x00011C8F || 0x00011D00 <= code && code <= 0x00011D06 || 0x00011D08 <= code && code <= 0x00011D09 || 0x00011D0B <= code && code <= 0x00011D30 || code == 0x00011D46 || 0x00011D50 <= code && code <= 0x00011D59 || 0x00011D60 <= code && code <= 0x00011D65 || 0x00011D67 <= code && code <= 0x00011D68

            else if code < 0x00011FBF then
                0x00011D6A <= code && code <= 0x00011D89 || code == 0x00011D98 || 0x00011DA0 <= code && code <= 0x00011DA9 || 0x00011DB0 <= code && code <= 0x00011DDB || 0x00011DE0 <= code && code <= 0x00011DE9 || 0x00011EE0 <= code && code <= 0x00011EF2 || code == 0x00011F02 || 0x00011F04 <= code && code <= 0x00011F10 || 0x00011F12 <= code && code <= 0x00011F33 || 0x00011F50 <= code && code <= 0x00011F59 || code == 0x00011FB0

            else
                0x00011FC0 <= code && code <= 0x00011FD4 || 0x00012000 <= code && code <= 0x00012399 || 0x00012400 <= code && code <= 0x0001246E || 0x00012480 <= code && code <= 0x00012543 || 0x00012F90 <= code && code <= 0x00012FF0 || 0x00013000 <= code && code <= 0x0001342F || 0x00013441 <= code && code <= 0x00013446 || 0x00013460 <= code && code <= 0x000143FA || 0x00014400 <= code && code <= 0x00014646 || 0x00016100 <= code && code <= 0x0001611D || 0x00016130 <= code && code <= 0x00016139 || 0x00016800 <= code && code <= 0x00016A38

        else if code < 0x00018CFE then
            if code < 0x00016D3F then
                0x00016A40 <= code && code <= 0x00016A5E || 0x00016A60 <= code && code <= 0x00016A69 || 0x00016A70 <= code && code <= 0x00016ABE || 0x00016AC0 <= code && code <= 0x00016AC9 || 0x00016AD0 <= code && code <= 0x00016AED || 0x00016B00 <= code && code <= 0x00016B2F || 0x00016B40 <= code && code <= 0x00016B43 || 0x00016B50 <= code && code <= 0x00016B59 || 0x00016B5B <= code && code <= 0x00016B61 || 0x00016B63 <= code && code <= 0x00016B77 || 0x00016B7D <= code && code <= 0x00016B8F

            else
                0x00016D40 <= code && code <= 0x00016D6C || 0x00016D70 <= code && code <= 0x00016D79 || 0x00016E40 <= code && code <= 0x00016E96 || 0x00016EA0 <= code && code <= 0x00016EB8 || 0x00016EBB <= code && code <= 0x00016ED3 || 0x00016F00 <= code && code <= 0x00016F4A || code == 0x00016F50 || 0x00016F93 <= code && code <= 0x00016F9F || 0x00016FE0 <= code && code <= 0x00016FE1 || code == 0x00016FE3 || 0x00016FF2 <= code && code <= 0x00016FF6 || 0x00017000 <= code && code <= 0x00018CD5

        else if code < 0x0001BC6F then
            0x00018CFF <= code && code <= 0x00018D1E || 0x00018D80 <= code && code <= 0x00018DF2 || 0x0001AFF0 <= code && code <= 0x0001AFF3 || 0x0001AFF5 <= code && code <= 0x0001AFFB || 0x0001AFFD <= code && code <= 0x0001AFFE || 0x0001B000 <= code && code <= 0x0001B122 || code == 0x0001B132 || 0x0001B150 <= code && code <= 0x0001B152 || code == 0x0001B155 || 0x0001B164 <= code && code <= 0x0001B167 || 0x0001B170 <= code && code <= 0x0001B2FB || 0x0001BC00 <= code && code <= 0x0001BC6A

        else
            0x0001BC70 <= code && code <= 0x0001BC7C || 0x0001BC80 <= code && code <= 0x0001BC88 || 0x0001BC90 <= code && code <= 0x0001BC99 || 0x0001CCF0 <= code && code <= 0x0001CCF9 || 0x0001D2C0 <= code && code <= 0x0001D2D3 || 0x0001D2E0 <= code && code <= 0x0001D2F3 || 0x0001D360 <= code && code <= 0x0001D378 || 0x0001D400 <= code && code <= 0x0001D454 || 0x0001D456 <= code && code <= 0x0001D49C || 0x0001D49E <= code && code <= 0x0001D49F || code == 0x0001D4A2 || 0x0001D4A5 <= code && code <= 0x0001D4A6

    else if code < 0x0001E7DF then
        if code < 0x0001D7A9 then
            if code < 0x0001D545 then
                0x0001D4A9 <= code && code <= 0x0001D4AC || 0x0001D4AE <= code && code <= 0x0001D4B9 || code == 0x0001D4BB || 0x0001D4BD <= code && code <= 0x0001D4C3 || 0x0001D4C5 <= code && code <= 0x0001D505 || 0x0001D507 <= code && code <= 0x0001D50A || 0x0001D50D <= code && code <= 0x0001D514 || 0x0001D516 <= code && code <= 0x0001D51C || 0x0001D51E <= code && code <= 0x0001D539 || 0x0001D53B <= code && code <= 0x0001D53E || 0x0001D540 <= code && code <= 0x0001D544

            else
                code == 0x0001D546 || 0x0001D54A <= code && code <= 0x0001D550 || 0x0001D552 <= code && code <= 0x0001D6A5 || 0x0001D6A8 <= code && code <= 0x0001D6C0 || 0x0001D6C2 <= code && code <= 0x0001D6DA || 0x0001D6DC <= code && code <= 0x0001D6FA || 0x0001D6FC <= code && code <= 0x0001D714 || 0x0001D716 <= code && code <= 0x0001D734 || 0x0001D736 <= code && code <= 0x0001D74E || 0x0001D750 <= code && code <= 0x0001D76E || 0x0001D770 <= code && code <= 0x0001D788 || 0x0001D78A <= code && code <= 0x0001D7A8

        else if code < 0x0001E2BF then
            0x0001D7AA <= code && code <= 0x0001D7C2 || 0x0001D7C4 <= code && code <= 0x0001D7CB || 0x0001D7CE <= code && code <= 0x0001D7FF || 0x0001DF00 <= code && code <= 0x0001DF1E || 0x0001DF25 <= code && code <= 0x0001DF2A || 0x0001E030 <= code && code <= 0x0001E06D || 0x0001E100 <= code && code <= 0x0001E12C || 0x0001E137 <= code && code <= 0x0001E13D || 0x0001E140 <= code && code <= 0x0001E149 || code == 0x0001E14E || 0x0001E290 <= code && code <= 0x0001E2AD

        else
            0x0001E2C0 <= code && code <= 0x0001E2EB || 0x0001E2F0 <= code && code <= 0x0001E2F9 || 0x0001E4D0 <= code && code <= 0x0001E4EB || 0x0001E4F0 <= code && code <= 0x0001E4F9 || 0x0001E5D0 <= code && code <= 0x0001E5ED || 0x0001E5F0 <= code && code <= 0x0001E5FA || 0x0001E6C0 <= code && code <= 0x0001E6DE || 0x0001E6E0 <= code && code <= 0x0001E6E2 || 0x0001E6E4 <= code && code <= 0x0001E6E5 || 0x0001E6E7 <= code && code <= 0x0001E6ED || 0x0001E6F0 <= code && code <= 0x0001E6F4 || 0x0001E6FE <= code && code <= 0x0001E6FF

    else if code < 0x0001EE50 then
        if code < 0x0001ECB0 then
            0x0001E7E0 <= code && code <= 0x0001E7E6 || 0x0001E7E8 <= code && code <= 0x0001E7EB || 0x0001E7ED <= code && code <= 0x0001E7EE || 0x0001E7F0 <= code && code <= 0x0001E7FE || 0x0001E800 <= code && code <= 0x0001E8C4 || 0x0001E8C7 <= code && code <= 0x0001E8CF || 0x0001E900 <= code && code <= 0x0001E943 || code == 0x0001E94B || 0x0001E950 <= code && code <= 0x0001E959 || 0x0001EC71 <= code && code <= 0x0001ECAB || 0x0001ECAD <= code && code <= 0x0001ECAF

        else
            0x0001ECB1 <= code && code <= 0x0001ECB4 || 0x0001ED01 <= code && code <= 0x0001ED2D || 0x0001ED2F <= code && code <= 0x0001ED3D || 0x0001EE00 <= code && code <= 0x0001EE03 || 0x0001EE05 <= code && code <= 0x0001EE1F || 0x0001EE21 <= code && code <= 0x0001EE22 || code == 0x0001EE24 || code == 0x0001EE27 || 0x0001EE29 <= code && code <= 0x0001EE32 || 0x0001EE34 <= code && code <= 0x0001EE37 || code == 0x0001EE42 || 0x0001EE4D <= code && code <= 0x0001EE4F || modBy 2 code == 1 && (0x0001EE39 <= code && code <= 0x0001EE3B || 0x0001EE47 <= code && code <= 0x0001EE4B)

    else if code < 0x0001EEA4 then
        0x0001EE51 <= code && code <= 0x0001EE52 || code == 0x0001EE54 || 0x0001EE61 <= code && code <= 0x0001EE62 || code == 0x0001EE64 || 0x0001EE67 <= code && code <= 0x0001EE6A || 0x0001EE6C <= code && code <= 0x0001EE72 || 0x0001EE74 <= code && code <= 0x0001EE77 || 0x0001EE79 <= code && code <= 0x0001EE7C || code == 0x0001EE7E || 0x0001EE80 <= code && code <= 0x0001EE89 || 0x0001EE8B <= code && code <= 0x0001EE9B || 0x0001EEA1 <= code && code <= 0x0001EEA3 || modBy 2 code == 1 && 0x0001EE57 <= code && code <= 0x0001EE5F

    else
        0x0001EEA5 <= code && code <= 0x0001EEA9 || 0x0001EEAB <= code && code <= 0x0001EEBB || 0x0001F100 <= code && code <= 0x0001F10C || 0x0001FBF0 <= code && code <= 0x0001FBF9 || 0x00020000 <= code && code <= 0x0002A6DF || 0x0002A700 <= code && code <= 0x0002B81D || 0x0002B820 <= code && code <= 0x0002CEAD || 0x0002CEB0 <= code && code <= 0x0002EBE0 || 0x0002EBF0 <= code && code <= 0x0002EE5D || 0x0002F800 <= code && code <= 0x0002FA1D || 0x00030000 <= code && code <= 0x0003134A || 0x00031350 <= code && code <= 0x00033479


{-| Detect digits (Unicode categories Nd, Nl, No)
-}
isDigit : Char -> Bool
isDigit c =
    let
        code =
            Char.toCode c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if code < 0x0100 then
        0x30 <= code && code <= 0x39 || 0xB2 <= code && code <= 0xB3 || code == 0xB9 || 0xBC <= code && code <= 0xBE

    else if code < 0x0001049F then
        if code < 0x206F then
            if code < 0x0E4F then
                if code < 0x0B65 then
                    0x0660 <= code && code <= 0x0669 || 0x06F0 <= code && code <= 0x06F9 || 0x07C0 <= code && code <= 0x07C9 || 0x0966 <= code && code <= 0x096F || 0x09E6 <= code && code <= 0x09EF || 0x09F4 <= code && code <= 0x09F9 || 0x0A66 <= code && code <= 0x0A6F || 0x0AE6 <= code && code <= 0x0AEF

                else
                    0x0B66 <= code && code <= 0x0B6F || 0x0B72 <= code && code <= 0x0B77 || 0x0BE6 <= code && code <= 0x0BF2 || 0x0C66 <= code && code <= 0x0C6F || 0x0C78 <= code && code <= 0x0C7E || 0x0CE6 <= code && code <= 0x0CEF || 0x0D58 <= code && code <= 0x0D5E || 0x0D66 <= code && code <= 0x0D78 || 0x0DE6 <= code && code <= 0x0DEF

            else if code < 0x180F then
                0x0E50 <= code && code <= 0x0E59 || 0x0ED0 <= code && code <= 0x0ED9 || 0x0F20 <= code && code <= 0x0F33 || 0x1040 <= code && code <= 0x1049 || 0x1090 <= code && code <= 0x1099 || 0x1369 <= code && code <= 0x137C || 0x16EE <= code && code <= 0x16F0 || 0x17E0 <= code && code <= 0x17E9 || 0x17F0 <= code && code <= 0x17F9

            else
                0x1810 <= code && code <= 0x1819 || 0x1946 <= code && code <= 0x194F || 0x19D0 <= code && code <= 0x19DA || 0x1A80 <= code && code <= 0x1A89 || 0x1A90 <= code && code <= 0x1A99 || 0x1B50 <= code && code <= 0x1B59 || 0x1BB0 <= code && code <= 0x1BB9 || 0x1C40 <= code && code <= 0x1C49 || 0x1C50 <= code && code <= 0x1C59

        else if code < 0xA61F then
            if code < 0x3006 then
                code == 0x2070 || 0x2074 <= code && code <= 0x2079 || 0x2080 <= code && code <= 0x2089 || 0x2150 <= code && code <= 0x2182 || 0x2185 <= code && code <= 0x2189 || 0x2460 <= code && code <= 0x249B || 0x24EA <= code && code <= 0x24FF || 0x2776 <= code && code <= 0x2793 || code == 0x2CFD

            else
                code == 0x3007 || 0x3021 <= code && code <= 0x3029 || 0x3038 <= code && code <= 0x303A || 0x3192 <= code && code <= 0x3195 || 0x3220 <= code && code <= 0x3229 || 0x3248 <= code && code <= 0x324F || 0x3251 <= code && code <= 0x325F || 0x3280 <= code && code <= 0x3289 || 0x32B1 <= code && code <= 0x32BF

        else if code < 0xFF0F then
            0xA620 <= code && code <= 0xA629 || 0xA6E6 <= code && code <= 0xA6EF || 0xA830 <= code && code <= 0xA835 || 0xA8D0 <= code && code <= 0xA8D9 || 0xA900 <= code && code <= 0xA909 || 0xA9D0 <= code && code <= 0xA9D9 || 0xA9F0 <= code && code <= 0xA9F9 || 0xAA50 <= code && code <= 0xAA59 || 0xABF0 <= code && code <= 0xABF9

        else
            0xFF10 <= code && code <= 0xFF19 || 0x00010107 <= code && code <= 0x00010133 || 0x00010140 <= code && code <= 0x00010178 || 0x0001018A <= code && code <= 0x0001018B || 0x000102E1 <= code && code <= 0x000102FB || 0x00010320 <= code && code <= 0x00010323 || code == 0x00010341 || code == 0x0001034A || 0x000103D1 <= code && code <= 0x000103D5

    else if code < 0x000118DF then
        if code < 0x00010D2F then
            if code < 0x000109D1 then
                0x000104A0 <= code && code <= 0x000104A9 || 0x00010858 <= code && code <= 0x0001085F || 0x00010879 <= code && code <= 0x0001087F || 0x000108A7 <= code && code <= 0x000108AF || 0x000108FB <= code && code <= 0x000108FF || 0x00010916 <= code && code <= 0x0001091B || 0x000109BC <= code && code <= 0x000109BD || 0x000109C0 <= code && code <= 0x000109CF

            else
                0x000109D2 <= code && code <= 0x000109FF || 0x00010A40 <= code && code <= 0x00010A48 || 0x00010A7D <= code && code <= 0x00010A7E || 0x00010A9D <= code && code <= 0x00010A9F || 0x00010AEB <= code && code <= 0x00010AEF || 0x00010B58 <= code && code <= 0x00010B5F || 0x00010B78 <= code && code <= 0x00010B7F || 0x00010BA9 <= code && code <= 0x00010BAF || 0x00010CFA <= code && code <= 0x00010CFF

        else if code < 0x000111CF then
            0x00010D30 <= code && code <= 0x00010D39 || 0x00010D40 <= code && code <= 0x00010D49 || 0x00010E60 <= code && code <= 0x00010E7E || 0x00010F1D <= code && code <= 0x00010F26 || 0x00010F51 <= code && code <= 0x00010F54 || 0x00010FC5 <= code && code <= 0x00010FCB || 0x00011052 <= code && code <= 0x0001106F || 0x000110F0 <= code && code <= 0x000110F9 || 0x00011136 <= code && code <= 0x0001113F

        else
            0x000111D0 <= code && code <= 0x000111D9 || 0x000111E1 <= code && code <= 0x000111F4 || 0x000112F0 <= code && code <= 0x000112F9 || 0x00011450 <= code && code <= 0x00011459 || 0x000114D0 <= code && code <= 0x000114D9 || 0x00011650 <= code && code <= 0x00011659 || 0x000116C0 <= code && code <= 0x000116C9 || 0x000116D0 <= code && code <= 0x000116E3 || 0x00011730 <= code && code <= 0x0001173B

    else if code < 0x0001CCEF then
        if code < 0x000123FF then
            0x000118E0 <= code && code <= 0x000118F2 || 0x00011950 <= code && code <= 0x00011959 || 0x00011BF0 <= code && code <= 0x00011BF9 || 0x00011C50 <= code && code <= 0x00011C6C || 0x00011D50 <= code && code <= 0x00011D59 || 0x00011DA0 <= code && code <= 0x00011DA9 || 0x00011DE0 <= code && code <= 0x00011DE9 || 0x00011F50 <= code && code <= 0x00011F59 || 0x00011FC0 <= code && code <= 0x00011FD4

        else
            0x00012400 <= code && code <= 0x0001246E || 0x00016130 <= code && code <= 0x00016139 || 0x00016A60 <= code && code <= 0x00016A69 || 0x00016AC0 <= code && code <= 0x00016AC9 || 0x00016B50 <= code && code <= 0x00016B59 || 0x00016B5B <= code && code <= 0x00016B61 || 0x00016D70 <= code && code <= 0x00016D79 || 0x00016E80 <= code && code <= 0x00016E96 || 0x00016FF4 <= code && code <= 0x00016FF6

    else if code < 0x0001E8C6 then
        0x0001CCF0 <= code && code <= 0x0001CCF9 || 0x0001D2C0 <= code && code <= 0x0001D2D3 || 0x0001D2E0 <= code && code <= 0x0001D2F3 || 0x0001D360 <= code && code <= 0x0001D378 || 0x0001D7CE <= code && code <= 0x0001D7FF || 0x0001E140 <= code && code <= 0x0001E149 || 0x0001E2F0 <= code && code <= 0x0001E2F9 || 0x0001E4F0 <= code && code <= 0x0001E4F9 || 0x0001E5F1 <= code && code <= 0x0001E5FA

    else
        0x0001E8C7 <= code && code <= 0x0001E8CF || 0x0001E950 <= code && code <= 0x0001E959 || 0x0001EC71 <= code && code <= 0x0001ECAB || 0x0001ECAD <= code && code <= 0x0001ECAF || 0x0001ECB1 <= code && code <= 0x0001ECB4 || 0x0001ED01 <= code && code <= 0x0001ED2D || 0x0001ED2F <= code && code <= 0x0001ED3D || 0x0001F100 <= code && code <= 0x0001F10C || 0x0001FBF0 <= code && code <= 0x0001FBF9


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
getCategory : Char -> Maybe Category
getCategory c =
    let
        code =
            Char.toCode c
    in
    if Basics.isNaN (Basics.toFloat code) then
        Just OtherSurrogate

    else if code < 0x0100 then
        if code < 0xA0 then
            if code < 0x3B then
                if code < 0x29 then
                    if code <= 0x1F then
                        Just OtherControl

                    else if code == 0x20 then
                        Just SeparatorSpace

                    else if 0x21 <= code && code <= 0x23 || 0x25 <= code && code <= 0x27 then
                        Just PunctuationOther

                    else if code == 0x24 then
                        Just SymbolCurrency

                    else if code == 0x28 then
                        Just PunctuationOpen

                    else
                        Nothing

                else if code == 0x29 then
                    Just PunctuationClose

                else if code == 0x2A || code == 0x2C || 0x2E <= code && code <= 0x2F || code == 0x3A then
                    Just PunctuationOther

                else if code == 0x2B then
                    Just SymbolMath

                else if code == 0x2D then
                    Just PunctuationDash

                else if 0x30 <= code && code <= 0x39 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x5E then
                if code == 0x3B || 0x3F <= code && code <= 0x40 || code == 0x5C then
                    Just PunctuationOther

                else if 0x3C <= code && code <= 0x3E then
                    Just SymbolMath

                else if 0x41 <= code && code <= 0x5A then
                    Just LetterUppercase

                else if code == 0x5B then
                    Just PunctuationOpen

                else if code == 0x5D then
                    Just PunctuationClose

                else
                    Nothing

            else if code == 0x5E || code == 0x60 then
                Just SymbolModifier

            else if code == 0x5F then
                Just PunctuationConnector

            else if 0x61 <= code && code <= 0x7A then
                Just LetterLowercase

            else if code == 0x7B then
                Just PunctuationOpen

            else if code == 0x7C || code == 0x7E then
                Just SymbolMath

            else if code == 0x7D then
                Just PunctuationClose

            else if 0x7F <= code && code <= 0x9F then
                Just OtherControl

            else
                Nothing

        else if code < 0xB1 then
            if code < 0xA9 then
                if code == 0xA0 then
                    Just SeparatorSpace

                else if code == 0xA1 || code == 0xA7 then
                    Just PunctuationOther

                else if 0xA2 <= code && code <= 0xA5 then
                    Just SymbolCurrency

                else if code == 0xA6 then
                    Just SymbolOther

                else if code == 0xA8 then
                    Just SymbolModifier

                else
                    Nothing

            else if code == 0xA9 || code == 0xAE || code == 0xB0 then
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

            else if 0xB2 <= code && code <= 0xB3 || code == 0xB9 then
                Just NumberOther

            else if code == 0xB4 || code == 0xB8 then
                Just SymbolModifier

            else if code == 0xB5 then
                Just LetterLowercase

            else if 0xB6 <= code && code <= 0xB7 then
                Just PunctuationOther

            else
                Nothing

        else if code == 0xBA then
            Just LetterOther

        else if code == 0xBB then
            Just PunctuationFinalQuote

        else if 0xBC <= code && code <= 0xBE then
            Just NumberOther

        else if code == 0xBF then
            Just PunctuationOther

        else if 0xC0 <= code && code <= 0xD6 || 0xD8 <= code && code <= 0xDE then
            Just LetterUppercase

        else if code == 0xD7 || code == 0xF7 then
            Just SymbolMath

        else if 0xDF <= code && code <= 0xF6 || 0xF8 <= code && code <= 0xFF then
            Just LetterLowercase

        else
            Nothing

    else if code < 0x2982 then
        if code < 0x0CBC then
            if code < 0x049C then
                if code < 0x01F0 then
                    if code < 0x0168 then
                        if code < 0x0132 then
                            if code < 0x0118 then
                                if code < 0x010B then
                                    if code == 0x0100 || code == 0x0102 || code == 0x0104 || code == 0x0106 || code == 0x0108 || code == 0x010A then
                                        Just LetterUppercase

                                    else if code == 0x0101 || code == 0x0103 || code == 0x0105 || code == 0x0107 || code == 0x0109 then
                                        Just LetterLowercase

                                    else
                                        Nothing

                                else if code < 0x0110 then
                                    if code == 0x010B || code == 0x010D || code == 0x010F then
                                        Just LetterLowercase

                                    else if code == 0x010C || code == 0x010E then
                                        Just LetterUppercase

                                    else
                                        Nothing

                                else if code == 0x0110 || code == 0x0112 || code == 0x0114 || code == 0x0116 then
                                    Just LetterUppercase

                                else if code == 0x0111 || code == 0x0113 || code == 0x0115 || code == 0x0117 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x0124 then
                                if code == 0x0118 || code == 0x011A || code == 0x011C || code == 0x011E || code == 0x0120 || code == 0x0122 then
                                    Just LetterUppercase

                                else if code == 0x0119 || code == 0x011B || code == 0x011D || code == 0x011F || code == 0x0121 || code == 0x0123 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x012A then
                                if code == 0x0124 || code == 0x0126 || code == 0x0128 then
                                    Just LetterUppercase

                                else if code == 0x0125 || code == 0x0127 || code == 0x0129 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code == 0x012A || code == 0x012C || code == 0x012E || code == 0x0130 then
                                Just LetterUppercase

                            else if code == 0x012B || code == 0x012D || code == 0x012F || code == 0x0131 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x014D then
                            if code < 0x013E then
                                if code == 0x0132 || code == 0x0134 || code == 0x0136 || code == 0x0139 || code == 0x013B || code == 0x013D then
                                    Just LetterUppercase

                                else if code == 0x0133 || code == 0x0135 || 0x0137 <= code && code <= 0x0138 || code == 0x013A || code == 0x013C then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x0144 then
                                if code == 0x013E || code == 0x0140 || code == 0x0142 then
                                    Just LetterLowercase

                                else if code == 0x013F || code == 0x0141 || code == 0x0143 then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code == 0x0144 || code == 0x0146 || 0x0148 <= code && code <= 0x0149 || code == 0x014B then
                                Just LetterLowercase

                            else if code == 0x0145 || code == 0x0147 || code == 0x014A || code == 0x014C then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x0159 then
                            if code == 0x014D || code == 0x014F || code == 0x0151 || code == 0x0153 || code == 0x0155 || code == 0x0157 then
                                Just LetterLowercase

                            else if code == 0x014E || code == 0x0150 || code == 0x0152 || code == 0x0154 || code == 0x0156 || code == 0x0158 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x015F then
                            if code == 0x0159 || code == 0x015B || code == 0x015D then
                                Just LetterLowercase

                            else if code == 0x015A || code == 0x015C || code == 0x015E then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x015F || code == 0x0161 || code == 0x0163 || code == 0x0165 || code == 0x0167 then
                            Just LetterLowercase

                        else if code == 0x0160 || code == 0x0162 || code == 0x0164 || code == 0x0166 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x01B0 then
                        if code < 0x0184 then
                            if code < 0x0173 then
                                if code == 0x0168 || code == 0x016A || code == 0x016C || code == 0x016E || code == 0x0170 || code == 0x0172 then
                                    Just LetterUppercase

                                else if code == 0x0169 || code == 0x016B || code == 0x016D || code == 0x016F || code == 0x0171 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x0179 then
                                if code == 0x0173 || code == 0x0175 || code == 0x0177 then
                                    Just LetterLowercase

                                else if code == 0x0174 || code == 0x0176 || code == 0x0178 then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code == 0x0179 || code == 0x017B || code == 0x017D || 0x0181 <= code && code <= 0x0182 then
                                Just LetterUppercase

                            else if code == 0x017A || code == 0x017C || 0x017E <= code && code <= 0x0180 || code == 0x0183 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x019D then
                            if code < 0x018D then
                                if code == 0x0184 || 0x0186 <= code && code <= 0x0187 || 0x0189 <= code && code <= 0x018B then
                                    Just LetterUppercase

                                else if code == 0x0185 || code == 0x0188 || code == 0x018C then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code == 0x018D || code == 0x0192 || code == 0x0195 || 0x0199 <= code && code <= 0x019B then
                                Just LetterLowercase

                            else if 0x018E <= code && code <= 0x0191 || 0x0193 <= code && code <= 0x0194 || 0x0196 <= code && code <= 0x0198 || code == 0x019C then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x01A4 then
                            if code == 0x019D || 0x019F <= code && code <= 0x01A0 || code == 0x01A2 then
                                Just LetterUppercase

                            else if code == 0x019E || code == 0x01A1 || code == 0x01A3 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x01A4 || 0x01A6 <= code && code <= 0x01A7 || code == 0x01A9 || code == 0x01AC || 0x01AE <= code && code <= 0x01AF then
                            Just LetterUppercase

                        else if code == 0x01A5 || code == 0x01A8 || 0x01AA <= code && code <= 0x01AB || code == 0x01AD then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x01D3 then
                        if code < 0x01C5 then
                            if code == 0x01B0 || code == 0x01B4 || code == 0x01B6 || 0x01B9 <= code && code <= 0x01BA || 0x01BD <= code && code <= 0x01BF then
                                Just LetterLowercase

                            else if 0x01B1 <= code && code <= 0x01B3 || code == 0x01B5 || 0x01B7 <= code && code <= 0x01B8 || code == 0x01BC || code == 0x01C4 then
                                Just LetterUppercase

                            else if code == 0x01BB || 0x01C0 <= code && code <= 0x01C3 then
                                Just LetterOther

                            else
                                Nothing

                        else if code < 0x01CB then
                            if code == 0x01C5 || code == 0x01C8 then
                                Just LetterTitlecase

                            else if code == 0x01C6 || code == 0x01C9 then
                                Just LetterLowercase

                            else if code == 0x01C7 || code == 0x01CA then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x01CB then
                            Just LetterTitlecase

                        else if code == 0x01CC || code == 0x01CE || code == 0x01D0 || code == 0x01D2 then
                            Just LetterLowercase

                        else if code == 0x01CD || code == 0x01CF || code == 0x01D1 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x01E1 then
                        if code < 0x01D8 then
                            if code == 0x01D3 || code == 0x01D5 || code == 0x01D7 then
                                Just LetterUppercase

                            else if code == 0x01D4 || code == 0x01D6 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x01D8 || code == 0x01DA || 0x01DC <= code && code <= 0x01DD || code == 0x01DF then
                            Just LetterLowercase

                        else if code == 0x01D9 || code == 0x01DB || code == 0x01DE || code == 0x01E0 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x01E7 then
                        if code == 0x01E1 || code == 0x01E3 || code == 0x01E5 then
                            Just LetterLowercase

                        else if code == 0x01E2 || code == 0x01E4 || code == 0x01E6 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x01E7 || code == 0x01E9 || code == 0x01EB || code == 0x01ED || code == 0x01EF then
                        Just LetterLowercase

                    else if code == 0x01E8 || code == 0x01EA || code == 0x01EC || code == 0x01EE then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x037A then
                    if code < 0x0224 then
                        if code < 0x020A then
                            if code < 0x01FD then
                                if code == 0x01F0 || code == 0x01F3 || code == 0x01F5 || code == 0x01F9 || code == 0x01FB then
                                    Just LetterLowercase

                                else if code == 0x01F1 || code == 0x01F4 || 0x01F6 <= code && code <= 0x01F8 || code == 0x01FA || code == 0x01FC then
                                    Just LetterUppercase

                                else if code == 0x01F2 then
                                    Just LetterTitlecase

                                else
                                    Nothing

                            else if code < 0x0202 then
                                if code == 0x01FD || code == 0x01FF || code == 0x0201 then
                                    Just LetterLowercase

                                else if code == 0x01FE || code == 0x0200 then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code == 0x0202 || code == 0x0204 || code == 0x0206 || code == 0x0208 then
                                Just LetterUppercase

                            else if code == 0x0203 || code == 0x0205 || code == 0x0207 || code == 0x0209 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x0216 then
                            if code == 0x020A || code == 0x020C || code == 0x020E || code == 0x0210 || code == 0x0212 || code == 0x0214 then
                                Just LetterUppercase

                            else if code == 0x020B || code == 0x020D || code == 0x020F || code == 0x0211 || code == 0x0213 || code == 0x0215 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x021C then
                            if code == 0x0216 || code == 0x0218 || code == 0x021A then
                                Just LetterUppercase

                            else if code == 0x0217 || code == 0x0219 || code == 0x021B then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x021C || code == 0x021E || code == 0x0220 || code == 0x0222 then
                            Just LetterUppercase

                        else if code == 0x021D || code == 0x021F || code == 0x0221 || code == 0x0223 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x0249 then
                        if code < 0x022F then
                            if code == 0x0224 || code == 0x0226 || code == 0x0228 || code == 0x022A || code == 0x022C || code == 0x022E then
                                Just LetterUppercase

                            else if code == 0x0225 || code == 0x0227 || code == 0x0229 || code == 0x022B || code == 0x022D then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x023C then
                            if code == 0x022F || code == 0x0231 || 0x0233 <= code && code <= 0x0239 then
                                Just LetterLowercase

                            else if code == 0x0230 || code == 0x0232 || 0x023A <= code && code <= 0x023B then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x023C || 0x023F <= code && code <= 0x0240 || code == 0x0242 || code == 0x0247 then
                            Just LetterLowercase

                        else if 0x023D <= code && code <= 0x023E || code == 0x0241 || 0x0243 <= code && code <= 0x0246 || code == 0x0248 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x02E4 then
                        if code < 0x0293 then
                            if code == 0x0249 || code == 0x024B || code == 0x024D || 0x024F <= code && code <= 0x0292 then
                                Just LetterLowercase

                            else if code == 0x024A || code == 0x024C || code == 0x024E then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x0293 || 0x0296 <= code && code <= 0x02AF then
                            Just LetterLowercase

                        else if 0x0294 <= code && code <= 0x0295 then
                            Just LetterOther

                        else if 0x02B0 <= code && code <= 0x02C1 || 0x02C6 <= code && code <= 0x02D1 || 0x02E0 <= code && code <= 0x02E3 then
                            Just LetterModifier

                        else if 0x02C2 <= code && code <= 0x02C5 || 0x02D2 <= code && code <= 0x02DF then
                            Just SymbolModifier

                        else
                            Nothing

                    else if code < 0x036F then
                        if code == 0x02E4 || code == 0x02EC || code == 0x02EE then
                            Just LetterModifier

                        else if 0x02E5 <= code && code <= 0x02EB || code == 0x02ED || 0x02EF <= code && code <= 0x02FF then
                            Just SymbolModifier

                        else if 0x0300 <= code && code <= 0x036E then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x036F then
                        Just MarkNonSpacing

                    else if code == 0x0370 || code == 0x0372 || code == 0x0376 then
                        Just LetterUppercase

                    else if code == 0x0371 || code == 0x0373 || code == 0x0377 then
                        Just LetterLowercase

                    else if code == 0x0374 then
                        Just LetterModifier

                    else if code == 0x0375 then
                        Just SymbolModifier

                    else
                        Nothing

                else if code < 0x0461 then
                    if code < 0x03DF then
                        if code < 0x03A2 then
                            if code == 0x037A then
                                Just LetterModifier

                            else if 0x037B <= code && code <= 0x037D || code == 0x0390 then
                                Just LetterLowercase

                            else if code == 0x037E || code == 0x0387 then
                                Just PunctuationOther

                            else if code == 0x037F || code == 0x0386 || 0x0388 <= code && code <= 0x038A || code == 0x038C || 0x038E <= code && code <= 0x038F || 0x0391 <= code && code <= 0x03A1 then
                                Just LetterUppercase

                            else if 0x0384 <= code && code <= 0x0385 then
                                Just SymbolModifier

                            else
                                Nothing

                        else if code < 0x03D7 then
                            if 0x03A3 <= code && code <= 0x03AB || code == 0x03CF || 0x03D2 <= code && code <= 0x03D4 then
                                Just LetterUppercase

                            else if 0x03AC <= code && code <= 0x03CE || 0x03D0 <= code && code <= 0x03D1 || 0x03D5 <= code && code <= 0x03D6 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x03D7 || code == 0x03D9 || code == 0x03DB || code == 0x03DD then
                            Just LetterLowercase

                        else if code == 0x03D8 || code == 0x03DA || code == 0x03DC || code == 0x03DE then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x03EB then
                        if code == 0x03DF || code == 0x03E1 || code == 0x03E3 || code == 0x03E5 || code == 0x03E7 || code == 0x03E9 then
                            Just LetterLowercase

                        else if code == 0x03E0 || code == 0x03E2 || code == 0x03E4 || code == 0x03E6 || code == 0x03E8 || code == 0x03EA then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x03F5 then
                        if code == 0x03EB || code == 0x03ED || 0x03EF <= code && code <= 0x03F3 then
                            Just LetterLowercase

                        else if code == 0x03EC || code == 0x03EE || code == 0x03F4 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x03F5 || code == 0x03F8 || 0x03FB <= code && code <= 0x03FC || 0x0430 <= code && code <= 0x045F then
                        Just LetterLowercase

                    else if code == 0x03F6 then
                        Just SymbolMath

                    else if code == 0x03F7 || 0x03F9 <= code && code <= 0x03FA || 0x03FD <= code && code <= 0x042F || code == 0x0460 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x047B then
                    if code < 0x046D then
                        if code == 0x0461 || code == 0x0463 || code == 0x0465 || code == 0x0467 || code == 0x0469 || code == 0x046B then
                            Just LetterLowercase

                        else if code == 0x0462 || code == 0x0464 || code == 0x0466 || code == 0x0468 || code == 0x046A || code == 0x046C then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x0473 then
                        if code == 0x046D || code == 0x046F || code == 0x0471 then
                            Just LetterLowercase

                        else if code == 0x046E || code == 0x0470 || code == 0x0472 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x0473 || code == 0x0475 || code == 0x0477 || code == 0x0479 then
                        Just LetterLowercase

                    else if code == 0x0474 || code == 0x0476 || code == 0x0478 || code == 0x047A then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x048D then
                    if code < 0x0480 then
                        if code == 0x047B || code == 0x047D || code == 0x047F then
                            Just LetterLowercase

                        else if code == 0x047C || code == 0x047E then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x0480 || code == 0x048A || code == 0x048C then
                        Just LetterUppercase

                    else if code == 0x0481 || code == 0x048B then
                        Just LetterLowercase

                    else if code == 0x0482 then
                        Just SymbolOther

                    else if 0x0483 <= code && code <= 0x0487 then
                        Just MarkNonSpacing

                    else if 0x0488 <= code && code <= 0x0489 then
                        Just MarkEnclosing

                    else
                        Nothing

                else if code < 0x0493 then
                    if code == 0x048D || code == 0x048F || code == 0x0491 then
                        Just LetterLowercase

                    else if code == 0x048E || code == 0x0490 || code == 0x0492 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code == 0x0493 || code == 0x0495 || code == 0x0497 || code == 0x0499 || code == 0x049B then
                    Just LetterLowercase

                else if code == 0x0494 || code == 0x0496 || code == 0x0498 || code == 0x049A then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x07B0 then
                if code < 0x0505 then
                    if code < 0x04D0 then
                        if code < 0x04B4 then
                            if code < 0x04A7 then
                                if code == 0x049C || code == 0x049E || code == 0x04A0 || code == 0x04A2 || code == 0x04A4 || code == 0x04A6 then
                                    Just LetterUppercase

                                else if code == 0x049D || code == 0x049F || code == 0x04A1 || code == 0x04A3 || code == 0x04A5 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code < 0x04AC then
                                if code == 0x04A7 || code == 0x04A9 || code == 0x04AB then
                                    Just LetterLowercase

                                else if code == 0x04A8 || code == 0x04AA then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if code == 0x04AC || code == 0x04AE || code == 0x04B0 || code == 0x04B2 then
                                Just LetterUppercase

                            else if code == 0x04AD || code == 0x04AF || code == 0x04B1 || code == 0x04B3 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x04C1 then
                            if code < 0x04B9 then
                                if code == 0x04B4 || code == 0x04B6 || code == 0x04B8 then
                                    Just LetterUppercase

                                else if code == 0x04B5 || code == 0x04B7 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if code == 0x04B9 || code == 0x04BB || code == 0x04BD || code == 0x04BF then
                                Just LetterLowercase

                            else if code == 0x04BA || code == 0x04BC || code == 0x04BE || code == 0x04C0 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x04C7 then
                            if code == 0x04C1 || code == 0x04C3 || code == 0x04C5 then
                                Just LetterUppercase

                            else if code == 0x04C2 || code == 0x04C4 || code == 0x04C6 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x04C7 || code == 0x04C9 || code == 0x04CB || code == 0x04CD then
                            Just LetterUppercase

                        else if code == 0x04C8 || code == 0x04CA || code == 0x04CC || 0x04CE <= code && code <= 0x04CF then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x04E9 then
                        if code < 0x04DB then
                            if code == 0x04D0 || code == 0x04D2 || code == 0x04D4 || code == 0x04D6 || code == 0x04D8 || code == 0x04DA then
                                Just LetterUppercase

                            else if code == 0x04D1 || code == 0x04D3 || code == 0x04D5 || code == 0x04D7 || code == 0x04D9 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x04E1 then
                            if code == 0x04DB || code == 0x04DD || code == 0x04DF then
                                Just LetterLowercase

                            else if code == 0x04DC || code == 0x04DE || code == 0x04E0 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x04E1 || code == 0x04E3 || code == 0x04E5 || code == 0x04E7 then
                            Just LetterLowercase

                        else if code == 0x04E2 || code == 0x04E4 || code == 0x04E6 || code == 0x04E8 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x04F6 then
                        if code < 0x04EE then
                            if code == 0x04E9 || code == 0x04EB || code == 0x04ED then
                                Just LetterLowercase

                            else if code == 0x04EA || code == 0x04EC then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x04EE || code == 0x04F0 || code == 0x04F2 || code == 0x04F4 then
                            Just LetterUppercase

                        else if code == 0x04EF || code == 0x04F1 || code == 0x04F3 || code == 0x04F5 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x04FC then
                        if code == 0x04F6 || code == 0x04F8 || code == 0x04FA then
                            Just LetterUppercase

                        else if code == 0x04F7 || code == 0x04F9 || code == 0x04FB then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0x04FC || code == 0x04FE || code == 0x0500 || code == 0x0502 || code == 0x0504 then
                        Just LetterUppercase

                    else if code == 0x04FD || code == 0x04FF || code == 0x0501 || code == 0x0503 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x05BD then
                    if code < 0x051E then
                        if code < 0x0510 then
                            if code == 0x0505 || code == 0x0507 || code == 0x0509 || code == 0x050B || code == 0x050D || code == 0x050F then
                                Just LetterLowercase

                            else if code == 0x0506 || code == 0x0508 || code == 0x050A || code == 0x050C || code == 0x050E then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x0516 then
                            if code == 0x0510 || code == 0x0512 || code == 0x0514 then
                                Just LetterUppercase

                            else if code == 0x0511 || code == 0x0513 || code == 0x0515 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x0516 || code == 0x0518 || code == 0x051A || code == 0x051C then
                            Just LetterUppercase

                        else if code == 0x0517 || code == 0x0519 || code == 0x051B || code == 0x051D then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x052A then
                        if code == 0x051E || code == 0x0520 || code == 0x0522 || code == 0x0524 || code == 0x0526 || code == 0x0528 then
                            Just LetterUppercase

                        else if code == 0x051F || code == 0x0521 || code == 0x0523 || code == 0x0525 || code == 0x0527 || code == 0x0529 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x0558 then
                        if code == 0x052A || code == 0x052C || code == 0x052E || 0x0531 <= code && code <= 0x0556 then
                            Just LetterUppercase

                        else if code == 0x052B || code == 0x052D || code == 0x052F then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0x0559 then
                        Just LetterModifier

                    else if 0x055A <= code && code <= 0x055F || code == 0x0589 then
                        Just PunctuationOther

                    else if 0x0560 <= code && code <= 0x0588 then
                        Just LetterLowercase

                    else if code == 0x058A then
                        Just PunctuationDash

                    else if 0x058D <= code && code <= 0x058E then
                        Just SymbolOther

                    else if code == 0x058F then
                        Just SymbolCurrency

                    else if 0x0591 <= code && code <= 0x05BC then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x0669 then
                    if code < 0x0605 then
                        if code < 0x05C3 then
                            if code == 0x05BD || code == 0x05BF || 0x05C1 <= code && code <= 0x05C2 then
                                Just MarkNonSpacing

                            else if code == 0x05BE then
                                Just PunctuationDash

                            else if code == 0x05C0 then
                                Just PunctuationOther

                            else
                                Nothing

                        else if code == 0x05C3 || code == 0x05C6 || 0x05F3 <= code && code <= 0x05F4 then
                            Just PunctuationOther

                        else if 0x05C4 <= code && code <= 0x05C5 || code == 0x05C7 then
                            Just MarkNonSpacing

                        else if 0x05D0 <= code && code <= 0x05EA || 0x05EF <= code && code <= 0x05F2 then
                            Just LetterOther

                        else if 0x0600 <= code && code <= 0x0604 then
                            Just OtherFormat

                        else
                            Nothing

                    else if code < 0x061A then
                        if code == 0x0605 then
                            Just OtherFormat

                        else if 0x0606 <= code && code <= 0x0608 then
                            Just SymbolMath

                        else if 0x0609 <= code && code <= 0x060A || 0x060C <= code && code <= 0x060D then
                            Just PunctuationOther

                        else if code == 0x060B then
                            Just SymbolCurrency

                        else if 0x060E <= code && code <= 0x060F then
                            Just SymbolOther

                        else if 0x0610 <= code && code <= 0x0619 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x061A || 0x064B <= code && code <= 0x065F then
                        Just MarkNonSpacing

                    else if code == 0x061B || 0x061D <= code && code <= 0x061F then
                        Just PunctuationOther

                    else if code == 0x061C then
                        Just OtherFormat

                    else if 0x0620 <= code && code <= 0x063F || 0x0641 <= code && code <= 0x064A then
                        Just LetterOther

                    else if code == 0x0640 then
                        Just LetterModifier

                    else if 0x0660 <= code && code <= 0x0668 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code < 0x06E9 then
                    if code < 0x06D4 then
                        if code == 0x0669 then
                            Just NumberDecimalDigit

                        else if 0x066A <= code && code <= 0x066D then
                            Just PunctuationOther

                        else if 0x066E <= code && code <= 0x066F || 0x0671 <= code && code <= 0x06D3 then
                            Just LetterOther

                        else if code == 0x0670 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x06D4 then
                        Just PunctuationOther

                    else if code == 0x06D5 then
                        Just LetterOther

                    else if 0x06D6 <= code && code <= 0x06DC || 0x06DF <= code && code <= 0x06E4 || 0x06E7 <= code && code <= 0x06E8 then
                        Just MarkNonSpacing

                    else if code == 0x06DD then
                        Just OtherFormat

                    else if code == 0x06DE then
                        Just SymbolOther

                    else if 0x06E5 <= code && code <= 0x06E6 then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0x06FF then
                    if code == 0x06E9 || 0x06FD <= code && code <= 0x06FE then
                        Just SymbolOther

                    else if 0x06EA <= code && code <= 0x06ED then
                        Just MarkNonSpacing

                    else if 0x06EE <= code && code <= 0x06EF || 0x06FA <= code && code <= 0x06FC then
                        Just LetterOther

                    else if 0x06F0 <= code && code <= 0x06F9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code == 0x06FF || code == 0x0710 || 0x0712 <= code && code <= 0x072F || 0x074D <= code && code <= 0x07A5 then
                    Just LetterOther

                else if 0x0700 <= code && code <= 0x070D then
                    Just PunctuationOther

                else if code == 0x070F then
                    Just OtherFormat

                else if code == 0x0711 || 0x0730 <= code && code <= 0x074A || 0x07A6 <= code && code <= 0x07AF then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x0A80 then
                if code < 0x0970 then
                    if code < 0x0887 then
                        if code < 0x0815 then
                            if code == 0x07B0 || 0x07EB <= code && code <= 0x07F3 || code == 0x07FD then
                                Just MarkNonSpacing

                            else if code == 0x07B1 || 0x07CA <= code && code <= 0x07EA || 0x0800 <= code && code <= 0x0814 then
                                Just LetterOther

                            else if 0x07C0 <= code && code <= 0x07C9 then
                                Just NumberDecimalDigit

                            else if 0x07F4 <= code && code <= 0x07F5 || code == 0x07FA then
                                Just LetterModifier

                            else if code == 0x07F6 then
                                Just SymbolOther

                            else if 0x07F7 <= code && code <= 0x07F9 then
                                Just PunctuationOther

                            else if 0x07FE <= code && code <= 0x07FF then
                                Just SymbolCurrency

                            else
                                Nothing

                        else if code < 0x0828 then
                            if code == 0x0815 then
                                Just LetterOther

                            else if 0x0816 <= code && code <= 0x0819 || 0x081B <= code && code <= 0x0823 || 0x0825 <= code && code <= 0x0827 then
                                Just MarkNonSpacing

                            else if code == 0x081A || code == 0x0824 then
                                Just LetterModifier

                            else
                                Nothing

                        else if code == 0x0828 then
                            Just LetterModifier

                        else if 0x0829 <= code && code <= 0x082D || 0x0859 <= code && code <= 0x085B then
                            Just MarkNonSpacing

                        else if 0x0830 <= code && code <= 0x083E || code == 0x085E then
                            Just PunctuationOther

                        else if 0x0840 <= code && code <= 0x0858 || 0x0860 <= code && code <= 0x086A || 0x0870 <= code && code <= 0x0886 then
                            Just LetterOther

                        else
                            Nothing

                    else if code < 0x093A then
                        if code == 0x0887 || 0x0889 <= code && code <= 0x088F || 0x08A0 <= code && code <= 0x08C8 || 0x0904 <= code && code <= 0x0939 then
                            Just LetterOther

                        else if code == 0x0888 then
                            Just SymbolModifier

                        else if 0x0890 <= code && code <= 0x0891 || code == 0x08E2 then
                            Just OtherFormat

                        else if 0x0897 <= code && code <= 0x089F || 0x08CA <= code && code <= 0x08E1 || 0x08E3 <= code && code <= 0x0902 then
                            Just MarkNonSpacing

                        else if code == 0x08C9 then
                            Just LetterModifier

                        else if code == 0x0903 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x094C then
                        if code == 0x093A || code == 0x093C || 0x0941 <= code && code <= 0x0948 then
                            Just MarkNonSpacing

                        else if code == 0x093B || 0x093E <= code && code <= 0x0940 || 0x0949 <= code && code <= 0x094B then
                            Just MarkSpacingCombining

                        else if code == 0x093D then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x094C || 0x094E <= code && code <= 0x094F then
                        Just MarkSpacingCombining

                    else if code == 0x094D || 0x0951 <= code && code <= 0x0957 || 0x0962 <= code && code <= 0x0963 then
                        Just MarkNonSpacing

                    else if code == 0x0950 || 0x0958 <= code && code <= 0x0961 then
                        Just LetterOther

                    else if 0x0964 <= code && code <= 0x0965 then
                        Just PunctuationOther

                    else if 0x0966 <= code && code <= 0x096F then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code < 0x09F9 then
                    if code < 0x09BD then
                        if code == 0x0970 then
                            Just PunctuationOther

                        else if code == 0x0971 then
                            Just LetterModifier

                        else if 0x0972 <= code && code <= 0x0980 || 0x0985 <= code && code <= 0x098C || 0x098F <= code && code <= 0x0990 || 0x0993 <= code && code <= 0x09A8 || 0x09AA <= code && code <= 0x09B0 || code == 0x09B2 || 0x09B6 <= code && code <= 0x09B9 then
                            Just LetterOther

                        else if code == 0x0981 || code == 0x09BC then
                            Just MarkNonSpacing

                        else if 0x0982 <= code && code <= 0x0983 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x09D6 then
                        if code == 0x09BD || code == 0x09CE then
                            Just LetterOther

                        else if 0x09BE <= code && code <= 0x09C0 || 0x09C7 <= code && code <= 0x09C8 || 0x09CB <= code && code <= 0x09CC then
                            Just MarkSpacingCombining

                        else if 0x09C1 <= code && code <= 0x09C4 || code == 0x09CD then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x09D7 then
                        Just MarkSpacingCombining

                    else if 0x09DC <= code && code <= 0x09DD || 0x09DF <= code && code <= 0x09E1 || 0x09F0 <= code && code <= 0x09F1 then
                        Just LetterOther

                    else if 0x09E2 <= code && code <= 0x09E3 then
                        Just MarkNonSpacing

                    else if 0x09E6 <= code && code <= 0x09EF then
                        Just NumberDecimalDigit

                    else if 0x09F2 <= code && code <= 0x09F3 then
                        Just SymbolCurrency

                    else if 0x09F4 <= code && code <= 0x09F8 then
                        Just NumberOther

                    else
                        Nothing

                else if code < 0x0A37 then
                    if code < 0x0A02 then
                        if code == 0x09F9 then
                            Just NumberOther

                        else if code == 0x09FA then
                            Just SymbolOther

                        else if code == 0x09FB then
                            Just SymbolCurrency

                        else if code == 0x09FC then
                            Just LetterOther

                        else if code == 0x09FD then
                            Just PunctuationOther

                        else if code == 0x09FE || code == 0x0A01 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x0A02 then
                        Just MarkNonSpacing

                    else if code == 0x0A03 then
                        Just MarkSpacingCombining

                    else if 0x0A05 <= code && code <= 0x0A0A || 0x0A0F <= code && code <= 0x0A10 || 0x0A13 <= code && code <= 0x0A28 || 0x0A2A <= code && code <= 0x0A30 || 0x0A32 <= code && code <= 0x0A33 || 0x0A35 <= code && code <= 0x0A36 then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x0A58 then
                    if 0x0A38 <= code && code <= 0x0A39 then
                        Just LetterOther

                    else if code == 0x0A3C || 0x0A41 <= code && code <= 0x0A42 || 0x0A47 <= code && code <= 0x0A48 || 0x0A4B <= code && code <= 0x0A4D || code == 0x0A51 then
                        Just MarkNonSpacing

                    else if 0x0A3E <= code && code <= 0x0A40 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if 0x0A59 <= code && code <= 0x0A5C || code == 0x0A5E || 0x0A72 <= code && code <= 0x0A74 then
                    Just LetterOther

                else if 0x0A66 <= code && code <= 0x0A6F then
                    Just NumberDecimalDigit

                else if 0x0A70 <= code && code <= 0x0A71 || code == 0x0A75 then
                    Just MarkNonSpacing

                else if code == 0x0A76 then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x0B84 then
                if code < 0x0B04 then
                    if code < 0x0AC8 then
                        if code < 0x0AB1 then
                            if 0x0A81 <= code && code <= 0x0A82 then
                                Just MarkNonSpacing

                            else if code == 0x0A83 then
                                Just MarkSpacingCombining

                            else if 0x0A85 <= code && code <= 0x0A8D || 0x0A8F <= code && code <= 0x0A91 || 0x0A93 <= code && code <= 0x0AA8 || 0x0AAA <= code && code <= 0x0AB0 then
                                Just LetterOther

                            else
                                Nothing

                        else if 0x0AB2 <= code && code <= 0x0AB3 || 0x0AB5 <= code && code <= 0x0AB9 || code == 0x0ABD then
                            Just LetterOther

                        else if code == 0x0ABC || 0x0AC1 <= code && code <= 0x0AC5 || code == 0x0AC7 then
                            Just MarkNonSpacing

                        else if 0x0ABE <= code && code <= 0x0AC0 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x0AE5 then
                        if code == 0x0AC8 || code == 0x0ACD || 0x0AE2 <= code && code <= 0x0AE3 then
                            Just MarkNonSpacing

                        else if code == 0x0AC9 || 0x0ACB <= code && code <= 0x0ACC then
                            Just MarkSpacingCombining

                        else if code == 0x0AD0 || 0x0AE0 <= code && code <= 0x0AE1 then
                            Just LetterOther

                        else
                            Nothing

                    else if 0x0AE6 <= code && code <= 0x0AEF then
                        Just NumberDecimalDigit

                    else if code == 0x0AF0 then
                        Just PunctuationOther

                    else if code == 0x0AF1 then
                        Just SymbolCurrency

                    else if code == 0x0AF9 then
                        Just LetterOther

                    else if 0x0AFA <= code && code <= 0x0AFF || code == 0x0B01 then
                        Just MarkNonSpacing

                    else if 0x0B02 <= code && code <= 0x0B03 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x0B4A then
                    if code < 0x0B3B then
                        if 0x0B05 <= code && code <= 0x0B0C || 0x0B0F <= code && code <= 0x0B10 || 0x0B13 <= code && code <= 0x0B28 || 0x0B2A <= code && code <= 0x0B30 || 0x0B32 <= code && code <= 0x0B33 || 0x0B35 <= code && code <= 0x0B39 then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x0B3C || code == 0x0B3F || 0x0B41 <= code && code <= 0x0B44 then
                        Just MarkNonSpacing

                    else if code == 0x0B3D then
                        Just LetterOther

                    else if code == 0x0B3E || code == 0x0B40 || 0x0B47 <= code && code <= 0x0B48 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x0B61 then
                    if 0x0B4B <= code && code <= 0x0B4C || code == 0x0B57 then
                        Just MarkSpacingCombining

                    else if code == 0x0B4D || 0x0B55 <= code && code <= 0x0B56 then
                        Just MarkNonSpacing

                    else if 0x0B5C <= code && code <= 0x0B5D || 0x0B5F <= code && code <= 0x0B60 then
                        Just LetterOther

                    else
                        Nothing

                else if code == 0x0B61 || code == 0x0B71 || code == 0x0B83 then
                    Just LetterOther

                else if 0x0B62 <= code && code <= 0x0B63 || code == 0x0B82 then
                    Just MarkNonSpacing

                else if 0x0B66 <= code && code <= 0x0B6F then
                    Just NumberDecimalDigit

                else if code == 0x0B70 then
                    Just SymbolOther

                else if 0x0B72 <= code && code <= 0x0B77 then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x0C0D then
                if code < 0x0BC9 then
                    if code < 0x0BA2 then
                        if 0x0B85 <= code && code <= 0x0B8A || 0x0B8E <= code && code <= 0x0B90 || 0x0B92 <= code && code <= 0x0B95 || 0x0B99 <= code && code <= 0x0B9A || code == 0x0B9C || 0x0B9E <= code && code <= 0x0B9F then
                            Just LetterOther

                        else
                            Nothing

                    else if 0x0BA3 <= code && code <= 0x0BA4 || 0x0BA8 <= code && code <= 0x0BAA || 0x0BAE <= code && code <= 0x0BB9 then
                        Just LetterOther

                    else if 0x0BBE <= code && code <= 0x0BBF || 0x0BC1 <= code && code <= 0x0BC2 || 0x0BC6 <= code && code <= 0x0BC8 then
                        Just MarkSpacingCombining

                    else if code == 0x0BC0 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x0BF2 then
                    if 0x0BCA <= code && code <= 0x0BCC || code == 0x0BD7 then
                        Just MarkSpacingCombining

                    else if code == 0x0BCD then
                        Just MarkNonSpacing

                    else if code == 0x0BD0 then
                        Just LetterOther

                    else if 0x0BE6 <= code && code <= 0x0BEF then
                        Just NumberDecimalDigit

                    else if 0x0BF0 <= code && code <= 0x0BF1 then
                        Just NumberOther

                    else
                        Nothing

                else if code == 0x0BF2 then
                    Just NumberOther

                else if 0x0BF3 <= code && code <= 0x0BF8 || code == 0x0BFA then
                    Just SymbolOther

                else if code == 0x0BF9 then
                    Just SymbolCurrency

                else if code == 0x0C00 || code == 0x0C04 then
                    Just MarkNonSpacing

                else if 0x0C01 <= code && code <= 0x0C03 then
                    Just MarkSpacingCombining

                else if 0x0C05 <= code && code <= 0x0C0C then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x0C61 then
                if code < 0x0C40 then
                    if 0x0C0E <= code && code <= 0x0C10 || 0x0C12 <= code && code <= 0x0C28 || 0x0C2A <= code && code <= 0x0C39 || code == 0x0C3D then
                        Just LetterOther

                    else if code == 0x0C3C || 0x0C3E <= code && code <= 0x0C3F then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x0C40 || 0x0C46 <= code && code <= 0x0C48 || 0x0C4A <= code && code <= 0x0C4D || 0x0C55 <= code && code <= 0x0C56 then
                    Just MarkNonSpacing

                else if 0x0C41 <= code && code <= 0x0C44 then
                    Just MarkSpacingCombining

                else if 0x0C58 <= code && code <= 0x0C5A || 0x0C5C <= code && code <= 0x0C5D || code == 0x0C60 then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x0C80 then
                if code == 0x0C61 then
                    Just LetterOther

                else if 0x0C62 <= code && code <= 0x0C63 then
                    Just MarkNonSpacing

                else if 0x0C66 <= code && code <= 0x0C6F then
                    Just NumberDecimalDigit

                else if code == 0x0C77 then
                    Just PunctuationOther

                else if 0x0C78 <= code && code <= 0x0C7E then
                    Just NumberOther

                else if code == 0x0C7F then
                    Just SymbolOther

                else
                    Nothing

            else if code == 0x0C80 || 0x0C85 <= code && code <= 0x0C8C || 0x0C8E <= code && code <= 0x0C90 || 0x0C92 <= code && code <= 0x0CA8 || 0x0CAA <= code && code <= 0x0CB3 || 0x0CB5 <= code && code <= 0x0CB9 then
                Just LetterOther

            else if code == 0x0C81 then
                Just MarkNonSpacing

            else if 0x0C82 <= code && code <= 0x0C83 then
                Just MarkSpacingCombining

            else if code == 0x0C84 then
                Just PunctuationOther

            else
                Nothing

        else if code < 0x1E34 then
            if code < 0x177F then
                if code < 0x0F7E then
                    if code < 0x0DF1 then
                        if code < 0x0D49 then
                            if code < 0x0CE5 then
                                if code < 0x0CC6 then
                                    if code == 0x0CBC || code == 0x0CBF then
                                        Just MarkNonSpacing

                                    else if code == 0x0CBD then
                                        Just LetterOther

                                    else if code == 0x0CBE || 0x0CC0 <= code && code <= 0x0CC4 then
                                        Just MarkSpacingCombining

                                    else
                                        Nothing

                                else if code == 0x0CC6 || 0x0CCC <= code && code <= 0x0CCD || 0x0CE2 <= code && code <= 0x0CE3 then
                                    Just MarkNonSpacing

                                else if 0x0CC7 <= code && code <= 0x0CC8 || 0x0CCA <= code && code <= 0x0CCB || 0x0CD5 <= code && code <= 0x0CD6 then
                                    Just MarkSpacingCombining

                                else if 0x0CDC <= code && code <= 0x0CDE || 0x0CE0 <= code && code <= 0x0CE1 then
                                    Just LetterOther

                                else
                                    Nothing

                            else if code < 0x0D0D then
                                if 0x0CE6 <= code && code <= 0x0CEF then
                                    Just NumberDecimalDigit

                                else if 0x0CF1 <= code && code <= 0x0CF2 || 0x0D04 <= code && code <= 0x0D0C then
                                    Just LetterOther

                                else if code == 0x0CF3 || 0x0D02 <= code && code <= 0x0D03 then
                                    Just MarkSpacingCombining

                                else if 0x0D00 <= code && code <= 0x0D01 then
                                    Just MarkNonSpacing

                                else
                                    Nothing

                            else if 0x0D0E <= code && code <= 0x0D10 || 0x0D12 <= code && code <= 0x0D3A || code == 0x0D3D then
                                Just LetterOther

                            else if 0x0D3B <= code && code <= 0x0D3C || 0x0D41 <= code && code <= 0x0D44 then
                                Just MarkNonSpacing

                            else if 0x0D3E <= code && code <= 0x0D40 || 0x0D46 <= code && code <= 0x0D48 then
                                Just MarkSpacingCombining

                            else
                                Nothing

                        else if code < 0x0D80 then
                            if code < 0x0D57 then
                                if 0x0D4A <= code && code <= 0x0D4C then
                                    Just MarkSpacingCombining

                                else if code == 0x0D4D then
                                    Just MarkNonSpacing

                                else if code == 0x0D4E || 0x0D54 <= code && code <= 0x0D56 then
                                    Just LetterOther

                                else if code == 0x0D4F then
                                    Just SymbolOther

                                else
                                    Nothing

                            else if code == 0x0D57 then
                                Just MarkSpacingCombining

                            else if 0x0D58 <= code && code <= 0x0D5E || 0x0D70 <= code && code <= 0x0D78 then
                                Just NumberOther

                            else if 0x0D5F <= code && code <= 0x0D61 || 0x0D7A <= code && code <= 0x0D7F then
                                Just LetterOther

                            else if 0x0D62 <= code && code <= 0x0D63 then
                                Just MarkNonSpacing

                            else if 0x0D66 <= code && code <= 0x0D6F then
                                Just NumberDecimalDigit

                            else if code == 0x0D79 then
                                Just SymbolOther

                            else
                                Nothing

                        else if code < 0x0DBF then
                            if code == 0x0D81 then
                                Just MarkNonSpacing

                            else if 0x0D82 <= code && code <= 0x0D83 then
                                Just MarkSpacingCombining

                            else if 0x0D85 <= code && code <= 0x0D96 || 0x0D9A <= code && code <= 0x0DB1 || 0x0DB3 <= code && code <= 0x0DBB || code == 0x0DBD then
                                Just LetterOther

                            else
                                Nothing

                        else if 0x0DC0 <= code && code <= 0x0DC6 then
                            Just LetterOther

                        else if code == 0x0DCA || 0x0DD2 <= code && code <= 0x0DD4 || code == 0x0DD6 then
                            Just MarkNonSpacing

                        else if 0x0DCF <= code && code <= 0x0DD1 || 0x0DD8 <= code && code <= 0x0DDF then
                            Just MarkSpacingCombining

                        else if 0x0DE6 <= code && code <= 0x0DEF then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if code < 0x0ECF then
                        if code < 0x0E80 then
                            if code < 0x0E3E then
                                if 0x0DF2 <= code && code <= 0x0DF3 then
                                    Just MarkSpacingCombining

                                else if code == 0x0DF4 then
                                    Just PunctuationOther

                                else if 0x0E01 <= code && code <= 0x0E30 || 0x0E32 <= code && code <= 0x0E33 then
                                    Just LetterOther

                                else if code == 0x0E31 || 0x0E34 <= code && code <= 0x0E3A then
                                    Just MarkNonSpacing

                                else
                                    Nothing

                            else if code == 0x0E3F then
                                Just SymbolCurrency

                            else if 0x0E40 <= code && code <= 0x0E45 then
                                Just LetterOther

                            else if code == 0x0E46 then
                                Just LetterModifier

                            else if 0x0E47 <= code && code <= 0x0E4E then
                                Just MarkNonSpacing

                            else if code == 0x0E4F || 0x0E5A <= code && code <= 0x0E5B then
                                Just PunctuationOther

                            else if 0x0E50 <= code && code <= 0x0E59 then
                                Just NumberDecimalDigit

                            else
                                Nothing

                        else if code < 0x0EB0 then
                            if 0x0E81 <= code && code <= 0x0E82 || code == 0x0E84 || 0x0E86 <= code && code <= 0x0E8A || 0x0E8C <= code && code <= 0x0EA3 || code == 0x0EA5 || 0x0EA7 <= code && code <= 0x0EAF then
                                Just LetterOther

                            else
                                Nothing

                        else if code == 0x0EB0 || 0x0EB2 <= code && code <= 0x0EB3 || code == 0x0EBD || 0x0EC0 <= code && code <= 0x0EC4 then
                            Just LetterOther

                        else if code == 0x0EB1 || 0x0EB4 <= code && code <= 0x0EBC || 0x0EC8 <= code && code <= 0x0ECE then
                            Just MarkNonSpacing

                        else if code == 0x0EC6 then
                            Just LetterModifier

                        else
                            Nothing

                    else if code < 0x0F34 then
                        if 0x0ED0 <= code && code <= 0x0ED9 || 0x0F20 <= code && code <= 0x0F29 then
                            Just NumberDecimalDigit

                        else if 0x0EDC <= code && code <= 0x0EDF || code == 0x0F00 then
                            Just LetterOther

                        else if 0x0F01 <= code && code <= 0x0F03 || code == 0x0F13 || 0x0F15 <= code && code <= 0x0F17 || 0x0F1A <= code && code <= 0x0F1F then
                            Just SymbolOther

                        else if 0x0F04 <= code && code <= 0x0F12 || code == 0x0F14 then
                            Just PunctuationOther

                        else if 0x0F18 <= code && code <= 0x0F19 then
                            Just MarkNonSpacing

                        else if 0x0F2A <= code && code <= 0x0F33 then
                            Just NumberOther

                        else
                            Nothing

                    else if code < 0x0F3A then
                        if code == 0x0F34 || code == 0x0F36 || code == 0x0F38 then
                            Just SymbolOther

                        else if code == 0x0F35 || code == 0x0F37 || code == 0x0F39 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x0F3A || code == 0x0F3C then
                        Just PunctuationOpen

                    else if code == 0x0F3B || code == 0x0F3D then
                        Just PunctuationClose

                    else if 0x0F3E <= code && code <= 0x0F3F then
                        Just MarkSpacingCombining

                    else if 0x0F40 <= code && code <= 0x0F47 || 0x0F49 <= code && code <= 0x0F6C then
                        Just LetterOther

                    else if 0x0F71 <= code && code <= 0x0F7D then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x10CC then
                    if code < 0x1049 then
                        if code < 0x0FD4 then
                            if code < 0x0F8C then
                                if code == 0x0F7E || 0x0F80 <= code && code <= 0x0F84 || 0x0F86 <= code && code <= 0x0F87 then
                                    Just MarkNonSpacing

                                else if code == 0x0F7F then
                                    Just MarkSpacingCombining

                                else if code == 0x0F85 then
                                    Just PunctuationOther

                                else if 0x0F88 <= code && code <= 0x0F8B then
                                    Just LetterOther

                                else
                                    Nothing

                            else if code == 0x0F8C then
                                Just LetterOther

                            else if 0x0F8D <= code && code <= 0x0F97 || 0x0F99 <= code && code <= 0x0FBC || code == 0x0FC6 then
                                Just MarkNonSpacing

                            else if 0x0FBE <= code && code <= 0x0FC5 || 0x0FC7 <= code && code <= 0x0FCC || 0x0FCE <= code && code <= 0x0FCF then
                                Just SymbolOther

                            else if 0x0FD0 <= code && code <= 0x0FD3 then
                                Just PunctuationOther

                            else
                                Nothing

                        else if code < 0x1031 then
                            if code == 0x0FD4 || 0x0FD9 <= code && code <= 0x0FDA then
                                Just PunctuationOther

                            else if 0x0FD5 <= code && code <= 0x0FD8 then
                                Just SymbolOther

                            else if 0x1000 <= code && code <= 0x102A then
                                Just LetterOther

                            else if 0x102B <= code && code <= 0x102C then
                                Just MarkSpacingCombining

                            else if 0x102D <= code && code <= 0x1030 then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if code == 0x1031 || code == 0x1038 || 0x103B <= code && code <= 0x103C then
                            Just MarkSpacingCombining

                        else if 0x1032 <= code && code <= 0x1037 || 0x1039 <= code && code <= 0x103A || 0x103D <= code && code <= 0x103E then
                            Just MarkNonSpacing

                        else if code == 0x103F then
                            Just LetterOther

                        else if 0x1040 <= code && code <= 0x1048 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if code < 0x1074 then
                        if code < 0x105D then
                            if code == 0x1049 then
                                Just NumberDecimalDigit

                            else if 0x104A <= code && code <= 0x104F then
                                Just PunctuationOther

                            else if 0x1050 <= code && code <= 0x1055 || 0x105A <= code && code <= 0x105C then
                                Just LetterOther

                            else if 0x1056 <= code && code <= 0x1057 then
                                Just MarkSpacingCombining

                            else if 0x1058 <= code && code <= 0x1059 then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if code == 0x105D || code == 0x1061 || 0x1065 <= code && code <= 0x1066 || 0x106E <= code && code <= 0x1070 then
                            Just LetterOther

                        else if 0x105E <= code && code <= 0x1060 || 0x1071 <= code && code <= 0x1073 then
                            Just MarkNonSpacing

                        else if 0x1062 <= code && code <= 0x1064 || 0x1067 <= code && code <= 0x106D then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x108D then
                        if code == 0x1074 || code == 0x1082 || 0x1085 <= code && code <= 0x1086 then
                            Just MarkNonSpacing

                        else if 0x1075 <= code && code <= 0x1081 then
                            Just LetterOther

                        else if 0x1083 <= code && code <= 0x1084 || 0x1087 <= code && code <= 0x108C then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code == 0x108D || code == 0x109D then
                        Just MarkNonSpacing

                    else if code == 0x108E then
                        Just LetterOther

                    else if code == 0x108F || 0x109A <= code && code <= 0x109C then
                        Just MarkSpacingCombining

                    else if 0x1090 <= code && code <= 0x1099 then
                        Just NumberDecimalDigit

                    else if 0x109E <= code && code <= 0x109F then
                        Just SymbolOther

                    else if 0x10A0 <= code && code <= 0x10C5 || code == 0x10C7 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x139F then
                    if code < 0x12B1 then
                        if code < 0x1249 then
                            if code == 0x10CD then
                                Just LetterUppercase

                            else if 0x10D0 <= code && code <= 0x10FA || 0x10FD <= code && code <= 0x10FF then
                                Just LetterLowercase

                            else if code == 0x10FB then
                                Just PunctuationOther

                            else if code == 0x10FC then
                                Just LetterModifier

                            else if 0x1100 <= code && code <= 0x1248 then
                                Just LetterOther

                            else
                                Nothing

                        else if 0x124A <= code && code <= 0x124D || 0x1250 <= code && code <= 0x1256 || code == 0x1258 || 0x125A <= code && code <= 0x125D || 0x1260 <= code && code <= 0x1288 || 0x128A <= code && code <= 0x128D || 0x1290 <= code && code <= 0x12B0 then
                            Just LetterOther

                        else
                            Nothing

                    else if code < 0x1311 then
                        if 0x12B2 <= code && code <= 0x12B5 || 0x12B8 <= code && code <= 0x12BE || code == 0x12C0 || 0x12C2 <= code && code <= 0x12C5 || 0x12C8 <= code && code <= 0x12D6 || 0x12D8 <= code && code <= 0x1310 then
                            Just LetterOther

                        else
                            Nothing

                    else if 0x1312 <= code && code <= 0x1315 || 0x1318 <= code && code <= 0x135A || 0x1380 <= code && code <= 0x138F then
                        Just LetterOther

                    else if 0x135D <= code && code <= 0x135F then
                        Just MarkNonSpacing

                    else if 0x1360 <= code && code <= 0x1368 then
                        Just PunctuationOther

                    else if 0x1369 <= code && code <= 0x137C then
                        Just NumberOther

                    else if 0x1390 <= code && code <= 0x1399 then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0x16ED then
                    if code < 0x166E then
                        if 0x13A0 <= code && code <= 0x13F5 then
                            Just LetterUppercase

                        else if 0x13F8 <= code && code <= 0x13FD then
                            Just LetterLowercase

                        else if code == 0x1400 then
                            Just PunctuationDash

                        else if 0x1401 <= code && code <= 0x166C then
                            Just LetterOther

                        else if code == 0x166D then
                            Just SymbolOther

                        else
                            Nothing

                    else if code == 0x166E || 0x16EB <= code && code <= 0x16EC then
                        Just PunctuationOther

                    else if 0x166F <= code && code <= 0x167F || 0x1681 <= code && code <= 0x169A || 0x16A0 <= code && code <= 0x16EA then
                        Just LetterOther

                    else if code == 0x1680 then
                        Just SeparatorSpace

                    else if code == 0x169B then
                        Just PunctuationOpen

                    else if code == 0x169C then
                        Just PunctuationClose

                    else
                        Nothing

                else if code < 0x1731 then
                    if code == 0x16ED then
                        Just PunctuationOther

                    else if 0x16EE <= code && code <= 0x16F0 then
                        Just NumberLetter

                    else if 0x16F1 <= code && code <= 0x16F8 || 0x1700 <= code && code <= 0x1711 || 0x171F <= code && code <= 0x1730 then
                        Just LetterOther

                    else if 0x1712 <= code && code <= 0x1714 then
                        Just MarkNonSpacing

                    else if code == 0x1715 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code == 0x1731 || 0x1740 <= code && code <= 0x1751 || 0x1760 <= code && code <= 0x176C || 0x176E <= code && code <= 0x1770 then
                    Just LetterOther

                else if 0x1732 <= code && code <= 0x1733 || 0x1752 <= code && code <= 0x1753 || 0x1772 <= code && code <= 0x1773 then
                    Just MarkNonSpacing

                else if code == 0x1734 then
                    Just MarkSpacingCombining

                else if 0x1735 <= code && code <= 0x1736 then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x1BA9 then
                if code < 0x1A16 then
                    if code < 0x187F then
                        if code < 0x17DC then
                            if 0x1780 <= code && code <= 0x17B3 then
                                Just LetterOther

                            else if 0x17B4 <= code && code <= 0x17B5 || 0x17B7 <= code && code <= 0x17BD || code == 0x17C6 || 0x17C9 <= code && code <= 0x17D3 then
                                Just MarkNonSpacing

                            else if code == 0x17B6 || 0x17BE <= code && code <= 0x17C5 || 0x17C7 <= code && code <= 0x17C8 then
                                Just MarkSpacingCombining

                            else if 0x17D4 <= code && code <= 0x17D6 || 0x17D8 <= code && code <= 0x17DA then
                                Just PunctuationOther

                            else if code == 0x17D7 then
                                Just LetterModifier

                            else if code == 0x17DB then
                                Just SymbolCurrency

                            else
                                Nothing

                        else if code < 0x180A then
                            if code == 0x17DC then
                                Just LetterOther

                            else if code == 0x17DD then
                                Just MarkNonSpacing

                            else if 0x17E0 <= code && code <= 0x17E9 then
                                Just NumberDecimalDigit

                            else if 0x17F0 <= code && code <= 0x17F9 then
                                Just NumberOther

                            else if 0x1800 <= code && code <= 0x1805 || 0x1807 <= code && code <= 0x1809 then
                                Just PunctuationOther

                            else if code == 0x1806 then
                                Just PunctuationDash

                            else
                                Nothing

                        else if code == 0x180A then
                            Just PunctuationOther

                        else if 0x180B <= code && code <= 0x180D || code == 0x180F then
                            Just MarkNonSpacing

                        else if code == 0x180E then
                            Just OtherFormat

                        else if 0x1810 <= code && code <= 0x1819 then
                            Just NumberDecimalDigit

                        else if 0x1820 <= code && code <= 0x1842 || 0x1844 <= code && code <= 0x1878 then
                            Just LetterOther

                        else if code == 0x1843 then
                            Just LetterModifier

                        else
                            Nothing

                    else if code < 0x1932 then
                        if 0x1880 <= code && code <= 0x1884 || 0x1887 <= code && code <= 0x18A8 || code == 0x18AA || 0x18B0 <= code && code <= 0x18F5 || 0x1900 <= code && code <= 0x191E then
                            Just LetterOther

                        else if 0x1885 <= code && code <= 0x1886 || code == 0x18A9 || 0x1920 <= code && code <= 0x1922 || 0x1927 <= code && code <= 0x1928 then
                            Just MarkNonSpacing

                        else if 0x1923 <= code && code <= 0x1926 || 0x1929 <= code && code <= 0x192B || 0x1930 <= code && code <= 0x1931 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x196F then
                        if code == 0x1932 || 0x1939 <= code && code <= 0x193B then
                            Just MarkNonSpacing

                        else if 0x1933 <= code && code <= 0x1938 then
                            Just MarkSpacingCombining

                        else if code == 0x1940 then
                            Just SymbolOther

                        else if 0x1944 <= code && code <= 0x1945 then
                            Just PunctuationOther

                        else if 0x1946 <= code && code <= 0x194F then
                            Just NumberDecimalDigit

                        else if 0x1950 <= code && code <= 0x196D then
                            Just LetterOther

                        else
                            Nothing

                    else if 0x1970 <= code && code <= 0x1974 || 0x1980 <= code && code <= 0x19AB || 0x19B0 <= code && code <= 0x19C9 || 0x1A00 <= code && code <= 0x1A15 then
                        Just LetterOther

                    else if 0x19D0 <= code && code <= 0x19D9 then
                        Just NumberDecimalDigit

                    else if code == 0x19DA then
                        Just NumberOther

                    else if 0x19DE <= code && code <= 0x19FF then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0x1ADF then
                    if code < 0x1A62 then
                        if code == 0x1A16 || 0x1A20 <= code && code <= 0x1A54 then
                            Just LetterOther

                        else if 0x1A17 <= code && code <= 0x1A18 || code == 0x1A1B || code == 0x1A56 || 0x1A58 <= code && code <= 0x1A5E || code == 0x1A60 then
                            Just MarkNonSpacing

                        else if 0x1A19 <= code && code <= 0x1A1A || code == 0x1A55 || code == 0x1A57 || code == 0x1A61 then
                            Just MarkSpacingCombining

                        else if 0x1A1E <= code && code <= 0x1A1F then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x1A8F then
                        if code == 0x1A62 || 0x1A65 <= code && code <= 0x1A6C || 0x1A73 <= code && code <= 0x1A7C || code == 0x1A7F then
                            Just MarkNonSpacing

                        else if 0x1A63 <= code && code <= 0x1A64 || 0x1A6D <= code && code <= 0x1A72 then
                            Just MarkSpacingCombining

                        else if 0x1A80 <= code && code <= 0x1A89 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if 0x1A90 <= code && code <= 0x1A99 then
                        Just NumberDecimalDigit

                    else if 0x1AA0 <= code && code <= 0x1AA6 || 0x1AA8 <= code && code <= 0x1AAD then
                        Just PunctuationOther

                    else if code == 0x1AA7 then
                        Just LetterModifier

                    else if 0x1AB0 <= code && code <= 0x1ABD || 0x1ABF <= code && code <= 0x1ADD then
                        Just MarkNonSpacing

                    else if code == 0x1ABE then
                        Just MarkEnclosing

                    else
                        Nothing

                else if code < 0x1B4D then
                    if code < 0x1B35 then
                        if 0x1AE0 <= code && code <= 0x1AEB || 0x1B00 <= code && code <= 0x1B03 || code == 0x1B34 then
                            Just MarkNonSpacing

                        else if code == 0x1B04 then
                            Just MarkSpacingCombining

                        else if 0x1B05 <= code && code <= 0x1B33 then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x1B35 || code == 0x1B3B || 0x1B3D <= code && code <= 0x1B41 || 0x1B43 <= code && code <= 0x1B44 then
                        Just MarkSpacingCombining

                    else if 0x1B36 <= code && code <= 0x1B3A || code == 0x1B3C || code == 0x1B42 then
                        Just MarkNonSpacing

                    else if 0x1B45 <= code && code <= 0x1B4C then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x1B7F then
                    if 0x1B4E <= code && code <= 0x1B4F || 0x1B5A <= code && code <= 0x1B60 || 0x1B7D <= code && code <= 0x1B7E then
                        Just PunctuationOther

                    else if 0x1B50 <= code && code <= 0x1B59 then
                        Just NumberDecimalDigit

                    else if 0x1B61 <= code && code <= 0x1B6A || 0x1B74 <= code && code <= 0x1B7C then
                        Just SymbolOther

                    else if 0x1B6B <= code && code <= 0x1B73 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x1B7F then
                    Just PunctuationOther

                else if 0x1B80 <= code && code <= 0x1B81 || 0x1BA2 <= code && code <= 0x1BA5 || code == 0x1BA8 then
                    Just MarkNonSpacing

                else if code == 0x1B82 || code == 0x1BA1 || 0x1BA6 <= code && code <= 0x1BA7 then
                    Just MarkSpacingCombining

                else if 0x1B83 <= code && code <= 0x1BA0 then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x1DBF then
                if code < 0x1C7D then
                    if code < 0x1BF1 then
                        if code < 0x1BE5 then
                            if code == 0x1BA9 || 0x1BAB <= code && code <= 0x1BAD then
                                Just MarkNonSpacing

                            else if code == 0x1BAA then
                                Just MarkSpacingCombining

                            else if 0x1BAE <= code && code <= 0x1BAF || 0x1BBA <= code && code <= 0x1BE4 then
                                Just LetterOther

                            else if 0x1BB0 <= code && code <= 0x1BB9 then
                                Just NumberDecimalDigit

                            else
                                Nothing

                        else if code == 0x1BE5 then
                            Just LetterOther

                        else if code == 0x1BE6 || 0x1BE8 <= code && code <= 0x1BE9 || code == 0x1BED || 0x1BEF <= code && code <= 0x1BF0 then
                            Just MarkNonSpacing

                        else if code == 0x1BE7 || 0x1BEA <= code && code <= 0x1BEC || code == 0x1BEE then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x1C35 then
                        if code == 0x1BF1 || 0x1C2C <= code && code <= 0x1C33 then
                            Just MarkNonSpacing

                        else if 0x1BF2 <= code && code <= 0x1BF3 || 0x1C24 <= code && code <= 0x1C2B || code == 0x1C34 then
                            Just MarkSpacingCombining

                        else if 0x1BFC <= code && code <= 0x1BFF then
                            Just PunctuationOther

                        else if 0x1C00 <= code && code <= 0x1C23 then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x1C35 then
                        Just MarkSpacingCombining

                    else if 0x1C36 <= code && code <= 0x1C37 then
                        Just MarkNonSpacing

                    else if 0x1C3B <= code && code <= 0x1C3F then
                        Just PunctuationOther

                    else if 0x1C40 <= code && code <= 0x1C49 || 0x1C50 <= code && code <= 0x1C59 then
                        Just NumberDecimalDigit

                    else if 0x1C4D <= code && code <= 0x1C4F || 0x1C5A <= code && code <= 0x1C77 then
                        Just LetterOther

                    else if 0x1C78 <= code && code <= 0x1C7C then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0x1CE8 then
                    if code < 0x1CBC then
                        if code == 0x1C7D then
                            Just LetterModifier

                        else if 0x1C7E <= code && code <= 0x1C7F then
                            Just PunctuationOther

                        else if 0x1C80 <= code && code <= 0x1C88 || code == 0x1C8A then
                            Just LetterLowercase

                        else if code == 0x1C89 || 0x1C90 <= code && code <= 0x1CBA then
                            Just LetterUppercase

                        else
                            Nothing

                    else if 0x1CBD <= code && code <= 0x1CBF then
                        Just LetterUppercase

                    else if 0x1CC0 <= code && code <= 0x1CC7 || code == 0x1CD3 then
                        Just PunctuationOther

                    else if 0x1CD0 <= code && code <= 0x1CD2 || 0x1CD4 <= code && code <= 0x1CE0 || 0x1CE2 <= code && code <= 0x1CE7 then
                        Just MarkNonSpacing

                    else if code == 0x1CE1 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x1CF7 then
                    if code == 0x1CE8 || code == 0x1CED || code == 0x1CF4 then
                        Just MarkNonSpacing

                    else if 0x1CE9 <= code && code <= 0x1CEC || 0x1CEE <= code && code <= 0x1CF3 || 0x1CF5 <= code && code <= 0x1CF6 then
                        Just LetterOther

                    else
                        Nothing

                else if code == 0x1CF7 then
                    Just MarkSpacingCombining

                else if 0x1CF8 <= code && code <= 0x1CF9 then
                    Just MarkNonSpacing

                else if code == 0x1CFA then
                    Just LetterOther

                else if 0x1D00 <= code && code <= 0x1D2B || 0x1D6B <= code && code <= 0x1D77 || 0x1D79 <= code && code <= 0x1D9A then
                    Just LetterLowercase

                else if 0x1D2C <= code && code <= 0x1D6A || code == 0x1D78 || 0x1D9B <= code && code <= 0x1DBE then
                    Just LetterModifier

                else
                    Nothing

            else if code < 0x1E18 then
                if code < 0x1E0A then
                    if code == 0x1DBF then
                        Just LetterModifier

                    else if 0x1DC0 <= code && code <= 0x1DFF then
                        Just MarkNonSpacing

                    else if code == 0x1E00 || code == 0x1E02 || code == 0x1E04 || code == 0x1E06 || code == 0x1E08 then
                        Just LetterUppercase

                    else if code == 0x1E01 || code == 0x1E03 || code == 0x1E05 || code == 0x1E07 || code == 0x1E09 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x1E10 then
                    if code == 0x1E0A || code == 0x1E0C || code == 0x1E0E then
                        Just LetterUppercase

                    else if code == 0x1E0B || code == 0x1E0D || code == 0x1E0F then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0x1E10 || code == 0x1E12 || code == 0x1E14 || code == 0x1E16 then
                    Just LetterUppercase

                else if code == 0x1E11 || code == 0x1E13 || code == 0x1E15 || code == 0x1E17 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x1E25 then
                if code < 0x1E1D then
                    if code == 0x1E18 || code == 0x1E1A || code == 0x1E1C then
                        Just LetterUppercase

                    else if code == 0x1E19 || code == 0x1E1B then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0x1E1D || code == 0x1E1F || code == 0x1E21 || code == 0x1E23 then
                    Just LetterLowercase

                else if code == 0x1E1E || code == 0x1E20 || code == 0x1E22 || code == 0x1E24 then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x1E2B then
                if code == 0x1E25 || code == 0x1E27 || code == 0x1E29 then
                    Just LetterLowercase

                else if code == 0x1E26 || code == 0x1E28 || code == 0x1E2A then
                    Just LetterUppercase

                else
                    Nothing

            else if code == 0x1E2B || code == 0x1E2D || code == 0x1E2F || code == 0x1E31 || code == 0x1E33 then
                Just LetterLowercase

            else if code == 0x1E2C || code == 0x1E2E || code == 0x1E30 || code == 0x1E32 then
                Just LetterUppercase

            else
                Nothing

        else if code < 0x1F7F then
            if code < 0x1EA3 then
                if code < 0x1E66 then
                    if code < 0x1E4C then
                        if code < 0x1E3F then
                            if code == 0x1E34 || code == 0x1E36 || code == 0x1E38 || code == 0x1E3A || code == 0x1E3C || code == 0x1E3E then
                                Just LetterUppercase

                            else if code == 0x1E35 || code == 0x1E37 || code == 0x1E39 || code == 0x1E3B || code == 0x1E3D then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0x1E44 then
                            if code == 0x1E3F || code == 0x1E41 || code == 0x1E43 then
                                Just LetterLowercase

                            else if code == 0x1E40 || code == 0x1E42 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0x1E44 || code == 0x1E46 || code == 0x1E48 || code == 0x1E4A then
                            Just LetterUppercase

                        else if code == 0x1E45 || code == 0x1E47 || code == 0x1E49 || code == 0x1E4B then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x1E58 then
                        if code == 0x1E4C || code == 0x1E4E || code == 0x1E50 || code == 0x1E52 || code == 0x1E54 || code == 0x1E56 then
                            Just LetterUppercase

                        else if code == 0x1E4D || code == 0x1E4F || code == 0x1E51 || code == 0x1E53 || code == 0x1E55 || code == 0x1E57 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x1E5E then
                        if code == 0x1E58 || code == 0x1E5A || code == 0x1E5C then
                            Just LetterUppercase

                        else if code == 0x1E59 || code == 0x1E5B || code == 0x1E5D then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0x1E5E || code == 0x1E60 || code == 0x1E62 || code == 0x1E64 then
                        Just LetterUppercase

                    else if code == 0x1E5F || code == 0x1E61 || code == 0x1E63 || code == 0x1E65 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x1E7F then
                    if code < 0x1E71 then
                        if code == 0x1E66 || code == 0x1E68 || code == 0x1E6A || code == 0x1E6C || code == 0x1E6E || code == 0x1E70 then
                            Just LetterUppercase

                        else if code == 0x1E67 || code == 0x1E69 || code == 0x1E6B || code == 0x1E6D || code == 0x1E6F then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x1E77 then
                        if code == 0x1E71 || code == 0x1E73 || code == 0x1E75 then
                            Just LetterLowercase

                        else if code == 0x1E72 || code == 0x1E74 || code == 0x1E76 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x1E77 || code == 0x1E79 || code == 0x1E7B || code == 0x1E7D then
                        Just LetterLowercase

                    else if code == 0x1E78 || code == 0x1E7A || code == 0x1E7C || code == 0x1E7E then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1E8C then
                    if code < 0x1E84 then
                        if code == 0x1E7F || code == 0x1E81 || code == 0x1E83 then
                            Just LetterLowercase

                        else if code == 0x1E80 || code == 0x1E82 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x1E84 || code == 0x1E86 || code == 0x1E88 || code == 0x1E8A then
                        Just LetterUppercase

                    else if code == 0x1E85 || code == 0x1E87 || code == 0x1E89 || code == 0x1E8B then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x1E92 then
                    if code == 0x1E8C || code == 0x1E8E || code == 0x1E90 then
                        Just LetterUppercase

                    else if code == 0x1E8D || code == 0x1E8F || code == 0x1E91 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0x1E92 || code == 0x1E94 || code == 0x1E9E || code == 0x1EA0 || code == 0x1EA2 then
                    Just LetterUppercase

                else if code == 0x1E93 || 0x1E95 <= code && code <= 0x1E9D || code == 0x1E9F || code == 0x1EA1 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x1ED7 then
                if code < 0x1EBC then
                    if code < 0x1EAE then
                        if code == 0x1EA3 || code == 0x1EA5 || code == 0x1EA7 || code == 0x1EA9 || code == 0x1EAB || code == 0x1EAD then
                            Just LetterLowercase

                        else if code == 0x1EA4 || code == 0x1EA6 || code == 0x1EA8 || code == 0x1EAA || code == 0x1EAC then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x1EB4 then
                        if code == 0x1EAE || code == 0x1EB0 || code == 0x1EB2 then
                            Just LetterUppercase

                        else if code == 0x1EAF || code == 0x1EB1 || code == 0x1EB3 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0x1EB4 || code == 0x1EB6 || code == 0x1EB8 || code == 0x1EBA then
                        Just LetterUppercase

                    else if code == 0x1EB5 || code == 0x1EB7 || code == 0x1EB9 || code == 0x1EBB then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x1EC8 then
                    if code == 0x1EBC || code == 0x1EBE || code == 0x1EC0 || code == 0x1EC2 || code == 0x1EC4 || code == 0x1EC6 then
                        Just LetterUppercase

                    else if code == 0x1EBD || code == 0x1EBF || code == 0x1EC1 || code == 0x1EC3 || code == 0x1EC5 || code == 0x1EC7 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x1ECE then
                    if code == 0x1EC8 || code == 0x1ECA || code == 0x1ECC then
                        Just LetterUppercase

                    else if code == 0x1EC9 || code == 0x1ECB || code == 0x1ECD then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0x1ECE || code == 0x1ED0 || code == 0x1ED2 || code == 0x1ED4 || code == 0x1ED6 then
                    Just LetterUppercase

                else if code == 0x1ECF || code == 0x1ED1 || code == 0x1ED3 || code == 0x1ED5 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x1EF1 then
                if code < 0x1EE3 then
                    if code == 0x1ED7 || code == 0x1ED9 || code == 0x1EDB || code == 0x1EDD || code == 0x1EDF || code == 0x1EE1 then
                        Just LetterLowercase

                    else if code == 0x1ED8 || code == 0x1EDA || code == 0x1EDC || code == 0x1EDE || code == 0x1EE0 || code == 0x1EE2 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x1EE9 then
                    if code == 0x1EE3 || code == 0x1EE5 || code == 0x1EE7 then
                        Just LetterLowercase

                    else if code == 0x1EE4 || code == 0x1EE6 || code == 0x1EE8 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code == 0x1EE9 || code == 0x1EEB || code == 0x1EED || code == 0x1EEF then
                    Just LetterLowercase

                else if code == 0x1EEA || code == 0x1EEC || code == 0x1EEE || code == 0x1EF0 then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x1EFE then
                if code < 0x1EF6 then
                    if code == 0x1EF1 || code == 0x1EF3 || code == 0x1EF5 then
                        Just LetterLowercase

                    else if code == 0x1EF2 || code == 0x1EF4 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code == 0x1EF6 || code == 0x1EF8 || code == 0x1EFA || code == 0x1EFC then
                    Just LetterUppercase

                else if code == 0x1EF7 || code == 0x1EF9 || code == 0x1EFB || code == 0x1EFD then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x1F2F then
                if code == 0x1EFE || 0x1F08 <= code && code <= 0x1F0F || 0x1F18 <= code && code <= 0x1F1D || 0x1F28 <= code && code <= 0x1F2E then
                    Just LetterUppercase

                else if 0x1EFF <= code && code <= 0x1F07 || 0x1F10 <= code && code <= 0x1F15 || 0x1F20 <= code && code <= 0x1F27 then
                    Just LetterLowercase

                else
                    Nothing

            else if code == 0x1F2F || 0x1F38 <= code && code <= 0x1F3F || 0x1F48 <= code && code <= 0x1F4D || 0x1F68 <= code && code <= 0x1F6F || modBy 2 code == 1 && 0x1F59 <= code && code <= 0x1F5F then
                Just LetterUppercase

            else if 0x1F30 <= code && code <= 0x1F37 || 0x1F40 <= code && code <= 0x1F45 || 0x1F50 <= code && code <= 0x1F57 || 0x1F60 <= code && code <= 0x1F67 || 0x1F70 <= code && code <= 0x1F7D then
                Just LetterLowercase

            else
                Nothing

        else if code < 0x212E then
            if code < 0x2043 then
                if code < 0x1FF5 then
                    if code < 0x1FC1 then
                        if code < 0x1FAF then
                            if 0x1F80 <= code && code <= 0x1F87 || 0x1F90 <= code && code <= 0x1F97 || 0x1FA0 <= code && code <= 0x1FA7 then
                                Just LetterLowercase

                            else if 0x1F88 <= code && code <= 0x1F8F || 0x1F98 <= code && code <= 0x1F9F || 0x1FA8 <= code && code <= 0x1FAE then
                                Just LetterTitlecase

                            else
                                Nothing

                        else if code == 0x1FAF || code == 0x1FBC then
                            Just LetterTitlecase

                        else if 0x1FB0 <= code && code <= 0x1FB4 || 0x1FB6 <= code && code <= 0x1FB7 || code == 0x1FBE then
                            Just LetterLowercase

                        else if 0x1FB8 <= code && code <= 0x1FBB then
                            Just LetterUppercase

                        else if code == 0x1FBD || 0x1FBF <= code && code <= 0x1FC0 then
                            Just SymbolModifier

                        else
                            Nothing

                    else if code < 0x1FD5 then
                        if code == 0x1FC1 || 0x1FCD <= code && code <= 0x1FCF then
                            Just SymbolModifier

                        else if 0x1FC2 <= code && code <= 0x1FC4 || 0x1FC6 <= code && code <= 0x1FC7 || 0x1FD0 <= code && code <= 0x1FD3 then
                            Just LetterLowercase

                        else if 0x1FC8 <= code && code <= 0x1FCB then
                            Just LetterUppercase

                        else if code == 0x1FCC then
                            Just LetterTitlecase

                        else
                            Nothing

                    else if 0x1FD6 <= code && code <= 0x1FD7 || 0x1FE0 <= code && code <= 0x1FE7 || 0x1FF2 <= code && code <= 0x1FF4 then
                        Just LetterLowercase

                    else if 0x1FD8 <= code && code <= 0x1FDB || 0x1FE8 <= code && code <= 0x1FEC then
                        Just LetterUppercase

                    else if 0x1FDD <= code && code <= 0x1FDF || 0x1FED <= code && code <= 0x1FEF then
                        Just SymbolModifier

                    else
                        Nothing

                else if code < 0x201D then
                    if 0x1FF6 <= code && code <= 0x1FF7 then
                        Just LetterLowercase

                    else if 0x1FF8 <= code && code <= 0x1FFB then
                        Just LetterUppercase

                    else if code == 0x1FFC then
                        Just LetterTitlecase

                    else if 0x1FFD <= code && code <= 0x1FFE then
                        Just SymbolModifier

                    else if 0x2000 <= code && code <= 0x200A then
                        Just SeparatorSpace

                    else if 0x200B <= code && code <= 0x200F then
                        Just OtherFormat

                    else if 0x2010 <= code && code <= 0x2015 then
                        Just PunctuationDash

                    else if 0x2016 <= code && code <= 0x2017 then
                        Just PunctuationOther

                    else if code == 0x2018 || 0x201B <= code && code <= 0x201C then
                        Just PunctuationInitialQuote

                    else if code == 0x2019 then
                        Just PunctuationFinalQuote

                    else if code == 0x201A then
                        Just PunctuationOpen

                    else
                        Nothing

                else if code < 0x202E then
                    if code == 0x201D then
                        Just PunctuationFinalQuote

                    else if code == 0x201E then
                        Just PunctuationOpen

                    else if code == 0x201F then
                        Just PunctuationInitialQuote

                    else if 0x2020 <= code && code <= 0x2027 then
                        Just PunctuationOther

                    else if code == 0x2028 then
                        Just SeparatorLine

                    else if code == 0x2029 then
                        Just SeparatorParagraph

                    else if 0x202A <= code && code <= 0x202D then
                        Just OtherFormat

                    else
                        Nothing

                else if code == 0x202E then
                    Just OtherFormat

                else if code == 0x202F then
                    Just SeparatorSpace

                else if 0x2030 <= code && code <= 0x2038 || 0x203B <= code && code <= 0x203E || 0x2041 <= code && code <= 0x2042 then
                    Just PunctuationOther

                else if code == 0x2039 then
                    Just PunctuationInitialQuote

                else if code == 0x203A then
                    Just PunctuationFinalQuote

                else if 0x203F <= code && code <= 0x2040 then
                    Just PunctuationConnector

                else
                    Nothing

            else if code < 0x20DC then
                if code < 0x2070 then
                    if code == 0x2043 || 0x2047 <= code && code <= 0x2051 || code == 0x2053 || 0x2055 <= code && code <= 0x205E then
                        Just PunctuationOther

                    else if code == 0x2044 || code == 0x2052 then
                        Just SymbolMath

                    else if code == 0x2045 then
                        Just PunctuationOpen

                    else if code == 0x2046 then
                        Just PunctuationClose

                    else if code == 0x2054 then
                        Just PunctuationConnector

                    else if code == 0x205F then
                        Just SeparatorSpace

                    else if 0x2060 <= code && code <= 0x2064 || 0x2066 <= code && code <= 0x206F then
                        Just OtherFormat

                    else
                        Nothing

                else if code < 0x207F then
                    if code == 0x2070 || 0x2074 <= code && code <= 0x2079 then
                        Just NumberOther

                    else if code == 0x2071 then
                        Just LetterModifier

                    else if 0x207A <= code && code <= 0x207C then
                        Just SymbolMath

                    else if code == 0x207D then
                        Just PunctuationOpen

                    else if code == 0x207E then
                        Just PunctuationClose

                    else
                        Nothing

                else if code == 0x207F || 0x2090 <= code && code <= 0x209C then
                    Just LetterModifier

                else if 0x2080 <= code && code <= 0x2089 then
                    Just NumberOther

                else if 0x208A <= code && code <= 0x208C then
                    Just SymbolMath

                else if code == 0x208D then
                    Just PunctuationOpen

                else if code == 0x208E then
                    Just PunctuationClose

                else if 0x20A0 <= code && code <= 0x20C1 then
                    Just SymbolCurrency

                else if 0x20D0 <= code && code <= 0x20DB then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x2112 then
                if code < 0x2102 then
                    if code == 0x20DC || code == 0x20E1 || 0x20E5 <= code && code <= 0x20F0 then
                        Just MarkNonSpacing

                    else if 0x20DD <= code && code <= 0x20E0 || 0x20E2 <= code && code <= 0x20E4 then
                        Just MarkEnclosing

                    else if 0x2100 <= code && code <= 0x2101 then
                        Just SymbolOther

                    else
                        Nothing

                else if code == 0x2102 || code == 0x2107 || 0x210B <= code && code <= 0x210D || 0x2110 <= code && code <= 0x2111 then
                    Just LetterUppercase

                else if 0x2103 <= code && code <= 0x2106 || 0x2108 <= code && code <= 0x2109 then
                    Just SymbolOther

                else if code == 0x210A || 0x210E <= code && code <= 0x210F then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0x211D then
                if code == 0x2112 || code == 0x2115 || 0x2119 <= code && code <= 0x211C then
                    Just LetterUppercase

                else if code == 0x2113 then
                    Just LetterLowercase

                else if code == 0x2114 || 0x2116 <= code && code <= 0x2117 then
                    Just SymbolOther

                else if code == 0x2118 then
                    Just SymbolMath

                else
                    Nothing

            else if code == 0x211D || code == 0x2124 || code == 0x2126 || code == 0x2128 || 0x212A <= code && code <= 0x212D then
                Just LetterUppercase

            else if 0x211E <= code && code <= 0x2123 || code == 0x2125 || code == 0x2127 || code == 0x2129 then
                Just SymbolOther

            else
                Nothing

        else if code < 0x232A then
            if code < 0x2199 then
                if code < 0x214A then
                    if code == 0x212E || 0x213A <= code && code <= 0x213B then
                        Just SymbolOther

                    else if code == 0x212F || code == 0x2134 || code == 0x2139 || 0x213C <= code && code <= 0x213D || 0x2146 <= code && code <= 0x2149 then
                        Just LetterLowercase

                    else if 0x2130 <= code && code <= 0x2133 || 0x213E <= code && code <= 0x213F || code == 0x2145 then
                        Just LetterUppercase

                    else if 0x2135 <= code && code <= 0x2138 then
                        Just LetterOther

                    else if 0x2140 <= code && code <= 0x2144 then
                        Just SymbolMath

                    else
                        Nothing

                else if code < 0x2182 then
                    if code == 0x214A || 0x214C <= code && code <= 0x214D || code == 0x214F then
                        Just SymbolOther

                    else if code == 0x214B then
                        Just SymbolMath

                    else if code == 0x214E then
                        Just LetterLowercase

                    else if 0x2150 <= code && code <= 0x215F then
                        Just NumberOther

                    else if 0x2160 <= code && code <= 0x2181 then
                        Just NumberLetter

                    else
                        Nothing

                else if code == 0x2182 || 0x2185 <= code && code <= 0x2188 then
                    Just NumberLetter

                else if code == 0x2183 then
                    Just LetterUppercase

                else if code == 0x2184 then
                    Just LetterLowercase

                else if code == 0x2189 then
                    Just NumberOther

                else if 0x218A <= code && code <= 0x218B || 0x2195 <= code && code <= 0x2198 then
                    Just SymbolOther

                else if 0x2190 <= code && code <= 0x2194 then
                    Just SymbolMath

                else
                    Nothing

            else if code < 0x21D1 then
                if code < 0x21A3 then
                    if code == 0x2199 || 0x219C <= code && code <= 0x219F || 0x21A1 <= code && code <= 0x21A2 then
                        Just SymbolOther

                    else if 0x219A <= code && code <= 0x219B || code == 0x21A0 then
                        Just SymbolMath

                    else
                        Nothing

                else if code == 0x21A3 || code == 0x21A6 || code == 0x21AE || 0x21CE <= code && code <= 0x21CF then
                    Just SymbolMath

                else if 0x21A4 <= code && code <= 0x21A5 || 0x21A7 <= code && code <= 0x21AD || 0x21AF <= code && code <= 0x21CD || code == 0x21D0 then
                    Just SymbolOther

                else
                    Nothing

            else if code < 0x2307 then
                if code == 0x21D1 || code == 0x21D3 || 0x21D5 <= code && code <= 0x21F3 || 0x2300 <= code && code <= 0x2306 then
                    Just SymbolOther

                else if code == 0x21D2 || code == 0x21D4 || 0x21F4 <= code && code <= 0x22FF then
                    Just SymbolMath

                else
                    Nothing

            else if code == 0x2307 || 0x230C <= code && code <= 0x231F || 0x2322 <= code && code <= 0x2328 then
                Just SymbolOther

            else if code == 0x2308 || code == 0x230A || code == 0x2329 then
                Just PunctuationOpen

            else if code == 0x2309 || code == 0x230B then
                Just PunctuationClose

            else if 0x2320 <= code && code <= 0x2321 then
                Just SymbolMath

            else
                Nothing

        else if code < 0x276D then
            if code < 0x25B6 then
                if code < 0x23DB then
                    if code == 0x232A then
                        Just PunctuationClose

                    else if 0x232B <= code && code <= 0x237B || 0x237D <= code && code <= 0x239A || 0x23B4 <= code && code <= 0x23DA then
                        Just SymbolOther

                    else if code == 0x237C || 0x239B <= code && code <= 0x23B3 then
                        Just SymbolMath

                    else
                        Nothing

                else if code == 0x23DB || 0x23E2 <= code && code <= 0x2429 || 0x2440 <= code && code <= 0x244A || 0x249C <= code && code <= 0x24E9 || 0x2500 <= code && code <= 0x25B5 then
                    Just SymbolOther

                else if 0x23DC <= code && code <= 0x23E1 then
                    Just SymbolMath

                else if 0x2460 <= code && code <= 0x249B || 0x24EA <= code && code <= 0x24FF then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x266E then
                if code == 0x25B6 || 0x25B8 <= code && code <= 0x25C0 || 0x25C2 <= code && code <= 0x25F7 || 0x2600 <= code && code <= 0x266D then
                    Just SymbolOther

                else if code == 0x25B7 || code == 0x25C1 || 0x25F8 <= code && code <= 0x25FF then
                    Just SymbolMath

                else
                    Nothing

            else if code == 0x266E || 0x2670 <= code && code <= 0x2767 then
                Just SymbolOther

            else if code == 0x266F then
                Just SymbolMath

            else if code == 0x2768 || code == 0x276A || code == 0x276C then
                Just PunctuationOpen

            else if code == 0x2769 || code == 0x276B then
                Just PunctuationClose

            else
                Nothing

        else if code < 0x27C6 then
            if code < 0x2772 then
                if code == 0x276D || code == 0x276F || code == 0x2771 then
                    Just PunctuationClose

                else if code == 0x276E || code == 0x2770 then
                    Just PunctuationOpen

                else
                    Nothing

            else if code == 0x2772 || code == 0x2774 || code == 0x27C5 then
                Just PunctuationOpen

            else if code == 0x2773 || code == 0x2775 then
                Just PunctuationClose

            else if 0x2776 <= code && code <= 0x2793 then
                Just NumberOther

            else if 0x2794 <= code && code <= 0x27BF then
                Just SymbolOther

            else if 0x27C0 <= code && code <= 0x27C4 then
                Just SymbolMath

            else
                Nothing

        else if code < 0x27EA then
            if code == 0x27C6 || code == 0x27E7 || code == 0x27E9 then
                Just PunctuationClose

            else if 0x27C7 <= code && code <= 0x27E5 then
                Just SymbolMath

            else if code == 0x27E6 || code == 0x27E8 then
                Just PunctuationOpen

            else
                Nothing

        else if code == 0x27EA || code == 0x27EC || code == 0x27EE then
            Just PunctuationOpen

        else if code == 0x27EB || code == 0x27ED || code == 0x27EF then
            Just PunctuationClose

        else if 0x27F0 <= code && code <= 0x27FF || 0x2900 <= code && code <= 0x2981 then
            Just SymbolMath

        else if 0x2800 <= code && code <= 0x28FF then
            Just SymbolOther

        else
            Nothing

    else if code < 0xFFE8 then
        if code < 0xA724 then
            if code < 0x2E22 then
                if code < 0x2CAD then
                    if code < 0x2C70 then
                        if code < 0x29D8 then
                            if code < 0x298D then
                                if code == 0x2982 then
                                    Just SymbolMath

                                else if code == 0x2983 || code == 0x2985 || code == 0x2987 || code == 0x2989 || code == 0x298B then
                                    Just PunctuationOpen

                                else if code == 0x2984 || code == 0x2986 || code == 0x2988 || code == 0x298A || code == 0x298C then
                                    Just PunctuationClose

                                else
                                    Nothing

                            else if code < 0x2992 then
                                if code == 0x298D || code == 0x298F || code == 0x2991 then
                                    Just PunctuationOpen

                                else if code == 0x298E || code == 0x2990 then
                                    Just PunctuationClose

                                else
                                    Nothing

                            else if code == 0x2992 || code == 0x2994 || code == 0x2996 || code == 0x2998 then
                                Just PunctuationClose

                            else if code == 0x2993 || code == 0x2995 || code == 0x2997 then
                                Just PunctuationOpen

                            else if 0x2999 <= code && code <= 0x29D7 then
                                Just SymbolMath

                            else
                                Nothing

                        else if code < 0x2B75 then
                            if code < 0x29FC then
                                if code == 0x29D8 || code == 0x29DA then
                                    Just PunctuationOpen

                                else if code == 0x29D9 || code == 0x29DB then
                                    Just PunctuationClose

                                else if 0x29DC <= code && code <= 0x29FB then
                                    Just SymbolMath

                                else
                                    Nothing

                            else if code == 0x29FC then
                                Just PunctuationOpen

                            else if code == 0x29FD then
                                Just PunctuationClose

                            else if 0x29FE <= code && code <= 0x2AFF || 0x2B30 <= code && code <= 0x2B44 || 0x2B47 <= code && code <= 0x2B4C then
                                Just SymbolMath

                            else if 0x2B00 <= code && code <= 0x2B2F || 0x2B45 <= code && code <= 0x2B46 || 0x2B4D <= code && code <= 0x2B73 then
                                Just SymbolOther

                            else
                                Nothing

                        else if code < 0x2C66 then
                            if 0x2B76 <= code && code <= 0x2BFF then
                                Just SymbolOther

                            else if 0x2C00 <= code && code <= 0x2C2F || code == 0x2C60 || 0x2C62 <= code && code <= 0x2C64 then
                                Just LetterUppercase

                            else if 0x2C30 <= code && code <= 0x2C5F || code == 0x2C61 || code == 0x2C65 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x2C66 || code == 0x2C68 || code == 0x2C6A || code == 0x2C6C then
                            Just LetterLowercase

                        else if code == 0x2C67 || code == 0x2C69 || code == 0x2C6B || 0x2C6D <= code && code <= 0x2C6F then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x2C92 then
                        if code < 0x2C84 then
                            if code == 0x2C70 || code == 0x2C72 || code == 0x2C75 || 0x2C7E <= code && code <= 0x2C80 || code == 0x2C82 then
                                Just LetterUppercase

                            else if code == 0x2C71 || 0x2C73 <= code && code <= 0x2C74 || 0x2C76 <= code && code <= 0x2C7B || code == 0x2C81 || code == 0x2C83 then
                                Just LetterLowercase

                            else if 0x2C7C <= code && code <= 0x2C7D then
                                Just LetterModifier

                            else
                                Nothing

                        else if code < 0x2C8A then
                            if code == 0x2C84 || code == 0x2C86 || code == 0x2C88 then
                                Just LetterUppercase

                            else if code == 0x2C85 || code == 0x2C87 || code == 0x2C89 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x2C8A || code == 0x2C8C || code == 0x2C8E || code == 0x2C90 then
                            Just LetterUppercase

                        else if code == 0x2C8B || code == 0x2C8D || code == 0x2C8F || code == 0x2C91 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x2C9E then
                        if code == 0x2C92 || code == 0x2C94 || code == 0x2C96 || code == 0x2C98 || code == 0x2C9A || code == 0x2C9C then
                            Just LetterUppercase

                        else if code == 0x2C93 || code == 0x2C95 || code == 0x2C97 || code == 0x2C99 || code == 0x2C9B || code == 0x2C9D then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0x2CA4 then
                        if code == 0x2C9E || code == 0x2CA0 || code == 0x2CA2 then
                            Just LetterUppercase

                        else if code == 0x2C9F || code == 0x2CA1 || code == 0x2CA3 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0x2CA4 || code == 0x2CA6 || code == 0x2CA8 || code == 0x2CAA || code == 0x2CAC then
                        Just LetterUppercase

                    else if code == 0x2CA5 || code == 0x2CA7 || code == 0x2CA9 || code == 0x2CAB then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0x2CE0 then
                    if code < 0x2CC5 then
                        if code < 0x2CB8 then
                            if code == 0x2CAD || code == 0x2CAF || code == 0x2CB1 || code == 0x2CB3 || code == 0x2CB5 || code == 0x2CB7 then
                                Just LetterLowercase

                            else if code == 0x2CAE || code == 0x2CB0 || code == 0x2CB2 || code == 0x2CB4 || code == 0x2CB6 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code < 0x2CBD then
                            if code == 0x2CB8 || code == 0x2CBA || code == 0x2CBC then
                                Just LetterUppercase

                            else if code == 0x2CB9 || code == 0x2CBB then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code == 0x2CBD || code == 0x2CBF || code == 0x2CC1 || code == 0x2CC3 then
                            Just LetterLowercase

                        else if code == 0x2CBE || code == 0x2CC0 || code == 0x2CC2 || code == 0x2CC4 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x2CD1 then
                        if code == 0x2CC5 || code == 0x2CC7 || code == 0x2CC9 || code == 0x2CCB || code == 0x2CCD || code == 0x2CCF then
                            Just LetterLowercase

                        else if code == 0x2CC6 || code == 0x2CC8 || code == 0x2CCA || code == 0x2CCC || code == 0x2CCE || code == 0x2CD0 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code < 0x2CD7 then
                        if code == 0x2CD1 || code == 0x2CD3 || code == 0x2CD5 then
                            Just LetterLowercase

                        else if code == 0x2CD2 || code == 0x2CD4 || code == 0x2CD6 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0x2CD7 || code == 0x2CD9 || code == 0x2CDB || code == 0x2CDD || code == 0x2CDF then
                        Just LetterLowercase

                    else if code == 0x2CD8 || code == 0x2CDA || code == 0x2CDC || code == 0x2CDE then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x2DB7 then
                    if code < 0x2CFC then
                        if code < 0x2CEB then
                            if code == 0x2CE0 || code == 0x2CE2 then
                                Just LetterUppercase

                            else if code == 0x2CE1 || 0x2CE3 <= code && code <= 0x2CE4 then
                                Just LetterLowercase

                            else if 0x2CE5 <= code && code <= 0x2CEA then
                                Just SymbolOther

                            else
                                Nothing

                        else if code == 0x2CEB || code == 0x2CED || code == 0x2CF2 then
                            Just LetterUppercase

                        else if code == 0x2CEC || code == 0x2CEE || code == 0x2CF3 then
                            Just LetterLowercase

                        else if 0x2CEF <= code && code <= 0x2CF1 then
                            Just MarkNonSpacing

                        else if 0x2CF9 <= code && code <= 0x2CFB then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x2D6E then
                        if code == 0x2CFC || 0x2CFE <= code && code <= 0x2CFF then
                            Just PunctuationOther

                        else if code == 0x2CFD then
                            Just NumberOther

                        else if 0x2D00 <= code && code <= 0x2D25 || code == 0x2D27 || code == 0x2D2D then
                            Just LetterLowercase

                        else if 0x2D30 <= code && code <= 0x2D67 then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x2D6F then
                        Just LetterModifier

                    else if code == 0x2D70 then
                        Just PunctuationOther

                    else if code == 0x2D7F then
                        Just MarkNonSpacing

                    else if 0x2D80 <= code && code <= 0x2D96 || 0x2DA0 <= code && code <= 0x2DA6 || 0x2DA8 <= code && code <= 0x2DAE || 0x2DB0 <= code && code <= 0x2DB6 then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x2E09 then
                    if 0x2DB8 <= code && code <= 0x2DBE || 0x2DC0 <= code && code <= 0x2DC6 || 0x2DC8 <= code && code <= 0x2DCE || 0x2DD0 <= code && code <= 0x2DD6 || 0x2DD8 <= code && code <= 0x2DDE then
                        Just LetterOther

                    else if 0x2DE0 <= code && code <= 0x2DFF then
                        Just MarkNonSpacing

                    else if 0x2E00 <= code && code <= 0x2E01 || 0x2E06 <= code && code <= 0x2E08 then
                        Just PunctuationOther

                    else if code == 0x2E02 || code == 0x2E04 then
                        Just PunctuationInitialQuote

                    else if code == 0x2E03 || code == 0x2E05 then
                        Just PunctuationFinalQuote

                    else
                        Nothing

                else if code < 0x2E17 then
                    if code == 0x2E09 || code == 0x2E0C then
                        Just PunctuationInitialQuote

                    else if code == 0x2E0A || code == 0x2E0D then
                        Just PunctuationFinalQuote

                    else if code == 0x2E0B || 0x2E0E <= code && code <= 0x2E16 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code == 0x2E17 || code == 0x2E1A then
                    Just PunctuationDash

                else if 0x2E18 <= code && code <= 0x2E19 || code == 0x2E1B || 0x2E1E <= code && code <= 0x2E1F then
                    Just PunctuationOther

                else if code == 0x2E1C || code == 0x2E20 then
                    Just PunctuationInitialQuote

                else if code == 0x2E1D || code == 0x2E21 then
                    Just PunctuationFinalQuote

                else
                    Nothing

            else if code < 0x4DFF then
                if code < 0x3016 then
                    if code < 0x2E5A then
                        if code < 0x2E3B then
                            if code == 0x2E22 || code == 0x2E24 || code == 0x2E26 || code == 0x2E28 then
                                Just PunctuationOpen

                            else if code == 0x2E23 || code == 0x2E25 || code == 0x2E27 || code == 0x2E29 then
                                Just PunctuationClose

                            else if 0x2E2A <= code && code <= 0x2E2E || 0x2E30 <= code && code <= 0x2E39 then
                                Just PunctuationOther

                            else if code == 0x2E2F then
                                Just LetterModifier

                            else if code == 0x2E3A then
                                Just PunctuationDash

                            else
                                Nothing

                        else if code < 0x2E4F then
                            if code == 0x2E3B || code == 0x2E40 then
                                Just PunctuationDash

                            else if 0x2E3C <= code && code <= 0x2E3F || code == 0x2E41 || 0x2E43 <= code && code <= 0x2E4E then
                                Just PunctuationOther

                            else if code == 0x2E42 then
                                Just PunctuationOpen

                            else
                                Nothing

                        else if code == 0x2E4F || 0x2E52 <= code && code <= 0x2E54 then
                            Just PunctuationOther

                        else if 0x2E50 <= code && code <= 0x2E51 then
                            Just SymbolOther

                        else if code == 0x2E55 || code == 0x2E57 || code == 0x2E59 then
                            Just PunctuationOpen

                        else if code == 0x2E56 || code == 0x2E58 then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code < 0x3006 then
                        if code == 0x2E5A || code == 0x2E5C then
                            Just PunctuationClose

                        else if code == 0x2E5B then
                            Just PunctuationOpen

                        else if code == 0x2E5D then
                            Just PunctuationDash

                        else if 0x2E80 <= code && code <= 0x2E99 || 0x2E9B <= code && code <= 0x2EF3 || 0x2F00 <= code && code <= 0x2FD5 || 0x2FF0 <= code && code <= 0x2FFF || code == 0x3004 then
                            Just SymbolOther

                        else if code == 0x3000 then
                            Just SeparatorSpace

                        else if 0x3001 <= code && code <= 0x3003 then
                            Just PunctuationOther

                        else if code == 0x3005 then
                            Just LetterModifier

                        else
                            Nothing

                    else if code < 0x300C then
                        if code == 0x3006 then
                            Just LetterOther

                        else if code == 0x3007 then
                            Just NumberLetter

                        else if code == 0x3008 || code == 0x300A then
                            Just PunctuationOpen

                        else if code == 0x3009 || code == 0x300B then
                            Just PunctuationClose

                        else
                            Nothing

                    else if code == 0x300C || code == 0x300E || code == 0x3010 || code == 0x3014 then
                        Just PunctuationOpen

                    else if code == 0x300D || code == 0x300F || code == 0x3011 || code == 0x3015 then
                        Just PunctuationClose

                    else if 0x3012 <= code && code <= 0x3013 then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0x309F then
                    if code < 0x302D then
                        if code == 0x3016 || code == 0x3018 || code == 0x301A || code == 0x301D then
                            Just PunctuationOpen

                        else if code == 0x3017 || code == 0x3019 || code == 0x301B || 0x301E <= code && code <= 0x301F then
                            Just PunctuationClose

                        else if code == 0x301C then
                            Just PunctuationDash

                        else if code == 0x3020 then
                            Just SymbolOther

                        else if 0x3021 <= code && code <= 0x3029 then
                            Just NumberLetter

                        else if 0x302A <= code && code <= 0x302C then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code < 0x303B then
                        if code == 0x302D then
                            Just MarkNonSpacing

                        else if 0x302E <= code && code <= 0x302F then
                            Just MarkSpacingCombining

                        else if code == 0x3030 then
                            Just PunctuationDash

                        else if 0x3031 <= code && code <= 0x3035 then
                            Just LetterModifier

                        else if 0x3036 <= code && code <= 0x3037 then
                            Just SymbolOther

                        else if 0x3038 <= code && code <= 0x303A then
                            Just NumberLetter

                        else
                            Nothing

                    else if code == 0x303B || 0x309D <= code && code <= 0x309E then
                        Just LetterModifier

                    else if code == 0x303C || 0x3041 <= code && code <= 0x3096 then
                        Just LetterOther

                    else if code == 0x303D then
                        Just PunctuationOther

                    else if 0x303E <= code && code <= 0x303F then
                        Just SymbolOther

                    else if 0x3099 <= code && code <= 0x309A then
                        Just MarkNonSpacing

                    else if 0x309B <= code && code <= 0x309C then
                        Just SymbolModifier

                    else
                        Nothing

                else if code < 0x31EF then
                    if code < 0x3104 then
                        if code == 0x309F || 0x30A1 <= code && code <= 0x30FA || code == 0x30FF then
                            Just LetterOther

                        else if code == 0x30A0 then
                            Just PunctuationDash

                        else if code == 0x30FB then
                            Just PunctuationOther

                        else if 0x30FC <= code && code <= 0x30FE then
                            Just LetterModifier

                        else
                            Nothing

                    else if 0x3105 <= code && code <= 0x312F || 0x3131 <= code && code <= 0x318E || 0x31A0 <= code && code <= 0x31BF then
                        Just LetterOther

                    else if 0x3190 <= code && code <= 0x3191 || 0x3196 <= code && code <= 0x319F || 0x31C0 <= code && code <= 0x31E5 then
                        Just SymbolOther

                    else if 0x3192 <= code && code <= 0x3195 then
                        Just NumberOther

                    else
                        Nothing

                else if code < 0x3250 then
                    if code == 0x31EF || 0x3200 <= code && code <= 0x321E || 0x322A <= code && code <= 0x3247 then
                        Just SymbolOther

                    else if 0x31F0 <= code && code <= 0x31FF then
                        Just LetterOther

                    else if 0x3220 <= code && code <= 0x3229 || 0x3248 <= code && code <= 0x324F then
                        Just NumberOther

                    else
                        Nothing

                else if code == 0x3250 || 0x3260 <= code && code <= 0x327F || 0x328A <= code && code <= 0x32B0 || 0x32C0 <= code && code <= 0x33FF || 0x4DC0 <= code && code <= 0x4DFE then
                    Just SymbolOther

                else if 0x3251 <= code && code <= 0x325F || 0x3280 <= code && code <= 0x3289 || 0x32B1 <= code && code <= 0x32BF then
                    Just NumberOther

                else if 0x3400 <= code && code <= 0x4DBF then
                    Just LetterOther

                else
                    Nothing

            else if code < 0xA666 then
                if code < 0xA64B then
                    if code < 0xA61F then
                        if code == 0x4DFF || 0xA490 <= code && code <= 0xA4C6 then
                            Just SymbolOther

                        else if 0x4E00 <= code && code <= 0xA014 || 0xA016 <= code && code <= 0xA48C || 0xA4D0 <= code && code <= 0xA4F7 || 0xA500 <= code && code <= 0xA60B || 0xA610 <= code && code <= 0xA61E then
                            Just LetterOther

                        else if code == 0xA015 || 0xA4F8 <= code && code <= 0xA4FD || code == 0xA60C then
                            Just LetterModifier

                        else if 0xA4FE <= code && code <= 0xA4FF || 0xA60D <= code && code <= 0xA60F then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0xA643 then
                        if code == 0xA61F || 0xA62A <= code && code <= 0xA62B then
                            Just LetterOther

                        else if 0xA620 <= code && code <= 0xA629 then
                            Just NumberDecimalDigit

                        else if code == 0xA640 || code == 0xA642 then
                            Just LetterUppercase

                        else if code == 0xA641 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0xA643 || code == 0xA645 || code == 0xA647 || code == 0xA649 then
                        Just LetterLowercase

                    else if code == 0xA644 || code == 0xA646 || code == 0xA648 || code == 0xA64A then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xA657 then
                    if code == 0xA64B || code == 0xA64D || code == 0xA64F || code == 0xA651 || code == 0xA653 || code == 0xA655 then
                        Just LetterLowercase

                    else if code == 0xA64C || code == 0xA64E || code == 0xA650 || code == 0xA652 || code == 0xA654 || code == 0xA656 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xA65D then
                    if code == 0xA657 || code == 0xA659 || code == 0xA65B then
                        Just LetterLowercase

                    else if code == 0xA658 || code == 0xA65A || code == 0xA65C then
                        Just LetterUppercase

                    else
                        Nothing

                else if code == 0xA65D || code == 0xA65F || code == 0xA661 || code == 0xA663 || code == 0xA665 then
                    Just LetterLowercase

                else if code == 0xA65E || code == 0xA660 || code == 0xA662 || code == 0xA664 then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0xA68B then
                if code < 0xA67D then
                    if code < 0xA66B then
                        if code == 0xA666 || code == 0xA668 || code == 0xA66A then
                            Just LetterUppercase

                        else if code == 0xA667 || code == 0xA669 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0xA66B || code == 0xA66D then
                        Just LetterLowercase

                    else if code == 0xA66C then
                        Just LetterUppercase

                    else if code == 0xA66E then
                        Just LetterOther

                    else if code == 0xA66F || 0xA674 <= code && code <= 0xA67C then
                        Just MarkNonSpacing

                    else if 0xA670 <= code && code <= 0xA672 then
                        Just MarkEnclosing

                    else if code == 0xA673 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0xA683 then
                    if code == 0xA67D then
                        Just MarkNonSpacing

                    else if code == 0xA67E then
                        Just PunctuationOther

                    else if code == 0xA67F then
                        Just LetterModifier

                    else if code == 0xA680 || code == 0xA682 then
                        Just LetterUppercase

                    else if code == 0xA681 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0xA683 || code == 0xA685 || code == 0xA687 || code == 0xA689 then
                    Just LetterLowercase

                else if code == 0xA684 || code == 0xA686 || code == 0xA688 || code == 0xA68A then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0xA698 then
                if code < 0xA690 then
                    if code == 0xA68B || code == 0xA68D || code == 0xA68F then
                        Just LetterLowercase

                    else if code == 0xA68C || code == 0xA68E then
                        Just LetterUppercase

                    else
                        Nothing

                else if code == 0xA690 || code == 0xA692 || code == 0xA694 || code == 0xA696 then
                    Just LetterUppercase

                else if code == 0xA691 || code == 0xA693 || code == 0xA695 || code == 0xA697 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xA6E5 then
                if code == 0xA698 || code == 0xA69A then
                    Just LetterUppercase

                else if code == 0xA699 || code == 0xA69B then
                    Just LetterLowercase

                else if 0xA69C <= code && code <= 0xA69D then
                    Just LetterModifier

                else if 0xA69E <= code && code <= 0xA69F then
                    Just MarkNonSpacing

                else if 0xA6A0 <= code && code <= 0xA6E4 then
                    Just LetterOther

                else
                    Nothing

            else if code == 0xA6E5 then
                Just LetterOther

            else if 0xA6E6 <= code && code <= 0xA6EF then
                Just NumberLetter

            else if 0xA6F0 <= code && code <= 0xA6F1 then
                Just MarkNonSpacing

            else if 0xA6F2 <= code && code <= 0xA6F7 then
                Just PunctuationOther

            else if 0xA700 <= code && code <= 0xA716 || 0xA720 <= code && code <= 0xA721 then
                Just SymbolModifier

            else if 0xA717 <= code && code <= 0xA71F then
                Just LetterModifier

            else if code == 0xA722 then
                Just LetterUppercase

            else if code == 0xA723 then
                Just LetterLowercase

            else
                Nothing

        else if code < 0xA9B2 then
            if code < 0xA798 then
                if code < 0xA758 then
                    if code < 0xA73E then
                        if code < 0xA731 then
                            if code == 0xA724 || code == 0xA726 || code == 0xA728 || code == 0xA72A || code == 0xA72C || code == 0xA72E then
                                Just LetterUppercase

                            else if code == 0xA725 || code == 0xA727 || code == 0xA729 || code == 0xA72B || code == 0xA72D || 0xA72F <= code && code <= 0xA730 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if code < 0xA736 then
                            if code == 0xA731 || code == 0xA733 || code == 0xA735 then
                                Just LetterLowercase

                            else if code == 0xA732 || code == 0xA734 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if code == 0xA736 || code == 0xA738 || code == 0xA73A || code == 0xA73C then
                            Just LetterUppercase

                        else if code == 0xA737 || code == 0xA739 || code == 0xA73B || code == 0xA73D then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0xA74A then
                        if code == 0xA73E || code == 0xA740 || code == 0xA742 || code == 0xA744 || code == 0xA746 || code == 0xA748 then
                            Just LetterUppercase

                        else if code == 0xA73F || code == 0xA741 || code == 0xA743 || code == 0xA745 || code == 0xA747 || code == 0xA749 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0xA750 then
                        if code == 0xA74A || code == 0xA74C || code == 0xA74E then
                            Just LetterUppercase

                        else if code == 0xA74B || code == 0xA74D || code == 0xA74F then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0xA750 || code == 0xA752 || code == 0xA754 || code == 0xA756 then
                        Just LetterUppercase

                    else if code == 0xA751 || code == 0xA753 || code == 0xA755 || code == 0xA757 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0xA778 then
                    if code < 0xA764 then
                        if code == 0xA758 || code == 0xA75A || code == 0xA75C || code == 0xA75E || code == 0xA760 || code == 0xA762 then
                            Just LetterUppercase

                        else if code == 0xA759 || code == 0xA75B || code == 0xA75D || code == 0xA75F || code == 0xA761 || code == 0xA763 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0xA76A then
                        if code == 0xA764 || code == 0xA766 || code == 0xA768 then
                            Just LetterUppercase

                        else if code == 0xA765 || code == 0xA767 || code == 0xA769 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code == 0xA76A || code == 0xA76C || code == 0xA76E then
                        Just LetterUppercase

                    else if code == 0xA76B || code == 0xA76D || code == 0xA76F || 0xA771 <= code && code <= 0xA777 then
                        Just LetterLowercase

                    else if code == 0xA770 then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0xA786 then
                    if code < 0xA77E then
                        if code == 0xA778 || code == 0xA77A || code == 0xA77C then
                            Just LetterLowercase

                        else if code == 0xA779 || code == 0xA77B || code == 0xA77D then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0xA77E || code == 0xA780 || code == 0xA782 || code == 0xA784 then
                        Just LetterUppercase

                    else if code == 0xA77F || code == 0xA781 || code == 0xA783 || code == 0xA785 then
                        Just LetterLowercase

                    else
                        Nothing

                else if code < 0xA78D then
                    if code == 0xA786 || code == 0xA78B then
                        Just LetterUppercase

                    else if code == 0xA787 || code == 0xA78C then
                        Just LetterLowercase

                    else if code == 0xA788 then
                        Just LetterModifier

                    else if 0xA789 <= code && code <= 0xA78A then
                        Just SymbolModifier

                    else
                        Nothing

                else if code == 0xA78D || code == 0xA790 || code == 0xA792 || code == 0xA796 then
                    Just LetterUppercase

                else if code == 0xA78E || code == 0xA791 || 0xA793 <= code && code <= 0xA795 || code == 0xA797 then
                    Just LetterLowercase

                else if code == 0xA78F then
                    Just LetterOther

                else
                    Nothing

            else if code < 0xA7D8 then
                if code < 0xA7B9 then
                    if code < 0xA7A3 then
                        if code == 0xA798 || code == 0xA79A || code == 0xA79C || code == 0xA79E || code == 0xA7A0 || code == 0xA7A2 then
                            Just LetterUppercase

                        else if code == 0xA799 || code == 0xA79B || code == 0xA79D || code == 0xA79F || code == 0xA7A1 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if code < 0xA7A9 then
                        if code == 0xA7A3 || code == 0xA7A5 || code == 0xA7A7 then
                            Just LetterLowercase

                        else if code == 0xA7A4 || code == 0xA7A6 || code == 0xA7A8 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if code == 0xA7A9 || code == 0xA7AF || code == 0xA7B5 || code == 0xA7B7 then
                        Just LetterLowercase

                    else if 0xA7AA <= code && code <= 0xA7AE || 0xA7B0 <= code && code <= 0xA7B4 || code == 0xA7B6 || code == 0xA7B8 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xA7C8 then
                    if code == 0xA7B9 || code == 0xA7BB || code == 0xA7BD || code == 0xA7BF || code == 0xA7C1 || code == 0xA7C3 then
                        Just LetterLowercase

                    else if code == 0xA7BA || code == 0xA7BC || code == 0xA7BE || code == 0xA7C0 || code == 0xA7C2 || 0xA7C4 <= code && code <= 0xA7C7 then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0xA7CF then
                    if code == 0xA7C8 || code == 0xA7CA || code == 0xA7CD then
                        Just LetterLowercase

                    else if code == 0xA7C9 || 0xA7CB <= code && code <= 0xA7CC || code == 0xA7CE then
                        Just LetterUppercase

                    else
                        Nothing

                else if code == 0xA7CF || code == 0xA7D1 || code == 0xA7D3 || code == 0xA7D5 || code == 0xA7D7 then
                    Just LetterLowercase

                else if code == 0xA7D0 || code == 0xA7D2 || code == 0xA7D4 || code == 0xA7D6 then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0xA83F then
                if code < 0xA802 then
                    if code == 0xA7D8 || code == 0xA7DA || code == 0xA7DC || code == 0xA7F5 then
                        Just LetterUppercase

                    else if code == 0xA7D9 || code == 0xA7DB || code == 0xA7F6 || code == 0xA7FA then
                        Just LetterLowercase

                    else if 0xA7F1 <= code && code <= 0xA7F4 || 0xA7F8 <= code && code <= 0xA7F9 then
                        Just LetterModifier

                    else if code == 0xA7F7 || 0xA7FB <= code && code <= 0xA801 then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0xA824 then
                    if code == 0xA802 || code == 0xA806 || code == 0xA80B then
                        Just MarkNonSpacing

                    else if 0xA803 <= code && code <= 0xA805 || 0xA807 <= code && code <= 0xA80A || 0xA80C <= code && code <= 0xA822 then
                        Just LetterOther

                    else if code == 0xA823 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code == 0xA824 || code == 0xA827 then
                    Just MarkSpacingCombining

                else if 0xA825 <= code && code <= 0xA826 || code == 0xA82C then
                    Just MarkNonSpacing

                else if 0xA828 <= code && code <= 0xA82B || 0xA836 <= code && code <= 0xA837 || code == 0xA839 then
                    Just SymbolOther

                else if 0xA830 <= code && code <= 0xA835 then
                    Just NumberOther

                else if code == 0xA838 then
                    Just SymbolCurrency

                else
                    Nothing

            else if code < 0xA8FC then
                if 0xA840 <= code && code <= 0xA873 || 0xA882 <= code && code <= 0xA8B3 || 0xA8F2 <= code && code <= 0xA8F7 || code == 0xA8FB then
                    Just LetterOther

                else if 0xA874 <= code && code <= 0xA877 || 0xA8CE <= code && code <= 0xA8CF || 0xA8F8 <= code && code <= 0xA8FA then
                    Just PunctuationOther

                else if 0xA880 <= code && code <= 0xA881 || 0xA8B4 <= code && code <= 0xA8C3 then
                    Just MarkSpacingCombining

                else if 0xA8C4 <= code && code <= 0xA8C5 || 0xA8E0 <= code && code <= 0xA8F1 then
                    Just MarkNonSpacing

                else if 0xA8D0 <= code && code <= 0xA8D9 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0xA92F then
                if code == 0xA8FC || code == 0xA92E then
                    Just PunctuationOther

                else if 0xA8FD <= code && code <= 0xA8FE || 0xA90A <= code && code <= 0xA925 then
                    Just LetterOther

                else if code == 0xA8FF || 0xA926 <= code && code <= 0xA92D then
                    Just MarkNonSpacing

                else if 0xA900 <= code && code <= 0xA909 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code == 0xA92F || code == 0xA95F then
                Just PunctuationOther

            else if 0xA930 <= code && code <= 0xA946 || 0xA960 <= code && code <= 0xA97C || 0xA984 <= code && code <= 0xA9B1 then
                Just LetterOther

            else if 0xA947 <= code && code <= 0xA951 || 0xA980 <= code && code <= 0xA982 then
                Just MarkNonSpacing

            else if 0xA952 <= code && code <= 0xA953 || code == 0xA983 then
                Just MarkSpacingCombining

            else
                Nothing

        else if code < 0xFD3D then
            if code < 0xAADF then
                if code < 0xAA4B then
                    if code < 0xA9E5 then
                        if code == 0xA9B2 || 0xA9E0 <= code && code <= 0xA9E4 then
                            Just LetterOther

                        else if code == 0xA9B3 || 0xA9B6 <= code && code <= 0xA9B9 || 0xA9BC <= code && code <= 0xA9BD then
                            Just MarkNonSpacing

                        else if 0xA9B4 <= code && code <= 0xA9B5 || 0xA9BA <= code && code <= 0xA9BB || 0xA9BE <= code && code <= 0xA9C0 then
                            Just MarkSpacingCombining

                        else if 0xA9C1 <= code && code <= 0xA9CD || 0xA9DE <= code && code <= 0xA9DF then
                            Just PunctuationOther

                        else if code == 0xA9CF then
                            Just LetterModifier

                        else if 0xA9D0 <= code && code <= 0xA9D9 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if code < 0xAA2E then
                        if code == 0xA9E5 || 0xAA29 <= code && code <= 0xAA2D then
                            Just MarkNonSpacing

                        else if code == 0xA9E6 then
                            Just LetterModifier

                        else if 0xA9E7 <= code && code <= 0xA9EF || 0xA9FA <= code && code <= 0xA9FE || 0xAA00 <= code && code <= 0xAA28 then
                            Just LetterOther

                        else if 0xA9F0 <= code && code <= 0xA9F9 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if code == 0xAA2E || 0xAA31 <= code && code <= 0xAA32 || 0xAA35 <= code && code <= 0xAA36 || code == 0xAA43 then
                        Just MarkNonSpacing

                    else if 0xAA2F <= code && code <= 0xAA30 || 0xAA33 <= code && code <= 0xAA34 then
                        Just MarkSpacingCombining

                    else if 0xAA40 <= code && code <= 0xAA42 || 0xAA44 <= code && code <= 0xAA4A then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0xAA7D then
                    if code == 0xAA4B || 0xAA60 <= code && code <= 0xAA6F || 0xAA71 <= code && code <= 0xAA76 || code == 0xAA7A then
                        Just LetterOther

                    else if code == 0xAA4C || code == 0xAA7C then
                        Just MarkNonSpacing

                    else if code == 0xAA4D || code == 0xAA7B then
                        Just MarkSpacingCombining

                    else if 0xAA50 <= code && code <= 0xAA59 then
                        Just NumberDecimalDigit

                    else if 0xAA5C <= code && code <= 0xAA5F then
                        Just PunctuationOther

                    else if code == 0xAA70 then
                        Just LetterModifier

                    else if 0xAA77 <= code && code <= 0xAA79 then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0xAAB8 then
                    if code == 0xAA7D then
                        Just MarkSpacingCombining

                    else if 0xAA7E <= code && code <= 0xAAAF || code == 0xAAB1 || 0xAAB5 <= code && code <= 0xAAB6 then
                        Just LetterOther

                    else if code == 0xAAB0 || 0xAAB2 <= code && code <= 0xAAB4 || code == 0xAAB7 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0xAAB8 || 0xAABE <= code && code <= 0xAABF || code == 0xAAC1 then
                    Just MarkNonSpacing

                else if 0xAAB9 <= code && code <= 0xAABD || code == 0xAAC0 || code == 0xAAC2 || 0xAADB <= code && code <= 0xAADC then
                    Just LetterOther

                else if code == 0xAADD then
                    Just LetterModifier

                else if code == 0xAADE then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0xABE8 then
                if code < 0xAB1F then
                    if code < 0xAAF1 then
                        if code == 0xAADF || code == 0xAAF0 then
                            Just PunctuationOther

                        else if 0xAAE0 <= code && code <= 0xAAEA then
                            Just LetterOther

                        else if code == 0xAAEB || 0xAAEE <= code && code <= 0xAAEF then
                            Just MarkSpacingCombining

                        else if 0xAAEC <= code && code <= 0xAAED then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0xAAF1 then
                        Just PunctuationOther

                    else if code == 0xAAF2 || 0xAB01 <= code && code <= 0xAB06 || 0xAB09 <= code && code <= 0xAB0E || 0xAB11 <= code && code <= 0xAB16 then
                        Just LetterOther

                    else if 0xAAF3 <= code && code <= 0xAAF4 then
                        Just LetterModifier

                    else if code == 0xAAF5 then
                        Just MarkSpacingCombining

                    else if code == 0xAAF6 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0xAB68 then
                    if 0xAB20 <= code && code <= 0xAB26 || 0xAB28 <= code && code <= 0xAB2E then
                        Just LetterOther

                    else if 0xAB30 <= code && code <= 0xAB5A || 0xAB60 <= code && code <= 0xAB67 then
                        Just LetterLowercase

                    else if code == 0xAB5B then
                        Just SymbolModifier

                    else if 0xAB5C <= code && code <= 0xAB5F then
                        Just LetterModifier

                    else
                        Nothing

                else if code == 0xAB68 || 0xAB70 <= code && code <= 0xABBF then
                    Just LetterLowercase

                else if code == 0xAB69 then
                    Just LetterModifier

                else if 0xAB6A <= code && code <= 0xAB6B then
                    Just SymbolModifier

                else if 0xABC0 <= code && code <= 0xABE2 then
                    Just LetterOther

                else if 0xABE3 <= code && code <= 0xABE4 || 0xABE6 <= code && code <= 0xABE7 then
                    Just MarkSpacingCombining

                else if code == 0xABE5 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0xFB12 then
                if code < 0xD7AF then
                    if code == 0xABE8 || code == 0xABED then
                        Just MarkNonSpacing

                    else if 0xABE9 <= code && code <= 0xABEA || code == 0xABEC then
                        Just MarkSpacingCombining

                    else if code == 0xABEB then
                        Just PunctuationOther

                    else if 0xABF0 <= code && code <= 0xABF9 then
                        Just NumberDecimalDigit

                    else if 0xAC00 <= code && code <= 0xD7A3 then
                        Just LetterOther

                    else
                        Nothing

                else if 0xD7B0 <= code && code <= 0xD7C6 || 0xD7CB <= code && code <= 0xD7FB || 0xF900 <= code && code <= 0xFA6D || 0xFA70 <= code && code <= 0xFAD9 then
                    Just LetterOther

                else if 0xD800 <= code && code <= 0xDFFF then
                    Just OtherSurrogate

                else if 0xE000 <= code && code <= 0xF8FF then
                    Just OtherPrivateUse

                else if 0xFB00 <= code && code <= 0xFB06 then
                    Just LetterLowercase

                else
                    Nothing

            else if code < 0xFB3D then
                if 0xFB13 <= code && code <= 0xFB17 then
                    Just LetterLowercase

                else if code == 0xFB1D || 0xFB1F <= code && code <= 0xFB28 || 0xFB2A <= code && code <= 0xFB36 || 0xFB38 <= code && code <= 0xFB3C then
                    Just LetterOther

                else if code == 0xFB1E then
                    Just MarkNonSpacing

                else if code == 0xFB29 then
                    Just SymbolMath

                else
                    Nothing

            else if code == 0xFB3E || 0xFB40 <= code && code <= 0xFB41 || 0xFB43 <= code && code <= 0xFB44 || 0xFB46 <= code && code <= 0xFBB1 || 0xFBD3 <= code && code <= 0xFD3C then
                Just LetterOther

            else if 0xFBB2 <= code && code <= 0xFBC2 then
                Just SymbolModifier

            else if 0xFBC3 <= code && code <= 0xFBD2 then
                Just SymbolOther

            else
                Nothing

        else if code < 0xFE63 then
            if code < 0xFE3A then
                if code < 0xFE0F then
                    if code == 0xFD3D || 0xFD50 <= code && code <= 0xFD8F || 0xFD92 <= code && code <= 0xFDC7 || 0xFDF0 <= code && code <= 0xFDFB then
                        Just LetterOther

                    else if code == 0xFD3E then
                        Just PunctuationClose

                    else if code == 0xFD3F then
                        Just PunctuationOpen

                    else if 0xFD40 <= code && code <= 0xFD4F || 0xFD90 <= code && code <= 0xFD91 || 0xFDC8 <= code && code <= 0xFDCF || 0xFDFD <= code && code <= 0xFDFF then
                        Just SymbolOther

                    else if code == 0xFDFC then
                        Just SymbolCurrency

                    else if 0xFE00 <= code && code <= 0xFE0E then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0xFE30 then
                    if code == 0xFE0F || 0xFE20 <= code && code <= 0xFE2F then
                        Just MarkNonSpacing

                    else if 0xFE10 <= code && code <= 0xFE16 || code == 0xFE19 then
                        Just PunctuationOther

                    else if code == 0xFE17 then
                        Just PunctuationOpen

                    else if code == 0xFE18 then
                        Just PunctuationClose

                    else
                        Nothing

                else if code == 0xFE30 then
                    Just PunctuationOther

                else if 0xFE31 <= code && code <= 0xFE32 then
                    Just PunctuationDash

                else if 0xFE33 <= code && code <= 0xFE34 then
                    Just PunctuationConnector

                else if code == 0xFE35 || code == 0xFE37 || code == 0xFE39 then
                    Just PunctuationOpen

                else if code == 0xFE36 || code == 0xFE38 then
                    Just PunctuationClose

                else
                    Nothing

            else if code < 0xFE47 then
                if code == 0xFE3A || code == 0xFE3C || code == 0xFE3E || code == 0xFE40 || code == 0xFE42 || code == 0xFE44 then
                    Just PunctuationClose

                else if code == 0xFE3B || code == 0xFE3D || code == 0xFE3F || code == 0xFE41 || code == 0xFE43 then
                    Just PunctuationOpen

                else if 0xFE45 <= code && code <= 0xFE46 then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0xFE58 then
                if code == 0xFE47 then
                    Just PunctuationOpen

                else if code == 0xFE48 then
                    Just PunctuationClose

                else if 0xFE49 <= code && code <= 0xFE4C || 0xFE50 <= code && code <= 0xFE52 || 0xFE54 <= code && code <= 0xFE57 then
                    Just PunctuationOther

                else if 0xFE4D <= code && code <= 0xFE4F then
                    Just PunctuationConnector

                else
                    Nothing

            else if code == 0xFE58 then
                Just PunctuationDash

            else if code == 0xFE59 || code == 0xFE5B || code == 0xFE5D then
                Just PunctuationOpen

            else if code == 0xFE5A || code == 0xFE5C || code == 0xFE5E then
                Just PunctuationClose

            else if 0xFE5F <= code && code <= 0xFE61 then
                Just PunctuationOther

            else if code == 0xFE62 then
                Just SymbolMath

            else
                Nothing

        else if code < 0xFF3E then
            if code < 0xFF09 then
                if code == 0xFE63 then
                    Just PunctuationDash

                else if 0xFE64 <= code && code <= 0xFE66 then
                    Just SymbolMath

                else if code == 0xFE68 || 0xFE6A <= code && code <= 0xFE6B || 0xFF01 <= code && code <= 0xFF03 || 0xFF05 <= code && code <= 0xFF07 then
                    Just PunctuationOther

                else if code == 0xFE69 || code == 0xFF04 then
                    Just SymbolCurrency

                else if 0xFE70 <= code && code <= 0xFE74 || 0xFE76 <= code && code <= 0xFEFC then
                    Just LetterOther

                else if code == 0xFEFF then
                    Just OtherFormat

                else if code == 0xFF08 then
                    Just PunctuationOpen

                else
                    Nothing

            else if code < 0xFF19 then
                if code == 0xFF09 then
                    Just PunctuationClose

                else if code == 0xFF0A || code == 0xFF0C || 0xFF0E <= code && code <= 0xFF0F then
                    Just PunctuationOther

                else if code == 0xFF0B then
                    Just SymbolMath

                else if code == 0xFF0D then
                    Just PunctuationDash

                else if 0xFF10 <= code && code <= 0xFF18 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code == 0xFF19 then
                Just NumberDecimalDigit

            else if 0xFF1A <= code && code <= 0xFF1B || 0xFF1F <= code && code <= 0xFF20 || code == 0xFF3C then
                Just PunctuationOther

            else if 0xFF1C <= code && code <= 0xFF1E then
                Just SymbolMath

            else if 0xFF21 <= code && code <= 0xFF3A then
                Just LetterUppercase

            else if code == 0xFF3B then
                Just PunctuationOpen

            else if code == 0xFF3D then
                Just PunctuationClose

            else
                Nothing

        else if code < 0xFF65 then
            if code < 0xFF5D then
                if code == 0xFF3E || code == 0xFF40 then
                    Just SymbolModifier

                else if code == 0xFF3F then
                    Just PunctuationConnector

                else if 0xFF41 <= code && code <= 0xFF5A then
                    Just LetterLowercase

                else if code == 0xFF5B then
                    Just PunctuationOpen

                else if code == 0xFF5C then
                    Just SymbolMath

                else
                    Nothing

            else if code == 0xFF5D || code == 0xFF60 || code == 0xFF63 then
                Just PunctuationClose

            else if code == 0xFF5E then
                Just SymbolMath

            else if code == 0xFF5F || code == 0xFF62 then
                Just PunctuationOpen

            else if code == 0xFF61 || code == 0xFF64 then
                Just PunctuationOther

            else
                Nothing

        else if code < 0xFFC9 then
            if code == 0xFF65 then
                Just PunctuationOther

            else if 0xFF66 <= code && code <= 0xFF6F || 0xFF71 <= code && code <= 0xFF9D || 0xFFA0 <= code && code <= 0xFFBE || 0xFFC2 <= code && code <= 0xFFC7 then
                Just LetterOther

            else if code == 0xFF70 || 0xFF9E <= code && code <= 0xFF9F then
                Just LetterModifier

            else
                Nothing

        else if 0xFFCA <= code && code <= 0xFFCF || 0xFFD2 <= code && code <= 0xFFD7 || 0xFFDA <= code && code <= 0xFFDC then
            Just LetterOther

        else if 0xFFE0 <= code && code <= 0xFFE1 || 0xFFE5 <= code && code <= 0xFFE6 then
            Just SymbolCurrency

        else if code == 0xFFE2 then
            Just SymbolMath

        else if code == 0xFFE3 then
            Just SymbolModifier

        else if code == 0xFFE4 then
            Just SymbolOther

        else
            Nothing

    else if code < 0x00011A9D then
        if code < 0x000111B2 then
            if code < 0x00010A5F then
                if code < 0x0001057B then
                    if code < 0x000102DF then
                        if code < 0x00010106 then
                            if code < 0x0001000C then
                                if code == 0xFFE8 || 0xFFED <= code && code <= 0xFFEE || 0xFFFC <= code && code <= 0xFFFD then
                                    Just SymbolOther

                                else if 0xFFE9 <= code && code <= 0xFFEC then
                                    Just SymbolMath

                                else if 0xFFF9 <= code && code <= 0xFFFB then
                                    Just OtherFormat

                                else if 0x00010000 <= code && code <= 0x0001000B then
                                    Just LetterOther

                                else
                                    Nothing

                            else if 0x0001000D <= code && code <= 0x00010026 || 0x00010028 <= code && code <= 0x0001003A || 0x0001003C <= code && code <= 0x0001003D || 0x0001003F <= code && code <= 0x0001004D || 0x00010050 <= code && code <= 0x0001005D || 0x00010080 <= code && code <= 0x000100FA then
                                Just LetterOther

                            else if 0x00010100 <= code && code <= 0x00010102 then
                                Just PunctuationOther

                            else
                                Nothing

                        else if code < 0x0001018B then
                            if 0x00010107 <= code && code <= 0x00010133 || 0x00010175 <= code && code <= 0x00010178 || code == 0x0001018A then
                                Just NumberOther

                            else if 0x00010137 <= code && code <= 0x0001013F || 0x00010179 <= code && code <= 0x00010189 then
                                Just SymbolOther

                            else if 0x00010140 <= code && code <= 0x00010174 then
                                Just NumberLetter

                            else
                                Nothing

                        else if code == 0x0001018B then
                            Just NumberOther

                        else if 0x0001018C <= code && code <= 0x0001018E || 0x00010190 <= code && code <= 0x0001019C || code == 0x000101A0 || 0x000101D0 <= code && code <= 0x000101FC then
                            Just SymbolOther

                        else if code == 0x000101FD then
                            Just MarkNonSpacing

                        else if 0x00010280 <= code && code <= 0x0001029C || 0x000102A0 <= code && code <= 0x000102D0 then
                            Just LetterOther

                        else
                            Nothing

                    else if code < 0x000103C7 then
                        if code < 0x00010341 then
                            if code == 0x000102E0 then
                                Just MarkNonSpacing

                            else if 0x000102E1 <= code && code <= 0x000102FB || 0x00010320 <= code && code <= 0x00010323 then
                                Just NumberOther

                            else if 0x00010300 <= code && code <= 0x0001031F || 0x0001032D <= code && code <= 0x00010340 then
                                Just LetterOther

                            else
                                Nothing

                        else if code == 0x00010341 || code == 0x0001034A then
                            Just NumberLetter

                        else if 0x00010342 <= code && code <= 0x00010349 || 0x00010350 <= code && code <= 0x00010375 || 0x00010380 <= code && code <= 0x0001039D || 0x000103A0 <= code && code <= 0x000103C3 then
                            Just LetterOther

                        else if 0x00010376 <= code && code <= 0x0001037A then
                            Just MarkNonSpacing

                        else if code == 0x0001039F then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x0001049F then
                        if 0x000103C8 <= code && code <= 0x000103CF || 0x00010450 <= code && code <= 0x0001049D then
                            Just LetterOther

                        else if code == 0x000103D0 then
                            Just PunctuationOther

                        else if 0x000103D1 <= code && code <= 0x000103D5 then
                            Just NumberLetter

                        else if 0x00010400 <= code && code <= 0x00010427 then
                            Just LetterUppercase

                        else if 0x00010428 <= code && code <= 0x0001044F then
                            Just LetterLowercase

                        else
                            Nothing

                    else if 0x000104A0 <= code && code <= 0x000104A9 then
                        Just NumberDecimalDigit

                    else if 0x000104B0 <= code && code <= 0x000104D3 || 0x00010570 <= code && code <= 0x0001057A then
                        Just LetterUppercase

                    else if 0x000104D8 <= code && code <= 0x000104FB then
                        Just LetterLowercase

                    else if 0x00010500 <= code && code <= 0x00010527 || 0x00010530 <= code && code <= 0x00010563 then
                        Just LetterOther

                    else if code == 0x0001056F then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x000108A6 then
                    if code < 0x000107B1 then
                        if code < 0x000105BA then
                            if 0x0001057C <= code && code <= 0x0001058A || 0x0001058C <= code && code <= 0x00010592 || 0x00010594 <= code && code <= 0x00010595 then
                                Just LetterUppercase

                            else if 0x00010597 <= code && code <= 0x000105A1 || 0x000105A3 <= code && code <= 0x000105B1 || 0x000105B3 <= code && code <= 0x000105B9 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if 0x000105BB <= code && code <= 0x000105BC then
                            Just LetterLowercase

                        else if 0x000105C0 <= code && code <= 0x000105F3 || 0x00010600 <= code && code <= 0x00010736 || 0x00010740 <= code && code <= 0x00010755 || 0x00010760 <= code && code <= 0x00010767 then
                            Just LetterOther

                        else if 0x00010780 <= code && code <= 0x00010785 || 0x00010787 <= code && code <= 0x000107B0 then
                            Just LetterModifier

                        else
                            Nothing

                    else if code < 0x0001083E then
                        if 0x000107B2 <= code && code <= 0x000107BA then
                            Just LetterModifier

                        else if 0x00010800 <= code && code <= 0x00010805 || code == 0x00010808 || 0x0001080A <= code && code <= 0x00010835 || 0x00010837 <= code && code <= 0x00010838 || code == 0x0001083C then
                            Just LetterOther

                        else
                            Nothing

                    else if 0x0001083F <= code && code <= 0x00010855 || 0x00010860 <= code && code <= 0x00010876 || 0x00010880 <= code && code <= 0x0001089E then
                        Just LetterOther

                    else if code == 0x00010857 then
                        Just PunctuationOther

                    else if 0x00010858 <= code && code <= 0x0001085F || 0x00010879 <= code && code <= 0x0001087F then
                        Just NumberOther

                    else if 0x00010877 <= code && code <= 0x00010878 then
                        Just SymbolOther

                    else
                        Nothing

                else if code < 0x000109BF then
                    if code < 0x0001091E then
                        if 0x000108A7 <= code && code <= 0x000108AF || 0x000108FB <= code && code <= 0x000108FF || 0x00010916 <= code && code <= 0x0001091B then
                            Just NumberOther

                        else if 0x000108E0 <= code && code <= 0x000108F2 || 0x000108F4 <= code && code <= 0x000108F5 || 0x00010900 <= code && code <= 0x00010915 then
                            Just LetterOther

                        else
                            Nothing

                    else if code == 0x0001091F || code == 0x0001093F then
                        Just PunctuationOther

                    else if 0x00010920 <= code && code <= 0x00010939 || 0x00010940 <= code && code <= 0x00010959 || 0x00010980 <= code && code <= 0x000109B7 || code == 0x000109BE then
                        Just LetterOther

                    else if 0x000109BC <= code && code <= 0x000109BD then
                        Just NumberOther

                    else
                        Nothing

                else if code < 0x00010A0F then
                    if code == 0x000109BF || code == 0x00010A00 then
                        Just LetterOther

                    else if 0x000109C0 <= code && code <= 0x000109CF || 0x000109D2 <= code && code <= 0x000109FF then
                        Just NumberOther

                    else if 0x00010A01 <= code && code <= 0x00010A03 || 0x00010A05 <= code && code <= 0x00010A06 || 0x00010A0C <= code && code <= 0x00010A0E then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x00010A0F || 0x00010A38 <= code && code <= 0x00010A3A || code == 0x00010A3F then
                    Just MarkNonSpacing

                else if 0x00010A10 <= code && code <= 0x00010A13 || 0x00010A15 <= code && code <= 0x00010A17 || 0x00010A19 <= code && code <= 0x00010A35 then
                    Just LetterOther

                else if 0x00010A40 <= code && code <= 0x00010A48 then
                    Just NumberOther

                else if 0x00010A50 <= code && code <= 0x00010A58 then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x00010F45 then
                if code < 0x00010D2F then
                    if code < 0x00010B3F then
                        if code < 0x00010AC7 then
                            if 0x00010A60 <= code && code <= 0x00010A7C || 0x00010A80 <= code && code <= 0x00010A9C || 0x00010AC0 <= code && code <= 0x00010AC6 then
                                Just LetterOther

                            else if 0x00010A7D <= code && code <= 0x00010A7E || 0x00010A9D <= code && code <= 0x00010A9F then
                                Just NumberOther

                            else if code == 0x00010A7F then
                                Just PunctuationOther

                            else
                                Nothing

                        else if code == 0x00010AC7 || 0x00010AC9 <= code && code <= 0x00010AE4 || 0x00010B00 <= code && code <= 0x00010B35 then
                            Just LetterOther

                        else if code == 0x00010AC8 then
                            Just SymbolOther

                        else if 0x00010AE5 <= code && code <= 0x00010AE6 then
                            Just MarkNonSpacing

                        else if 0x00010AEB <= code && code <= 0x00010AEF then
                            Just NumberOther

                        else if 0x00010AF0 <= code && code <= 0x00010AF6 || 0x00010B39 <= code && code <= 0x00010B3E then
                            Just PunctuationOther

                        else
                            Nothing

                    else if code < 0x00010BA8 then
                        if code == 0x00010B3F || 0x00010B99 <= code && code <= 0x00010B9C then
                            Just PunctuationOther

                        else if 0x00010B40 <= code && code <= 0x00010B55 || 0x00010B60 <= code && code <= 0x00010B72 || 0x00010B80 <= code && code <= 0x00010B91 then
                            Just LetterOther

                        else if 0x00010B58 <= code && code <= 0x00010B5F || 0x00010B78 <= code && code <= 0x00010B7F then
                            Just NumberOther

                        else
                            Nothing

                    else if 0x00010BA9 <= code && code <= 0x00010BAF || 0x00010CFA <= code && code <= 0x00010CFF then
                        Just NumberOther

                    else if 0x00010C00 <= code && code <= 0x00010C48 || 0x00010D00 <= code && code <= 0x00010D23 then
                        Just LetterOther

                    else if 0x00010C80 <= code && code <= 0x00010CB2 then
                        Just LetterUppercase

                    else if 0x00010CC0 <= code && code <= 0x00010CF2 then
                        Just LetterLowercase

                    else if 0x00010D24 <= code && code <= 0x00010D27 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x00010EAA then
                    if code < 0x00010D68 then
                        if 0x00010D30 <= code && code <= 0x00010D39 || 0x00010D40 <= code && code <= 0x00010D49 then
                            Just NumberDecimalDigit

                        else if 0x00010D4A <= code && code <= 0x00010D4D || code == 0x00010D4F then
                            Just LetterOther

                        else if code == 0x00010D4E then
                            Just LetterModifier

                        else if 0x00010D50 <= code && code <= 0x00010D65 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if 0x00010D69 <= code && code <= 0x00010D6D then
                        Just MarkNonSpacing

                    else if code == 0x00010D6E then
                        Just PunctuationDash

                    else if code == 0x00010D6F then
                        Just LetterModifier

                    else if 0x00010D70 <= code && code <= 0x00010D85 then
                        Just LetterLowercase

                    else if 0x00010D8E <= code && code <= 0x00010D8F then
                        Just SymbolMath

                    else if 0x00010E60 <= code && code <= 0x00010E7E then
                        Just NumberOther

                    else if 0x00010E80 <= code && code <= 0x00010EA9 then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x00010ECF then
                    if 0x00010EAB <= code && code <= 0x00010EAC then
                        Just MarkNonSpacing

                    else if code == 0x00010EAD then
                        Just PunctuationDash

                    else if 0x00010EB0 <= code && code <= 0x00010EB1 || 0x00010EC2 <= code && code <= 0x00010EC4 || 0x00010EC6 <= code && code <= 0x00010EC7 then
                        Just LetterOther

                    else if code == 0x00010EC5 then
                        Just LetterModifier

                    else
                        Nothing

                else if code == 0x00010ED0 then
                    Just PunctuationOther

                else if 0x00010ED1 <= code && code <= 0x00010ED8 then
                    Just SymbolOther

                else if 0x00010EFA <= code && code <= 0x00010EFF then
                    Just MarkNonSpacing

                else if 0x00010F00 <= code && code <= 0x00010F1C || code == 0x00010F27 || 0x00010F30 <= code && code <= 0x00010F44 then
                    Just LetterOther

                else if 0x00010F1D <= code && code <= 0x00010F26 then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x000110B2 then
                if code < 0x00011002 then
                    if code == 0x00010F45 || 0x00010F70 <= code && code <= 0x00010F81 || 0x00010FB0 <= code && code <= 0x00010FC4 || 0x00010FE0 <= code && code <= 0x00010FF6 then
                        Just LetterOther

                    else if 0x00010F46 <= code && code <= 0x00010F50 || 0x00010F82 <= code && code <= 0x00010F85 || code == 0x00011001 then
                        Just MarkNonSpacing

                    else if 0x00010F51 <= code && code <= 0x00010F54 || 0x00010FC5 <= code && code <= 0x00010FCB then
                        Just NumberOther

                    else if 0x00010F55 <= code && code <= 0x00010F59 || 0x00010F86 <= code && code <= 0x00010F89 then
                        Just PunctuationOther

                    else if code == 0x00011000 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x00011070 then
                    if code == 0x00011002 then
                        Just MarkSpacingCombining

                    else if 0x00011003 <= code && code <= 0x00011037 then
                        Just LetterOther

                    else if 0x00011038 <= code && code <= 0x00011046 then
                        Just MarkNonSpacing

                    else if 0x00011047 <= code && code <= 0x0001104D then
                        Just PunctuationOther

                    else if 0x00011052 <= code && code <= 0x00011065 then
                        Just NumberOther

                    else if 0x00011066 <= code && code <= 0x0001106F then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if code == 0x00011070 || 0x00011073 <= code && code <= 0x00011074 || 0x0001107F <= code && code <= 0x00011081 then
                    Just MarkNonSpacing

                else if 0x00011071 <= code && code <= 0x00011072 || code == 0x00011075 || 0x00011083 <= code && code <= 0x000110AF then
                    Just LetterOther

                else if code == 0x00011082 || 0x000110B0 <= code && code <= 0x000110B1 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x0001112B then
                if code < 0x000110C1 then
                    if code == 0x000110B2 || 0x000110B7 <= code && code <= 0x000110B8 then
                        Just MarkSpacingCombining

                    else if 0x000110B3 <= code && code <= 0x000110B6 || 0x000110B9 <= code && code <= 0x000110BA then
                        Just MarkNonSpacing

                    else if 0x000110BB <= code && code <= 0x000110BC || 0x000110BE <= code && code <= 0x000110C0 then
                        Just PunctuationOther

                    else if code == 0x000110BD then
                        Just OtherFormat

                    else
                        Nothing

                else if code == 0x000110C1 then
                    Just PunctuationOther

                else if code == 0x000110C2 || 0x00011100 <= code && code <= 0x00011102 || 0x00011127 <= code && code <= 0x0001112A then
                    Just MarkNonSpacing

                else if code == 0x000110CD then
                    Just OtherFormat

                else if 0x000110D0 <= code && code <= 0x000110E8 || 0x00011103 <= code && code <= 0x00011126 then
                    Just LetterOther

                else if 0x000110F0 <= code && code <= 0x000110F9 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x00011146 then
                if code == 0x0001112B || 0x0001112D <= code && code <= 0x00011134 then
                    Just MarkNonSpacing

                else if code == 0x0001112C || code == 0x00011145 then
                    Just MarkSpacingCombining

                else if 0x00011136 <= code && code <= 0x0001113F then
                    Just NumberDecimalDigit

                else if 0x00011140 <= code && code <= 0x00011143 then
                    Just PunctuationOther

                else if code == 0x00011144 then
                    Just LetterOther

                else
                    Nothing

            else if code == 0x00011146 || code == 0x00011182 then
                Just MarkSpacingCombining

            else if code == 0x00011147 || 0x00011150 <= code && code <= 0x00011172 || code == 0x00011176 || 0x00011183 <= code && code <= 0x000111B1 then
                Just LetterOther

            else if code == 0x00011173 || 0x00011180 <= code && code <= 0x00011181 then
                Just MarkNonSpacing

            else if 0x00011174 <= code && code <= 0x00011175 then
                Just PunctuationOther

            else
                Nothing

        else if code < 0x000114C5 then
            if code < 0x00011346 then
                if code < 0x0001123E then
                    if code < 0x000111DA then
                        if code == 0x000111B2 || 0x000111C1 <= code && code <= 0x000111C4 then
                            Just LetterOther

                        else if 0x000111B3 <= code && code <= 0x000111B5 || 0x000111BF <= code && code <= 0x000111C0 || code == 0x000111CE then
                            Just MarkSpacingCombining

                        else if 0x000111B6 <= code && code <= 0x000111BE || 0x000111C9 <= code && code <= 0x000111CC || code == 0x000111CF then
                            Just MarkNonSpacing

                        else if 0x000111C5 <= code && code <= 0x000111C8 || code == 0x000111CD then
                            Just PunctuationOther

                        else if 0x000111D0 <= code && code <= 0x000111D9 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if code < 0x0001122B then
                        if code == 0x000111DA || code == 0x000111DC || 0x00011200 <= code && code <= 0x00011211 || 0x00011213 <= code && code <= 0x0001122A then
                            Just LetterOther

                        else if code == 0x000111DB || 0x000111DD <= code && code <= 0x000111DF then
                            Just PunctuationOther

                        else if 0x000111E1 <= code && code <= 0x000111F4 then
                            Just NumberOther

                        else
                            Nothing

                    else if code == 0x0001122B then
                        Just LetterOther

                    else if 0x0001122C <= code && code <= 0x0001122E || 0x00011232 <= code && code <= 0x00011233 || code == 0x00011235 then
                        Just MarkSpacingCombining

                    else if 0x0001122F <= code && code <= 0x00011231 || code == 0x00011234 || 0x00011236 <= code && code <= 0x00011237 then
                        Just MarkNonSpacing

                    else if 0x00011238 <= code && code <= 0x0001123D then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x000112EF then
                    if code < 0x0001128E then
                        if code == 0x0001123E || code == 0x00011241 then
                            Just MarkNonSpacing

                        else if 0x0001123F <= code && code <= 0x00011240 || 0x00011280 <= code && code <= 0x00011286 || code == 0x00011288 || 0x0001128A <= code && code <= 0x0001128D then
                            Just LetterOther

                        else
                            Nothing

                    else if 0x0001128F <= code && code <= 0x0001129D || 0x0001129F <= code && code <= 0x000112A8 || 0x000112B0 <= code && code <= 0x000112DE then
                        Just LetterOther

                    else if code == 0x000112A9 then
                        Just PunctuationOther

                    else if code == 0x000112DF || 0x000112E3 <= code && code <= 0x000112EA then
                        Just MarkNonSpacing

                    else if 0x000112E0 <= code && code <= 0x000112E2 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code < 0x00011331 then
                    if 0x000112F0 <= code && code <= 0x000112F9 then
                        Just NumberDecimalDigit

                    else if 0x00011300 <= code && code <= 0x00011301 then
                        Just MarkNonSpacing

                    else if 0x00011302 <= code && code <= 0x00011303 then
                        Just MarkSpacingCombining

                    else if 0x00011305 <= code && code <= 0x0001130C || 0x0001130F <= code && code <= 0x00011310 || 0x00011313 <= code && code <= 0x00011328 || 0x0001132A <= code && code <= 0x00011330 then
                        Just LetterOther

                    else
                        Nothing

                else if 0x00011332 <= code && code <= 0x00011333 || 0x00011335 <= code && code <= 0x00011339 || code == 0x0001133D then
                    Just LetterOther

                else if 0x0001133B <= code && code <= 0x0001133C || code == 0x00011340 then
                    Just MarkNonSpacing

                else if 0x0001133E <= code && code <= 0x0001133F || 0x00011341 <= code && code <= 0x00011344 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x000113D6 then
                if code < 0x000113B7 then
                    if 0x00011347 <= code && code <= 0x00011348 || 0x0001134B <= code && code <= 0x0001134D || code == 0x00011357 || 0x00011362 <= code && code <= 0x00011363 then
                        Just MarkSpacingCombining

                    else if code == 0x00011350 || 0x0001135D <= code && code <= 0x00011361 || 0x00011380 <= code && code <= 0x00011389 || code == 0x0001138B || code == 0x0001138E || 0x00011390 <= code && code <= 0x000113B5 then
                        Just LetterOther

                    else if 0x00011366 <= code && code <= 0x0001136C || 0x00011370 <= code && code <= 0x00011374 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code < 0x000113CD then
                    if code == 0x000113B7 then
                        Just LetterOther

                    else if 0x000113B8 <= code && code <= 0x000113BA || code == 0x000113C2 || code == 0x000113C5 || 0x000113C7 <= code && code <= 0x000113CA || code == 0x000113CC then
                        Just MarkSpacingCombining

                    else if 0x000113BB <= code && code <= 0x000113C0 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x000113CD || code == 0x000113CF then
                    Just MarkSpacingCombining

                else if code == 0x000113CE || code == 0x000113D0 || code == 0x000113D2 then
                    Just MarkNonSpacing

                else if code == 0x000113D1 || code == 0x000113D3 then
                    Just LetterOther

                else if 0x000113D4 <= code && code <= 0x000113D5 then
                    Just PunctuationOther

                else
                    Nothing

            else if code < 0x0001145C then
                if code < 0x00011441 then
                    if 0x000113D7 <= code && code <= 0x000113D8 then
                        Just PunctuationOther

                    else if 0x000113E1 <= code && code <= 0x000113E2 || 0x00011438 <= code && code <= 0x0001143F then
                        Just MarkNonSpacing

                    else if 0x00011400 <= code && code <= 0x00011434 then
                        Just LetterOther

                    else if 0x00011435 <= code && code <= 0x00011437 || code == 0x00011440 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code == 0x00011441 || code == 0x00011445 then
                    Just MarkSpacingCombining

                else if 0x00011442 <= code && code <= 0x00011444 || code == 0x00011446 then
                    Just MarkNonSpacing

                else if 0x00011447 <= code && code <= 0x0001144A then
                    Just LetterOther

                else if 0x0001144B <= code && code <= 0x0001144F || 0x0001145A <= code && code <= 0x0001145B then
                    Just PunctuationOther

                else if 0x00011450 <= code && code <= 0x00011459 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x000114B8 then
                if code == 0x0001145D then
                    Just PunctuationOther

                else if code == 0x0001145E || 0x000114B3 <= code && code <= 0x000114B7 then
                    Just MarkNonSpacing

                else if 0x0001145F <= code && code <= 0x00011461 || 0x00011480 <= code && code <= 0x000114AF then
                    Just LetterOther

                else if 0x000114B0 <= code && code <= 0x000114B2 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code == 0x000114B8 || code == 0x000114BA || 0x000114BF <= code && code <= 0x000114C0 || 0x000114C2 <= code && code <= 0x000114C3 then
                Just MarkNonSpacing

            else if code == 0x000114B9 || 0x000114BB <= code && code <= 0x000114BE || code == 0x000114C1 then
                Just MarkSpacingCombining

            else if code == 0x000114C4 then
                Just LetterOther

            else
                Nothing

        else if code < 0x00011837 then
            if code < 0x000116AA then
                if code < 0x000115DB then
                    if code < 0x000115B1 then
                        if code == 0x000114C5 || code == 0x000114C7 || 0x00011580 <= code && code <= 0x000115AE then
                            Just LetterOther

                        else if code == 0x000114C6 then
                            Just PunctuationOther

                        else if 0x000114D0 <= code && code <= 0x000114D9 then
                            Just NumberDecimalDigit

                        else if 0x000115AF <= code && code <= 0x000115B0 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code == 0x000115B1 || 0x000115B8 <= code && code <= 0x000115BB || code == 0x000115BE then
                        Just MarkSpacingCombining

                    else if 0x000115B2 <= code && code <= 0x000115B5 || 0x000115BC <= code && code <= 0x000115BD || 0x000115BF <= code && code <= 0x000115C0 then
                        Just MarkNonSpacing

                    else if 0x000115C1 <= code && code <= 0x000115D7 then
                        Just PunctuationOther

                    else if 0x000115D8 <= code && code <= 0x000115DA then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x0001163D then
                    if code == 0x000115DB || 0x00011600 <= code && code <= 0x0001162F then
                        Just LetterOther

                    else if 0x000115DC <= code && code <= 0x000115DD || 0x00011633 <= code && code <= 0x0001163A then
                        Just MarkNonSpacing

                    else if 0x00011630 <= code && code <= 0x00011632 || 0x0001163B <= code && code <= 0x0001163C then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code == 0x0001163D || 0x0001163F <= code && code <= 0x00011640 then
                    Just MarkNonSpacing

                else if code == 0x0001163E then
                    Just MarkSpacingCombining

                else if 0x00011641 <= code && code <= 0x00011643 || 0x00011660 <= code && code <= 0x0001166C then
                    Just PunctuationOther

                else if code == 0x00011644 || 0x00011680 <= code && code <= 0x000116A9 then
                    Just LetterOther

                else if 0x00011650 <= code && code <= 0x00011659 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x0001171D then
                if code < 0x000116B5 then
                    if code == 0x000116AA then
                        Just LetterOther

                    else if code == 0x000116AB || code == 0x000116AD || 0x000116B0 <= code && code <= 0x000116B4 then
                        Just MarkNonSpacing

                    else if code == 0x000116AC || 0x000116AE <= code && code <= 0x000116AF then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if code == 0x000116B5 || code == 0x000116B7 then
                    Just MarkNonSpacing

                else if code == 0x000116B6 then
                    Just MarkSpacingCombining

                else if code == 0x000116B8 || 0x00011700 <= code && code <= 0x0001171A then
                    Just LetterOther

                else if code == 0x000116B9 then
                    Just PunctuationOther

                else if 0x000116C0 <= code && code <= 0x000116C9 || 0x000116D0 <= code && code <= 0x000116E3 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x0001172F then
                if code == 0x0001171D || code == 0x0001171F || 0x00011722 <= code && code <= 0x00011725 || 0x00011727 <= code && code <= 0x0001172B then
                    Just MarkNonSpacing

                else if code == 0x0001171E || 0x00011720 <= code && code <= 0x00011721 || code == 0x00011726 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if 0x00011730 <= code && code <= 0x00011739 then
                Just NumberDecimalDigit

            else if 0x0001173A <= code && code <= 0x0001173B then
                Just NumberOther

            else if 0x0001173C <= code && code <= 0x0001173E then
                Just PunctuationOther

            else if code == 0x0001173F then
                Just SymbolOther

            else if 0x00011740 <= code && code <= 0x00011746 || 0x00011800 <= code && code <= 0x0001182B then
                Just LetterOther

            else if 0x0001182C <= code && code <= 0x0001182E then
                Just MarkSpacingCombining

            else if 0x0001182F <= code && code <= 0x00011836 then
                Just MarkNonSpacing

            else
                Nothing

        else if code < 0x000119D0 then
            if code < 0x0001192F then
                if code < 0x000118DF then
                    if code == 0x00011837 || 0x00011839 <= code && code <= 0x0001183A then
                        Just MarkNonSpacing

                    else if code == 0x00011838 then
                        Just MarkSpacingCombining

                    else if code == 0x0001183B then
                        Just PunctuationOther

                    else if 0x000118A0 <= code && code <= 0x000118BF then
                        Just LetterUppercase

                    else if 0x000118C0 <= code && code <= 0x000118DE then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0x000118DF then
                    Just LetterLowercase

                else if 0x000118E0 <= code && code <= 0x000118E9 then
                    Just NumberDecimalDigit

                else if 0x000118EA <= code && code <= 0x000118F2 then
                    Just NumberOther

                else if 0x000118FF <= code && code <= 0x00011906 || code == 0x00011909 || 0x0001190C <= code && code <= 0x00011913 || 0x00011915 <= code && code <= 0x00011916 || 0x00011918 <= code && code <= 0x0001192E then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x0001193F then
                if code == 0x0001192F then
                    Just LetterOther

                else if 0x00011930 <= code && code <= 0x00011935 || 0x00011937 <= code && code <= 0x00011938 || code == 0x0001193D then
                    Just MarkSpacingCombining

                else if 0x0001193B <= code && code <= 0x0001193C || code == 0x0001193E then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code == 0x0001193F || code == 0x00011941 || 0x000119A0 <= code && code <= 0x000119A7 || 0x000119AA <= code && code <= 0x000119CF then
                Just LetterOther

            else if code == 0x00011940 || code == 0x00011942 then
                Just MarkSpacingCombining

            else if code == 0x00011943 then
                Just MarkNonSpacing

            else if 0x00011944 <= code && code <= 0x00011946 then
                Just PunctuationOther

            else if 0x00011950 <= code && code <= 0x00011959 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code < 0x00011A38 then
            if code < 0x000119E1 then
                if code == 0x000119D0 then
                    Just LetterOther

                else if 0x000119D1 <= code && code <= 0x000119D3 || 0x000119DC <= code && code <= 0x000119DF then
                    Just MarkSpacingCombining

                else if 0x000119D4 <= code && code <= 0x000119D7 || 0x000119DA <= code && code <= 0x000119DB || code == 0x000119E0 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code == 0x000119E1 || code == 0x000119E3 || code == 0x00011A00 || 0x00011A0B <= code && code <= 0x00011A32 then
                Just LetterOther

            else if code == 0x000119E2 then
                Just PunctuationOther

            else if code == 0x000119E4 then
                Just MarkSpacingCombining

            else if 0x00011A01 <= code && code <= 0x00011A0A || 0x00011A33 <= code && code <= 0x00011A37 then
                Just MarkNonSpacing

            else
                Nothing

        else if code < 0x00011A50 then
            if code == 0x00011A38 || 0x00011A3B <= code && code <= 0x00011A3E || code == 0x00011A47 then
                Just MarkNonSpacing

            else if code == 0x00011A39 then
                Just MarkSpacingCombining

            else if code == 0x00011A3A then
                Just LetterOther

            else if 0x00011A3F <= code && code <= 0x00011A46 then
                Just PunctuationOther

            else
                Nothing

        else if code == 0x00011A50 || 0x00011A5C <= code && code <= 0x00011A89 then
            Just LetterOther

        else if 0x00011A51 <= code && code <= 0x00011A56 || 0x00011A59 <= code && code <= 0x00011A5B || 0x00011A8A <= code && code <= 0x00011A96 || 0x00011A98 <= code && code <= 0x00011A99 then
            Just MarkNonSpacing

        else if 0x00011A57 <= code && code <= 0x00011A58 || code == 0x00011A97 then
            Just MarkSpacingCombining

        else if 0x00011A9A <= code && code <= 0x00011A9C then
            Just PunctuationOther

        else
            Nothing

    else if code < 0x0001D4BA then
        if code < 0x00016A6F then
            if code < 0x00011D97 then
                if code < 0x00011C71 then
                    if code < 0x00011BEF then
                        if code == 0x00011A9D || 0x00011AB0 <= code && code <= 0x00011AF8 || 0x00011BC0 <= code && code <= 0x00011BE0 then
                            Just LetterOther

                        else if 0x00011A9E <= code && code <= 0x00011AA2 || 0x00011B00 <= code && code <= 0x00011B09 || code == 0x00011BE1 then
                            Just PunctuationOther

                        else if code == 0x00011B60 || 0x00011B62 <= code && code <= 0x00011B64 || code == 0x00011B66 then
                            Just MarkNonSpacing

                        else if code == 0x00011B61 || code == 0x00011B65 || code == 0x00011B67 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code < 0x00011C3D then
                        if 0x00011BF0 <= code && code <= 0x00011BF9 then
                            Just NumberDecimalDigit

                        else if 0x00011C00 <= code && code <= 0x00011C08 || 0x00011C0A <= code && code <= 0x00011C2E then
                            Just LetterOther

                        else if code == 0x00011C2F then
                            Just MarkSpacingCombining

                        else if 0x00011C30 <= code && code <= 0x00011C36 || 0x00011C38 <= code && code <= 0x00011C3C then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if code == 0x00011C3D || code == 0x00011C3F then
                        Just MarkNonSpacing

                    else if code == 0x00011C3E then
                        Just MarkSpacingCombining

                    else if code == 0x00011C40 then
                        Just LetterOther

                    else if 0x00011C41 <= code && code <= 0x00011C45 || code == 0x00011C70 then
                        Just PunctuationOther

                    else if 0x00011C50 <= code && code <= 0x00011C59 then
                        Just NumberDecimalDigit

                    else if 0x00011C5A <= code && code <= 0x00011C6C then
                        Just NumberOther

                    else
                        Nothing

                else if code < 0x00011D39 then
                    if code < 0x00011CB1 then
                        if code == 0x00011C71 then
                            Just PunctuationOther

                        else if 0x00011C72 <= code && code <= 0x00011C8F then
                            Just LetterOther

                        else if 0x00011C92 <= code && code <= 0x00011CA7 || 0x00011CAA <= code && code <= 0x00011CB0 then
                            Just MarkNonSpacing

                        else if code == 0x00011CA9 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if code == 0x00011CB1 || code == 0x00011CB4 then
                        Just MarkSpacingCombining

                    else if 0x00011CB2 <= code && code <= 0x00011CB3 || 0x00011CB5 <= code && code <= 0x00011CB6 || 0x00011D31 <= code && code <= 0x00011D36 then
                        Just MarkNonSpacing

                    else if 0x00011D00 <= code && code <= 0x00011D06 || 0x00011D08 <= code && code <= 0x00011D09 || 0x00011D0B <= code && code <= 0x00011D30 then
                        Just LetterOther

                    else
                        Nothing

                else if code < 0x00011D66 then
                    if code == 0x00011D3A || 0x00011D3C <= code && code <= 0x00011D3D || 0x00011D3F <= code && code <= 0x00011D45 || code == 0x00011D47 then
                        Just MarkNonSpacing

                    else if code == 0x00011D46 || 0x00011D60 <= code && code <= 0x00011D65 then
                        Just LetterOther

                    else if 0x00011D50 <= code && code <= 0x00011D59 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if 0x00011D67 <= code && code <= 0x00011D68 || 0x00011D6A <= code && code <= 0x00011D89 then
                    Just LetterOther

                else if 0x00011D8A <= code && code <= 0x00011D8E || 0x00011D93 <= code && code <= 0x00011D94 || code == 0x00011D96 then
                    Just MarkSpacingCombining

                else if 0x00011D90 <= code && code <= 0x00011D91 || code == 0x00011D95 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x00011FBF then
                if code < 0x00011F02 then
                    if code == 0x00011D97 || 0x00011EF3 <= code && code <= 0x00011EF4 || 0x00011F00 <= code && code <= 0x00011F01 then
                        Just MarkNonSpacing

                    else if code == 0x00011D98 || 0x00011DB0 <= code && code <= 0x00011DD8 || 0x00011DDA <= code && code <= 0x00011DDB || 0x00011EE0 <= code && code <= 0x00011EF2 then
                        Just LetterOther

                    else if 0x00011DA0 <= code && code <= 0x00011DA9 || 0x00011DE0 <= code && code <= 0x00011DE9 then
                        Just NumberDecimalDigit

                    else if code == 0x00011DD9 then
                        Just LetterModifier

                    else if 0x00011EF5 <= code && code <= 0x00011EF6 then
                        Just MarkSpacingCombining

                    else if 0x00011EF7 <= code && code <= 0x00011EF8 then
                        Just PunctuationOther

                    else
                        Nothing

                else if code < 0x00011F3F then
                    if code == 0x00011F02 || 0x00011F04 <= code && code <= 0x00011F10 || 0x00011F12 <= code && code <= 0x00011F33 then
                        Just LetterOther

                    else if code == 0x00011F03 || 0x00011F34 <= code && code <= 0x00011F35 || code == 0x00011F3E then
                        Just MarkSpacingCombining

                    else if 0x00011F36 <= code && code <= 0x00011F3A then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x00011F3F || code == 0x00011F41 then
                    Just MarkSpacingCombining

                else if code == 0x00011F40 || code == 0x00011F42 || code == 0x00011F5A then
                    Just MarkNonSpacing

                else if 0x00011F43 <= code && code <= 0x00011F4F then
                    Just PunctuationOther

                else if 0x00011F50 <= code && code <= 0x00011F59 then
                    Just NumberDecimalDigit

                else if code == 0x00011FB0 then
                    Just LetterOther

                else
                    Nothing

            else if code < 0x0001343F then
                if code < 0x000123FF then
                    if 0x00011FC0 <= code && code <= 0x00011FD4 then
                        Just NumberOther

                    else if 0x00011FD5 <= code && code <= 0x00011FDC || 0x00011FE1 <= code && code <= 0x00011FF1 then
                        Just SymbolOther

                    else if 0x00011FDD <= code && code <= 0x00011FE0 then
                        Just SymbolCurrency

                    else if code == 0x00011FFF then
                        Just PunctuationOther

                    else if 0x00012000 <= code && code <= 0x00012399 then
                        Just LetterOther

                    else
                        Nothing

                else if 0x00012400 <= code && code <= 0x0001246E then
                    Just NumberLetter

                else if 0x00012470 <= code && code <= 0x00012474 || 0x00012FF1 <= code && code <= 0x00012FF2 then
                    Just PunctuationOther

                else if 0x00012480 <= code && code <= 0x00012543 || 0x00012F90 <= code && code <= 0x00012FF0 || 0x00013000 <= code && code <= 0x0001342F then
                    Just LetterOther

                else if 0x00013430 <= code && code <= 0x0001343E then
                    Just OtherFormat

                else
                    Nothing

            else if code < 0x0001611D then
                if code == 0x0001343F then
                    Just OtherFormat

                else if code == 0x00013440 || 0x00013447 <= code && code <= 0x00013455 then
                    Just MarkNonSpacing

                else if 0x00013441 <= code && code <= 0x00013446 || 0x00013460 <= code && code <= 0x000143FA || 0x00014400 <= code && code <= 0x00014646 || 0x00016100 <= code && code <= 0x0001611C then
                    Just LetterOther

                else
                    Nothing

            else if code == 0x0001611D || 0x00016800 <= code && code <= 0x00016A38 || 0x00016A40 <= code && code <= 0x00016A5E then
                Just LetterOther

            else if 0x0001611E <= code && code <= 0x00016129 || 0x0001612D <= code && code <= 0x0001612F then
                Just MarkNonSpacing

            else if 0x0001612A <= code && code <= 0x0001612C then
                Just MarkSpacingCombining

            else if 0x00016130 <= code && code <= 0x00016139 || 0x00016A60 <= code && code <= 0x00016A69 then
                Just NumberDecimalDigit

            else if code == 0x00016A6E then
                Just PunctuationOther

            else
                Nothing

        else if code < 0x0001BBFF then
            if code < 0x00016E9F then
                if code < 0x00016B4F then
                    if code < 0x00016AFF then
                        if code == 0x00016A6F || code == 0x00016AF5 then
                            Just PunctuationOther

                        else if 0x00016A70 <= code && code <= 0x00016ABE || 0x00016AD0 <= code && code <= 0x00016AED then
                            Just LetterOther

                        else if 0x00016AC0 <= code && code <= 0x00016AC9 then
                            Just NumberDecimalDigit

                        else if 0x00016AF0 <= code && code <= 0x00016AF4 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if 0x00016B00 <= code && code <= 0x00016B2F then
                        Just LetterOther

                    else if 0x00016B30 <= code && code <= 0x00016B36 then
                        Just MarkNonSpacing

                    else if 0x00016B37 <= code && code <= 0x00016B3B || code == 0x00016B44 then
                        Just PunctuationOther

                    else if 0x00016B3C <= code && code <= 0x00016B3F || code == 0x00016B45 then
                        Just SymbolOther

                    else if 0x00016B40 <= code && code <= 0x00016B43 then
                        Just LetterModifier

                    else
                        Nothing

                else if code < 0x00016D6A then
                    if 0x00016B50 <= code && code <= 0x00016B59 then
                        Just NumberDecimalDigit

                    else if 0x00016B5B <= code && code <= 0x00016B61 then
                        Just NumberOther

                    else if 0x00016B63 <= code && code <= 0x00016B77 || 0x00016B7D <= code && code <= 0x00016B8F || 0x00016D43 <= code && code <= 0x00016D69 then
                        Just LetterOther

                    else if 0x00016D40 <= code && code <= 0x00016D42 then
                        Just LetterModifier

                    else
                        Nothing

                else if code == 0x00016D6A then
                    Just LetterOther

                else if 0x00016D6B <= code && code <= 0x00016D6C then
                    Just LetterModifier

                else if 0x00016D6D <= code && code <= 0x00016D6F || 0x00016E97 <= code && code <= 0x00016E9A then
                    Just PunctuationOther

                else if 0x00016D70 <= code && code <= 0x00016D79 then
                    Just NumberDecimalDigit

                else if 0x00016E40 <= code && code <= 0x00016E5F then
                    Just LetterUppercase

                else if 0x00016E60 <= code && code <= 0x00016E7F then
                    Just LetterLowercase

                else if 0x00016E80 <= code && code <= 0x00016E96 then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x00016FF1 then
                if code < 0x00016F8E then
                    if 0x00016EA0 <= code && code <= 0x00016EB8 then
                        Just LetterUppercase

                    else if 0x00016EBB <= code && code <= 0x00016ED3 then
                        Just LetterLowercase

                    else if 0x00016F00 <= code && code <= 0x00016F4A || code == 0x00016F50 then
                        Just LetterOther

                    else if code == 0x00016F4F then
                        Just MarkNonSpacing

                    else if 0x00016F51 <= code && code <= 0x00016F87 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if 0x00016F8F <= code && code <= 0x00016F92 || code == 0x00016FE4 then
                    Just MarkNonSpacing

                else if 0x00016F93 <= code && code <= 0x00016F9F || 0x00016FE0 <= code && code <= 0x00016FE1 || code == 0x00016FE3 then
                    Just LetterModifier

                else if code == 0x00016FE2 then
                    Just PunctuationOther

                else if code == 0x00016FF0 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if code < 0x0001AFF4 then
                if code == 0x00016FF1 then
                    Just MarkSpacingCombining

                else if 0x00016FF2 <= code && code <= 0x00016FF3 || 0x0001AFF0 <= code && code <= 0x0001AFF3 then
                    Just LetterModifier

                else if 0x00016FF4 <= code && code <= 0x00016FF6 then
                    Just NumberLetter

                else if 0x00017000 <= code && code <= 0x00018CD5 || 0x00018CFF <= code && code <= 0x00018D1E || 0x00018D80 <= code && code <= 0x00018DF2 then
                    Just LetterOther

                else
                    Nothing

            else if 0x0001AFF5 <= code && code <= 0x0001AFFB || 0x0001AFFD <= code && code <= 0x0001AFFE then
                Just LetterModifier

            else if 0x0001B000 <= code && code <= 0x0001B122 || code == 0x0001B132 || 0x0001B150 <= code && code <= 0x0001B152 || code == 0x0001B155 || 0x0001B164 <= code && code <= 0x0001B167 || 0x0001B170 <= code && code <= 0x0001B2FB then
                Just LetterOther

            else
                Nothing

        else if code < 0x0001D17A then
            if code < 0x0001CEDF then
                if code < 0x0001BC9E then
                    if 0x0001BC00 <= code && code <= 0x0001BC6A || 0x0001BC70 <= code && code <= 0x0001BC7C || 0x0001BC80 <= code && code <= 0x0001BC88 || 0x0001BC90 <= code && code <= 0x0001BC99 then
                        Just LetterOther

                    else if code == 0x0001BC9C then
                        Just SymbolOther

                    else if code == 0x0001BC9D then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if code == 0x0001BC9E then
                    Just MarkNonSpacing

                else if code == 0x0001BC9F then
                    Just PunctuationOther

                else if 0x0001BCA0 <= code && code <= 0x0001BCA3 then
                    Just OtherFormat

                else if 0x0001CC00 <= code && code <= 0x0001CCEF || 0x0001CCFA <= code && code <= 0x0001CCFC || 0x0001CD00 <= code && code <= 0x0001CEB3 || 0x0001CEBA <= code && code <= 0x0001CED0 then
                    Just SymbolOther

                else if 0x0001CCF0 <= code && code <= 0x0001CCF9 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code < 0x0001D0FF then
                if 0x0001CEE0 <= code && code <= 0x0001CEEF || 0x0001CF50 <= code && code <= 0x0001CFC3 || 0x0001D000 <= code && code <= 0x0001D0F5 then
                    Just SymbolOther

                else if code == 0x0001CEF0 then
                    Just SymbolMath

                else if 0x0001CF00 <= code && code <= 0x0001CF2D || 0x0001CF30 <= code && code <= 0x0001CF46 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if 0x0001D100 <= code && code <= 0x0001D126 || 0x0001D129 <= code && code <= 0x0001D164 || 0x0001D16A <= code && code <= 0x0001D16C then
                Just SymbolOther

            else if 0x0001D165 <= code && code <= 0x0001D166 || 0x0001D16D <= code && code <= 0x0001D172 then
                Just MarkSpacingCombining

            else if 0x0001D167 <= code && code <= 0x0001D169 then
                Just MarkNonSpacing

            else if 0x0001D173 <= code && code <= 0x0001D179 then
                Just OtherFormat

            else
                Nothing

        else if code < 0x0001D3FF then
            if code < 0x0001D1FF then
                if code == 0x0001D17A then
                    Just OtherFormat

                else if 0x0001D17B <= code && code <= 0x0001D182 || 0x0001D185 <= code && code <= 0x0001D18B || 0x0001D1AA <= code && code <= 0x0001D1AD then
                    Just MarkNonSpacing

                else if 0x0001D183 <= code && code <= 0x0001D184 || 0x0001D18C <= code && code <= 0x0001D1A9 || 0x0001D1AE <= code && code <= 0x0001D1EA then
                    Just SymbolOther

                else
                    Nothing

            else if 0x0001D200 <= code && code <= 0x0001D241 || code == 0x0001D245 || 0x0001D300 <= code && code <= 0x0001D356 then
                Just SymbolOther

            else if 0x0001D242 <= code && code <= 0x0001D244 then
                Just MarkNonSpacing

            else if 0x0001D2C0 <= code && code <= 0x0001D2D3 || 0x0001D2E0 <= code && code <= 0x0001D2F3 || 0x0001D360 <= code && code <= 0x0001D378 then
                Just NumberOther

            else
                Nothing

        else if code < 0x0001D49B then
            if 0x0001D400 <= code && code <= 0x0001D419 || 0x0001D434 <= code && code <= 0x0001D44D || 0x0001D468 <= code && code <= 0x0001D481 then
                Just LetterUppercase

            else if 0x0001D41A <= code && code <= 0x0001D433 || 0x0001D44E <= code && code <= 0x0001D454 || 0x0001D456 <= code && code <= 0x0001D467 || 0x0001D482 <= code && code <= 0x0001D49A then
                Just LetterLowercase

            else
                Nothing

        else if code == 0x0001D49B || 0x0001D4B6 <= code && code <= 0x0001D4B9 then
            Just LetterLowercase

        else if code == 0x0001D49C || 0x0001D49E <= code && code <= 0x0001D49F || code == 0x0001D4A2 || 0x0001D4A5 <= code && code <= 0x0001D4A6 || 0x0001D4A9 <= code && code <= 0x0001D4AC || 0x0001D4AE <= code && code <= 0x0001D4B5 then
            Just LetterUppercase

        else
            Nothing

    else if code < 0x0001E6E6 then
        if code < 0x0001D7C3 then
            if code < 0x0001D655 then
                if code < 0x0001D53F then
                    if code == 0x0001D4BB || 0x0001D4BD <= code && code <= 0x0001D4C3 || 0x0001D4C5 <= code && code <= 0x0001D4CF || 0x0001D4EA <= code && code <= 0x0001D503 || 0x0001D51E <= code && code <= 0x0001D537 then
                        Just LetterLowercase

                    else if 0x0001D4D0 <= code && code <= 0x0001D4E9 || 0x0001D504 <= code && code <= 0x0001D505 || 0x0001D507 <= code && code <= 0x0001D50A || 0x0001D50D <= code && code <= 0x0001D514 || 0x0001D516 <= code && code <= 0x0001D51C || 0x0001D538 <= code && code <= 0x0001D539 || 0x0001D53B <= code && code <= 0x0001D53E then
                        Just LetterUppercase

                    else
                        Nothing

                else if code < 0x0001D59F then
                    if 0x0001D540 <= code && code <= 0x0001D544 || code == 0x0001D546 || 0x0001D54A <= code && code <= 0x0001D550 || 0x0001D56C <= code && code <= 0x0001D585 then
                        Just LetterUppercase

                    else if 0x0001D552 <= code && code <= 0x0001D56B || 0x0001D586 <= code && code <= 0x0001D59E then
                        Just LetterLowercase

                    else
                        Nothing

                else if code == 0x0001D59F || 0x0001D5BA <= code && code <= 0x0001D5D3 || 0x0001D5EE <= code && code <= 0x0001D607 || 0x0001D622 <= code && code <= 0x0001D63B then
                    Just LetterLowercase

                else if 0x0001D5A0 <= code && code <= 0x0001D5B9 || 0x0001D5D4 <= code && code <= 0x0001D5ED || 0x0001D608 <= code && code <= 0x0001D621 || 0x0001D63C <= code && code <= 0x0001D654 then
                    Just LetterUppercase

                else
                    Nothing

            else if code < 0x0001D715 then
                if code == 0x0001D655 || 0x0001D670 <= code && code <= 0x0001D689 || 0x0001D6A8 <= code && code <= 0x0001D6C0 || 0x0001D6E2 <= code && code <= 0x0001D6FA then
                    Just LetterUppercase

                else if 0x0001D656 <= code && code <= 0x0001D66F || 0x0001D68A <= code && code <= 0x0001D6A5 || 0x0001D6C2 <= code && code <= 0x0001D6DA || 0x0001D6DC <= code && code <= 0x0001D6E1 || 0x0001D6FC <= code && code <= 0x0001D714 then
                    Just LetterLowercase

                else if code == 0x0001D6C1 || code == 0x0001D6DB || code == 0x0001D6FB then
                    Just SymbolMath

                else
                    Nothing

            else if code < 0x0001D755 then
                if code == 0x0001D715 || code == 0x0001D735 || code == 0x0001D74F then
                    Just SymbolMath

                else if 0x0001D716 <= code && code <= 0x0001D71B || 0x0001D736 <= code && code <= 0x0001D74E || 0x0001D750 <= code && code <= 0x0001D754 then
                    Just LetterLowercase

                else if 0x0001D71C <= code && code <= 0x0001D734 then
                    Just LetterUppercase

                else
                    Nothing

            else if code == 0x0001D755 || 0x0001D770 <= code && code <= 0x0001D788 || 0x0001D78A <= code && code <= 0x0001D78F || 0x0001D7AA <= code && code <= 0x0001D7C2 then
                Just LetterLowercase

            else if 0x0001D756 <= code && code <= 0x0001D76E || 0x0001D790 <= code && code <= 0x0001D7A8 then
                Just LetterUppercase

            else if code == 0x0001D76F || code == 0x0001D789 || code == 0x0001D7A9 then
                Just SymbolMath

            else
                Nothing

        else if code < 0x0001E02F then
            if code < 0x0001DA84 then
                if code == 0x0001D7C3 then
                    Just SymbolMath

                else if 0x0001D7C4 <= code && code <= 0x0001D7C9 || code == 0x0001D7CB then
                    Just LetterLowercase

                else if code == 0x0001D7CA then
                    Just LetterUppercase

                else if 0x0001D7CE <= code && code <= 0x0001D7FF then
                    Just NumberDecimalDigit

                else if 0x0001D800 <= code && code <= 0x0001D9FF || 0x0001DA37 <= code && code <= 0x0001DA3A || 0x0001DA6D <= code && code <= 0x0001DA74 || 0x0001DA76 <= code && code <= 0x0001DA83 then
                    Just SymbolOther

                else if 0x0001DA00 <= code && code <= 0x0001DA36 || 0x0001DA3B <= code && code <= 0x0001DA6C || code == 0x0001DA75 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if code < 0x0001DF0A then
                if code == 0x0001DA84 || 0x0001DA9B <= code && code <= 0x0001DA9F || 0x0001DAA1 <= code && code <= 0x0001DAAF then
                    Just MarkNonSpacing

                else if 0x0001DA85 <= code && code <= 0x0001DA86 then
                    Just SymbolOther

                else if 0x0001DA87 <= code && code <= 0x0001DA8B then
                    Just PunctuationOther

                else if 0x0001DF00 <= code && code <= 0x0001DF09 then
                    Just LetterLowercase

                else
                    Nothing

            else if code == 0x0001DF0A then
                Just LetterOther

            else if 0x0001DF0B <= code && code <= 0x0001DF1E || 0x0001DF25 <= code && code <= 0x0001DF2A then
                Just LetterLowercase

            else if 0x0001E000 <= code && code <= 0x0001E006 || 0x0001E008 <= code && code <= 0x0001E018 || 0x0001E01B <= code && code <= 0x0001E021 || 0x0001E023 <= code && code <= 0x0001E024 || 0x0001E026 <= code && code <= 0x0001E02A then
                Just MarkNonSpacing

            else
                Nothing

        else if code < 0x0001E2FE then
            if code < 0x0001E14D then
                if 0x0001E030 <= code && code <= 0x0001E06D || 0x0001E137 <= code && code <= 0x0001E13D then
                    Just LetterModifier

                else if code == 0x0001E08F || 0x0001E130 <= code && code <= 0x0001E136 then
                    Just MarkNonSpacing

                else if 0x0001E100 <= code && code <= 0x0001E12C then
                    Just LetterOther

                else if 0x0001E140 <= code && code <= 0x0001E149 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if code == 0x0001E14E || 0x0001E290 <= code && code <= 0x0001E2AD || 0x0001E2C0 <= code && code <= 0x0001E2EB then
                Just LetterOther

            else if code == 0x0001E14F then
                Just SymbolOther

            else if code == 0x0001E2AE || 0x0001E2EC <= code && code <= 0x0001E2EF then
                Just MarkNonSpacing

            else if 0x0001E2F0 <= code && code <= 0x0001E2F9 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code < 0x0001E5EF then
            if code == 0x0001E2FF then
                Just SymbolCurrency

            else if 0x0001E4D0 <= code && code <= 0x0001E4EA || 0x0001E5D0 <= code && code <= 0x0001E5ED then
                Just LetterOther

            else if code == 0x0001E4EB then
                Just LetterModifier

            else if 0x0001E4EC <= code && code <= 0x0001E4EF || code == 0x0001E5EE then
                Just MarkNonSpacing

            else if 0x0001E4F0 <= code && code <= 0x0001E4F9 then
                Just NumberDecimalDigit

            else
                Nothing

        else if code == 0x0001E5EF || code == 0x0001E6E3 then
            Just MarkNonSpacing

        else if code == 0x0001E5F0 || 0x0001E6C0 <= code && code <= 0x0001E6DE || 0x0001E6E0 <= code && code <= 0x0001E6E2 || 0x0001E6E4 <= code && code <= 0x0001E6E5 then
            Just LetterOther

        else if 0x0001E5F1 <= code && code <= 0x0001E5FA then
            Just NumberDecimalDigit

        else if code == 0x0001E5FF then
            Just PunctuationOther

        else
            Nothing

    else if code < 0x0001F02F then
        if code < 0x0001ED2D then
            if code < 0x0001E8CF then
                if code < 0x0001E6FE then
                    if code == 0x0001E6E6 || 0x0001E6EE <= code && code <= 0x0001E6EF || code == 0x0001E6F5 then
                        Just MarkNonSpacing

                    else if 0x0001E6E7 <= code && code <= 0x0001E6ED || 0x0001E6F0 <= code && code <= 0x0001E6F4 then
                        Just LetterOther

                    else
                        Nothing

                else if code == 0x0001E6FE || 0x0001E7E0 <= code && code <= 0x0001E7E6 || 0x0001E7E8 <= code && code <= 0x0001E7EB || 0x0001E7ED <= code && code <= 0x0001E7EE || 0x0001E7F0 <= code && code <= 0x0001E7FE || 0x0001E800 <= code && code <= 0x0001E8C4 then
                    Just LetterOther

                else if code == 0x0001E6FF then
                    Just LetterModifier

                else if 0x0001E8C7 <= code && code <= 0x0001E8CE then
                    Just NumberOther

                else
                    Nothing

            else if code < 0x0001E95D then
                if code == 0x0001E8CF then
                    Just NumberOther

                else if 0x0001E8D0 <= code && code <= 0x0001E8D6 || 0x0001E944 <= code && code <= 0x0001E94A then
                    Just MarkNonSpacing

                else if 0x0001E900 <= code && code <= 0x0001E921 then
                    Just LetterUppercase

                else if 0x0001E922 <= code && code <= 0x0001E943 then
                    Just LetterLowercase

                else if code == 0x0001E94B then
                    Just LetterModifier

                else if 0x0001E950 <= code && code <= 0x0001E959 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if 0x0001E95E <= code && code <= 0x0001E95F then
                Just PunctuationOther

            else if 0x0001EC71 <= code && code <= 0x0001ECAB || 0x0001ECAD <= code && code <= 0x0001ECAF || 0x0001ECB1 <= code && code <= 0x0001ECB4 || 0x0001ED01 <= code && code <= 0x0001ED2C then
                Just NumberOther

            else if code == 0x0001ECAC then
                Just SymbolOther

            else if code == 0x0001ECB0 then
                Just SymbolCurrency

            else
                Nothing

        else if code < 0x0001EE60 then
            if code < 0x0001EE26 then
                if code == 0x0001ED2D || 0x0001ED2F <= code && code <= 0x0001ED3D then
                    Just NumberOther

                else if code == 0x0001ED2E then
                    Just SymbolOther

                else if 0x0001EE00 <= code && code <= 0x0001EE03 || 0x0001EE05 <= code && code <= 0x0001EE1F || 0x0001EE21 <= code && code <= 0x0001EE22 || code == 0x0001EE24 then
                    Just LetterOther

                else
                    Nothing

            else if code == 0x0001EE27 || 0x0001EE29 <= code && code <= 0x0001EE32 || 0x0001EE34 <= code && code <= 0x0001EE37 || code == 0x0001EE42 || 0x0001EE4D <= code && code <= 0x0001EE4F || 0x0001EE51 <= code && code <= 0x0001EE52 || code == 0x0001EE54 || modBy 2 code == 1 && (0x0001EE39 <= code && code <= 0x0001EE3B || 0x0001EE47 <= code && code <= 0x0001EE4B || 0x0001EE57 <= code && code <= 0x0001EE5F) then
                Just LetterOther

            else
                Nothing

        else if code < 0x0001EE7F then
            if 0x0001EE61 <= code && code <= 0x0001EE62 || code == 0x0001EE64 || 0x0001EE67 <= code && code <= 0x0001EE6A || 0x0001EE6C <= code && code <= 0x0001EE72 || 0x0001EE74 <= code && code <= 0x0001EE77 || 0x0001EE79 <= code && code <= 0x0001EE7C || code == 0x0001EE7E then
                Just LetterOther

            else
                Nothing

        else if 0x0001EE80 <= code && code <= 0x0001EE89 || 0x0001EE8B <= code && code <= 0x0001EE9B || 0x0001EEA1 <= code && code <= 0x0001EEA3 || 0x0001EEA5 <= code && code <= 0x0001EEA9 || 0x0001EEAB <= code && code <= 0x0001EEBB then
            Just LetterOther

        else if 0x0001EEF0 <= code && code <= 0x0001EEF1 then
            Just SymbolMath

        else if 0x0001F000 <= code && code <= 0x0001F02B then
            Just SymbolOther

        else
            Nothing

    else if code < 0x0001F8BF then
        if code < 0x0001F3FA then
            if code < 0x0001F10C then
                if 0x0001F030 <= code && code <= 0x0001F093 || 0x0001F0A0 <= code && code <= 0x0001F0AE || 0x0001F0B1 <= code && code <= 0x0001F0BF || 0x0001F0C1 <= code && code <= 0x0001F0CF || 0x0001F0D1 <= code && code <= 0x0001F0F5 then
                    Just SymbolOther

                else if 0x0001F100 <= code && code <= 0x0001F10B then
                    Just NumberOther

                else
                    Nothing

            else if code == 0x0001F10C then
                Just NumberOther

            else if 0x0001F10D <= code && code <= 0x0001F1AD || 0x0001F1E6 <= code && code <= 0x0001F202 || 0x0001F210 <= code && code <= 0x0001F23B || 0x0001F240 <= code && code <= 0x0001F248 || 0x0001F250 <= code && code <= 0x0001F251 || 0x0001F260 <= code && code <= 0x0001F265 || 0x0001F300 <= code && code <= 0x0001F3F9 then
                Just SymbolOther

            else
                Nothing

        else if code < 0x0001F7EF then
            if code == 0x0001F3FA || 0x0001F400 <= code && code <= 0x0001F6D8 || 0x0001F6DC <= code && code <= 0x0001F6EC || 0x0001F6F0 <= code && code <= 0x0001F6FC || 0x0001F700 <= code && code <= 0x0001F7D9 || 0x0001F7E0 <= code && code <= 0x0001F7EB then
                Just SymbolOther

            else if 0x0001F3FB <= code && code <= 0x0001F3FF then
                Just SymbolModifier

            else
                Nothing

        else if code == 0x0001F7F0 || 0x0001F800 <= code && code <= 0x0001F80B || 0x0001F810 <= code && code <= 0x0001F847 || 0x0001F850 <= code && code <= 0x0001F859 || 0x0001F860 <= code && code <= 0x0001F887 || 0x0001F890 <= code && code <= 0x0001F8AD || 0x0001F8B0 <= code && code <= 0x0001F8BB then
            Just SymbolOther

        else
            Nothing

    else if code < 0x0001FBEF then
        if code < 0x0001FA8D then
            if 0x0001F8C0 <= code && code <= 0x0001F8C1 || 0x0001F900 <= code && code <= 0x0001FA57 || 0x0001FA60 <= code && code <= 0x0001FA6D || 0x0001FA70 <= code && code <= 0x0001FA7C || 0x0001FA80 <= code && code <= 0x0001FA8A then
                Just SymbolOther

            else if 0x0001F8D0 <= code && code <= 0x0001F8D8 then
                Just SymbolMath

            else
                Nothing

        else if 0x0001FA8E <= code && code <= 0x0001FAC6 || code == 0x0001FAC8 || 0x0001FACD <= code && code <= 0x0001FADC || 0x0001FADF <= code && code <= 0x0001FAEA || 0x0001FAEF <= code && code <= 0x0001FAF8 || 0x0001FB00 <= code && code <= 0x0001FB92 || 0x0001FB94 <= code && code <= 0x0001FBEE then
            Just SymbolOther

        else
            Nothing

    else if code < 0x0002EBEF then
        if code == 0x0001FBEF || code == 0x0001FBFA then
            Just SymbolOther

        else if 0x0001FBF0 <= code && code <= 0x0001FBF9 then
            Just NumberDecimalDigit

        else if 0x00020000 <= code && code <= 0x0002A6DF || 0x0002A700 <= code && code <= 0x0002B81D || 0x0002B820 <= code && code <= 0x0002CEAD || 0x0002CEB0 <= code && code <= 0x0002EBE0 then
            Just LetterOther

        else
            Nothing

    else if 0x0002EBF0 <= code && code <= 0x0002EE5D || 0x0002F800 <= code && code <= 0x0002FA1D || 0x00030000 <= code && code <= 0x0003134A || 0x00031350 <= code && code <= 0x00033479 then
        Just LetterOther

    else if code == 0x000E0001 || 0x000E0020 <= code && code <= 0x000E007F then
        Just OtherFormat

    else if 0x000E0100 <= code && code <= 0x000E01EF then
        Just MarkNonSpacing

    else if 0x000F0000 <= code && code <= 0x0010FFFD then
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


{-| Detect spaces (Unicode category Zs)
-}
isSpace : Char -> Bool
isSpace c =
    let
        code =
            Char.toCode c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if code < 0x0100 then
        code == 0x20 || code == 0xA0

    else
        code == 0x1680 || 0x2000 <= code && code <= 0x200A || code == 0x202F || code == 0x205F || code == 0x3000


{-| Detect spaces (Unicode categories Zs, Zl, Zp)
-}
isSeparator : Char -> Bool
isSeparator c =
    let
        code =
            Char.toCode c
    in
    if Basics.isNaN (Basics.toFloat code) then
        False

    else if code < 0x0100 then
        code == 0x20 || code == 0xA0

    else
        code == 0x1680 || 0x2000 <= code && code <= 0x200A || 0x2028 <= code && code <= 0x2029 || code == 0x202F || code == 0x205F || code == 0x3000
