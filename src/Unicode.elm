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
            (Char.toUpper c == c) && (Char.toLower c /= c)

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if simple then
        (code <= 0x215F) || r 0x2170 0x24B5 || r 0x24D0 0x000F0000

    else if l 0x0001D4CF then
        if l 0x213D then
            r 0x03D2 0x03D4 || e 0x2102 || e 0x2107 || r 0x210B 0x210D || r 0x2110 0x2112 || e 0x2115 || r 0x2119 0x211D || e 0x2124 || e 0x2128 || r 0x212A 0x212D || r 0x2130 0x2133

        else
            r 0x213E 0x213F || e 0x2145 || r 0x0001D400 0x0001D419 || r 0x0001D434 0x0001D44D || r 0x0001D468 0x0001D481 || e 0x0001D49C || r 0x0001D49E 0x0001D49F || e 0x0001D4A2 || r 0x0001D4A5 0x0001D4A6 || r 0x0001D4A9 0x0001D4AC || r 0x0001D4AE 0x0001D4B5

    else if l 0x0001D59F then
        r 0x0001D4D0 0x0001D4E9 || r 0x0001D504 0x0001D505 || r 0x0001D507 0x0001D50A || r 0x0001D50D 0x0001D514 || r 0x0001D516 0x0001D51C || r 0x0001D538 0x0001D539 || r 0x0001D53B 0x0001D53E || r 0x0001D540 0x0001D544 || e 0x0001D546 || r 0x0001D54A 0x0001D550 || r 0x0001D56C 0x0001D585

    else
        r 0x0001D5A0 0x0001D5B9 || r 0x0001D5D4 0x0001D5ED || r 0x0001D608 0x0001D621 || r 0x0001D63C 0x0001D655 || r 0x0001D670 0x0001D689 || r 0x0001D6A8 0x0001D6C0 || r 0x0001D6E2 0x0001D6FA || r 0x0001D71C 0x0001D734 || r 0x0001D756 0x0001D76E || r 0x0001D790 0x0001D7A8 || e 0x0001D7CA


{-| Detect lower case characters (Unicode category Ll)
-}
isLower : Char -> Bool
isLower c =
    let
        code =
            Char.toCode c

        simple =
            (Char.toLower c == c) && (Char.toUpper c /= c)

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if simple then
        (code <= 0x0344) || r 0x0346 0x216F || r 0x2180 0x24CF || r 0x24EA 0x000F0000

    else if l 0xA7F9 then
        if l 0x2109 then
            if l 0x024E then
                r 0x0137 0x0138 || r 0x018C 0x018D || r 0x0199 0x019B || r 0x01AA 0x01AB || r 0x01B9 0x01BA || r 0x01BD 0x01BF || e 0x0221 || r 0x0233 0x0239

            else
                r 0x024F 0x0293 || r 0x0295 0x02AF || r 0x03FB 0x03FC || r 0x0560 0x0588 || r 0x1D00 0x1D2B || r 0x1D6B 0x1D77 || r 0x1D79 0x1D9A || r 0x1E95 0x1E9D || e 0x1E9F

        else if l 0x2C70 then
            e 0x210A || r 0x210E 0x210F || e 0x2113 || e 0x212F || e 0x2134 || e 0x2139 || r 0x213C 0x213D || r 0x2146 0x2149

        else
            e 0x2C71 || r 0x2C73 0x2C74 || r 0x2C76 0x2C7B || r 0x2CE3 0x2CE4 || r 0xA72F 0xA731 || r 0xA771 0xA778 || e 0xA78E || r 0xA793 0xA795 || e 0xA7AF || ((modBy 2 code == 1) && r 0xA7D3 0xA7D5)

    else if l 0x0001D621 then
        if l 0x0001D4BA then
            e 0xA7FA || r 0xAB30 0xAB5A || r 0xAB60 0xAB68 || r 0x0001D41A 0x0001D433 || r 0x0001D44E 0x0001D454 || r 0x0001D456 0x0001D467 || r 0x0001D482 0x0001D49B || r 0x0001D4B6 0x0001D4B9

        else
            e 0x0001D4BB || r 0x0001D4BD 0x0001D4C3 || r 0x0001D4C5 0x0001D4CF || r 0x0001D4EA 0x0001D503 || r 0x0001D51E 0x0001D537 || r 0x0001D552 0x0001D56B || r 0x0001D586 0x0001D59F || r 0x0001D5BA 0x0001D5D3 || r 0x0001D5EE 0x0001D607

    else if l 0x0001D74F then
        r 0x0001D622 0x0001D63B || r 0x0001D656 0x0001D66F || r 0x0001D68A 0x0001D6A5 || r 0x0001D6C2 0x0001D6DA || r 0x0001D6DC 0x0001D6E1 || r 0x0001D6FC 0x0001D714 || r 0x0001D716 0x0001D71B || r 0x0001D736 0x0001D74E

    else
        r 0x0001D750 0x0001D755 || r 0x0001D770 0x0001D788 || r 0x0001D78A 0x0001D78F || r 0x0001D7AA 0x0001D7C2 || r 0x0001D7C4 0x0001D7C9 || e 0x0001D7CB || r 0x0001DF00 0x0001DF09 || r 0x0001DF0B 0x0001DF1E || r 0x0001DF25 0x0001DF2A


{-| Detect letters (Unicode categories Lu, Ll, Lt, Lm, Lo)
-}
isAlpha : Char -> Bool
isAlpha c =
    let
        code =
            Char.toCode c

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if l 0x0100 then
        r 0x41 0x5A || r 0x61 0x7A || e 0xAA || e 0xB5 || e 0xBA || r 0xC0 0xD6 || r 0xD8 0xF6 || r 0xF8 0xFF

    else if l 0xAA43 then
        if l 0x109F then
            if l 0x0AF8 then
                if l 0x085F then
                    if l 0x061F then
                        if l 0x038B then
                            r 0x0100 0x02C1 || r 0x02C6 0x02D1 || r 0x02E0 0x02E4 || r 0x0370 0x0374 || r 0x0376 0x0377 || r 0x037A 0x037D || e 0x037F || e 0x0386 || r 0x0388 0x038A || ((modBy 2 code == 0) && r 0x02EC 0x02EE)

                        else
                            e 0x038C || r 0x038E 0x03A1 || r 0x03A3 0x03F5 || r 0x03F7 0x0481 || r 0x048A 0x052F || r 0x0531 0x0556 || e 0x0559 || r 0x0560 0x0588 || r 0x05D0 0x05EA || r 0x05EF 0x05F2

                    else if l 0x074C then
                        r 0x0620 0x064A || r 0x066E 0x066F || r 0x0671 0x06D3 || e 0x06D5 || r 0x06E5 0x06E6 || r 0x06EE 0x06EF || r 0x06FA 0x06FC || e 0x06FF || e 0x0710 || r 0x0712 0x072F

                    else
                        r 0x074D 0x07A5 || e 0x07B1 || r 0x07CA 0x07EA || r 0x07F4 0x07F5 || e 0x07FA || r 0x0800 0x0815 || e 0x081A || e 0x0824 || e 0x0828 || r 0x0840 0x0858

                else if l 0x09FB then
                    if l 0x098E then
                        r 0x0860 0x086A || r 0x0870 0x0887 || r 0x0889 0x088E || r 0x08A0 0x08C9 || r 0x0904 0x0939 || e 0x093D || e 0x0950 || r 0x0958 0x0961 || r 0x0971 0x0980 || r 0x0985 0x098C

                    else
                        r 0x098F 0x0990 || r 0x0993 0x09A8 || r 0x09AA 0x09B0 || e 0x09B2 || r 0x09B6 0x09B9 || e 0x09BD || e 0x09CE || r 0x09DC 0x09DD || r 0x09DF 0x09E1 || r 0x09F0 0x09F1

                else if l 0x0A71 then
                    e 0x09FC || r 0x0A05 0x0A0A || r 0x0A0F 0x0A10 || r 0x0A13 0x0A28 || r 0x0A2A 0x0A30 || r 0x0A32 0x0A33 || r 0x0A35 0x0A36 || r 0x0A38 0x0A39 || r 0x0A59 0x0A5C || e 0x0A5E

                else
                    r 0x0A72 0x0A74 || r 0x0A85 0x0A8D || r 0x0A8F 0x0A91 || r 0x0A93 0x0AA8 || r 0x0AAA 0x0AB0 || r 0x0AB2 0x0AB3 || r 0x0AB5 0x0AB9 || e 0x0ABD || e 0x0AD0 || r 0x0AE0 0x0AE1

            else if l 0x0D03 then
                if l 0x0BAD then
                    if l 0x0B70 then
                        e 0x0AF9 || r 0x0B05 0x0B0C || r 0x0B0F 0x0B10 || r 0x0B13 0x0B28 || r 0x0B2A 0x0B30 || r 0x0B32 0x0B33 || r 0x0B35 0x0B39 || e 0x0B3D || r 0x0B5C 0x0B5D || r 0x0B5F 0x0B61

                    else
                        e 0x0B71 || e 0x0B83 || r 0x0B85 0x0B8A || r 0x0B8E 0x0B90 || r 0x0B92 0x0B95 || r 0x0B99 0x0B9A || e 0x0B9C || r 0x0B9E 0x0B9F || r 0x0BA3 0x0BA4 || r 0x0BA8 0x0BAA

                else if l 0x0C7F then
                    r 0x0BAE 0x0BB9 || e 0x0BD0 || r 0x0C05 0x0C0C || r 0x0C0E 0x0C10 || r 0x0C12 0x0C28 || r 0x0C2A 0x0C39 || e 0x0C3D || r 0x0C58 0x0C5A || e 0x0C5D || r 0x0C60 0x0C61

                else
                    e 0x0C80 || r 0x0C85 0x0C8C || r 0x0C8E 0x0C90 || r 0x0C92 0x0CA8 || r 0x0CAA 0x0CB3 || r 0x0CB5 0x0CB9 || e 0x0CBD || r 0x0CDD 0x0CDE || r 0x0CE0 0x0CE1 || r 0x0CF1 0x0CF2

            else if l 0x0EA4 then
                if l 0x0DB2 then
                    r 0x0D04 0x0D0C || r 0x0D0E 0x0D10 || r 0x0D12 0x0D3A || e 0x0D3D || e 0x0D4E || r 0x0D54 0x0D56 || r 0x0D5F 0x0D61 || r 0x0D7A 0x0D7F || r 0x0D85 0x0D96 || r 0x0D9A 0x0DB1

                else
                    r 0x0DB3 0x0DBB || e 0x0DBD || r 0x0DC0 0x0DC6 || r 0x0E01 0x0E30 || r 0x0E32 0x0E33 || r 0x0E40 0x0E46 || r 0x0E81 0x0E82 || e 0x0E84 || r 0x0E86 0x0E8A || r 0x0E8C 0x0EA3

            else if l 0x0F87 then
                e 0x0EA5 || r 0x0EA7 0x0EB0 || r 0x0EB2 0x0EB3 || e 0x0EBD || r 0x0EC0 0x0EC4 || e 0x0EC6 || r 0x0EDC 0x0EDF || e 0x0F00 || r 0x0F40 0x0F47 || r 0x0F49 0x0F6C

            else
                r 0x0F88 0x0F8C || r 0x1000 0x102A || e 0x103F || r 0x1050 0x1055 || r 0x105A 0x105D || e 0x1061 || r 0x1065 0x1066 || r 0x106E 0x1070 || r 0x1075 0x1081 || e 0x108E

        else if l 0x1FF1 then
            if l 0x18A9 then
                if l 0x1317 then
                    if l 0x125F then
                        r 0x10A0 0x10C5 || e 0x10C7 || e 0x10CD || r 0x10D0 0x10FA || r 0x10FC 0x1248 || r 0x124A 0x124D || r 0x1250 0x1256 || e 0x1258 || r 0x125A 0x125D

                    else
                        r 0x1260 0x1288 || r 0x128A 0x128D || r 0x1290 0x12B0 || r 0x12B2 0x12B5 || r 0x12B8 0x12BE || e 0x12C0 || r 0x12C2 0x12C5 || r 0x12C8 0x12D6 || r 0x12D8 0x1310 || r 0x1312 0x1315

                else if l 0x171E then
                    r 0x1318 0x135A || r 0x1380 0x138F || r 0x13A0 0x13F5 || r 0x13F8 0x13FD || r 0x1401 0x166C || r 0x166F 0x167F || r 0x1681 0x169A || r 0x16A0 0x16EA || r 0x16F1 0x16F8 || r 0x1700 0x1711

                else
                    r 0x171F 0x1731 || r 0x1740 0x1751 || r 0x1760 0x176C || r 0x176E 0x1770 || r 0x1780 0x17B3 || e 0x17D7 || e 0x17DC || r 0x1820 0x1878 || r 0x1880 0x1884 || r 0x1887 0x18A8

            else if l 0x1CBC then
                if l 0x1B04 then
                    e 0x18AA || r 0x18B0 0x18F5 || r 0x1900 0x191E || r 0x1950 0x196D || r 0x1970 0x1974 || r 0x1980 0x19AB || r 0x19B0 0x19C9 || r 0x1A00 0x1A16 || r 0x1A20 0x1A54 || e 0x1AA7

                else
                    r 0x1B05 0x1B33 || r 0x1B45 0x1B4C || r 0x1B83 0x1BA0 || r 0x1BAE 0x1BAF || r 0x1BBA 0x1BE5 || r 0x1C00 0x1C23 || r 0x1C4D 0x1C4F || r 0x1C5A 0x1C7D || r 0x1C80 0x1C88 || r 0x1C90 0x1CBA

            else if l 0x1F4F then
                r 0x1CBD 0x1CBF || r 0x1CE9 0x1CEC || r 0x1CEE 0x1CF3 || r 0x1CF5 0x1CF6 || e 0x1CFA || r 0x1D00 0x1DBF || r 0x1E00 0x1F15 || r 0x1F18 0x1F1D || r 0x1F20 0x1F45 || r 0x1F48 0x1F4D

            else
                r 0x1F50 0x1F57 || r 0x1F60 0x1F7D || r 0x1F80 0x1FB4 || r 0x1FB6 0x1FBC || e 0x1FBE || r 0x1FC2 0x1FC4 || r 0x1FC6 0x1FCC || r 0x1FD0 0x1FD3 || r 0x1FD6 0x1FDB || r 0x1FE0 0x1FEC || ((modBy 2 code == 1) && r 0x1F59 0x1F5F)

        else if l 0x30FB then
            if l 0x2D26 then
                if l 0x2129 then
                    r 0x1FF2 0x1FF4 || r 0x1FF6 0x1FFC || e 0x2071 || e 0x207F || r 0x2090 0x209C || e 0x2102 || e 0x2107 || r 0x210A 0x2113 || e 0x2115 || r 0x2119 0x211D || ((modBy 2 code == 0) && r 0x2124 0x2128)

                else
                    r 0x212A 0x212D || r 0x212F 0x2139 || r 0x213C 0x213F || r 0x2145 0x2149 || e 0x214E || r 0x2183 0x2184 || r 0x2C00 0x2CE4 || r 0x2CEB 0x2CEE || r 0x2CF2 0x2CF3 || r 0x2D00 0x2D25

            else if l 0x2DC7 then
                e 0x2D27 || e 0x2D2D || r 0x2D30 0x2D67 || e 0x2D6F || r 0x2D80 0x2D96 || r 0x2DA0 0x2DA6 || r 0x2DA8 0x2DAE || r 0x2DB0 0x2DB6 || r 0x2DB8 0x2DBE || r 0x2DC0 0x2DC6

            else
                r 0x2DC8 0x2DCE || r 0x2DD0 0x2DD6 || r 0x2DD8 0x2DDE || e 0x2E2F || r 0x3005 0x3006 || r 0x3031 0x3035 || r 0x303B 0x303C || r 0x3041 0x3096 || r 0x309D 0x309F || r 0x30A1 0x30FA

        else if l 0xA7D5 then
            if l 0xA4FF then
                r 0x30FC 0x30FF || r 0x3105 0x312F || r 0x3131 0x318E || r 0x31A0 0x31BF || r 0x31F0 0x31FF || e 0x3400 || e 0x4DBF || e 0x4E00 || r 0x9FFF 0xA48C || r 0xA4D0 0xA4FD

            else
                r 0xA500 0xA60C || r 0xA610 0xA61F || r 0xA62A 0xA62B || r 0xA640 0xA66E || r 0xA67F 0xA69D || r 0xA6A0 0xA6E5 || r 0xA717 0xA71F || r 0xA722 0xA788 || r 0xA78B 0xA7CA || r 0xA7D0 0xA7D1 || ((modBy 2 code == 1) && r 0xA7D3 0xA7D4)

        else if l 0xA909 then
            r 0xA7D6 0xA7D9 || r 0xA7F2 0xA801 || r 0xA803 0xA805 || r 0xA807 0xA80A || r 0xA80C 0xA822 || r 0xA840 0xA873 || r 0xA882 0xA8B3 || r 0xA8F2 0xA8F7 || e 0xA8FB || r 0xA8FD 0xA8FE || ((modBy 2 code == 1) && e 0xA7D5)

        else
            r 0xA90A 0xA925 || r 0xA930 0xA946 || r 0xA960 0xA97C || r 0xA984 0xA9B2 || e 0xA9CF || r 0xA9E0 0xA9E4 || r 0xA9E6 0xA9EF || r 0xA9FA 0xA9FE || r 0xAA00 0xAA28 || r 0xAA40 0xAA42

    else if l 0x00011643 then
        if l 0x0001077F then
            if l 0xFE75 then
                if l 0xD7A2 then
                    if l 0xAAF1 then
                        r 0xAA44 0xAA4B || r 0xAA60 0xAA76 || e 0xAA7A || r 0xAA7E 0xAAAF || e 0xAAB1 || r 0xAAB5 0xAAB6 || r 0xAAB9 0xAABD || r 0xAADB 0xAADD || r 0xAAE0 0xAAEA || ((modBy 2 code == 0) && r 0xAAC0 0xAAC2)

                    else
                        r 0xAAF2 0xAAF4 || r 0xAB01 0xAB06 || r 0xAB09 0xAB0E || r 0xAB11 0xAB16 || r 0xAB20 0xAB26 || r 0xAB28 0xAB2E || r 0xAB30 0xAB5A || r 0xAB5C 0xAB69 || r 0xAB70 0xABE2 || e 0xAC00

                else if l 0xFB37 then
                    e 0xD7A3 || r 0xD7B0 0xD7C6 || r 0xD7CB 0xD7FB || r 0xF900 0xFA6D || r 0xFA70 0xFAD9 || r 0xFB00 0xFB06 || r 0xFB13 0xFB17 || e 0xFB1D || r 0xFB1F 0xFB28 || r 0xFB2A 0xFB36

                else
                    r 0xFB38 0xFB3C || e 0xFB3E || r 0xFB40 0xFB41 || r 0xFB43 0xFB44 || r 0xFB46 0xFBB1 || r 0xFBD3 0xFD3D || r 0xFD50 0xFD8F || r 0xFD92 0xFDC7 || r 0xFDF0 0xFDFB || r 0xFE70 0xFE74

            else if l 0x0001034F then
                if l 0x00010027 then
                    r 0xFE76 0xFEFC || r 0xFF21 0xFF3A || r 0xFF41 0xFF5A || r 0xFF66 0xFFBE || r 0xFFC2 0xFFC7 || r 0xFFCA 0xFFCF || r 0xFFD2 0xFFD7 || r 0xFFDA 0xFFDC || r 0x00010000 0x0001000B || r 0x0001000D 0x00010026

                else
                    r 0x00010028 0x0001003A || r 0x0001003C 0x0001003D || r 0x0001003F 0x0001004D || r 0x00010050 0x0001005D || r 0x00010080 0x000100FA || r 0x00010280 0x0001029C || r 0x000102A0 0x000102D0 || r 0x00010300 0x0001031F || r 0x0001032D 0x00010340 || r 0x00010342 0x00010349

            else if l 0x0001057B then
                r 0x00010350 0x00010375 || r 0x00010380 0x0001039D || r 0x000103A0 0x000103C3 || r 0x000103C8 0x000103CF || r 0x00010400 0x0001049D || r 0x000104B0 0x000104D3 || r 0x000104D8 0x000104FB || r 0x00010500 0x00010527 || r 0x00010530 0x00010563 || r 0x00010570 0x0001057A

            else
                r 0x0001057C 0x0001058A || r 0x0001058C 0x00010592 || r 0x00010594 0x00010595 || r 0x00010597 0x000105A1 || r 0x000105A3 0x000105B1 || r 0x000105B3 0x000105B9 || r 0x000105BB 0x000105BC || r 0x00010600 0x00010736 || r 0x00010740 0x00010755 || r 0x00010760 0x00010767

        else if l 0x00010FDF then
            if l 0x00010A18 then
                if l 0x0001087F then
                    r 0x00010780 0x00010785 || r 0x00010787 0x000107B0 || r 0x000107B2 0x000107BA || r 0x00010800 0x00010805 || e 0x00010808 || r 0x0001080A 0x00010835 || r 0x00010837 0x00010838 || e 0x0001083C || r 0x0001083F 0x00010855 || r 0x00010860 0x00010876

                else
                    r 0x00010880 0x0001089E || r 0x000108E0 0x000108F2 || r 0x000108F4 0x000108F5 || r 0x00010900 0x00010915 || r 0x00010920 0x00010939 || r 0x00010980 0x000109B7 || r 0x000109BE 0x000109BF || e 0x00010A00 || r 0x00010A10 0x00010A13 || r 0x00010A15 0x00010A17

            else if l 0x00010C7F then
                r 0x00010A19 0x00010A35 || r 0x00010A60 0x00010A7C || r 0x00010A80 0x00010A9C || r 0x00010AC0 0x00010AC7 || r 0x00010AC9 0x00010AE4 || r 0x00010B00 0x00010B35 || r 0x00010B40 0x00010B55 || r 0x00010B60 0x00010B72 || r 0x00010B80 0x00010B91 || r 0x00010C00 0x00010C48

            else
                r 0x00010C80 0x00010CB2 || r 0x00010CC0 0x00010CF2 || r 0x00010D00 0x00010D23 || r 0x00010E80 0x00010EA9 || r 0x00010EB0 0x00010EB1 || r 0x00010F00 0x00010F1C || e 0x00010F27 || r 0x00010F30 0x00010F45 || r 0x00010F70 0x00010F81 || r 0x00010FB0 0x00010FC4

        else if l 0x0001129E then
            if l 0x00011175 then
                r 0x00010FE0 0x00010FF6 || r 0x00011003 0x00011037 || r 0x00011071 0x00011072 || e 0x00011075 || r 0x00011083 0x000110AF || r 0x000110D0 0x000110E8 || r 0x00011103 0x00011126 || e 0x00011144 || e 0x00011147 || r 0x00011150 0x00011172

            else
                e 0x00011176 || r 0x00011183 0x000111B2 || r 0x000111C1 0x000111C4 || r 0x00011200 0x00011211 || r 0x00011213 0x0001122B || r 0x0001123F 0x00011240 || r 0x00011280 0x00011286 || e 0x00011288 || r 0x0001128A 0x0001128D || r 0x0001128F 0x0001129D || ((modBy 2 code == 0) && r 0x000111DA 0x000111DC)

        else if l 0x0001135C then
            r 0x0001129F 0x000112A8 || r 0x000112B0 0x000112DE || r 0x00011305 0x0001130C || r 0x0001130F 0x00011310 || r 0x00011313 0x00011328 || r 0x0001132A 0x00011330 || r 0x00011332 0x00011333 || r 0x00011335 0x00011339 || e 0x0001133D || e 0x00011350

        else
            r 0x0001135D 0x00011361 || r 0x00011400 0x00011434 || r 0x00011447 0x0001144A || r 0x0001145F 0x00011461 || r 0x00011480 0x000114AF || r 0x000114C4 0x000114C5 || e 0x000114C7 || r 0x00011580 0x000115AE || r 0x000115D8 0x000115DB || r 0x00011600 0x0001162F

    else if l 0x0001D4A4 then
        if l 0x00012F8F then
            if l 0x00011AAF then
                if l 0x00011914 then
                    e 0x00011644 || r 0x00011680 0x000116AA || e 0x000116B8 || r 0x00011700 0x0001171A || r 0x00011740 0x00011746 || r 0x00011800 0x0001182B || r 0x000118A0 0x000118DF || r 0x000118FF 0x00011906 || e 0x00011909 || r 0x0001190C 0x00011913

                else
                    r 0x00011915 0x00011916 || r 0x00011918 0x0001192F || r 0x000119A0 0x000119A7 || r 0x000119AA 0x000119D0 || e 0x00011A00 || r 0x00011A0B 0x00011A32 || e 0x00011A3A || e 0x00011A50 || r 0x00011A5C 0x00011A89 || e 0x00011A9D || ((modBy 2 code == 1) && (r 0x0001193F 0x00011941 || r 0x000119E1 0x000119E3))

            else if l 0x00011D66 then
                r 0x00011AB0 0x00011AF8 || r 0x00011C00 0x00011C08 || r 0x00011C0A 0x00011C2E || e 0x00011C40 || r 0x00011C72 0x00011C8F || r 0x00011D00 0x00011D06 || r 0x00011D08 0x00011D09 || r 0x00011D0B 0x00011D30 || e 0x00011D46 || r 0x00011D60 0x00011D65

            else
                r 0x00011D67 0x00011D68 || r 0x00011D6A 0x00011D89 || e 0x00011D98 || r 0x00011EE0 0x00011EF2 || e 0x00011F02 || r 0x00011F04 0x00011F10 || r 0x00011F12 0x00011F33 || e 0x00011FB0 || r 0x00012000 0x00012399 || r 0x00012480 0x00012543

        else if l 0x000187FF then
            if l 0x00016B62 then
                r 0x00012F90 0x00012FF0 || r 0x00013000 0x0001342F || r 0x00013441 0x00013446 || r 0x00014400 0x00014646 || r 0x00016800 0x00016A38 || r 0x00016A40 0x00016A5E || r 0x00016A70 0x00016ABE || r 0x00016AD0 0x00016AED || r 0x00016B00 0x00016B2F || r 0x00016B40 0x00016B43

            else
                r 0x00016B63 0x00016B77 || r 0x00016B7D 0x00016B8F || r 0x00016E40 0x00016E7F || r 0x00016F00 0x00016F4A || e 0x00016F50 || r 0x00016F93 0x00016F9F || r 0x00016FE0 0x00016FE1 || e 0x00016FE3 || e 0x00017000 || e 0x000187F7

        else if l 0x0001B163 then
            r 0x00018800 0x00018CD5 || e 0x00018D00 || e 0x00018D08 || r 0x0001AFF0 0x0001AFF3 || r 0x0001AFF5 0x0001AFFB || r 0x0001AFFD 0x0001AFFE || r 0x0001B000 0x0001B122 || e 0x0001B132 || r 0x0001B150 0x0001B152 || e 0x0001B155

        else
            r 0x0001B164 0x0001B167 || r 0x0001B170 0x0001B2FB || r 0x0001BC00 0x0001BC6A || r 0x0001BC70 0x0001BC7C || r 0x0001BC80 0x0001BC88 || r 0x0001BC90 0x0001BC99 || r 0x0001D400 0x0001D454 || r 0x0001D456 0x0001D49C || r 0x0001D49E 0x0001D49F || e 0x0001D4A2

    else if l 0x0001E8FF then
        if l 0x0001D735 then
            if l 0x0001D53A then
                r 0x0001D4A5 0x0001D4A6 || r 0x0001D4A9 0x0001D4AC || r 0x0001D4AE 0x0001D4B9 || e 0x0001D4BB || r 0x0001D4BD 0x0001D4C3 || r 0x0001D4C5 0x0001D505 || r 0x0001D507 0x0001D50A || r 0x0001D50D 0x0001D514 || r 0x0001D516 0x0001D51C || r 0x0001D51E 0x0001D539

            else
                r 0x0001D53B 0x0001D53E || r 0x0001D540 0x0001D544 || e 0x0001D546 || r 0x0001D54A 0x0001D550 || r 0x0001D552 0x0001D6A5 || r 0x0001D6A8 0x0001D6C0 || r 0x0001D6C2 0x0001D6DA || r 0x0001D6DC 0x0001D6FA || r 0x0001D6FC 0x0001D714 || r 0x0001D716 0x0001D734

        else if l 0x0001E136 then
            r 0x0001D736 0x0001D74E || r 0x0001D750 0x0001D76E || r 0x0001D770 0x0001D788 || r 0x0001D78A 0x0001D7A8 || r 0x0001D7AA 0x0001D7C2 || r 0x0001D7C4 0x0001D7CB || r 0x0001DF00 0x0001DF1E || r 0x0001DF25 0x0001DF2A || r 0x0001E030 0x0001E06D || r 0x0001E100 0x0001E12C

        else
            r 0x0001E137 0x0001E13D || e 0x0001E14E || r 0x0001E290 0x0001E2AD || r 0x0001E2C0 0x0001E2EB || r 0x0001E4D0 0x0001E4EB || r 0x0001E7E0 0x0001E7E6 || r 0x0001E7E8 0x0001E7EB || r 0x0001E7ED 0x0001E7EE || r 0x0001E7F0 0x0001E7FE || r 0x0001E800 0x0001E8C4

    else if l 0x0001EE7F then
        if l 0x0001EE4C then
            r 0x0001E900 0x0001E943 || e 0x0001E94B || r 0x0001EE00 0x0001EE03 || r 0x0001EE05 0x0001EE1F || r 0x0001EE21 0x0001EE22 || e 0x0001EE24 || e 0x0001EE27 || r 0x0001EE29 0x0001EE32 || r 0x0001EE34 0x0001EE37 || e 0x0001EE42 || ((modBy 2 code == 1) && (r 0x0001EE39 0x0001EE3B || r 0x0001EE47 0x0001EE4B))

        else
            r 0x0001EE4D 0x0001EE4F || r 0x0001EE51 0x0001EE52 || e 0x0001EE54 || r 0x0001EE61 0x0001EE62 || e 0x0001EE64 || r 0x0001EE67 0x0001EE6A || r 0x0001EE6C 0x0001EE72 || r 0x0001EE74 0x0001EE77 || r 0x0001EE79 0x0001EE7C || e 0x0001EE7E || ((modBy 2 code == 1) && r 0x0001EE57 0x0001EE5F)

    else if l 0x0002B81C then
        r 0x0001EE80 0x0001EE89 || r 0x0001EE8B 0x0001EE9B || r 0x0001EEA1 0x0001EEA3 || r 0x0001EEA5 0x0001EEA9 || r 0x0001EEAB 0x0001EEBB || e 0x00020000 || e 0x0002A6DF || e 0x0002A700 || e 0x0002B739 || e 0x0002B740

    else
        e 0x0002B81D || e 0x0002B820 || e 0x0002CEA1 || e 0x0002CEB0 || e 0x0002EBE0 || r 0x0002F800 0x0002FA1D || e 0x00030000 || e 0x0003134A || e 0x00031350 || e 0x000323AF


{-| Detect digits (Unicode categories Nd, Nl, No)
-}
isDigit : Char -> Bool
isDigit c =
    let
        code =
            Char.toCode c

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if l 0x0100 then
        r 0x30 0x39 || r 0xB2 0xB3 || e 0xB9 || r 0xBC 0xBE

    else if l 0x000102E0 then
        if l 0x1C3F then
            if l 0x0DE5 then
                if l 0x0B65 then
                    r 0x0660 0x0669 || r 0x06F0 0x06F9 || r 0x07C0 0x07C9 || r 0x0966 0x096F || r 0x09E6 0x09EF || r 0x09F4 0x09F9 || r 0x0A66 0x0A6F || r 0x0AE6 0x0AEF

                else
                    r 0x0B66 0x0B6F || r 0x0B72 0x0B77 || r 0x0BE6 0x0BF2 || r 0x0C66 0x0C6F || r 0x0C78 0x0C7E || r 0x0CE6 0x0CEF || r 0x0D58 0x0D5E || r 0x0D66 0x0D78

            else if l 0x17DF then
                r 0x0DE6 0x0DEF || r 0x0E50 0x0E59 || r 0x0ED0 0x0ED9 || r 0x0F20 0x0F33 || r 0x1040 0x1049 || r 0x1090 0x1099 || r 0x1369 0x137C || r 0x16EE 0x16F0

            else
                r 0x17E0 0x17E9 || r 0x17F0 0x17F9 || r 0x1810 0x1819 || r 0x1946 0x194F || r 0x19D0 0x19DA || r 0x1A80 0x1A89 || r 0x1A90 0x1A99 || r 0x1B50 0x1B59 || r 0x1BB0 0x1BB9

        else if l 0x3247 then
            if l 0x24E9 then
                r 0x1C40 0x1C49 || r 0x1C50 0x1C59 || e 0x2070 || r 0x2074 0x2079 || r 0x2080 0x2089 || r 0x2150 0x2182 || r 0x2185 0x2189 || r 0x2460 0x249B

            else
                r 0x24EA 0x24FF || r 0x2776 0x2793 || e 0x2CFD || e 0x3007 || r 0x3021 0x3029 || r 0x3038 0x303A || r 0x3192 0x3195 || r 0x3220 0x3229

        else if l 0xA8FF then
            r 0x3248 0x324F || r 0x3251 0x325F || r 0x3280 0x3289 || r 0x32B1 0x32BF || r 0xA620 0xA629 || r 0xA6E6 0xA6EF || r 0xA830 0xA835 || r 0xA8D0 0xA8D9

        else
            r 0xA900 0xA909 || r 0xA9D0 0xA9D9 || r 0xA9F0 0xA9F9 || r 0xAA50 0xAA59 || r 0xABF0 0xABF9 || r 0xFF10 0xFF19 || r 0x00010107 0x00010133 || r 0x00010140 0x00010178 || r 0x0001018A 0x0001018B

    else if l 0x0001144F then
        if l 0x00010A9C then
            if l 0x000108A6 then
                r 0x000102E1 0x000102FB || r 0x00010320 0x00010323 || e 0x00010341 || e 0x0001034A || r 0x000103D1 0x000103D5 || r 0x000104A0 0x000104A9 || r 0x00010858 0x0001085F || r 0x00010879 0x0001087F

            else
                r 0x000108A7 0x000108AF || r 0x000108FB 0x000108FF || r 0x00010916 0x0001091B || r 0x000109BC 0x000109BD || r 0x000109C0 0x000109CF || r 0x000109D2 0x000109FF || r 0x00010A40 0x00010A48 || r 0x00010A7D 0x00010A7E

        else if l 0x00010F1C then
            r 0x00010A9D 0x00010A9F || r 0x00010AEB 0x00010AEF || r 0x00010B58 0x00010B5F || r 0x00010B78 0x00010B7F || r 0x00010BA9 0x00010BAF || r 0x00010CFA 0x00010CFF || r 0x00010D30 0x00010D39 || r 0x00010E60 0x00010E7E

        else
            r 0x00010F1D 0x00010F26 || r 0x00010F51 0x00010F54 || r 0x00010FC5 0x00010FCB || r 0x00011052 0x0001106F || r 0x000110F0 0x000110F9 || r 0x00011136 0x0001113F || r 0x000111D0 0x000111D9 || r 0x000111E1 0x000111F4 || r 0x000112F0 0x000112F9

    else if l 0x00016E7F then
        if l 0x00011D4F then
            r 0x00011450 0x00011459 || r 0x000114D0 0x000114D9 || r 0x00011650 0x00011659 || r 0x000116C0 0x000116C9 || r 0x00011730 0x0001173B || r 0x000118E0 0x000118F2 || r 0x00011950 0x00011959 || r 0x00011C50 0x00011C6C

        else
            r 0x00011D50 0x00011D59 || r 0x00011DA0 0x00011DA9 || r 0x00011F50 0x00011F59 || r 0x00011FC0 0x00011FD4 || r 0x00012400 0x0001246E || r 0x00016A60 0x00016A69 || r 0x00016AC0 0x00016AC9 || r 0x00016B50 0x00016B59 || r 0x00016B5B 0x00016B61

    else if l 0x0001E8C6 then
        r 0x00016E80 0x00016E96 || r 0x0001D2C0 0x0001D2D3 || r 0x0001D2E0 0x0001D2F3 || r 0x0001D360 0x0001D378 || r 0x0001D7CE 0x0001D7FF || r 0x0001E140 0x0001E149 || r 0x0001E2F0 0x0001E2F9 || r 0x0001E4F0 0x0001E4F9

    else
        r 0x0001E8C7 0x0001E8CF || r 0x0001E950 0x0001E959 || r 0x0001EC71 0x0001ECAB || r 0x0001ECAD 0x0001ECAF || r 0x0001ECB1 0x0001ECB4 || r 0x0001ED01 0x0001ED2D || r 0x0001ED2F 0x0001ED3D || r 0x0001F100 0x0001F10C || r 0x0001FBF0 0x0001FBF9


{-| Detect letters or digits (Unicode categories Lu, Ll, Lt, Lm, Lo, Nd, Nl, No)
-}
isAlphaNum : Char -> Bool
isAlphaNum c =
    let
        code =
            Char.toCode c

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if l 0x0100 then
        r 0x30 0x39 || r 0x41 0x5A || r 0x61 0x7A || e 0xAA || r 0xB2 0xB3 || e 0xB5 || r 0xB9 0xBA || r 0xBC 0xBE || r 0xC0 0xD6 || r 0xD8 0xF6 || r 0xF8 0xFF

    else if l 0xAAF1 then
        if l 0x1257 then
            if l 0x0B3C then
                if l 0x094F then
                    if l 0x0670 then
                        if l 0x03A2 then
                            r 0x0100 0x02C1 || r 0x02C6 0x02D1 || r 0x02E0 0x02E4 || r 0x0370 0x0374 || r 0x0376 0x0377 || r 0x037A 0x037D || e 0x037F || e 0x0386 || r 0x0388 0x038A || e 0x038C || r 0x038E 0x03A1 || ((modBy 2 code == 0) && r 0x02EC 0x02EE)

                        else
                            r 0x03A3 0x03F5 || r 0x03F7 0x0481 || r 0x048A 0x052F || r 0x0531 0x0556 || e 0x0559 || r 0x0560 0x0588 || r 0x05D0 0x05EA || r 0x05EF 0x05F2 || r 0x0620 0x064A || r 0x0660 0x0669 || r 0x066E 0x066F

                    else if l 0x07F9 then
                        r 0x0671 0x06D3 || e 0x06D5 || r 0x06E5 0x06E6 || r 0x06EE 0x06FC || e 0x06FF || e 0x0710 || r 0x0712 0x072F || r 0x074D 0x07A5 || e 0x07B1 || r 0x07C0 0x07EA || r 0x07F4 0x07F5

                    else
                        e 0x07FA || r 0x0800 0x0815 || e 0x081A || e 0x0824 || e 0x0828 || r 0x0840 0x0858 || r 0x0860 0x086A || r 0x0870 0x0887 || r 0x0889 0x088E || r 0x08A0 0x08C9 || r 0x0904 0x0939 || e 0x093D

                else if l 0x0A34 then
                    if l 0x09CD then
                        e 0x0950 || r 0x0958 0x0961 || r 0x0966 0x096F || r 0x0971 0x0980 || r 0x0985 0x098C || r 0x098F 0x0990 || r 0x0993 0x09A8 || r 0x09AA 0x09B0 || e 0x09B2 || r 0x09B6 0x09B9 || e 0x09BD

                    else
                        e 0x09CE || r 0x09DC 0x09DD || r 0x09DF 0x09E1 || r 0x09E6 0x09F1 || r 0x09F4 0x09F9 || e 0x09FC || r 0x0A05 0x0A0A || r 0x0A0F 0x0A10 || r 0x0A13 0x0A28 || r 0x0A2A 0x0A30 || r 0x0A32 0x0A33

                else if l 0x0AB4 then
                    r 0x0A35 0x0A36 || r 0x0A38 0x0A39 || r 0x0A59 0x0A5C || e 0x0A5E || r 0x0A66 0x0A6F || r 0x0A72 0x0A74 || r 0x0A85 0x0A8D || r 0x0A8F 0x0A91 || r 0x0A93 0x0AA8 || r 0x0AAA 0x0AB0 || r 0x0AB2 0x0AB3

                else
                    r 0x0AB5 0x0AB9 || e 0x0ABD || e 0x0AD0 || r 0x0AE0 0x0AE1 || r 0x0AE6 0x0AEF || e 0x0AF9 || r 0x0B05 0x0B0C || r 0x0B0F 0x0B10 || r 0x0B13 0x0B28 || r 0x0B2A 0x0B30 || r 0x0B32 0x0B33 || r 0x0B35 0x0B39

            else if l 0x0D65 then
                if l 0x0C57 then
                    if l 0x0B9D then
                        e 0x0B3D || r 0x0B5C 0x0B5D || r 0x0B5F 0x0B61 || r 0x0B66 0x0B6F || r 0x0B71 0x0B77 || e 0x0B83 || r 0x0B85 0x0B8A || r 0x0B8E 0x0B90 || r 0x0B92 0x0B95 || r 0x0B99 0x0B9A || e 0x0B9C

                    else
                        r 0x0B9E 0x0B9F || r 0x0BA3 0x0BA4 || r 0x0BA8 0x0BAA || r 0x0BAE 0x0BB9 || e 0x0BD0 || r 0x0BE6 0x0BF2 || r 0x0C05 0x0C0C || r 0x0C0E 0x0C10 || r 0x0C12 0x0C28 || r 0x0C2A 0x0C39 || e 0x0C3D

                else if l 0x0CBC then
                    r 0x0C58 0x0C5A || e 0x0C5D || r 0x0C60 0x0C61 || r 0x0C66 0x0C6F || r 0x0C78 0x0C7E || e 0x0C80 || r 0x0C85 0x0C8C || r 0x0C8E 0x0C90 || r 0x0C92 0x0CA8 || r 0x0CAA 0x0CB3 || r 0x0CB5 0x0CB9

                else
                    e 0x0CBD || r 0x0CDD 0x0CDE || r 0x0CE0 0x0CE1 || r 0x0CE6 0x0CEF || r 0x0CF1 0x0CF2 || r 0x0D04 0x0D0C || r 0x0D0E 0x0D10 || r 0x0D12 0x0D3A || e 0x0D3D || e 0x0D4E || r 0x0D54 0x0D56 || r 0x0D58 0x0D61

            else if l 0x0EDB then
                if l 0x0E4F then
                    r 0x0D66 0x0D78 || r 0x0D7A 0x0D7F || r 0x0D85 0x0D96 || r 0x0D9A 0x0DB1 || r 0x0DB3 0x0DBB || e 0x0DBD || r 0x0DC0 0x0DC6 || r 0x0DE6 0x0DEF || r 0x0E01 0x0E30 || r 0x0E32 0x0E33 || r 0x0E40 0x0E46

                else
                    r 0x0E50 0x0E59 || r 0x0E81 0x0E82 || e 0x0E84 || r 0x0E86 0x0E8A || r 0x0E8C 0x0EA3 || e 0x0EA5 || r 0x0EA7 0x0EB0 || r 0x0EB2 0x0EB3 || e 0x0EBD || r 0x0EC0 0x0EC4 || e 0x0EC6 || r 0x0ED0 0x0ED9

            else if l 0x1064 then
                r 0x0EDC 0x0EDF || e 0x0F00 || r 0x0F20 0x0F33 || r 0x0F40 0x0F47 || r 0x0F49 0x0F6C || r 0x0F88 0x0F8C || r 0x1000 0x102A || r 0x103F 0x1049 || r 0x1050 0x1055 || r 0x105A 0x105D || e 0x1061

            else
                r 0x1065 0x1066 || r 0x106E 0x1070 || r 0x1075 0x1081 || e 0x108E || r 0x1090 0x1099 || r 0x10A0 0x10C5 || e 0x10C7 || e 0x10CD || r 0x10D0 0x10FA || r 0x10FC 0x1248 || r 0x124A 0x124D || r 0x1250 0x1256

        else if l 0x2129 then
            if l 0x1A1F then
                if l 0x16FF then
                    if l 0x1311 then
                        e 0x1258 || r 0x125A 0x125D || r 0x1260 0x1288 || r 0x128A 0x128D || r 0x1290 0x12B0 || r 0x12B2 0x12B5 || r 0x12B8 0x12BE || e 0x12C0 || r 0x12C2 0x12C5 || r 0x12C8 0x12D6 || r 0x12D8 0x1310

                    else
                        r 0x1312 0x1315 || r 0x1318 0x135A || r 0x1369 0x137C || r 0x1380 0x138F || r 0x13A0 0x13F5 || r 0x13F8 0x13FD || r 0x1401 0x166C || r 0x166F 0x167F || r 0x1681 0x169A || r 0x16A0 0x16EA || r 0x16EE 0x16F8

                else if l 0x181F then
                    r 0x1700 0x1711 || r 0x171F 0x1731 || r 0x1740 0x1751 || r 0x1760 0x176C || r 0x176E 0x1770 || r 0x1780 0x17B3 || e 0x17D7 || e 0x17DC || r 0x17E0 0x17E9 || r 0x17F0 0x17F9 || r 0x1810 0x1819

                else
                    r 0x1820 0x1878 || r 0x1880 0x1884 || r 0x1887 0x18A8 || e 0x18AA || r 0x18B0 0x18F5 || r 0x1900 0x191E || r 0x1946 0x196D || r 0x1970 0x1974 || r 0x1980 0x19AB || r 0x19B0 0x19C9 || r 0x19D0 0x19DA || r 0x1A00 0x1A16

            else if l 0x1F1F then
                if l 0x1C4C then
                    r 0x1A20 0x1A54 || r 0x1A80 0x1A89 || r 0x1A90 0x1A99 || e 0x1AA7 || r 0x1B05 0x1B33 || r 0x1B45 0x1B4C || r 0x1B50 0x1B59 || r 0x1B83 0x1BA0 || r 0x1BAE 0x1BE5 || r 0x1C00 0x1C23 || r 0x1C40 0x1C49

                else
                    r 0x1C4D 0x1C7D || r 0x1C80 0x1C88 || r 0x1C90 0x1CBA || r 0x1CBD 0x1CBF || r 0x1CE9 0x1CEC || r 0x1CEE 0x1CF3 || r 0x1CF5 0x1CF6 || e 0x1CFA || r 0x1D00 0x1DBF || r 0x1E00 0x1F15 || r 0x1F18 0x1F1D

            else if l 0x1FDF then
                r 0x1F20 0x1F45 || r 0x1F48 0x1F4D || r 0x1F50 0x1F57 || r 0x1F60 0x1F7D || r 0x1F80 0x1FB4 || r 0x1FB6 0x1FBC || e 0x1FBE || r 0x1FC2 0x1FC4 || r 0x1FC6 0x1FCC || r 0x1FD0 0x1FD3 || r 0x1FD6 0x1FDB || ((modBy 2 code == 1) && r 0x1F59 0x1F5F)

            else
                r 0x1FE0 0x1FEC || r 0x1FF2 0x1FF4 || r 0x1FF6 0x1FFC || r 0x2070 0x2071 || r 0x2074 0x2079 || r 0x207F 0x2089 || r 0x2090 0x209C || e 0x2102 || e 0x2107 || r 0x210A 0x2113 || e 0x2115 || r 0x2119 0x211D || ((modBy 2 code == 0) && r 0x2124 0x2128)

        else if l 0x32B0 then
            if l 0x2DB7 then
                if l 0x2CF1 then
                    r 0x212A 0x212D || r 0x212F 0x2139 || r 0x213C 0x213F || r 0x2145 0x2149 || e 0x214E || r 0x2150 0x2189 || r 0x2460 0x249B || r 0x24EA 0x24FF || r 0x2776 0x2793 || r 0x2C00 0x2CE4 || r 0x2CEB 0x2CEE

                else
                    r 0x2CF2 0x2CF3 || e 0x2CFD || r 0x2D00 0x2D25 || e 0x2D27 || e 0x2D2D || r 0x2D30 0x2D67 || e 0x2D6F || r 0x2D80 0x2D96 || r 0x2DA0 0x2DA6 || r 0x2DA8 0x2DAE || r 0x2DB0 0x2DB6

            else if l 0x309C then
                r 0x2DB8 0x2DBE || r 0x2DC0 0x2DC6 || r 0x2DC8 0x2DCE || r 0x2DD0 0x2DD6 || r 0x2DD8 0x2DDE || e 0x2E2F || r 0x3005 0x3007 || r 0x3021 0x3029 || r 0x3031 0x3035 || r 0x3038 0x303C || r 0x3041 0x3096

            else
                r 0x309D 0x309F || r 0x30A1 0x30FA || r 0x30FC 0x30FF || r 0x3105 0x312F || r 0x3131 0x318E || r 0x3192 0x3195 || r 0x31A0 0x31BF || r 0x31F0 0x31FF || r 0x3220 0x3229 || r 0x3248 0x324F || r 0x3251 0x325F || r 0x3280 0x3289

        else if l 0xA8CF then
            if l 0xA716 then
                r 0x32B1 0x32BF || e 0x3400 || e 0x4DBF || e 0x4E00 || r 0x9FFF 0xA48C || r 0xA4D0 0xA4FD || r 0xA500 0xA60C || r 0xA610 0xA62B || r 0xA640 0xA66E || r 0xA67F 0xA69D || r 0xA6A0 0xA6EF

            else
                r 0xA717 0xA71F || r 0xA722 0xA788 || r 0xA78B 0xA7CA || r 0xA7D0 0xA7D1 || r 0xA7D6 0xA7D9 || r 0xA7F2 0xA801 || r 0xA803 0xA805 || r 0xA807 0xA80A || r 0xA80C 0xA822 || r 0xA830 0xA835 || r 0xA840 0xA873 || r 0xA882 0xA8B3 || ((modBy 2 code == 1) && r 0xA7D3 0xA7D5)

        else if l 0xA9FF then
            r 0xA8D0 0xA8D9 || r 0xA8F2 0xA8F7 || e 0xA8FB || r 0xA8FD 0xA8FE || r 0xA900 0xA925 || r 0xA930 0xA946 || r 0xA960 0xA97C || r 0xA984 0xA9B2 || r 0xA9CF 0xA9D9 || r 0xA9E0 0xA9E4 || r 0xA9E6 0xA9FE

        else
            r 0xAA00 0xAA28 || r 0xAA40 0xAA42 || r 0xAA44 0xAA4B || r 0xAA50 0xAA59 || r 0xAA60 0xAA76 || e 0xAA7A || r 0xAA7E 0xAAAF || e 0xAAB1 || r 0xAAB5 0xAAB6 || r 0xAAB9 0xAABD || r 0xAADB 0xAADD || r 0xAAE0 0xAAEA || ((modBy 2 code == 0) && r 0xAAC0 0xAAC2)

    else if l 0x0001189F then
        if l 0x000108F3 then
            if l 0x0001004F then
                if l 0xFB3D then
                    if l 0xD7A2 then
                        r 0xAAF2 0xAAF4 || r 0xAB01 0xAB06 || r 0xAB09 0xAB0E || r 0xAB11 0xAB16 || r 0xAB20 0xAB26 || r 0xAB28 0xAB2E || r 0xAB30 0xAB5A || r 0xAB5C 0xAB69 || r 0xAB70 0xABE2 || r 0xABF0 0xABF9 || e 0xAC00

                    else
                        e 0xD7A3 || r 0xD7B0 0xD7C6 || r 0xD7CB 0xD7FB || r 0xF900 0xFA6D || r 0xFA70 0xFAD9 || r 0xFB00 0xFB06 || r 0xFB13 0xFB17 || e 0xFB1D || r 0xFB1F 0xFB28 || r 0xFB2A 0xFB36 || r 0xFB38 0xFB3C

                else if l 0xFF20 then
                    e 0xFB3E || r 0xFB40 0xFB41 || r 0xFB43 0xFB44 || r 0xFB46 0xFBB1 || r 0xFBD3 0xFD3D || r 0xFD50 0xFD8F || r 0xFD92 0xFDC7 || r 0xFDF0 0xFDFB || r 0xFE70 0xFE74 || r 0xFE76 0xFEFC || r 0xFF10 0xFF19

                else
                    r 0xFF21 0xFF3A || r 0xFF41 0xFF5A || r 0xFF66 0xFFBE || r 0xFFC2 0xFFC7 || r 0xFFCA 0xFFCF || r 0xFFD2 0xFFD7 || r 0xFFDA 0xFFDC || r 0x00010000 0x0001000B || r 0x0001000D 0x00010026 || r 0x00010028 0x0001003A || r 0x0001003C 0x0001003D || r 0x0001003F 0x0001004D

            else if l 0x0001057B then
                if l 0x0001037F then
                    r 0x00010050 0x0001005D || r 0x00010080 0x000100FA || r 0x00010107 0x00010133 || r 0x00010140 0x00010178 || r 0x0001018A 0x0001018B || r 0x00010280 0x0001029C || r 0x000102A0 0x000102D0 || r 0x000102E1 0x000102FB || r 0x00010300 0x00010323 || r 0x0001032D 0x0001034A || r 0x00010350 0x00010375

                else
                    r 0x00010380 0x0001039D || r 0x000103A0 0x000103C3 || r 0x000103C8 0x000103CF || r 0x000103D1 0x000103D5 || r 0x00010400 0x0001049D || r 0x000104A0 0x000104A9 || r 0x000104B0 0x000104D3 || r 0x000104D8 0x000104FB || r 0x00010500 0x00010527 || r 0x00010530 0x00010563 || r 0x00010570 0x0001057A

            else if l 0x00010786 then
                r 0x0001057C 0x0001058A || r 0x0001058C 0x00010592 || r 0x00010594 0x00010595 || r 0x00010597 0x000105A1 || r 0x000105A3 0x000105B1 || r 0x000105B3 0x000105B9 || r 0x000105BB 0x000105BC || r 0x00010600 0x00010736 || r 0x00010740 0x00010755 || r 0x00010760 0x00010767 || r 0x00010780 0x00010785

            else
                r 0x00010787 0x000107B0 || r 0x000107B2 0x000107BA || r 0x00010800 0x00010805 || e 0x00010808 || r 0x0001080A 0x00010835 || r 0x00010837 0x00010838 || e 0x0001083C || r 0x0001083F 0x00010855 || r 0x00010858 0x00010876 || r 0x00010879 0x0001089E || r 0x000108A7 0x000108AF || r 0x000108E0 0x000108F2

        else if l 0x0001114F then
            if l 0x00010CBF then
                if l 0x00010A7F then
                    r 0x000108F4 0x000108F5 || r 0x000108FB 0x0001091B || r 0x00010920 0x00010939 || r 0x00010980 0x000109B7 || r 0x000109BC 0x000109CF || r 0x000109D2 0x00010A00 || r 0x00010A10 0x00010A13 || r 0x00010A15 0x00010A17 || r 0x00010A19 0x00010A35 || r 0x00010A40 0x00010A48 || r 0x00010A60 0x00010A7E

                else
                    r 0x00010A80 0x00010A9F || r 0x00010AC0 0x00010AC7 || r 0x00010AC9 0x00010AE4 || r 0x00010AEB 0x00010AEF || r 0x00010B00 0x00010B35 || r 0x00010B40 0x00010B55 || r 0x00010B58 0x00010B72 || r 0x00010B78 0x00010B91 || r 0x00010BA9 0x00010BAF || r 0x00010C00 0x00010C48 || r 0x00010C80 0x00010CB2

            else if l 0x00010FDF then
                r 0x00010CC0 0x00010CF2 || r 0x00010CFA 0x00010D23 || r 0x00010D30 0x00010D39 || r 0x00010E60 0x00010E7E || r 0x00010E80 0x00010EA9 || r 0x00010EB0 0x00010EB1 || r 0x00010F00 0x00010F27 || r 0x00010F30 0x00010F45 || r 0x00010F51 0x00010F54 || r 0x00010F70 0x00010F81 || r 0x00010FB0 0x00010FCB

            else
                r 0x00010FE0 0x00010FF6 || r 0x00011003 0x00011037 || r 0x00011052 0x0001106F || r 0x00011071 0x00011072 || e 0x00011075 || r 0x00011083 0x000110AF || r 0x000110D0 0x000110E8 || r 0x000110F0 0x000110F9 || r 0x00011103 0x00011126 || r 0x00011136 0x0001113F || e 0x00011144 || e 0x00011147

        else if l 0x0001133C then
            if l 0x00011287 then
                r 0x00011150 0x00011172 || e 0x00011176 || r 0x00011183 0x000111B2 || r 0x000111C1 0x000111C4 || r 0x000111D0 0x000111DA || e 0x000111DC || r 0x000111E1 0x000111F4 || r 0x00011200 0x00011211 || r 0x00011213 0x0001122B || r 0x0001123F 0x00011240 || r 0x00011280 0x00011286

            else
                e 0x00011288 || r 0x0001128A 0x0001128D || r 0x0001128F 0x0001129D || r 0x0001129F 0x000112A8 || r 0x000112B0 0x000112DE || r 0x000112F0 0x000112F9 || r 0x00011305 0x0001130C || r 0x0001130F 0x00011310 || r 0x00011313 0x00011328 || r 0x0001132A 0x00011330 || r 0x00011332 0x00011333 || r 0x00011335 0x00011339

        else if l 0x0001157F then
            e 0x0001133D || e 0x00011350 || r 0x0001135D 0x00011361 || r 0x00011400 0x00011434 || r 0x00011447 0x0001144A || r 0x00011450 0x00011459 || r 0x0001145F 0x00011461 || r 0x00011480 0x000114AF || r 0x000114C4 0x000114C5 || e 0x000114C7 || r 0x000114D0 0x000114D9

        else
            r 0x00011580 0x000115AE || r 0x000115D8 0x000115DB || r 0x00011600 0x0001162F || e 0x00011644 || r 0x00011650 0x00011659 || r 0x00011680 0x000116AA || e 0x000116B8 || r 0x000116C0 0x000116C9 || r 0x00011700 0x0001171A || r 0x00011730 0x0001173B || r 0x00011740 0x00011746 || r 0x00011800 0x0001182B

    else if l 0x0001D4AD then
        if l 0x000167FF then
            if l 0x00011D07 then
                if l 0x00011A39 then
                    r 0x000118A0 0x000118F2 || r 0x000118FF 0x00011906 || e 0x00011909 || r 0x0001190C 0x00011913 || r 0x00011915 0x00011916 || r 0x00011918 0x0001192F || r 0x00011950 0x00011959 || r 0x000119A0 0x000119A7 || r 0x000119AA 0x000119D0 || e 0x00011A00 || r 0x00011A0B 0x00011A32 || ((modBy 2 code == 1) && (r 0x0001193F 0x00011941 || r 0x000119E1 0x000119E3))

                else
                    e 0x00011A3A || e 0x00011A50 || r 0x00011A5C 0x00011A89 || e 0x00011A9D || r 0x00011AB0 0x00011AF8 || r 0x00011C00 0x00011C08 || r 0x00011C0A 0x00011C2E || e 0x00011C40 || r 0x00011C50 0x00011C6C || r 0x00011C72 0x00011C8F || r 0x00011D00 0x00011D06

            else if l 0x00011F03 then
                r 0x00011D08 0x00011D09 || r 0x00011D0B 0x00011D30 || e 0x00011D46 || r 0x00011D50 0x00011D59 || r 0x00011D60 0x00011D65 || r 0x00011D67 0x00011D68 || r 0x00011D6A 0x00011D89 || e 0x00011D98 || r 0x00011DA0 0x00011DA9 || r 0x00011EE0 0x00011EF2 || e 0x00011F02

            else
                r 0x00011F04 0x00011F10 || r 0x00011F12 0x00011F33 || r 0x00011F50 0x00011F59 || e 0x00011FB0 || r 0x00011FC0 0x00011FD4 || r 0x00012000 0x00012399 || r 0x00012400 0x0001246E || r 0x00012480 0x00012543 || r 0x00012F90 0x00012FF0 || r 0x00013000 0x0001342F || r 0x00013441 0x00013446 || r 0x00014400 0x00014646

        else if l 0x00018D07 then
            if l 0x00016B7C then
                r 0x00016800 0x00016A38 || r 0x00016A40 0x00016A5E || r 0x00016A60 0x00016A69 || r 0x00016A70 0x00016ABE || r 0x00016AC0 0x00016AC9 || r 0x00016AD0 0x00016AED || r 0x00016B00 0x00016B2F || r 0x00016B40 0x00016B43 || r 0x00016B50 0x00016B59 || r 0x00016B5B 0x00016B61 || r 0x00016B63 0x00016B77

            else
                r 0x00016B7D 0x00016B8F || r 0x00016E40 0x00016E96 || r 0x00016F00 0x00016F4A || e 0x00016F50 || r 0x00016F93 0x00016F9F || r 0x00016FE0 0x00016FE1 || e 0x00016FE3 || e 0x00017000 || e 0x000187F7 || r 0x00018800 0x00018CD5 || e 0x00018D00

        else if l 0x0001BC6F then
            e 0x00018D08 || r 0x0001AFF0 0x0001AFF3 || r 0x0001AFF5 0x0001AFFB || r 0x0001AFFD 0x0001AFFE || r 0x0001B000 0x0001B122 || e 0x0001B132 || r 0x0001B150 0x0001B152 || e 0x0001B155 || r 0x0001B164 0x0001B167 || r 0x0001B170 0x0001B2FB || r 0x0001BC00 0x0001BC6A

        else
            r 0x0001BC70 0x0001BC7C || r 0x0001BC80 0x0001BC88 || r 0x0001BC90 0x0001BC99 || r 0x0001D2C0 0x0001D2D3 || r 0x0001D2E0 0x0001D2F3 || r 0x0001D360 0x0001D378 || r 0x0001D400 0x0001D454 || r 0x0001D456 0x0001D49C || r 0x0001D49E 0x0001D49F || e 0x0001D4A2 || r 0x0001D4A5 0x0001D4A6 || r 0x0001D4A9 0x0001D4AC

    else if l 0x0001E94F then
        if l 0x0001D7A9 then
            if l 0x0001D549 then
                r 0x0001D4AE 0x0001D4B9 || e 0x0001D4BB || r 0x0001D4BD 0x0001D4C3 || r 0x0001D4C5 0x0001D505 || r 0x0001D507 0x0001D50A || r 0x0001D50D 0x0001D514 || r 0x0001D516 0x0001D51C || r 0x0001D51E 0x0001D539 || r 0x0001D53B 0x0001D53E || r 0x0001D540 0x0001D544 || e 0x0001D546

            else
                r 0x0001D54A 0x0001D550 || r 0x0001D552 0x0001D6A5 || r 0x0001D6A8 0x0001D6C0 || r 0x0001D6C2 0x0001D6DA || r 0x0001D6DC 0x0001D6FA || r 0x0001D6FC 0x0001D714 || r 0x0001D716 0x0001D734 || r 0x0001D736 0x0001D74E || r 0x0001D750 0x0001D76E || r 0x0001D770 0x0001D788 || r 0x0001D78A 0x0001D7A8

        else if l 0x0001E2BF then
            r 0x0001D7AA 0x0001D7C2 || r 0x0001D7C4 0x0001D7CB || r 0x0001D7CE 0x0001D7FF || r 0x0001DF00 0x0001DF1E || r 0x0001DF25 0x0001DF2A || r 0x0001E030 0x0001E06D || r 0x0001E100 0x0001E12C || r 0x0001E137 0x0001E13D || r 0x0001E140 0x0001E149 || e 0x0001E14E || r 0x0001E290 0x0001E2AD

        else
            r 0x0001E2C0 0x0001E2EB || r 0x0001E2F0 0x0001E2F9 || r 0x0001E4D0 0x0001E4EB || r 0x0001E4F0 0x0001E4F9 || r 0x0001E7E0 0x0001E7E6 || r 0x0001E7E8 0x0001E7EB || r 0x0001E7ED 0x0001E7EE || r 0x0001E7F0 0x0001E7FE || r 0x0001E800 0x0001E8C4 || r 0x0001E8C7 0x0001E8CF || r 0x0001E900 0x0001E943 || e 0x0001E94B

    else if l 0x0001EE7D then
        if l 0x0001EE28 then
            r 0x0001E950 0x0001E959 || r 0x0001EC71 0x0001ECAB || r 0x0001ECAD 0x0001ECAF || r 0x0001ECB1 0x0001ECB4 || r 0x0001ED01 0x0001ED2D || r 0x0001ED2F 0x0001ED3D || r 0x0001EE00 0x0001EE03 || r 0x0001EE05 0x0001EE1F || r 0x0001EE21 0x0001EE22 || e 0x0001EE24 || e 0x0001EE27

        else
            r 0x0001EE29 0x0001EE32 || r 0x0001EE34 0x0001EE37 || e 0x0001EE42 || r 0x0001EE4D 0x0001EE4F || r 0x0001EE51 0x0001EE52 || e 0x0001EE54 || r 0x0001EE61 0x0001EE62 || e 0x0001EE64 || r 0x0001EE67 0x0001EE6A || r 0x0001EE6C 0x0001EE72 || r 0x0001EE74 0x0001EE77 || r 0x0001EE79 0x0001EE7C || ((modBy 2 code == 1) && (r 0x0001EE39 0x0001EE3B || r 0x0001EE47 0x0001EE4B || r 0x0001EE57 0x0001EE5F))

    else if l 0x0002B738 then
        e 0x0001EE7E || r 0x0001EE80 0x0001EE89 || r 0x0001EE8B 0x0001EE9B || r 0x0001EEA1 0x0001EEA3 || r 0x0001EEA5 0x0001EEA9 || r 0x0001EEAB 0x0001EEBB || r 0x0001F100 0x0001F10C || r 0x0001FBF0 0x0001FBF9 || e 0x00020000 || e 0x0002A6DF || e 0x0002A700

    else
        e 0x0002B739 || e 0x0002B740 || e 0x0002B81D || e 0x0002B820 || e 0x0002CEA1 || e 0x0002CEB0 || e 0x0002EBE0 || r 0x0002F800 0x0002FA1D || e 0x00030000 || e 0x0003134A || e 0x00031350 || e 0x000323AF


{-| Detect spaces (Unicode category Zs)
-}
isSpace : Char -> Bool
isSpace c =
    let
        code =
            Char.toCode c

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if l 0x0100 then
        e 0x20 || e 0xA0

    else
        e 0x1680 || r 0x2000 0x200A || e 0x202F || e 0x205F || e 0x3000


{-| Detect spaces (Unicode categories Zs, Zl, Zp)
-}
isSeparator : Char -> Bool
isSeparator c =
    let
        code =
            Char.toCode c

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if l 0x0100 then
        e 0x20 || e 0xA0

    else
        e 0x1680 || r 0x2000 0x200A || r 0x2028 0x2029 || e 0x202F || e 0x205F || e 0x3000


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

        l hex =
            code < hex

        e hex =
            hex == code

        r from to =
            (from <= code) && (code <= to)
    in
    if l 0x0100 then
        if l 0xA0 then
            if l 0x3B then
                if l 0x29 then
                    if code <= 0x1F then
                        Just OtherControl

                    else if e 0x20 then
                        Just SeparatorSpace

                    else if r 0x21 0x23 || r 0x25 0x27 then
                        Just PunctuationOther

                    else if e 0x24 then
                        Just SymbolCurrency

                    else if e 0x28 then
                        Just PunctuationOpen

                    else
                        Nothing

                else if e 0x29 then
                    Just PunctuationClose

                else if e 0x2A || e 0x2C || r 0x2E 0x2F || e 0x3A then
                    Just PunctuationOther

                else if e 0x2B then
                    Just SymbolMath

                else if e 0x2D then
                    Just PunctuationDash

                else if r 0x30 0x39 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if l 0x5E then
                if e 0x3B || r 0x3F 0x40 || e 0x5C then
                    Just PunctuationOther

                else if r 0x3C 0x3E then
                    Just SymbolMath

                else if r 0x41 0x5A then
                    Just LetterUppercase

                else if e 0x5B then
                    Just PunctuationOpen

                else if e 0x5D then
                    Just PunctuationClose

                else
                    Nothing

            else if e 0x5E || e 0x60 then
                Just SymbolModifier

            else if e 0x5F then
                Just PunctuationConnector

            else if r 0x61 0x7A then
                Just LetterLowercase

            else if e 0x7B then
                Just PunctuationOpen

            else if e 0x7C || e 0x7E then
                Just SymbolMath

            else if e 0x7D then
                Just PunctuationClose

            else if r 0x7F 0x9F then
                Just OtherControl

            else
                Nothing

        else if l 0xB1 then
            if l 0xA9 then
                if e 0xA0 then
                    Just SeparatorSpace

                else if e 0xA1 || e 0xA7 then
                    Just PunctuationOther

                else if r 0xA2 0xA5 then
                    Just SymbolCurrency

                else if e 0xA6 then
                    Just SymbolOther

                else if e 0xA8 then
                    Just SymbolModifier

                else
                    Nothing

            else if e 0xA9 || e 0xAE || e 0xB0 then
                Just SymbolOther

            else if e 0xAA then
                Just LetterOther

            else if e 0xAB then
                Just PunctuationInitialQuote

            else if e 0xAC then
                Just SymbolMath

            else if e 0xAD then
                Just OtherFormat

            else if e 0xAF then
                Just SymbolModifier

            else
                Nothing

        else if l 0xBA then
            if e 0xB1 then
                Just SymbolMath

            else if r 0xB2 0xB3 || e 0xB9 then
                Just NumberOther

            else if e 0xB4 || e 0xB8 then
                Just SymbolModifier

            else if e 0xB5 then
                Just LetterLowercase

            else if r 0xB6 0xB7 then
                Just PunctuationOther

            else
                Nothing

        else if e 0xBA then
            Just LetterOther

        else if e 0xBB then
            Just PunctuationFinalQuote

        else if r 0xBC 0xBE then
            Just NumberOther

        else if e 0xBF then
            Just PunctuationOther

        else if r 0xC0 0xD6 || r 0xD8 0xDE then
            Just LetterUppercase

        else if e 0xD7 || e 0xF7 then
            Just SymbolMath

        else if r 0xDF 0xF6 || r 0xF8 0xFF then
            Just LetterLowercase

        else
            Nothing

    else if l 0x245F then
        if l 0x0C3C then
            if l 0x0490 then
                if l 0x01E9 then
                    if l 0x0165 then
                        if l 0x0130 then
                            if l 0x0117 then
                                if l 0x010A then
                                    if e 0x0100 || e 0x0102 || e 0x0104 || e 0x0106 || e 0x0108 then
                                        Just LetterUppercase

                                    else if e 0x0101 || e 0x0103 || e 0x0105 || e 0x0107 || e 0x0109 then
                                        Just LetterLowercase

                                    else
                                        Nothing

                                else if l 0x010F then
                                    if e 0x010A || e 0x010C || e 0x010E then
                                        Just LetterUppercase

                                    else if e 0x010B || e 0x010D then
                                        Just LetterLowercase

                                    else
                                        Nothing

                                else if e 0x010F || e 0x0111 || e 0x0113 || e 0x0115 then
                                    Just LetterLowercase

                                else if e 0x0110 || e 0x0112 || e 0x0114 || e 0x0116 then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if l 0x0122 then
                                if e 0x0117 || e 0x0119 || e 0x011B || e 0x011D || e 0x011F || e 0x0121 then
                                    Just LetterLowercase

                                else if e 0x0118 || e 0x011A || e 0x011C || e 0x011E || e 0x0120 then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if l 0x0128 then
                                if e 0x0122 || e 0x0124 || e 0x0126 then
                                    Just LetterUppercase

                                else if e 0x0123 || e 0x0125 || e 0x0127 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if e 0x0128 || e 0x012A || e 0x012C || e 0x012E then
                                Just LetterUppercase

                            else if e 0x0129 || e 0x012B || e 0x012D || e 0x012F then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x014A then
                            if l 0x013C then
                                if e 0x0130 || e 0x0132 || e 0x0134 || e 0x0136 || e 0x0139 || e 0x013B then
                                    Just LetterUppercase

                                else if e 0x0131 || e 0x0133 || e 0x0135 || r 0x0137 0x0138 || e 0x013A then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if l 0x0141 then
                                if e 0x013C || e 0x013E || e 0x0140 then
                                    Just LetterLowercase

                                else if e 0x013D || e 0x013F then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if e 0x0141 || e 0x0143 || e 0x0145 || e 0x0147 then
                                Just LetterUppercase

                            else if e 0x0142 || e 0x0144 || e 0x0146 || r 0x0148 0x0149 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x0156 then
                            if e 0x014A || e 0x014C || e 0x014E || e 0x0150 || e 0x0152 || e 0x0154 then
                                Just LetterUppercase

                            else if e 0x014B || e 0x014D || e 0x014F || e 0x0151 || e 0x0153 || e 0x0155 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x015C then
                            if e 0x0156 || e 0x0158 || e 0x015A then
                                Just LetterUppercase

                            else if e 0x0157 || e 0x0159 || e 0x015B then
                                Just LetterLowercase

                            else
                                Nothing

                        else if e 0x015C || e 0x015E || e 0x0160 || e 0x0162 || e 0x0164 then
                            Just LetterUppercase

                        else if e 0x015D || e 0x015F || e 0x0161 || e 0x0163 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x01A9 then
                        if l 0x017D then
                            if l 0x016F then
                                if e 0x0165 || e 0x0167 || e 0x0169 || e 0x016B || e 0x016D then
                                    Just LetterLowercase

                                else if e 0x0166 || e 0x0168 || e 0x016A || e 0x016C || e 0x016E then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if l 0x0174 then
                                if e 0x016F || e 0x0171 || e 0x0173 then
                                    Just LetterLowercase

                                else if e 0x0170 || e 0x0172 then
                                    Just LetterUppercase

                                else
                                    Nothing

                            else if e 0x0174 || e 0x0176 || r 0x0178 0x0179 || e 0x017B then
                                Just LetterUppercase

                            else if e 0x0175 || e 0x0177 || e 0x017A || e 0x017C then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x0194 then
                            if l 0x0185 then
                                if e 0x017D || r 0x0181 0x0182 || e 0x0184 then
                                    Just LetterUppercase

                                else if r 0x017E 0x0180 || e 0x0183 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if e 0x0185 || e 0x0188 || r 0x018C 0x018D || e 0x0192 then
                                Just LetterLowercase

                            else if r 0x0186 0x0187 || r 0x0189 0x018B || r 0x018E 0x0191 || e 0x0193 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x01A0 then
                            if e 0x0194 || r 0x0196 0x0198 || r 0x019C 0x019D || e 0x019F then
                                Just LetterUppercase

                            else if e 0x0195 || r 0x0199 0x019B || e 0x019E then
                                Just LetterLowercase

                            else
                                Nothing

                        else if e 0x01A0 || e 0x01A2 || e 0x01A4 || r 0x01A6 0x01A7 then
                            Just LetterUppercase

                        else if e 0x01A1 || e 0x01A3 || e 0x01A5 || e 0x01A8 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x01CD then
                        if l 0x01BA then
                            if e 0x01A9 || e 0x01AC || r 0x01AE 0x01AF || r 0x01B1 0x01B3 || e 0x01B5 || r 0x01B7 0x01B8 then
                                Just LetterUppercase

                            else if r 0x01AA 0x01AB || e 0x01AD || e 0x01B0 || e 0x01B4 || e 0x01B6 || e 0x01B9 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x01C5 then
                            if e 0x01BA || r 0x01BD 0x01BF then
                                Just LetterLowercase

                            else if e 0x01BB || r 0x01C0 0x01C3 then
                                Just LetterOther

                            else if e 0x01BC || e 0x01C4 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x01C5 || e 0x01C8 || e 0x01CB then
                            Just LetterTitlecase

                        else if e 0x01C6 || e 0x01C9 || e 0x01CC then
                            Just LetterLowercase

                        else if e 0x01C7 || e 0x01CA then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x01D9 then
                        if e 0x01CD || e 0x01CF || e 0x01D1 || e 0x01D3 || e 0x01D5 || e 0x01D7 then
                            Just LetterUppercase

                        else if e 0x01CE || e 0x01D0 || e 0x01D2 || e 0x01D4 || e 0x01D6 || e 0x01D8 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x01E0 then
                        if e 0x01D9 || e 0x01DB || e 0x01DE then
                            Just LetterUppercase

                        else if e 0x01DA || r 0x01DC 0x01DD || e 0x01DF then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0x01E0 || e 0x01E2 || e 0x01E4 || e 0x01E6 || e 0x01E8 then
                        Just LetterUppercase

                    else if e 0x01E1 || e 0x01E3 || e 0x01E5 || e 0x01E7 then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x036F then
                    if l 0x021D then
                        if l 0x0203 then
                            if l 0x01F4 then
                                if e 0x01E9 || e 0x01EB || e 0x01ED || r 0x01EF 0x01F0 || e 0x01F3 then
                                    Just LetterLowercase

                                else if e 0x01EA || e 0x01EC || e 0x01EE || e 0x01F1 then
                                    Just LetterUppercase

                                else if e 0x01F2 then
                                    Just LetterTitlecase

                                else
                                    Nothing

                            else if l 0x01FB then
                                if e 0x01F4 || r 0x01F6 0x01F8 || e 0x01FA then
                                    Just LetterUppercase

                                else if e 0x01F5 || e 0x01F9 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if e 0x01FB || e 0x01FD || e 0x01FF || e 0x0201 then
                                Just LetterLowercase

                            else if e 0x01FC || e 0x01FE || e 0x0200 || e 0x0202 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x020F then
                            if e 0x0203 || e 0x0205 || e 0x0207 || e 0x0209 || e 0x020B || e 0x020D then
                                Just LetterLowercase

                            else if e 0x0204 || e 0x0206 || e 0x0208 || e 0x020A || e 0x020C || e 0x020E then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x0215 then
                            if e 0x020F || e 0x0211 || e 0x0213 then
                                Just LetterLowercase

                            else if e 0x0210 || e 0x0212 || e 0x0214 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x0215 || e 0x0217 || e 0x0219 || e 0x021B then
                            Just LetterLowercase

                        else if e 0x0216 || e 0x0218 || e 0x021A || e 0x021C then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x023E then
                        if l 0x0229 then
                            if e 0x021D || e 0x021F || e 0x0221 || e 0x0223 || e 0x0225 || e 0x0227 then
                                Just LetterLowercase

                            else if e 0x021E || e 0x0220 || e 0x0222 || e 0x0224 || e 0x0226 || e 0x0228 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x022F then
                            if e 0x0229 || e 0x022B || e 0x022D then
                                Just LetterLowercase

                            else if e 0x022A || e 0x022C || e 0x022E then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x022F || e 0x0231 || r 0x0233 0x0239 || e 0x023C then
                            Just LetterLowercase

                        else if e 0x0230 || e 0x0232 || r 0x023A 0x023B || e 0x023D then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x024E then
                        if e 0x023E || e 0x0241 || r 0x0243 0x0246 || e 0x0248 || e 0x024A || e 0x024C then
                            Just LetterUppercase

                        else if r 0x023F 0x0240 || e 0x0242 || e 0x0247 || e 0x0249 || e 0x024B || e 0x024D then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x02D1 then
                        if e 0x024E then
                            Just LetterUppercase

                        else if r 0x024F 0x0293 || r 0x0295 0x02AF then
                            Just LetterLowercase

                        else if e 0x0294 then
                            Just LetterOther

                        else if r 0x02B0 0x02C1 || r 0x02C6 0x02D0 then
                            Just LetterModifier

                        else if r 0x02C2 0x02C5 then
                            Just SymbolModifier

                        else
                            Nothing

                    else if e 0x02D1 || r 0x02E0 0x02E4 || e 0x02EC || e 0x02EE then
                        Just LetterModifier

                    else if r 0x02D2 0x02DF || r 0x02E5 0x02EB || e 0x02ED || r 0x02EF 0x02FF then
                        Just SymbolModifier

                    else if r 0x0300 0x036E then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x03F3 then
                    if l 0x03D1 then
                        if l 0x037E then
                            if e 0x036F then
                                Just MarkNonSpacing

                            else if e 0x0370 || e 0x0372 || e 0x0376 then
                                Just LetterUppercase

                            else if e 0x0371 || e 0x0373 || e 0x0377 || r 0x037B 0x037D then
                                Just LetterLowercase

                            else if e 0x0374 || e 0x037A then
                                Just LetterModifier

                            else if e 0x0375 then
                                Just SymbolModifier

                            else
                                Nothing

                        else if l 0x038D then
                            if e 0x037E || e 0x0387 then
                                Just PunctuationOther

                            else if e 0x037F || e 0x0386 || r 0x0388 0x038A || e 0x038C then
                                Just LetterUppercase

                            else if r 0x0384 0x0385 then
                                Just SymbolModifier

                            else
                                Nothing

                        else if r 0x038E 0x038F || r 0x0391 0x03A1 || r 0x03A3 0x03AB || e 0x03CF then
                            Just LetterUppercase

                        else if e 0x0390 || r 0x03AC 0x03CE || e 0x03D0 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x03E1 then
                        if e 0x03D1 || r 0x03D5 0x03D7 || e 0x03D9 || e 0x03DB || e 0x03DD || e 0x03DF then
                            Just LetterLowercase

                        else if r 0x03D2 0x03D4 || e 0x03D8 || e 0x03DA || e 0x03DC || e 0x03DE || e 0x03E0 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x03E7 then
                        if e 0x03E1 || e 0x03E3 || e 0x03E5 then
                            Just LetterLowercase

                        else if e 0x03E2 || e 0x03E4 || e 0x03E6 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x03E7 || e 0x03E9 || e 0x03EB || e 0x03ED || r 0x03EF 0x03F2 then
                        Just LetterLowercase

                    else if e 0x03E8 || e 0x03EA || e 0x03EC || e 0x03EE then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x046F then
                    if l 0x0461 then
                        if e 0x03F3 || e 0x03F5 || e 0x03F8 || r 0x03FB 0x03FC || r 0x0430 0x045F then
                            Just LetterLowercase

                        else if e 0x03F4 || e 0x03F7 || r 0x03F9 0x03FA || r 0x03FD 0x042F || e 0x0460 then
                            Just LetterUppercase

                        else if e 0x03F6 then
                            Just SymbolMath

                        else
                            Nothing

                    else if l 0x0467 then
                        if e 0x0461 || e 0x0463 || e 0x0465 then
                            Just LetterLowercase

                        else if e 0x0462 || e 0x0464 || e 0x0466 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x0467 || e 0x0469 || e 0x046B || e 0x046D then
                        Just LetterLowercase

                    else if e 0x0468 || e 0x046A || e 0x046C || e 0x046E then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x047C then
                    if l 0x0474 then
                        if e 0x046F || e 0x0471 || e 0x0473 then
                            Just LetterLowercase

                        else if e 0x0470 || e 0x0472 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x0474 || e 0x0476 || e 0x0478 || e 0x047A then
                        Just LetterUppercase

                    else if e 0x0475 || e 0x0477 || e 0x0479 || e 0x047B then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x0482 then
                    if e 0x047C || e 0x047E || e 0x0480 then
                        Just LetterUppercase

                    else if e 0x047D || e 0x047F || e 0x0481 then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0x0482 then
                    Just SymbolOther

                else if r 0x0483 0x0487 then
                    Just MarkNonSpacing

                else if r 0x0488 0x0489 then
                    Just MarkEnclosing

                else if e 0x048A || e 0x048C || e 0x048E then
                    Just LetterUppercase

                else if e 0x048B || e 0x048D || e 0x048F then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0x06DE then
                if l 0x04F5 then
                    if l 0x04C1 then
                        if l 0x04A7 then
                            if l 0x049A then
                                if e 0x0490 || e 0x0492 || e 0x0494 || e 0x0496 || e 0x0498 then
                                    Just LetterUppercase

                                else if e 0x0491 || e 0x0493 || e 0x0495 || e 0x0497 || e 0x0499 then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if l 0x049F then
                                if e 0x049A || e 0x049C || e 0x049E then
                                    Just LetterUppercase

                                else if e 0x049B || e 0x049D then
                                    Just LetterLowercase

                                else
                                    Nothing

                            else if e 0x049F || e 0x04A1 || e 0x04A3 || e 0x04A5 then
                                Just LetterLowercase

                            else if e 0x04A0 || e 0x04A2 || e 0x04A4 || e 0x04A6 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x04B3 then
                            if e 0x04A7 || e 0x04A9 || e 0x04AB || e 0x04AD || e 0x04AF || e 0x04B1 then
                                Just LetterLowercase

                            else if e 0x04A8 || e 0x04AA || e 0x04AC || e 0x04AE || e 0x04B0 || e 0x04B2 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x04B9 then
                            if e 0x04B3 || e 0x04B5 || e 0x04B7 then
                                Just LetterLowercase

                            else if e 0x04B4 || e 0x04B6 || e 0x04B8 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x04B9 || e 0x04BB || e 0x04BD || e 0x04BF then
                            Just LetterLowercase

                        else if e 0x04BA || e 0x04BC || e 0x04BE || e 0x04C0 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x04DA then
                        if l 0x04CC then
                            if e 0x04C1 || e 0x04C3 || e 0x04C5 || e 0x04C7 || e 0x04C9 || e 0x04CB then
                                Just LetterUppercase

                            else if e 0x04C2 || e 0x04C4 || e 0x04C6 || e 0x04C8 || e 0x04CA then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x04D2 then
                            if e 0x04CC || r 0x04CE 0x04CF || e 0x04D1 then
                                Just LetterLowercase

                            else if e 0x04CD || e 0x04D0 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x04D2 || e 0x04D4 || e 0x04D6 || e 0x04D8 then
                            Just LetterUppercase

                        else if e 0x04D3 || e 0x04D5 || e 0x04D7 || e 0x04D9 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x04E6 then
                        if e 0x04DA || e 0x04DC || e 0x04DE || e 0x04E0 || e 0x04E2 || e 0x04E4 then
                            Just LetterUppercase

                        else if e 0x04DB || e 0x04DD || e 0x04DF || e 0x04E1 || e 0x04E3 || e 0x04E5 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x04EC then
                        if e 0x04E6 || e 0x04E8 || e 0x04EA then
                            Just LetterUppercase

                        else if e 0x04E7 || e 0x04E9 || e 0x04EB then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0x04EC || e 0x04EE || e 0x04F0 || e 0x04F2 || e 0x04F4 then
                        Just LetterUppercase

                    else if e 0x04ED || e 0x04EF || e 0x04F1 || e 0x04F3 then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x0527 then
                    if l 0x050D then
                        if l 0x0500 then
                            if e 0x04F5 || e 0x04F7 || e 0x04F9 || e 0x04FB || e 0x04FD || e 0x04FF then
                                Just LetterLowercase

                            else if e 0x04F6 || e 0x04F8 || e 0x04FA || e 0x04FC || e 0x04FE then
                                Just LetterUppercase

                            else
                                Nothing

                        else if l 0x0505 then
                            if e 0x0500 || e 0x0502 || e 0x0504 then
                                Just LetterUppercase

                            else if e 0x0501 || e 0x0503 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if e 0x0505 || e 0x0507 || e 0x0509 || e 0x050B then
                            Just LetterLowercase

                        else if e 0x0506 || e 0x0508 || e 0x050A || e 0x050C then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x0519 then
                        if e 0x050D || e 0x050F || e 0x0511 || e 0x0513 || e 0x0515 || e 0x0517 then
                            Just LetterLowercase

                        else if e 0x050E || e 0x0510 || e 0x0512 || e 0x0514 || e 0x0516 || e 0x0518 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x051F then
                        if e 0x0519 || e 0x051B || e 0x051D then
                            Just LetterLowercase

                        else if e 0x051A || e 0x051C || e 0x051E then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x051F || e 0x0521 || e 0x0523 || e 0x0525 then
                        Just LetterLowercase

                    else if e 0x0520 || e 0x0522 || e 0x0524 || e 0x0526 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x05CF then
                    if l 0x0588 then
                        if l 0x052C then
                            if e 0x0527 || e 0x0529 || e 0x052B then
                                Just LetterLowercase

                            else if e 0x0528 || e 0x052A then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x052C || e 0x052E || r 0x0531 0x0556 then
                            Just LetterUppercase

                        else if e 0x052D || e 0x052F || r 0x0560 0x0587 then
                            Just LetterLowercase

                        else if e 0x0559 then
                            Just LetterModifier

                        else if r 0x055A 0x055F then
                            Just PunctuationOther

                        else
                            Nothing

                    else if l 0x05BE then
                        if e 0x0588 then
                            Just LetterLowercase

                        else if e 0x0589 then
                            Just PunctuationOther

                        else if e 0x058A then
                            Just PunctuationDash

                        else if r 0x058D 0x058E then
                            Just SymbolOther

                        else if e 0x058F then
                            Just SymbolCurrency

                        else if r 0x0591 0x05BD then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x05BE then
                        Just PunctuationDash

                    else if e 0x05BF || r 0x05C1 0x05C2 || r 0x05C4 0x05C5 || e 0x05C7 then
                        Just MarkNonSpacing

                    else if e 0x05C0 || e 0x05C3 || e 0x05C6 then
                        Just PunctuationOther

                    else
                        Nothing

                else if l 0x061F then
                    if l 0x060A then
                        if r 0x05D0 0x05EA || r 0x05EF 0x05F2 then
                            Just LetterOther

                        else if r 0x05F3 0x05F4 || e 0x0609 then
                            Just PunctuationOther

                        else if r 0x0600 0x0605 then
                            Just OtherFormat

                        else if r 0x0606 0x0608 then
                            Just SymbolMath

                        else
                            Nothing

                    else if e 0x060A || r 0x060C 0x060D || e 0x061B || r 0x061D 0x061E then
                        Just PunctuationOther

                    else if e 0x060B then
                        Just SymbolCurrency

                    else if r 0x060E 0x060F then
                        Just SymbolOther

                    else if r 0x0610 0x061A then
                        Just MarkNonSpacing

                    else if e 0x061C then
                        Just OtherFormat

                    else
                        Nothing

                else if l 0x066D then
                    if e 0x061F || r 0x066A 0x066C then
                        Just PunctuationOther

                    else if r 0x0620 0x063F || r 0x0641 0x064A then
                        Just LetterOther

                    else if e 0x0640 then
                        Just LetterModifier

                    else if r 0x064B 0x065F then
                        Just MarkNonSpacing

                    else if r 0x0660 0x0669 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if e 0x066D || e 0x06D4 then
                    Just PunctuationOther

                else if r 0x066E 0x066F || r 0x0671 0x06D3 || e 0x06D5 then
                    Just LetterOther

                else if e 0x0670 || r 0x06D6 0x06DC then
                    Just MarkNonSpacing

                else if e 0x06DD then
                    Just OtherFormat

                else
                    Nothing

            else if l 0x0A02 then
                if l 0x08E2 then
                    if l 0x07F6 then
                        if l 0x070E then
                            if e 0x06DE || e 0x06E9 || r 0x06FD 0x06FE then
                                Just SymbolOther

                            else if r 0x06DF 0x06E4 || r 0x06E7 0x06E8 || r 0x06EA 0x06ED then
                                Just MarkNonSpacing

                            else if r 0x06E5 0x06E6 then
                                Just LetterModifier

                            else if r 0x06EE 0x06EF || r 0x06FA 0x06FC || e 0x06FF then
                                Just LetterOther

                            else if r 0x06F0 0x06F9 then
                                Just NumberDecimalDigit

                            else if r 0x0700 0x070D then
                                Just PunctuationOther

                            else
                                Nothing

                        else if e 0x070F then
                            Just OtherFormat

                        else if e 0x0710 || r 0x0712 0x072F || r 0x074D 0x07A5 || e 0x07B1 || r 0x07CA 0x07EA then
                            Just LetterOther

                        else if e 0x0711 || r 0x0730 0x074A || r 0x07A6 0x07B0 || r 0x07EB 0x07F3 then
                            Just MarkNonSpacing

                        else if r 0x07C0 0x07C9 then
                            Just NumberDecimalDigit

                        else if r 0x07F4 0x07F5 then
                            Just LetterModifier

                        else
                            Nothing

                    else if l 0x082F then
                        if l 0x0815 then
                            if e 0x07F6 then
                                Just SymbolOther

                            else if r 0x07F7 0x07F9 then
                                Just PunctuationOther

                            else if e 0x07FA then
                                Just LetterModifier

                            else if e 0x07FD then
                                Just MarkNonSpacing

                            else if r 0x07FE 0x07FF then
                                Just SymbolCurrency

                            else if r 0x0800 0x0814 then
                                Just LetterOther

                            else
                                Nothing

                        else if e 0x0815 then
                            Just LetterOther

                        else if r 0x0816 0x0819 || r 0x081B 0x0823 || r 0x0825 0x0827 || r 0x0829 0x082D then
                            Just MarkNonSpacing

                        else if e 0x081A || e 0x0824 || e 0x0828 then
                            Just LetterModifier

                        else
                            Nothing

                    else if l 0x0887 then
                        if r 0x0830 0x083E || e 0x085E then
                            Just PunctuationOther

                        else if r 0x0840 0x0858 || r 0x0860 0x086A || r 0x0870 0x0886 then
                            Just LetterOther

                        else if r 0x0859 0x085B then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x0887 || r 0x0889 0x088E || r 0x08A0 0x08C8 then
                        Just LetterOther

                    else if e 0x0888 then
                        Just SymbolModifier

                    else if r 0x0890 0x0891 then
                        Just OtherFormat

                    else if r 0x0898 0x089F || r 0x08CA 0x08E1 then
                        Just MarkNonSpacing

                    else if e 0x08C9 then
                        Just LetterModifier

                    else
                        Nothing

                else if l 0x0992 then
                    if l 0x094F then
                        if l 0x093B then
                            if e 0x08E2 then
                                Just OtherFormat

                            else if r 0x08E3 0x0902 || e 0x093A then
                                Just MarkNonSpacing

                            else if e 0x0903 then
                                Just MarkSpacingCombining

                            else if r 0x0904 0x0939 then
                                Just LetterOther

                            else
                                Nothing

                        else if e 0x093B || r 0x093E 0x0940 || r 0x0949 0x094C || e 0x094E then
                            Just MarkSpacingCombining

                        else if e 0x093C || r 0x0941 0x0948 || e 0x094D then
                            Just MarkNonSpacing

                        else if e 0x093D then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0x096F then
                        if e 0x094F then
                            Just MarkSpacingCombining

                        else if e 0x0950 || r 0x0958 0x0961 then
                            Just LetterOther

                        else if r 0x0951 0x0957 || r 0x0962 0x0963 then
                            Just MarkNonSpacing

                        else if r 0x0964 0x0965 then
                            Just PunctuationOther

                        else if r 0x0966 0x096E then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if e 0x096F then
                        Just NumberDecimalDigit

                    else if e 0x0970 then
                        Just PunctuationOther

                    else if e 0x0971 then
                        Just LetterModifier

                    else if r 0x0972 0x0980 || r 0x0985 0x098C || r 0x098F 0x0990 then
                        Just LetterOther

                    else if e 0x0981 then
                        Just MarkNonSpacing

                    else if r 0x0982 0x0983 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if l 0x09DB then
                    if l 0x09BD then
                        if r 0x0993 0x09A8 || r 0x09AA 0x09B0 || e 0x09B2 || r 0x09B6 0x09B9 then
                            Just LetterOther

                        else if e 0x09BC then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x09BD || e 0x09CE then
                        Just LetterOther

                    else if r 0x09BE 0x09C0 || r 0x09C7 0x09C8 || r 0x09CB 0x09CC || e 0x09D7 then
                        Just MarkSpacingCombining

                    else if r 0x09C1 0x09C4 || e 0x09CD then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x09F3 then
                    if r 0x09DC 0x09DD || r 0x09DF 0x09E1 || r 0x09F0 0x09F1 then
                        Just LetterOther

                    else if r 0x09E2 0x09E3 then
                        Just MarkNonSpacing

                    else if r 0x09E6 0x09EF then
                        Just NumberDecimalDigit

                    else if e 0x09F2 then
                        Just SymbolCurrency

                    else
                        Nothing

                else if e 0x09F3 || e 0x09FB then
                    Just SymbolCurrency

                else if r 0x09F4 0x09F9 then
                    Just NumberOther

                else if e 0x09FA then
                    Just SymbolOther

                else if e 0x09FC then
                    Just LetterOther

                else if e 0x09FD then
                    Just PunctuationOther

                else if e 0x09FE || e 0x0A01 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if l 0x0B29 then
                if l 0x0A8E then
                    if l 0x0A46 then
                        if e 0x0A02 || e 0x0A3C || r 0x0A41 0x0A42 then
                            Just MarkNonSpacing

                        else if e 0x0A03 || r 0x0A3E 0x0A40 then
                            Just MarkSpacingCombining

                        else if r 0x0A05 0x0A0A || r 0x0A0F 0x0A10 || r 0x0A13 0x0A28 || r 0x0A2A 0x0A30 || r 0x0A32 0x0A33 || r 0x0A35 0x0A36 || r 0x0A38 0x0A39 then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0x0A6F then
                        if r 0x0A47 0x0A48 || r 0x0A4B 0x0A4D || e 0x0A51 then
                            Just MarkNonSpacing

                        else if r 0x0A59 0x0A5C || e 0x0A5E then
                            Just LetterOther

                        else if r 0x0A66 0x0A6E then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if e 0x0A6F then
                        Just NumberDecimalDigit

                    else if r 0x0A70 0x0A71 || e 0x0A75 || r 0x0A81 0x0A82 then
                        Just MarkNonSpacing

                    else if r 0x0A72 0x0A74 || r 0x0A85 0x0A8D then
                        Just LetterOther

                    else if e 0x0A76 then
                        Just PunctuationOther

                    else if e 0x0A83 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if l 0x0ACF then
                    if l 0x0ABC then
                        if r 0x0A8F 0x0A91 || r 0x0A93 0x0AA8 || r 0x0AAA 0x0AB0 || r 0x0AB2 0x0AB3 || r 0x0AB5 0x0AB9 then
                            Just LetterOther

                        else
                            Nothing

                    else if e 0x0ABC || r 0x0AC1 0x0AC5 || r 0x0AC7 0x0AC8 || e 0x0ACD then
                        Just MarkNonSpacing

                    else if e 0x0ABD then
                        Just LetterOther

                    else if r 0x0ABE 0x0AC0 || e 0x0AC9 || r 0x0ACB 0x0ACC then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if l 0x0AF8 then
                    if e 0x0AD0 || r 0x0AE0 0x0AE1 then
                        Just LetterOther

                    else if r 0x0AE2 0x0AE3 then
                        Just MarkNonSpacing

                    else if r 0x0AE6 0x0AEF then
                        Just NumberDecimalDigit

                    else if e 0x0AF0 then
                        Just PunctuationOther

                    else if e 0x0AF1 then
                        Just SymbolCurrency

                    else
                        Nothing

                else if e 0x0AF9 || r 0x0B05 0x0B0C || r 0x0B0F 0x0B10 || r 0x0B13 0x0B28 then
                    Just LetterOther

                else if r 0x0AFA 0x0AFF || e 0x0B01 then
                    Just MarkNonSpacing

                else if r 0x0B02 0x0B03 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if l 0x0B98 then
                if l 0x0B56 then
                    if l 0x0B3E then
                        if r 0x0B2A 0x0B30 || r 0x0B32 0x0B33 || r 0x0B35 0x0B39 || e 0x0B3D then
                            Just LetterOther

                        else if e 0x0B3C then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x0B3E || e 0x0B40 || r 0x0B47 0x0B48 || r 0x0B4B 0x0B4C then
                        Just MarkSpacingCombining

                    else if e 0x0B3F || r 0x0B41 0x0B44 || e 0x0B4D || e 0x0B55 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x0B70 then
                    if e 0x0B56 || r 0x0B62 0x0B63 then
                        Just MarkNonSpacing

                    else if e 0x0B57 then
                        Just MarkSpacingCombining

                    else if r 0x0B5C 0x0B5D || r 0x0B5F 0x0B61 then
                        Just LetterOther

                    else if r 0x0B66 0x0B6F then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if e 0x0B70 then
                    Just SymbolOther

                else if e 0x0B71 || e 0x0B83 || r 0x0B85 0x0B8A || r 0x0B8E 0x0B90 || r 0x0B92 0x0B95 then
                    Just LetterOther

                else if r 0x0B72 0x0B77 then
                    Just NumberOther

                else if e 0x0B82 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if l 0x0BD6 then
                if l 0x0BBD then
                    if r 0x0B99 0x0B9A || e 0x0B9C || r 0x0B9E 0x0B9F || r 0x0BA3 0x0BA4 || r 0x0BA8 0x0BAA || r 0x0BAE 0x0BB9 then
                        Just LetterOther

                    else
                        Nothing

                else if r 0x0BBE 0x0BBF || r 0x0BC1 0x0BC2 || r 0x0BC6 0x0BC8 || r 0x0BCA 0x0BCC then
                    Just MarkSpacingCombining

                else if e 0x0BC0 || e 0x0BCD then
                    Just MarkNonSpacing

                else if e 0x0BD0 then
                    Just LetterOther

                else
                    Nothing

            else if l 0x0BFF then
                if e 0x0BD7 then
                    Just MarkSpacingCombining

                else if r 0x0BE6 0x0BEF then
                    Just NumberDecimalDigit

                else if r 0x0BF0 0x0BF2 then
                    Just NumberOther

                else if r 0x0BF3 0x0BF8 || e 0x0BFA then
                    Just SymbolOther

                else if e 0x0BF9 then
                    Just SymbolCurrency

                else
                    Nothing

            else if e 0x0C00 || e 0x0C04 then
                Just MarkNonSpacing

            else if r 0x0C01 0x0C03 then
                Just MarkSpacingCombining

            else if r 0x0C05 0x0C0C || r 0x0C0E 0x0C10 || r 0x0C12 0x0C28 || r 0x0C2A 0x0C39 then
                Just LetterOther

            else
                Nothing

        else if l 0x1E14 then
            if l 0x1368 then
                if l 0x0EC7 then
                    if l 0x0D49 then
                        if l 0x0CBC then
                            if l 0x0C76 then
                                if e 0x0C3C || r 0x0C3E 0x0C40 || r 0x0C46 0x0C48 || r 0x0C4A 0x0C4D || r 0x0C55 0x0C56 || r 0x0C62 0x0C63 then
                                    Just MarkNonSpacing

                                else if e 0x0C3D || r 0x0C58 0x0C5A || e 0x0C5D || r 0x0C60 0x0C61 then
                                    Just LetterOther

                                else if r 0x0C41 0x0C44 then
                                    Just MarkSpacingCombining

                                else if r 0x0C66 0x0C6F then
                                    Just NumberDecimalDigit

                                else
                                    Nothing

                            else if e 0x0C77 || e 0x0C84 then
                                Just PunctuationOther

                            else if r 0x0C78 0x0C7E then
                                Just NumberOther

                            else if e 0x0C7F then
                                Just SymbolOther

                            else if e 0x0C80 || r 0x0C85 0x0C8C || r 0x0C8E 0x0C90 || r 0x0C92 0x0CA8 || r 0x0CAA 0x0CB3 || r 0x0CB5 0x0CB9 then
                                Just LetterOther

                            else if e 0x0C81 then
                                Just MarkNonSpacing

                            else if r 0x0C82 0x0C83 then
                                Just MarkSpacingCombining

                            else
                                Nothing

                        else if l 0x0CE5 then
                            if l 0x0CC6 then
                                if e 0x0CBC || e 0x0CBF then
                                    Just MarkNonSpacing

                                else if e 0x0CBD then
                                    Just LetterOther

                                else if e 0x0CBE || r 0x0CC0 0x0CC4 then
                                    Just MarkSpacingCombining

                                else
                                    Nothing

                            else if e 0x0CC6 || r 0x0CCC 0x0CCD || r 0x0CE2 0x0CE3 then
                                Just MarkNonSpacing

                            else if r 0x0CC7 0x0CC8 || r 0x0CCA 0x0CCB || r 0x0CD5 0x0CD6 then
                                Just MarkSpacingCombining

                            else if r 0x0CDD 0x0CDE || r 0x0CE0 0x0CE1 then
                                Just LetterOther

                            else
                                Nothing

                        else if l 0x0D0D then
                            if r 0x0CE6 0x0CEF then
                                Just NumberDecimalDigit

                            else if r 0x0CF1 0x0CF2 || r 0x0D04 0x0D0C then
                                Just LetterOther

                            else if e 0x0CF3 || r 0x0D02 0x0D03 then
                                Just MarkSpacingCombining

                            else if r 0x0D00 0x0D01 then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if r 0x0D0E 0x0D10 || r 0x0D12 0x0D3A || e 0x0D3D then
                            Just LetterOther

                        else if r 0x0D3B 0x0D3C || r 0x0D41 0x0D44 then
                            Just MarkNonSpacing

                        else if r 0x0D3E 0x0D40 || r 0x0D46 0x0D48 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if l 0x0DE5 then
                        if l 0x0D79 then
                            if r 0x0D4A 0x0D4C || e 0x0D57 then
                                Just MarkSpacingCombining

                            else if e 0x0D4D || r 0x0D62 0x0D63 then
                                Just MarkNonSpacing

                            else if e 0x0D4E || r 0x0D54 0x0D56 || r 0x0D5F 0x0D61 then
                                Just LetterOther

                            else if e 0x0D4F then
                                Just SymbolOther

                            else if r 0x0D58 0x0D5E || r 0x0D70 0x0D78 then
                                Just NumberOther

                            else if r 0x0D66 0x0D6F then
                                Just NumberDecimalDigit

                            else
                                Nothing

                        else if l 0x0DBC then
                            if e 0x0D79 then
                                Just SymbolOther

                            else if r 0x0D7A 0x0D7F || r 0x0D85 0x0D96 || r 0x0D9A 0x0DB1 || r 0x0DB3 0x0DBB then
                                Just LetterOther

                            else if e 0x0D81 then
                                Just MarkNonSpacing

                            else if r 0x0D82 0x0D83 then
                                Just MarkSpacingCombining

                            else
                                Nothing

                        else if e 0x0DBD || r 0x0DC0 0x0DC6 then
                            Just LetterOther

                        else if e 0x0DCA || r 0x0DD2 0x0DD4 || e 0x0DD6 then
                            Just MarkNonSpacing

                        else if r 0x0DCF 0x0DD1 || r 0x0DD8 0x0DDF then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if l 0x0E59 then
                        if l 0x0E33 then
                            if r 0x0DE6 0x0DEF then
                                Just NumberDecimalDigit

                            else if r 0x0DF2 0x0DF3 then
                                Just MarkSpacingCombining

                            else if e 0x0DF4 then
                                Just PunctuationOther

                            else if r 0x0E01 0x0E30 || e 0x0E32 then
                                Just LetterOther

                            else if e 0x0E31 then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if e 0x0E33 || r 0x0E40 0x0E45 then
                            Just LetterOther

                        else if r 0x0E34 0x0E3A || r 0x0E47 0x0E4E then
                            Just MarkNonSpacing

                        else if e 0x0E3F then
                            Just SymbolCurrency

                        else if e 0x0E46 then
                            Just LetterModifier

                        else if e 0x0E4F then
                            Just PunctuationOther

                        else if r 0x0E50 0x0E58 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if l 0x0EA6 then
                        if e 0x0E59 then
                            Just NumberDecimalDigit

                        else if r 0x0E5A 0x0E5B then
                            Just PunctuationOther

                        else if r 0x0E81 0x0E82 || e 0x0E84 || r 0x0E86 0x0E8A || r 0x0E8C 0x0EA3 || e 0x0EA5 then
                            Just LetterOther

                        else
                            Nothing

                    else if r 0x0EA7 0x0EB0 || r 0x0EB2 0x0EB3 || e 0x0EBD || r 0x0EC0 0x0EC4 then
                        Just LetterOther

                    else if e 0x0EB1 || r 0x0EB4 0x0EBC then
                        Just MarkNonSpacing

                    else if e 0x0EC6 then
                        Just LetterModifier

                    else
                        Nothing

                else if l 0x103E then
                    if l 0x0F48 then
                        if l 0x0F29 then
                            if r 0x0EC8 0x0ECE || r 0x0F18 0x0F19 then
                                Just MarkNonSpacing

                            else if r 0x0ED0 0x0ED9 || r 0x0F20 0x0F28 then
                                Just NumberDecimalDigit

                            else if r 0x0EDC 0x0EDF || e 0x0F00 then
                                Just LetterOther

                            else if r 0x0F01 0x0F03 || e 0x0F13 || r 0x0F15 0x0F17 || r 0x0F1A 0x0F1F then
                                Just SymbolOther

                            else if r 0x0F04 0x0F12 || e 0x0F14 then
                                Just PunctuationOther

                            else
                                Nothing

                        else if l 0x0F38 then
                            if e 0x0F29 then
                                Just NumberDecimalDigit

                            else if r 0x0F2A 0x0F33 then
                                Just NumberOther

                            else if e 0x0F34 || e 0x0F36 then
                                Just SymbolOther

                            else if e 0x0F35 || e 0x0F37 then
                                Just MarkNonSpacing

                            else
                                Nothing

                        else if e 0x0F38 then
                            Just SymbolOther

                        else if e 0x0F39 then
                            Just MarkNonSpacing

                        else if e 0x0F3A || e 0x0F3C then
                            Just PunctuationOpen

                        else if e 0x0F3B || e 0x0F3D then
                            Just PunctuationClose

                        else if r 0x0F3E 0x0F3F then
                            Just MarkSpacingCombining

                        else if r 0x0F40 0x0F47 then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0x0FCD then
                        if r 0x0F49 0x0F6C || r 0x0F88 0x0F8C then
                            Just LetterOther

                        else if r 0x0F71 0x0F7E || r 0x0F80 0x0F84 || r 0x0F86 0x0F87 || r 0x0F8D 0x0F97 || r 0x0F99 0x0FBC || e 0x0FC6 then
                            Just MarkNonSpacing

                        else if e 0x0F7F then
                            Just MarkSpacingCombining

                        else if e 0x0F85 then
                            Just PunctuationOther

                        else if r 0x0FBE 0x0FC5 || r 0x0FC7 0x0FCC then
                            Just SymbolOther

                        else
                            Nothing

                    else if l 0x102C then
                        if r 0x0FCE 0x0FCF || r 0x0FD5 0x0FD8 then
                            Just SymbolOther

                        else if r 0x0FD0 0x0FD4 || r 0x0FD9 0x0FDA then
                            Just PunctuationOther

                        else if r 0x1000 0x102A then
                            Just LetterOther

                        else if e 0x102B then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if e 0x102C || e 0x1031 || e 0x1038 || r 0x103B 0x103C then
                        Just MarkSpacingCombining

                    else if r 0x102D 0x1030 || r 0x1032 0x1037 || r 0x1039 0x103A || e 0x103D then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x109D then
                    if l 0x1066 then
                        if e 0x103E || r 0x1058 0x1059 || r 0x105E 0x1060 then
                            Just MarkNonSpacing

                        else if e 0x103F || r 0x1050 0x1055 || r 0x105A 0x105D || e 0x1061 || e 0x1065 then
                            Just LetterOther

                        else if r 0x1040 0x1049 then
                            Just NumberDecimalDigit

                        else if r 0x104A 0x104F then
                            Just PunctuationOther

                        else if r 0x1056 0x1057 || r 0x1062 0x1064 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if l 0x1084 then
                        if e 0x1066 || r 0x106E 0x1070 || r 0x1075 0x1081 then
                            Just LetterOther

                        else if r 0x1067 0x106D || e 0x1083 then
                            Just MarkSpacingCombining

                        else if r 0x1071 0x1074 || e 0x1082 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x1084 || r 0x1087 0x108C || e 0x108F || r 0x109A 0x109C then
                        Just MarkSpacingCombining

                    else if r 0x1085 0x1086 || e 0x108D then
                        Just MarkNonSpacing

                    else if e 0x108E then
                        Just LetterOther

                    else if r 0x1090 0x1099 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0x1259 then
                    if l 0x10FA then
                        if e 0x109D then
                            Just MarkNonSpacing

                        else if r 0x109E 0x109F then
                            Just SymbolOther

                        else if r 0x10A0 0x10C5 || e 0x10C7 || e 0x10CD then
                            Just LetterUppercase

                        else if r 0x10D0 0x10F9 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0x10FA || r 0x10FD 0x10FF then
                        Just LetterLowercase

                    else if e 0x10FB then
                        Just PunctuationOther

                    else if e 0x10FC then
                        Just LetterModifier

                    else if r 0x1100 0x1248 || r 0x124A 0x124D || r 0x1250 0x1256 || e 0x1258 then
                        Just LetterOther

                    else
                        Nothing

                else if l 0x12C1 then
                    if r 0x125A 0x125D || r 0x1260 0x1288 || r 0x128A 0x128D || r 0x1290 0x12B0 || r 0x12B2 0x12B5 || r 0x12B8 0x12BE || e 0x12C0 then
                        Just LetterOther

                    else
                        Nothing

                else if r 0x12C2 0x12C5 || r 0x12C8 0x12D6 || r 0x12D8 0x1310 || r 0x1312 0x1315 || r 0x1318 0x135A then
                    Just LetterOther

                else if r 0x135D 0x135F then
                    Just MarkNonSpacing

                else if r 0x1360 0x1367 then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x1A8F then
                if l 0x180A then
                    if l 0x1734 then
                        if l 0x1680 then
                            if e 0x1368 || e 0x166E then
                                Just PunctuationOther

                            else if r 0x1369 0x137C then
                                Just NumberOther

                            else if r 0x1380 0x138F || r 0x1401 0x166C || r 0x166F 0x167F then
                                Just LetterOther

                            else if r 0x1390 0x1399 || e 0x166D then
                                Just SymbolOther

                            else if r 0x13A0 0x13F5 then
                                Just LetterUppercase

                            else if r 0x13F8 0x13FD then
                                Just LetterLowercase

                            else if e 0x1400 then
                                Just PunctuationDash

                            else
                                Nothing

                        else if l 0x16ED then
                            if e 0x1680 then
                                Just SeparatorSpace

                            else if r 0x1681 0x169A || r 0x16A0 0x16EA then
                                Just LetterOther

                            else if e 0x169B then
                                Just PunctuationOpen

                            else if e 0x169C then
                                Just PunctuationClose

                            else if r 0x16EB 0x16EC then
                                Just PunctuationOther

                            else
                                Nothing

                        else if e 0x16ED then
                            Just PunctuationOther

                        else if r 0x16EE 0x16F0 then
                            Just NumberLetter

                        else if r 0x16F1 0x16F8 || r 0x1700 0x1711 || r 0x171F 0x1731 then
                            Just LetterOther

                        else if r 0x1712 0x1714 || r 0x1732 0x1733 then
                            Just MarkNonSpacing

                        else if e 0x1715 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if l 0x17C6 then
                        if e 0x1734 || e 0x17B6 || r 0x17BE 0x17C5 then
                            Just MarkSpacingCombining

                        else if r 0x1735 0x1736 then
                            Just PunctuationOther

                        else if r 0x1740 0x1751 || r 0x1760 0x176C || r 0x176E 0x1770 || r 0x1780 0x17B3 then
                            Just LetterOther

                        else if r 0x1752 0x1753 || r 0x1772 0x1773 || r 0x17B4 0x17B5 || r 0x17B7 0x17BD then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if l 0x17DB then
                        if e 0x17C6 || r 0x17C9 0x17D3 then
                            Just MarkNonSpacing

                        else if r 0x17C7 0x17C8 then
                            Just MarkSpacingCombining

                        else if r 0x17D4 0x17D6 || r 0x17D8 0x17DA then
                            Just PunctuationOther

                        else if e 0x17D7 then
                            Just LetterModifier

                        else
                            Nothing

                    else if e 0x17DB then
                        Just SymbolCurrency

                    else if e 0x17DC then
                        Just LetterOther

                    else if e 0x17DD then
                        Just MarkNonSpacing

                    else if r 0x17E0 0x17E9 then
                        Just NumberDecimalDigit

                    else if r 0x17F0 0x17F9 then
                        Just NumberOther

                    else if r 0x1800 0x1805 || r 0x1807 0x1809 then
                        Just PunctuationOther

                    else if e 0x1806 then
                        Just PunctuationDash

                    else
                        Nothing

                else if l 0x194F then
                    if l 0x18AF then
                        if l 0x1842 then
                            if e 0x180A then
                                Just PunctuationOther

                            else if r 0x180B 0x180D || e 0x180F then
                                Just MarkNonSpacing

                            else if e 0x180E then
                                Just OtherFormat

                            else if r 0x1810 0x1819 then
                                Just NumberDecimalDigit

                            else if r 0x1820 0x1841 then
                                Just LetterOther

                            else
                                Nothing

                        else if e 0x1842 || r 0x1844 0x1878 || r 0x1880 0x1884 || r 0x1887 0x18A8 || e 0x18AA then
                            Just LetterOther

                        else if e 0x1843 then
                            Just LetterModifier

                        else if r 0x1885 0x1886 || e 0x18A9 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if l 0x192F then
                        if r 0x18B0 0x18F5 || r 0x1900 0x191E then
                            Just LetterOther

                        else if r 0x1920 0x1922 || r 0x1927 0x1928 then
                            Just MarkNonSpacing

                        else if r 0x1923 0x1926 || r 0x1929 0x192B then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if r 0x1930 0x1931 || r 0x1933 0x1938 then
                        Just MarkSpacingCombining

                    else if e 0x1932 || r 0x1939 0x193B then
                        Just MarkNonSpacing

                    else if e 0x1940 then
                        Just SymbolOther

                    else if r 0x1944 0x1945 then
                        Just PunctuationOther

                    else if r 0x1946 0x194E then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0x1A1F then
                    if l 0x19D9 then
                        if e 0x194F || r 0x19D0 0x19D8 then
                            Just NumberDecimalDigit

                        else if r 0x1950 0x196D || r 0x1970 0x1974 || r 0x1980 0x19AB || r 0x19B0 0x19C9 then
                            Just LetterOther

                        else
                            Nothing

                    else if e 0x19D9 then
                        Just NumberDecimalDigit

                    else if e 0x19DA then
                        Just NumberOther

                    else if r 0x19DE 0x19FF then
                        Just SymbolOther

                    else if r 0x1A00 0x1A16 then
                        Just LetterOther

                    else if r 0x1A17 0x1A18 || e 0x1A1B then
                        Just MarkNonSpacing

                    else if r 0x1A19 0x1A1A then
                        Just MarkSpacingCombining

                    else if e 0x1A1E then
                        Just PunctuationOther

                    else
                        Nothing

                else if l 0x1A60 then
                    if e 0x1A1F then
                        Just PunctuationOther

                    else if r 0x1A20 0x1A54 then
                        Just LetterOther

                    else if e 0x1A55 || e 0x1A57 then
                        Just MarkSpacingCombining

                    else if e 0x1A56 || r 0x1A58 0x1A5E then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if e 0x1A60 || e 0x1A62 || r 0x1A65 0x1A6C || r 0x1A73 0x1A7C || e 0x1A7F then
                    Just MarkNonSpacing

                else if e 0x1A61 || r 0x1A63 0x1A64 || r 0x1A6D 0x1A72 then
                    Just MarkSpacingCombining

                else if r 0x1A80 0x1A89 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if l 0x1C3A then
                if l 0x1B7F then
                    if l 0x1B35 then
                        if r 0x1A90 0x1A99 then
                            Just NumberDecimalDigit

                        else if r 0x1AA0 0x1AA6 || r 0x1AA8 0x1AAD then
                            Just PunctuationOther

                        else if e 0x1AA7 then
                            Just LetterModifier

                        else if r 0x1AB0 0x1ABD || r 0x1ABF 0x1ACE || r 0x1B00 0x1B03 || e 0x1B34 then
                            Just MarkNonSpacing

                        else if e 0x1ABE then
                            Just MarkEnclosing

                        else if e 0x1B04 then
                            Just MarkSpacingCombining

                        else if r 0x1B05 0x1B33 then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0x1B44 then
                        if e 0x1B35 || e 0x1B3B || r 0x1B3D 0x1B41 || e 0x1B43 then
                            Just MarkSpacingCombining

                        else if r 0x1B36 0x1B3A || e 0x1B3C || e 0x1B42 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x1B44 then
                        Just MarkSpacingCombining

                    else if r 0x1B45 0x1B4C then
                        Just LetterOther

                    else if r 0x1B50 0x1B59 then
                        Just NumberDecimalDigit

                    else if r 0x1B5A 0x1B60 || r 0x1B7D 0x1B7E then
                        Just PunctuationOther

                    else if r 0x1B61 0x1B6A || r 0x1B74 0x1B7C then
                        Just SymbolOther

                    else if r 0x1B6B 0x1B73 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x1BE6 then
                    if r 0x1B80 0x1B81 || r 0x1BA2 0x1BA5 || r 0x1BA8 0x1BA9 || r 0x1BAB 0x1BAD then
                        Just MarkNonSpacing

                    else if e 0x1B82 || e 0x1BA1 || r 0x1BA6 0x1BA7 || e 0x1BAA then
                        Just MarkSpacingCombining

                    else if r 0x1B83 0x1BA0 || r 0x1BAE 0x1BAF || r 0x1BBA 0x1BE5 then
                        Just LetterOther

                    else if r 0x1BB0 0x1BB9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0x1BF1 then
                    if e 0x1BE6 || r 0x1BE8 0x1BE9 || e 0x1BED || r 0x1BEF 0x1BF0 then
                        Just MarkNonSpacing

                    else if e 0x1BE7 || r 0x1BEA 0x1BEC || e 0x1BEE then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if e 0x1BF1 || r 0x1C2C 0x1C33 || r 0x1C36 0x1C37 then
                    Just MarkNonSpacing

                else if r 0x1BF2 0x1BF3 || r 0x1C24 0x1C2B || r 0x1C34 0x1C35 then
                    Just MarkSpacingCombining

                else if r 0x1BFC 0x1BFF then
                    Just PunctuationOther

                else if r 0x1C00 0x1C23 then
                    Just LetterOther

                else
                    Nothing

            else if l 0x1D2B then
                if l 0x1CD2 then
                    if r 0x1C3B 0x1C3F || r 0x1C7E 0x1C7F || r 0x1CC0 0x1CC7 then
                        Just PunctuationOther

                    else if r 0x1C40 0x1C49 || r 0x1C50 0x1C59 then
                        Just NumberDecimalDigit

                    else if r 0x1C4D 0x1C4F || r 0x1C5A 0x1C77 then
                        Just LetterOther

                    else if r 0x1C78 0x1C7D then
                        Just LetterModifier

                    else if r 0x1C80 0x1C88 then
                        Just LetterLowercase

                    else if r 0x1C90 0x1CBA || r 0x1CBD 0x1CBF then
                        Just LetterUppercase

                    else if r 0x1CD0 0x1CD1 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x1CED then
                    if e 0x1CD2 || r 0x1CD4 0x1CE0 || r 0x1CE2 0x1CE8 then
                        Just MarkNonSpacing

                    else if e 0x1CD3 then
                        Just PunctuationOther

                    else if e 0x1CE1 then
                        Just MarkSpacingCombining

                    else if r 0x1CE9 0x1CEC then
                        Just LetterOther

                    else
                        Nothing

                else if e 0x1CED || e 0x1CF4 || r 0x1CF8 0x1CF9 then
                    Just MarkNonSpacing

                else if r 0x1CEE 0x1CF3 || r 0x1CF5 0x1CF6 || e 0x1CFA then
                    Just LetterOther

                else if e 0x1CF7 then
                    Just MarkSpacingCombining

                else if r 0x1D00 0x1D2A then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0x1E05 then
                if e 0x1D2B || r 0x1D6B 0x1D77 || r 0x1D79 0x1D9A || e 0x1E01 || e 0x1E03 then
                    Just LetterLowercase

                else if r 0x1D2C 0x1D6A || e 0x1D78 || r 0x1D9B 0x1DBF then
                    Just LetterModifier

                else if r 0x1DC0 0x1DFF then
                    Just MarkNonSpacing

                else if e 0x1E00 || e 0x1E02 || e 0x1E04 then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0x1E0B then
                if e 0x1E05 || e 0x1E07 || e 0x1E09 then
                    Just LetterLowercase

                else if e 0x1E06 || e 0x1E08 || e 0x1E0A then
                    Just LetterUppercase

                else
                    Nothing

            else if e 0x1E0B || e 0x1E0D || e 0x1E0F || e 0x1E11 || e 0x1E13 then
                Just LetterLowercase

            else if e 0x1E0C || e 0x1E0E || e 0x1E10 || e 0x1E12 then
                Just LetterUppercase

            else
                Nothing

        else if l 0x1EE6 then
            if l 0x1E78 then
                if l 0x1E45 then
                    if l 0x1E2B then
                        if l 0x1E1E then
                            if e 0x1E14 || e 0x1E16 || e 0x1E18 || e 0x1E1A || e 0x1E1C then
                                Just LetterUppercase

                            else if e 0x1E15 || e 0x1E17 || e 0x1E19 || e 0x1E1B || e 0x1E1D then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0x1E23 then
                            if e 0x1E1E || e 0x1E20 || e 0x1E22 then
                                Just LetterUppercase

                            else if e 0x1E1F || e 0x1E21 then
                                Just LetterLowercase

                            else
                                Nothing

                        else if e 0x1E23 || e 0x1E25 || e 0x1E27 || e 0x1E29 then
                            Just LetterLowercase

                        else if e 0x1E24 || e 0x1E26 || e 0x1E28 || e 0x1E2A then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x1E37 then
                        if e 0x1E2B || e 0x1E2D || e 0x1E2F || e 0x1E31 || e 0x1E33 || e 0x1E35 then
                            Just LetterLowercase

                        else if e 0x1E2C || e 0x1E2E || e 0x1E30 || e 0x1E32 || e 0x1E34 || e 0x1E36 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x1E3D then
                        if e 0x1E37 || e 0x1E39 || e 0x1E3B then
                            Just LetterLowercase

                        else if e 0x1E38 || e 0x1E3A || e 0x1E3C then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x1E3D || e 0x1E3F || e 0x1E41 || e 0x1E43 then
                        Just LetterLowercase

                    else if e 0x1E3E || e 0x1E40 || e 0x1E42 || e 0x1E44 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x1E5D then
                    if l 0x1E50 then
                        if e 0x1E45 || e 0x1E47 || e 0x1E49 || e 0x1E4B || e 0x1E4D || e 0x1E4F then
                            Just LetterLowercase

                        else if e 0x1E46 || e 0x1E48 || e 0x1E4A || e 0x1E4C || e 0x1E4E then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0x1E55 then
                        if e 0x1E50 || e 0x1E52 || e 0x1E54 then
                            Just LetterUppercase

                        else if e 0x1E51 || e 0x1E53 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0x1E55 || e 0x1E57 || e 0x1E59 || e 0x1E5B then
                        Just LetterLowercase

                    else if e 0x1E56 || e 0x1E58 || e 0x1E5A || e 0x1E5C then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x1E69 then
                    if e 0x1E5D || e 0x1E5F || e 0x1E61 || e 0x1E63 || e 0x1E65 || e 0x1E67 then
                        Just LetterLowercase

                    else if e 0x1E5E || e 0x1E60 || e 0x1E62 || e 0x1E64 || e 0x1E66 || e 0x1E68 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x1E6F then
                    if e 0x1E69 || e 0x1E6B || e 0x1E6D then
                        Just LetterLowercase

                    else if e 0x1E6A || e 0x1E6C || e 0x1E6E then
                        Just LetterUppercase

                    else
                        Nothing

                else if e 0x1E6F || e 0x1E71 || e 0x1E73 || e 0x1E75 || e 0x1E77 then
                    Just LetterLowercase

                else if e 0x1E70 || e 0x1E72 || e 0x1E74 || e 0x1E76 then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0x1EB2 then
                if l 0x1E90 then
                    if l 0x1E83 then
                        if e 0x1E78 || e 0x1E7A || e 0x1E7C || e 0x1E7E || e 0x1E80 || e 0x1E82 then
                            Just LetterUppercase

                        else if e 0x1E79 || e 0x1E7B || e 0x1E7D || e 0x1E7F || e 0x1E81 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x1E88 then
                        if e 0x1E83 || e 0x1E85 || e 0x1E87 then
                            Just LetterLowercase

                        else if e 0x1E84 || e 0x1E86 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x1E88 || e 0x1E8A || e 0x1E8C || e 0x1E8E then
                        Just LetterUppercase

                    else if e 0x1E89 || e 0x1E8B || e 0x1E8D || e 0x1E8F then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x1EA4 then
                    if e 0x1E90 || e 0x1E92 || e 0x1E94 || e 0x1E9E || e 0x1EA0 || e 0x1EA2 then
                        Just LetterUppercase

                    else if e 0x1E91 || e 0x1E93 || r 0x1E95 0x1E9D || e 0x1E9F || e 0x1EA1 || e 0x1EA3 then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x1EAA then
                    if e 0x1EA4 || e 0x1EA6 || e 0x1EA8 then
                        Just LetterUppercase

                    else if e 0x1EA5 || e 0x1EA7 || e 0x1EA9 then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0x1EAA || e 0x1EAC || e 0x1EAE || e 0x1EB0 then
                    Just LetterUppercase

                else if e 0x1EAB || e 0x1EAD || e 0x1EAF || e 0x1EB1 then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0x1ECB then
                if l 0x1EBD then
                    if e 0x1EB2 || e 0x1EB4 || e 0x1EB6 || e 0x1EB8 || e 0x1EBA || e 0x1EBC then
                        Just LetterUppercase

                    else if e 0x1EB3 || e 0x1EB5 || e 0x1EB7 || e 0x1EB9 || e 0x1EBB then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x1EC3 then
                    if e 0x1EBD || e 0x1EBF || e 0x1EC1 then
                        Just LetterLowercase

                    else if e 0x1EBE || e 0x1EC0 || e 0x1EC2 then
                        Just LetterUppercase

                    else
                        Nothing

                else if e 0x1EC3 || e 0x1EC5 || e 0x1EC7 || e 0x1EC9 then
                    Just LetterLowercase

                else if e 0x1EC4 || e 0x1EC6 || e 0x1EC8 || e 0x1ECA then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0x1ED7 then
                if e 0x1ECB || e 0x1ECD || e 0x1ECF || e 0x1ED1 || e 0x1ED3 || e 0x1ED5 then
                    Just LetterLowercase

                else if e 0x1ECC || e 0x1ECE || e 0x1ED0 || e 0x1ED2 || e 0x1ED4 || e 0x1ED6 then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0x1EDD then
                if e 0x1ED7 || e 0x1ED9 || e 0x1EDB then
                    Just LetterLowercase

                else if e 0x1ED8 || e 0x1EDA || e 0x1EDC then
                    Just LetterUppercase

                else
                    Nothing

            else if e 0x1EDD || e 0x1EDF || e 0x1EE1 || e 0x1EE3 || e 0x1EE5 then
                Just LetterLowercase

            else if e 0x1EDE || e 0x1EE0 || e 0x1EE2 || e 0x1EE4 then
                Just LetterUppercase

            else
                Nothing

        else if l 0x206F then
            if l 0x1FBE then
                if l 0x1EFE then
                    if l 0x1EF1 then
                        if e 0x1EE6 || e 0x1EE8 || e 0x1EEA || e 0x1EEC || e 0x1EEE || e 0x1EF0 then
                            Just LetterUppercase

                        else if e 0x1EE7 || e 0x1EE9 || e 0x1EEB || e 0x1EED || e 0x1EEF then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x1EF6 then
                        if e 0x1EF1 || e 0x1EF3 || e 0x1EF5 then
                            Just LetterLowercase

                        else if e 0x1EF2 || e 0x1EF4 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x1EF6 || e 0x1EF8 || e 0x1EFA || e 0x1EFC then
                        Just LetterUppercase

                    else if e 0x1EF7 || e 0x1EF9 || e 0x1EFB || e 0x1EFD then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x1F67 then
                    if l 0x1F27 then
                        if e 0x1EFE || r 0x1F08 0x1F0F || r 0x1F18 0x1F1D then
                            Just LetterUppercase

                        else if r 0x1EFF 0x1F07 || r 0x1F10 0x1F15 || r 0x1F20 0x1F26 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0x1F27 || r 0x1F30 0x1F37 || r 0x1F40 0x1F45 || r 0x1F50 0x1F57 || r 0x1F60 0x1F66 then
                        Just LetterLowercase

                    else if r 0x1F28 0x1F2F || r 0x1F38 0x1F3F || r 0x1F48 0x1F4D || ((modBy 2 code == 1) && r 0x1F59 0x1F5F) then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x1F9F then
                    if e 0x1F67 || r 0x1F70 0x1F7D || r 0x1F80 0x1F87 || r 0x1F90 0x1F97 then
                        Just LetterLowercase

                    else if r 0x1F68 0x1F6F then
                        Just LetterUppercase

                    else if r 0x1F88 0x1F8F || r 0x1F98 0x1F9E then
                        Just LetterTitlecase

                    else
                        Nothing

                else if e 0x1F9F || r 0x1FA8 0x1FAF || e 0x1FBC then
                    Just LetterTitlecase

                else if r 0x1FA0 0x1FA7 || r 0x1FB0 0x1FB4 || r 0x1FB6 0x1FB7 then
                    Just LetterLowercase

                else if r 0x1FB8 0x1FBB then
                    Just LetterUppercase

                else if e 0x1FBD then
                    Just SymbolModifier

                else
                    Nothing

            else if l 0x201A then
                if l 0x1FE7 then
                    if e 0x1FBE || r 0x1FC2 0x1FC4 || r 0x1FC6 0x1FC7 || r 0x1FD0 0x1FD3 || r 0x1FD6 0x1FD7 || r 0x1FE0 0x1FE6 then
                        Just LetterLowercase

                    else if r 0x1FBF 0x1FC1 || r 0x1FCD 0x1FCF || r 0x1FDD 0x1FDF then
                        Just SymbolModifier

                    else if r 0x1FC8 0x1FCB || r 0x1FD8 0x1FDB then
                        Just LetterUppercase

                    else if e 0x1FCC then
                        Just LetterTitlecase

                    else
                        Nothing

                else if l 0x1FFC then
                    if e 0x1FE7 || r 0x1FF2 0x1FF4 || r 0x1FF6 0x1FF7 then
                        Just LetterLowercase

                    else if r 0x1FE8 0x1FEC || r 0x1FF8 0x1FFB then
                        Just LetterUppercase

                    else if r 0x1FED 0x1FEF then
                        Just SymbolModifier

                    else
                        Nothing

                else if e 0x1FFC then
                    Just LetterTitlecase

                else if r 0x1FFD 0x1FFE then
                    Just SymbolModifier

                else if r 0x2000 0x200A then
                    Just SeparatorSpace

                else if r 0x200B 0x200F then
                    Just OtherFormat

                else if r 0x2010 0x2015 then
                    Just PunctuationDash

                else if r 0x2016 0x2017 then
                    Just PunctuationOther

                else if e 0x2018 then
                    Just PunctuationInitialQuote

                else if e 0x2019 then
                    Just PunctuationFinalQuote

                else
                    Nothing

            else if l 0x203A then
                if e 0x201A || e 0x201E then
                    Just PunctuationOpen

                else if r 0x201B 0x201C || e 0x201F || e 0x2039 then
                    Just PunctuationInitialQuote

                else if e 0x201D then
                    Just PunctuationFinalQuote

                else if r 0x2020 0x2027 || r 0x2030 0x2038 then
                    Just PunctuationOther

                else if e 0x2028 then
                    Just SeparatorLine

                else if e 0x2029 then
                    Just SeparatorParagraph

                else if r 0x202A 0x202E then
                    Just OtherFormat

                else if e 0x202F then
                    Just SeparatorSpace

                else
                    Nothing

            else if l 0x2046 then
                if e 0x203A then
                    Just PunctuationFinalQuote

                else if r 0x203B 0x203E || r 0x2041 0x2043 then
                    Just PunctuationOther

                else if r 0x203F 0x2040 then
                    Just PunctuationConnector

                else if e 0x2044 then
                    Just SymbolMath

                else if e 0x2045 then
                    Just PunctuationOpen

                else
                    Nothing

            else if e 0x2046 then
                Just PunctuationClose

            else if r 0x2047 0x2051 || e 0x2053 || r 0x2055 0x205E then
                Just PunctuationOther

            else if e 0x2052 then
                Just SymbolMath

            else if e 0x2054 then
                Just PunctuationConnector

            else if e 0x205F then
                Just SeparatorSpace

            else if r 0x2060 0x2064 || r 0x2066 0x206E then
                Just OtherFormat

            else
                Nothing

        else if l 0x213F then
            if l 0x210A then
                if l 0x208F then
                    if e 0x206F then
                        Just OtherFormat

                    else if e 0x2070 || r 0x2074 0x2079 || r 0x2080 0x2089 then
                        Just NumberOther

                    else if e 0x2071 || e 0x207F then
                        Just LetterModifier

                    else if r 0x207A 0x207C || r 0x208A 0x208C then
                        Just SymbolMath

                    else if e 0x207D || e 0x208D then
                        Just PunctuationOpen

                    else if e 0x207E || e 0x208E then
                        Just PunctuationClose

                    else
                        Nothing

                else if r 0x2090 0x209C then
                    Just LetterModifier

                else if r 0x20A0 0x20C0 then
                    Just SymbolCurrency

                else if r 0x20D0 0x20DC || e 0x20E1 || r 0x20E5 0x20F0 then
                    Just MarkNonSpacing

                else if r 0x20DD 0x20E0 || r 0x20E2 0x20E4 then
                    Just MarkEnclosing

                else if r 0x2100 0x2101 || r 0x2103 0x2106 || r 0x2108 0x2109 then
                    Just SymbolOther

                else if e 0x2102 || e 0x2107 then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0x2125 then
                if e 0x210A || r 0x210E 0x210F || e 0x2113 then
                    Just LetterLowercase

                else if r 0x210B 0x210D || r 0x2110 0x2112 || e 0x2115 || r 0x2119 0x211D || e 0x2124 then
                    Just LetterUppercase

                else if e 0x2114 || r 0x2116 0x2117 || r 0x211E 0x2123 then
                    Just SymbolOther

                else if e 0x2118 then
                    Just SymbolMath

                else
                    Nothing

            else if l 0x212E then
                if e 0x2125 || e 0x2127 || e 0x2129 then
                    Just SymbolOther

                else if e 0x2126 || e 0x2128 || r 0x212A 0x212D then
                    Just LetterUppercase

                else
                    Nothing

            else if e 0x212E || r 0x213A 0x213B then
                Just SymbolOther

            else if e 0x212F || e 0x2134 || e 0x2139 || r 0x213C 0x213D then
                Just LetterLowercase

            else if r 0x2130 0x2133 || e 0x213E then
                Just LetterUppercase

            else if r 0x2135 0x2138 then
                Just LetterOther

            else
                Nothing

        else if l 0x21AD then
            if l 0x2184 then
                if e 0x213F || e 0x2145 || e 0x2183 then
                    Just LetterUppercase

                else if r 0x2140 0x2144 || e 0x214B then
                    Just SymbolMath

                else if r 0x2146 0x2149 || e 0x214E then
                    Just LetterLowercase

                else if e 0x214A || r 0x214C 0x214D || e 0x214F then
                    Just SymbolOther

                else if r 0x2150 0x215F then
                    Just NumberOther

                else if r 0x2160 0x2182 then
                    Just NumberLetter

                else
                    Nothing

            else if l 0x219B then
                if e 0x2184 then
                    Just LetterLowercase

                else if r 0x2185 0x2188 then
                    Just NumberLetter

                else if e 0x2189 then
                    Just NumberOther

                else if r 0x218A 0x218B || r 0x2195 0x2199 then
                    Just SymbolOther

                else if r 0x2190 0x2194 || e 0x219A then
                    Just SymbolMath

                else
                    Nothing

            else if e 0x219B || e 0x21A0 || e 0x21A3 || e 0x21A6 then
                Just SymbolMath

            else if r 0x219C 0x219F || r 0x21A1 0x21A2 || r 0x21A4 0x21A5 || r 0x21A7 0x21AC then
                Just SymbolOther

            else
                Nothing

        else if l 0x230A then
            if l 0x21D2 then
                if e 0x21AD || r 0x21AF 0x21CD || r 0x21D0 0x21D1 then
                    Just SymbolOther

                else if e 0x21AE || r 0x21CE 0x21CF then
                    Just SymbolMath

                else
                    Nothing

            else if e 0x21D2 || e 0x21D4 || r 0x21F4 0x22FF then
                Just SymbolMath

            else if e 0x21D3 || r 0x21D5 0x21F3 || r 0x2300 0x2307 then
                Just SymbolOther

            else if e 0x2308 then
                Just PunctuationOpen

            else if e 0x2309 then
                Just PunctuationClose

            else
                Nothing

        else if l 0x232A then
            if e 0x230A || e 0x2329 then
                Just PunctuationOpen

            else if e 0x230B then
                Just PunctuationClose

            else if r 0x230C 0x231F || r 0x2322 0x2328 then
                Just SymbolOther

            else if r 0x2320 0x2321 then
                Just SymbolMath

            else
                Nothing

        else if e 0x232A then
            Just PunctuationClose

        else if r 0x232B 0x237B || r 0x237D 0x239A || r 0x23B4 0x23DB || r 0x23E2 0x2426 || r 0x2440 0x244A then
            Just SymbolOther

        else if e 0x237C || r 0x239B 0x23B3 || r 0x23DC 0x23E1 then
            Just SymbolMath

        else
            Nothing

    else if l 0xFE5A then
        if l 0xA662 then
            if l 0x2CE2 then
                if l 0x2C75 then
                    if l 0x2986 then
                        if l 0x2773 then
                            if l 0x266F then
                                if r 0x2460 0x249B || r 0x24EA 0x24FF then
                                    Just NumberOther

                                else if r 0x249C 0x24E9 || r 0x2500 0x25B6 || r 0x25B8 0x25C0 || r 0x25C2 0x25F7 || r 0x2600 0x266E then
                                    Just SymbolOther

                                else if e 0x25B7 || e 0x25C1 || r 0x25F8 0x25FF then
                                    Just SymbolMath

                                else
                                    Nothing

                            else if l 0x276B then
                                if e 0x266F then
                                    Just SymbolMath

                                else if r 0x2670 0x2767 then
                                    Just SymbolOther

                                else if e 0x2768 || e 0x276A then
                                    Just PunctuationOpen

                                else if e 0x2769 then
                                    Just PunctuationClose

                                else
                                    Nothing

                            else if e 0x276B || e 0x276D || e 0x276F || e 0x2771 then
                                Just PunctuationClose

                            else if e 0x276C || e 0x276E || e 0x2770 || e 0x2772 then
                                Just PunctuationOpen

                            else
                                Nothing

                        else if l 0x27E8 then
                            if e 0x2773 || e 0x2775 || e 0x27C6 || e 0x27E7 then
                                Just PunctuationClose

                            else if e 0x2774 || e 0x27C5 || e 0x27E6 then
                                Just PunctuationOpen

                            else if r 0x2776 0x2793 then
                                Just NumberOther

                            else if r 0x2794 0x27BF then
                                Just SymbolOther

                            else if r 0x27C0 0x27C4 || r 0x27C7 0x27E5 then
                                Just SymbolMath

                            else
                                Nothing

                        else if l 0x27EE then
                            if e 0x27E8 || e 0x27EA || e 0x27EC then
                                Just PunctuationOpen

                            else if e 0x27E9 || e 0x27EB || e 0x27ED then
                                Just PunctuationClose

                            else
                                Nothing

                        else if e 0x27EE || e 0x2983 || e 0x2985 then
                            Just PunctuationOpen

                        else if e 0x27EF || e 0x2984 then
                            Just PunctuationClose

                        else if r 0x27F0 0x27FF || r 0x2900 0x2982 then
                            Just SymbolMath

                        else if r 0x2800 0x28FF then
                            Just SymbolOther

                        else
                            Nothing

                    else if l 0x29FB then
                        if l 0x2991 then
                            if e 0x2986 || e 0x2988 || e 0x298A || e 0x298C || e 0x298E || e 0x2990 then
                                Just PunctuationClose

                            else if e 0x2987 || e 0x2989 || e 0x298B || e 0x298D || e 0x298F then
                                Just PunctuationOpen

                            else
                                Nothing

                        else if l 0x2997 then
                            if e 0x2991 || e 0x2993 || e 0x2995 then
                                Just PunctuationOpen

                            else if e 0x2992 || e 0x2994 || e 0x2996 then
                                Just PunctuationClose

                            else
                                Nothing

                        else if e 0x2997 || e 0x29D8 || e 0x29DA then
                            Just PunctuationOpen

                        else if e 0x2998 || e 0x29D9 || e 0x29DB then
                            Just PunctuationClose

                        else if r 0x2999 0x29D7 || r 0x29DC 0x29FA then
                            Just SymbolMath

                        else
                            Nothing

                    else if l 0x2C5F then
                        if l 0x2B44 then
                            if e 0x29FB || r 0x29FE 0x2AFF || r 0x2B30 0x2B43 then
                                Just SymbolMath

                            else if e 0x29FC then
                                Just PunctuationOpen

                            else if e 0x29FD then
                                Just PunctuationClose

                            else if r 0x2B00 0x2B2F then
                                Just SymbolOther

                            else
                                Nothing

                        else if e 0x2B44 || r 0x2B47 0x2B4C then
                            Just SymbolMath

                        else if r 0x2B45 0x2B46 || r 0x2B4D 0x2B73 || r 0x2B76 0x2B95 || r 0x2B97 0x2BFF then
                            Just SymbolOther

                        else if r 0x2C00 0x2C2F then
                            Just LetterUppercase

                        else if r 0x2C30 0x2C5E then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x2C68 then
                        if e 0x2C5F || e 0x2C61 || r 0x2C65 0x2C66 then
                            Just LetterLowercase

                        else if e 0x2C60 || r 0x2C62 0x2C64 || e 0x2C67 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x2C68 || e 0x2C6A || e 0x2C6C || e 0x2C71 || r 0x2C73 0x2C74 then
                        Just LetterLowercase

                    else if e 0x2C69 || e 0x2C6B || r 0x2C6D 0x2C70 || e 0x2C72 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x2CAE then
                    if l 0x2C94 then
                        if l 0x2C87 then
                            if e 0x2C75 || r 0x2C7E 0x2C80 || e 0x2C82 || e 0x2C84 || e 0x2C86 then
                                Just LetterUppercase

                            else if r 0x2C76 0x2C7B || e 0x2C81 || e 0x2C83 || e 0x2C85 then
                                Just LetterLowercase

                            else if r 0x2C7C 0x2C7D then
                                Just LetterModifier

                            else
                                Nothing

                        else if l 0x2C8C then
                            if e 0x2C87 || e 0x2C89 || e 0x2C8B then
                                Just LetterLowercase

                            else if e 0x2C88 || e 0x2C8A then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0x2C8C || e 0x2C8E || e 0x2C90 || e 0x2C92 then
                            Just LetterUppercase

                        else if e 0x2C8D || e 0x2C8F || e 0x2C91 || e 0x2C93 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x2CA0 then
                        if e 0x2C94 || e 0x2C96 || e 0x2C98 || e 0x2C9A || e 0x2C9C || e 0x2C9E then
                            Just LetterUppercase

                        else if e 0x2C95 || e 0x2C97 || e 0x2C99 || e 0x2C9B || e 0x2C9D || e 0x2C9F then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x2CA6 then
                        if e 0x2CA0 || e 0x2CA2 || e 0x2CA4 then
                            Just LetterUppercase

                        else if e 0x2CA1 || e 0x2CA3 || e 0x2CA5 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0x2CA6 || e 0x2CA8 || e 0x2CAA || e 0x2CAC then
                        Just LetterUppercase

                    else if e 0x2CA7 || e 0x2CA9 || e 0x2CAB || e 0x2CAD then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x2CC7 then
                    if l 0x2CB9 then
                        if e 0x2CAE || e 0x2CB0 || e 0x2CB2 || e 0x2CB4 || e 0x2CB6 || e 0x2CB8 then
                            Just LetterUppercase

                        else if e 0x2CAF || e 0x2CB1 || e 0x2CB3 || e 0x2CB5 || e 0x2CB7 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0x2CBF then
                        if e 0x2CB9 || e 0x2CBB || e 0x2CBD then
                            Just LetterLowercase

                        else if e 0x2CBA || e 0x2CBC || e 0x2CBE then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0x2CBF || e 0x2CC1 || e 0x2CC3 || e 0x2CC5 then
                        Just LetterLowercase

                    else if e 0x2CC0 || e 0x2CC2 || e 0x2CC4 || e 0x2CC6 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x2CD3 then
                    if e 0x2CC7 || e 0x2CC9 || e 0x2CCB || e 0x2CCD || e 0x2CCF || e 0x2CD1 then
                        Just LetterLowercase

                    else if e 0x2CC8 || e 0x2CCA || e 0x2CCC || e 0x2CCE || e 0x2CD0 || e 0x2CD2 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0x2CD9 then
                    if e 0x2CD3 || e 0x2CD5 || e 0x2CD7 then
                        Just LetterLowercase

                    else if e 0x2CD4 || e 0x2CD6 || e 0x2CD8 then
                        Just LetterUppercase

                    else
                        Nothing

                else if e 0x2CD9 || e 0x2CDB || e 0x2CDD || e 0x2CDF || e 0x2CE1 then
                    Just LetterLowercase

                else if e 0x2CDA || e 0x2CDC || e 0x2CDE || e 0x2CE0 then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0x3014 then
                if l 0x2E20 then
                    if l 0x2DB7 then
                        if l 0x2CFD then
                            if e 0x2CE2 || e 0x2CEB || e 0x2CED || e 0x2CF2 then
                                Just LetterUppercase

                            else if r 0x2CE3 0x2CE4 || e 0x2CEC || e 0x2CEE || e 0x2CF3 then
                                Just LetterLowercase

                            else if r 0x2CE5 0x2CEA then
                                Just SymbolOther

                            else if r 0x2CEF 0x2CF1 then
                                Just MarkNonSpacing

                            else if r 0x2CF9 0x2CFC then
                                Just PunctuationOther

                            else
                                Nothing

                        else if l 0x2D6E then
                            if e 0x2CFD then
                                Just NumberOther

                            else if r 0x2CFE 0x2CFF then
                                Just PunctuationOther

                            else if r 0x2D00 0x2D25 || e 0x2D27 || e 0x2D2D then
                                Just LetterLowercase

                            else if r 0x2D30 0x2D67 then
                                Just LetterOther

                            else
                                Nothing

                        else if e 0x2D6F then
                            Just LetterModifier

                        else if e 0x2D70 then
                            Just PunctuationOther

                        else if e 0x2D7F then
                            Just MarkNonSpacing

                        else if r 0x2D80 0x2D96 || r 0x2DA0 0x2DA6 || r 0x2DA8 0x2DAE || r 0x2DB0 0x2DB6 then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0x2E08 then
                        if r 0x2DB8 0x2DBE || r 0x2DC0 0x2DC6 || r 0x2DC8 0x2DCE || r 0x2DD0 0x2DD6 || r 0x2DD8 0x2DDE then
                            Just LetterOther

                        else if r 0x2DE0 0x2DFF then
                            Just MarkNonSpacing

                        else if r 0x2E00 0x2E01 || r 0x2E06 0x2E07 then
                            Just PunctuationOther

                        else if e 0x2E02 || e 0x2E04 then
                            Just PunctuationInitialQuote

                        else if e 0x2E03 || e 0x2E05 then
                            Just PunctuationFinalQuote

                        else
                            Nothing

                    else if l 0x2E16 then
                        if e 0x2E08 || e 0x2E0B || r 0x2E0E 0x2E15 then
                            Just PunctuationOther

                        else if e 0x2E09 || e 0x2E0C then
                            Just PunctuationInitialQuote

                        else if e 0x2E0A || e 0x2E0D then
                            Just PunctuationFinalQuote

                        else
                            Nothing

                    else if e 0x2E16 || r 0x2E18 0x2E19 || e 0x2E1B || r 0x2E1E 0x2E1F then
                        Just PunctuationOther

                    else if e 0x2E17 || e 0x2E1A then
                        Just PunctuationDash

                    else if e 0x2E1C then
                        Just PunctuationInitialQuote

                    else if e 0x2E1D then
                        Just PunctuationFinalQuote

                    else
                        Nothing

                else if l 0x2E58 then
                    if l 0x2E2F then
                        if e 0x2E20 then
                            Just PunctuationInitialQuote

                        else if e 0x2E21 then
                            Just PunctuationFinalQuote

                        else if e 0x2E22 || e 0x2E24 || e 0x2E26 || e 0x2E28 then
                            Just PunctuationOpen

                        else if e 0x2E23 || e 0x2E25 || e 0x2E27 || e 0x2E29 then
                            Just PunctuationClose

                        else if r 0x2E2A 0x2E2E then
                            Just PunctuationOther

                        else
                            Nothing

                    else if l 0x2E41 then
                        if e 0x2E2F then
                            Just LetterModifier

                        else if r 0x2E30 0x2E39 || r 0x2E3C 0x2E3F then
                            Just PunctuationOther

                        else if r 0x2E3A 0x2E3B || e 0x2E40 then
                            Just PunctuationDash

                        else
                            Nothing

                    else if e 0x2E41 || r 0x2E43 0x2E4F || r 0x2E52 0x2E54 then
                        Just PunctuationOther

                    else if e 0x2E42 || e 0x2E55 || e 0x2E57 then
                        Just PunctuationOpen

                    else if r 0x2E50 0x2E51 then
                        Just SymbolOther

                    else if e 0x2E56 then
                        Just PunctuationClose

                    else
                        Nothing

                else if l 0x3004 then
                    if e 0x2E58 || e 0x2E5A || e 0x2E5C then
                        Just PunctuationClose

                    else if e 0x2E59 || e 0x2E5B then
                        Just PunctuationOpen

                    else if e 0x2E5D then
                        Just PunctuationDash

                    else if r 0x2E80 0x2E99 || r 0x2E9B 0x2EF3 || r 0x2F00 0x2FD5 || r 0x2FF0 0x2FFB then
                        Just SymbolOther

                    else if e 0x3000 then
                        Just SeparatorSpace

                    else if r 0x3001 0x3003 then
                        Just PunctuationOther

                    else
                        Nothing

                else if l 0x300A then
                    if e 0x3004 then
                        Just SymbolOther

                    else if e 0x3005 then
                        Just LetterModifier

                    else if e 0x3006 then
                        Just LetterOther

                    else if e 0x3007 then
                        Just NumberLetter

                    else if e 0x3008 then
                        Just PunctuationOpen

                    else if e 0x3009 then
                        Just PunctuationClose

                    else
                        Nothing

                else if e 0x300A || e 0x300C || e 0x300E || e 0x3010 then
                    Just PunctuationOpen

                else if e 0x300B || e 0x300D || e 0x300F || e 0x3011 then
                    Just PunctuationClose

                else if r 0x3012 0x3013 then
                    Just SymbolOther

                else
                    Nothing

            else if l 0x32BF then
                if l 0x309A then
                    if l 0x3020 then
                        if e 0x3014 || e 0x3016 || e 0x3018 || e 0x301A || e 0x301D then
                            Just PunctuationOpen

                        else if e 0x3015 || e 0x3017 || e 0x3019 || e 0x301B || r 0x301E 0x301F then
                            Just PunctuationClose

                        else if e 0x301C then
                            Just PunctuationDash

                        else
                            Nothing

                    else if l 0x3037 then
                        if e 0x3020 || e 0x3036 then
                            Just SymbolOther

                        else if r 0x3021 0x3029 then
                            Just NumberLetter

                        else if r 0x302A 0x302D then
                            Just MarkNonSpacing

                        else if r 0x302E 0x302F then
                            Just MarkSpacingCombining

                        else if e 0x3030 then
                            Just PunctuationDash

                        else if r 0x3031 0x3035 then
                            Just LetterModifier

                        else
                            Nothing

                    else if e 0x3037 || r 0x303E 0x303F then
                        Just SymbolOther

                    else if r 0x3038 0x303A then
                        Just NumberLetter

                    else if e 0x303B then
                        Just LetterModifier

                    else if e 0x303C || r 0x3041 0x3096 then
                        Just LetterOther

                    else if e 0x303D then
                        Just PunctuationOther

                    else if e 0x3099 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x3195 then
                    if l 0x30FA then
                        if e 0x309A then
                            Just MarkNonSpacing

                        else if r 0x309B 0x309C then
                            Just SymbolModifier

                        else if r 0x309D 0x309E then
                            Just LetterModifier

                        else if e 0x309F || r 0x30A1 0x30F9 then
                            Just LetterOther

                        else if e 0x30A0 then
                            Just PunctuationDash

                        else
                            Nothing

                    else if e 0x30FA || e 0x30FF || r 0x3105 0x312F || r 0x3131 0x318E then
                        Just LetterOther

                    else if e 0x30FB then
                        Just PunctuationOther

                    else if r 0x30FC 0x30FE then
                        Just LetterModifier

                    else if r 0x3190 0x3191 then
                        Just SymbolOther

                    else if r 0x3192 0x3194 then
                        Just NumberOther

                    else
                        Nothing

                else if l 0x3229 then
                    if e 0x3195 || r 0x3220 0x3228 then
                        Just NumberOther

                    else if r 0x3196 0x319F || r 0x31C0 0x31E3 || r 0x3200 0x321E then
                        Just SymbolOther

                    else if r 0x31A0 0x31BF || r 0x31F0 0x31FF then
                        Just LetterOther

                    else
                        Nothing

                else if e 0x3229 || r 0x3248 0x324F || r 0x3251 0x325F || r 0x3280 0x3289 || r 0x32B1 0x32BE then
                    Just NumberOther

                else if r 0x322A 0x3247 || e 0x3250 || r 0x3260 0x327F || r 0x328A 0x32B0 then
                    Just SymbolOther

                else
                    Nothing

            else if l 0xA646 then
                if l 0xA4FD then
                    if e 0x32BF then
                        Just NumberOther

                    else if r 0x32C0 0x33FF || r 0x4DC0 0x4DFF || r 0xA490 0xA4C6 then
                        Just SymbolOther

                    else if e 0x3400 || e 0x4DBF || e 0x4E00 || r 0x9FFF 0xA014 || r 0xA016 0xA48C || r 0xA4D0 0xA4F7 then
                        Just LetterOther

                    else if e 0xA015 || r 0xA4F8 0xA4FC then
                        Just LetterModifier

                    else
                        Nothing

                else if l 0xA629 then
                    if e 0xA4FD || e 0xA60C then
                        Just LetterModifier

                    else if r 0xA4FE 0xA4FF || r 0xA60D 0xA60F then
                        Just PunctuationOther

                    else if r 0xA500 0xA60B || r 0xA610 0xA61F then
                        Just LetterOther

                    else if r 0xA620 0xA628 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if e 0xA629 then
                    Just NumberDecimalDigit

                else if r 0xA62A 0xA62B then
                    Just LetterOther

                else if e 0xA640 || e 0xA642 || e 0xA644 then
                    Just LetterUppercase

                else if e 0xA641 || e 0xA643 || e 0xA645 then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0xA653 then
                if l 0xA64B then
                    if e 0xA646 || e 0xA648 || e 0xA64A then
                        Just LetterUppercase

                    else if e 0xA647 || e 0xA649 then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0xA64B || e 0xA64D || e 0xA64F || e 0xA651 then
                    Just LetterLowercase

                else if e 0xA64C || e 0xA64E || e 0xA650 || e 0xA652 then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0xA659 then
                if e 0xA653 || e 0xA655 || e 0xA657 then
                    Just LetterLowercase

                else if e 0xA654 || e 0xA656 || e 0xA658 then
                    Just LetterUppercase

                else
                    Nothing

            else if e 0xA659 || e 0xA65B || e 0xA65D || e 0xA65F || e 0xA661 then
                Just LetterLowercase

            else if e 0xA65A || e 0xA65C || e 0xA65E || e 0xA660 then
                Just LetterUppercase

            else
                Nothing

        else if l 0xA7D5 then
            if l 0xA750 then
                if l 0xA6E5 then
                    if l 0xA685 then
                        if l 0xA66D then
                            if e 0xA662 || e 0xA664 || e 0xA666 || e 0xA668 || e 0xA66A || e 0xA66C then
                                Just LetterUppercase

                            else if e 0xA663 || e 0xA665 || e 0xA667 || e 0xA669 || e 0xA66B then
                                Just LetterLowercase

                            else
                                Nothing

                        else if l 0xA67D then
                            if e 0xA66D then
                                Just LetterLowercase

                            else if e 0xA66E then
                                Just LetterOther

                            else if e 0xA66F || r 0xA674 0xA67C then
                                Just MarkNonSpacing

                            else if r 0xA670 0xA672 then
                                Just MarkEnclosing

                            else if e 0xA673 then
                                Just PunctuationOther

                            else
                                Nothing

                        else if e 0xA67D then
                            Just MarkNonSpacing

                        else if e 0xA67E then
                            Just PunctuationOther

                        else if e 0xA67F then
                            Just LetterModifier

                        else if e 0xA680 || e 0xA682 || e 0xA684 then
                            Just LetterUppercase

                        else if e 0xA681 || e 0xA683 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0xA691 then
                        if e 0xA685 || e 0xA687 || e 0xA689 || e 0xA68B || e 0xA68D || e 0xA68F then
                            Just LetterLowercase

                        else if e 0xA686 || e 0xA688 || e 0xA68A || e 0xA68C || e 0xA68E || e 0xA690 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if l 0xA697 then
                        if e 0xA691 || e 0xA693 || e 0xA695 then
                            Just LetterLowercase

                        else if e 0xA692 || e 0xA694 || e 0xA696 then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0xA697 || e 0xA699 || e 0xA69B then
                        Just LetterLowercase

                    else if e 0xA698 || e 0xA69A then
                        Just LetterUppercase

                    else if r 0xA69C 0xA69D then
                        Just LetterModifier

                    else if r 0xA69E 0xA69F then
                        Just MarkNonSpacing

                    else if r 0xA6A0 0xA6E4 then
                        Just LetterOther

                    else
                        Nothing

                else if l 0xA735 then
                    if l 0xA726 then
                        if e 0xA6E5 then
                            Just LetterOther

                        else if r 0xA6E6 0xA6EF then
                            Just NumberLetter

                        else if r 0xA6F0 0xA6F1 then
                            Just MarkNonSpacing

                        else if r 0xA6F2 0xA6F7 then
                            Just PunctuationOther

                        else if r 0xA700 0xA716 || r 0xA720 0xA721 then
                            Just SymbolModifier

                        else if r 0xA717 0xA71F then
                            Just LetterModifier

                        else if e 0xA722 || e 0xA724 then
                            Just LetterUppercase

                        else if e 0xA723 || e 0xA725 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0xA72B then
                        if e 0xA726 || e 0xA728 || e 0xA72A then
                            Just LetterUppercase

                        else if e 0xA727 || e 0xA729 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if e 0xA72B || e 0xA72D || r 0xA72F 0xA731 || e 0xA733 then
                        Just LetterLowercase

                    else if e 0xA72C || e 0xA72E || e 0xA732 || e 0xA734 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0xA741 then
                    if e 0xA735 || e 0xA737 || e 0xA739 || e 0xA73B || e 0xA73D || e 0xA73F then
                        Just LetterLowercase

                    else if e 0xA736 || e 0xA738 || e 0xA73A || e 0xA73C || e 0xA73E || e 0xA740 then
                        Just LetterUppercase

                    else
                        Nothing

                else if l 0xA747 then
                    if e 0xA741 || e 0xA743 || e 0xA745 then
                        Just LetterLowercase

                    else if e 0xA742 || e 0xA744 || e 0xA746 then
                        Just LetterUppercase

                    else
                        Nothing

                else if e 0xA747 || e 0xA749 || e 0xA74B || e 0xA74D || e 0xA74F then
                    Just LetterLowercase

                else if e 0xA748 || e 0xA74A || e 0xA74C || e 0xA74E then
                    Just LetterUppercase

                else
                    Nothing

            else if l 0xA78B then
                if l 0xA768 then
                    if l 0xA75B then
                        if e 0xA750 || e 0xA752 || e 0xA754 || e 0xA756 || e 0xA758 || e 0xA75A then
                            Just LetterUppercase

                        else if e 0xA751 || e 0xA753 || e 0xA755 || e 0xA757 || e 0xA759 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0xA760 then
                        if e 0xA75B || e 0xA75D || e 0xA75F then
                            Just LetterLowercase

                        else if e 0xA75C || e 0xA75E then
                            Just LetterUppercase

                        else
                            Nothing

                    else if e 0xA760 || e 0xA762 || e 0xA764 || e 0xA766 then
                        Just LetterUppercase

                    else if e 0xA761 || e 0xA763 || e 0xA765 || e 0xA767 then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0xA77B then
                    if e 0xA768 || e 0xA76A || e 0xA76C || e 0xA76E || e 0xA779 then
                        Just LetterUppercase

                    else if e 0xA769 || e 0xA76B || e 0xA76D || e 0xA76F || r 0xA771 0xA778 || e 0xA77A then
                        Just LetterLowercase

                    else if e 0xA770 then
                        Just LetterModifier

                    else
                        Nothing

                else if l 0xA782 then
                    if e 0xA77B || r 0xA77D 0xA77E || e 0xA780 then
                        Just LetterUppercase

                    else if e 0xA77C || e 0xA77F || e 0xA781 then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0xA782 || e 0xA784 || e 0xA786 then
                    Just LetterUppercase

                else if e 0xA783 || e 0xA785 || e 0xA787 then
                    Just LetterLowercase

                else if e 0xA788 then
                    Just LetterModifier

                else if r 0xA789 0xA78A then
                    Just SymbolModifier

                else
                    Nothing

            else if l 0xA7A6 then
                if l 0xA798 then
                    if e 0xA78B || e 0xA78D || e 0xA790 || e 0xA792 || e 0xA796 then
                        Just LetterUppercase

                    else if e 0xA78C || e 0xA78E || e 0xA791 || r 0xA793 0xA795 || e 0xA797 then
                        Just LetterLowercase

                    else if e 0xA78F then
                        Just LetterOther

                    else
                        Nothing

                else if l 0xA79E then
                    if e 0xA798 || e 0xA79A || e 0xA79C then
                        Just LetterUppercase

                    else if e 0xA799 || e 0xA79B || e 0xA79D then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0xA79E || e 0xA7A0 || e 0xA7A2 || e 0xA7A4 then
                    Just LetterUppercase

                else if e 0xA79F || e 0xA7A1 || e 0xA7A3 || e 0xA7A5 then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0xA7BA then
                if e 0xA7A6 || e 0xA7A8 || r 0xA7AA 0xA7AE || r 0xA7B0 0xA7B4 || e 0xA7B6 || e 0xA7B8 then
                    Just LetterUppercase

                else if e 0xA7A7 || e 0xA7A9 || e 0xA7AF || e 0xA7B5 || e 0xA7B7 || e 0xA7B9 then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0xA7C0 then
                if e 0xA7BA || e 0xA7BC || e 0xA7BE then
                    Just LetterUppercase

                else if e 0xA7BB || e 0xA7BD || e 0xA7BF then
                    Just LetterLowercase

                else
                    Nothing

            else if e 0xA7C0 || e 0xA7C2 || r 0xA7C4 0xA7C7 || e 0xA7C9 || e 0xA7D0 then
                Just LetterUppercase

            else if e 0xA7C1 || e 0xA7C3 || e 0xA7C8 || e 0xA7CA || ((modBy 2 code == 1) && r 0xA7D1 0xA7D4) then
                Just LetterLowercase

            else
                Nothing

        else if l 0xAADA then
            if l 0xA97F then
                if l 0xA838 then
                    if l 0xA802 then
                        if e 0xA7D6 || e 0xA7D8 || e 0xA7F5 then
                            Just LetterUppercase

                        else if e 0xA7D7 || e 0xA7D9 || e 0xA7F6 || e 0xA7FA || ((modBy 2 code == 1) && e 0xA7D5) then
                            Just LetterLowercase

                        else if r 0xA7F2 0xA7F4 || r 0xA7F8 0xA7F9 then
                            Just LetterModifier

                        else if e 0xA7F7 || r 0xA7FB 0xA801 then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0xA822 then
                        if e 0xA802 || e 0xA806 || e 0xA80B then
                            Just MarkNonSpacing

                        else if r 0xA803 0xA805 || r 0xA807 0xA80A || r 0xA80C 0xA821 then
                            Just LetterOther

                        else
                            Nothing

                    else if e 0xA822 then
                        Just LetterOther

                    else if r 0xA823 0xA824 || e 0xA827 then
                        Just MarkSpacingCombining

                    else if r 0xA825 0xA826 || e 0xA82C then
                        Just MarkNonSpacing

                    else if r 0xA828 0xA82B || r 0xA836 0xA837 then
                        Just SymbolOther

                    else if r 0xA830 0xA835 then
                        Just NumberOther

                    else
                        Nothing

                else if l 0xA8FA then
                    if l 0xA8B3 then
                        if e 0xA838 then
                            Just SymbolCurrency

                        else if e 0xA839 then
                            Just SymbolOther

                        else if r 0xA840 0xA873 || r 0xA882 0xA8B2 then
                            Just LetterOther

                        else if r 0xA874 0xA877 then
                            Just PunctuationOther

                        else if r 0xA880 0xA881 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if e 0xA8B3 || r 0xA8F2 0xA8F7 then
                        Just LetterOther

                    else if r 0xA8B4 0xA8C3 then
                        Just MarkSpacingCombining

                    else if r 0xA8C4 0xA8C5 || r 0xA8E0 0xA8F1 then
                        Just MarkNonSpacing

                    else if r 0xA8CE 0xA8CF || r 0xA8F8 0xA8F9 then
                        Just PunctuationOther

                    else if r 0xA8D0 0xA8D9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0xA925 then
                    if e 0xA8FA || e 0xA8FC then
                        Just PunctuationOther

                    else if e 0xA8FB || r 0xA8FD 0xA8FE || r 0xA90A 0xA924 then
                        Just LetterOther

                    else if e 0xA8FF then
                        Just MarkNonSpacing

                    else if r 0xA900 0xA909 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if e 0xA925 || r 0xA930 0xA946 || r 0xA960 0xA97C then
                    Just LetterOther

                else if r 0xA926 0xA92D || r 0xA947 0xA951 then
                    Just MarkNonSpacing

                else if r 0xA92E 0xA92F || e 0xA95F then
                    Just PunctuationOther

                else if r 0xA952 0xA953 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if l 0xAA3F then
                if l 0xA9DD then
                    if r 0xA980 0xA982 || e 0xA9B3 || r 0xA9B6 0xA9B9 || r 0xA9BC 0xA9BD then
                        Just MarkNonSpacing

                    else if e 0xA983 || r 0xA9B4 0xA9B5 || r 0xA9BA 0xA9BB || r 0xA9BE 0xA9C0 then
                        Just MarkSpacingCombining

                    else if r 0xA984 0xA9B2 then
                        Just LetterOther

                    else if r 0xA9C1 0xA9CD then
                        Just PunctuationOther

                    else if e 0xA9CF then
                        Just LetterModifier

                    else if r 0xA9D0 0xA9D9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0xA9F9 then
                    if r 0xA9DE 0xA9DF then
                        Just PunctuationOther

                    else if r 0xA9E0 0xA9E4 || r 0xA9E7 0xA9EF then
                        Just LetterOther

                    else if e 0xA9E5 then
                        Just MarkNonSpacing

                    else if e 0xA9E6 then
                        Just LetterModifier

                    else if r 0xA9F0 0xA9F8 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if e 0xA9F9 then
                    Just NumberDecimalDigit

                else if r 0xA9FA 0xA9FE || r 0xAA00 0xAA28 then
                    Just LetterOther

                else if r 0xAA29 0xAA2E || r 0xAA31 0xAA32 || r 0xAA35 0xAA36 then
                    Just MarkNonSpacing

                else if r 0xAA2F 0xAA30 || r 0xAA33 0xAA34 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if l 0xAA7B then
                if r 0xAA40 0xAA42 || r 0xAA44 0xAA4B || r 0xAA60 0xAA6F || r 0xAA71 0xAA76 || e 0xAA7A then
                    Just LetterOther

                else if e 0xAA43 || e 0xAA4C then
                    Just MarkNonSpacing

                else if e 0xAA4D then
                    Just MarkSpacingCombining

                else if r 0xAA50 0xAA59 then
                    Just NumberDecimalDigit

                else if r 0xAA5C 0xAA5F then
                    Just PunctuationOther

                else if e 0xAA70 then
                    Just LetterModifier

                else if r 0xAA77 0xAA79 then
                    Just SymbolOther

                else
                    Nothing

            else if l 0xAAB4 then
                if e 0xAA7B || e 0xAA7D then
                    Just MarkSpacingCombining

                else if e 0xAA7C || e 0xAAB0 || r 0xAAB2 0xAAB3 then
                    Just MarkNonSpacing

                else if r 0xAA7E 0xAAAF || e 0xAAB1 then
                    Just LetterOther

                else
                    Nothing

            else if e 0xAAB4 || r 0xAAB7 0xAAB8 || r 0xAABE 0xAABF || e 0xAAC1 then
                Just MarkNonSpacing

            else if r 0xAAB5 0xAAB6 || r 0xAAB9 0xAABD || e 0xAAC0 || e 0xAAC2 then
                Just LetterOther

            else
                Nothing

        else if l 0xFB29 then
            if l 0xABE2 then
                if l 0xAB00 then
                    if r 0xAADB 0xAADC || r 0xAAE0 0xAAEA || e 0xAAF2 then
                        Just LetterOther

                    else if e 0xAADD || r 0xAAF3 0xAAF4 then
                        Just LetterModifier

                    else if r 0xAADE 0xAADF || r 0xAAF0 0xAAF1 then
                        Just PunctuationOther

                    else if e 0xAAEB || r 0xAAEE 0xAAEF || e 0xAAF5 then
                        Just MarkSpacingCombining

                    else if r 0xAAEC 0xAAED || e 0xAAF6 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0xAB5A then
                    if r 0xAB01 0xAB06 || r 0xAB09 0xAB0E || r 0xAB11 0xAB16 || r 0xAB20 0xAB26 || r 0xAB28 0xAB2E then
                        Just LetterOther

                    else if r 0xAB30 0xAB59 then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0xAB5A || r 0xAB60 0xAB68 || r 0xAB70 0xABBF then
                    Just LetterLowercase

                else if e 0xAB5B || r 0xAB6A 0xAB6B then
                    Just SymbolModifier

                else if r 0xAB5C 0xAB5F || e 0xAB69 then
                    Just LetterModifier

                else if r 0xABC0 0xABE1 then
                    Just LetterOther

                else
                    Nothing

            else if l 0xD7CA then
                if l 0xABEA then
                    if e 0xABE2 then
                        Just LetterOther

                    else if r 0xABE3 0xABE4 || r 0xABE6 0xABE7 || e 0xABE9 then
                        Just MarkSpacingCombining

                    else if e 0xABE5 || e 0xABE8 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if e 0xABEA || e 0xABEC then
                    Just MarkSpacingCombining

                else if e 0xABEB then
                    Just PunctuationOther

                else if e 0xABED then
                    Just MarkNonSpacing

                else if r 0xABF0 0xABF9 then
                    Just NumberDecimalDigit

                else if e 0xAC00 || e 0xD7A3 || r 0xD7B0 0xD7C6 then
                    Just LetterOther

                else
                    Nothing

            else if l 0xF8FF then
                if r 0xD7CB 0xD7FB then
                    Just LetterOther

                else if e 0xD800 || r 0xDB7F 0xDB80 || r 0xDBFF 0xDC00 || e 0xDFFF then
                    Just OtherSurrogate

                else if r 0xE000 0xF8FE then
                    Just OtherPrivateUse

                else
                    Nothing

            else if e 0xF8FF then
                Just OtherPrivateUse

            else if r 0xF900 0xFA6D || r 0xFA70 0xFAD9 || e 0xFB1D || r 0xFB1F 0xFB28 then
                Just LetterOther

            else if r 0xFB00 0xFB06 || r 0xFB13 0xFB17 then
                Just LetterLowercase

            else if e 0xFB1E then
                Just MarkNonSpacing

            else
                Nothing

        else if l 0xFE32 then
            if l 0xFD91 then
                if l 0xFB45 then
                    if e 0xFB29 then
                        Just SymbolMath

                    else if r 0xFB2A 0xFB36 || r 0xFB38 0xFB3C || e 0xFB3E || r 0xFB40 0xFB41 || r 0xFB43 0xFB44 then
                        Just LetterOther

                    else
                        Nothing

                else if r 0xFB46 0xFBB1 || r 0xFBD3 0xFD3D || r 0xFD50 0xFD8F then
                    Just LetterOther

                else if r 0xFBB2 0xFBC2 then
                    Just SymbolModifier

                else if e 0xFD3E then
                    Just PunctuationClose

                else if e 0xFD3F then
                    Just PunctuationOpen

                else if r 0xFD40 0xFD4F then
                    Just SymbolOther

                else
                    Nothing

            else if l 0xFE0F then
                if r 0xFD92 0xFDC7 || r 0xFDF0 0xFDFB then
                    Just LetterOther

                else if e 0xFDCF || r 0xFDFD 0xFDFF then
                    Just SymbolOther

                else if e 0xFDFC then
                    Just SymbolCurrency

                else if r 0xFE00 0xFE0E then
                    Just MarkNonSpacing

                else
                    Nothing

            else if e 0xFE0F || r 0xFE20 0xFE2F then
                Just MarkNonSpacing

            else if r 0xFE10 0xFE16 || e 0xFE19 || e 0xFE30 then
                Just PunctuationOther

            else if e 0xFE17 then
                Just PunctuationOpen

            else if e 0xFE18 then
                Just PunctuationClose

            else if e 0xFE31 then
                Just PunctuationDash

            else
                Nothing

        else if l 0xFE3F then
            if e 0xFE32 then
                Just PunctuationDash

            else if r 0xFE33 0xFE34 then
                Just PunctuationConnector

            else if e 0xFE35 || e 0xFE37 || e 0xFE39 || e 0xFE3B || e 0xFE3D then
                Just PunctuationOpen

            else if e 0xFE36 || e 0xFE38 || e 0xFE3A || e 0xFE3C || e 0xFE3E then
                Just PunctuationClose

            else
                Nothing

        else if l 0xFE46 then
            if e 0xFE3F || e 0xFE41 || e 0xFE43 then
                Just PunctuationOpen

            else if e 0xFE40 || e 0xFE42 || e 0xFE44 then
                Just PunctuationClose

            else if e 0xFE45 then
                Just PunctuationOther

            else
                Nothing

        else if e 0xFE46 || r 0xFE49 0xFE4C || r 0xFE50 0xFE52 || r 0xFE54 0xFE57 then
            Just PunctuationOther

        else if e 0xFE47 || e 0xFE59 then
            Just PunctuationOpen

        else if e 0xFE48 then
            Just PunctuationClose

        else if r 0xFE4D 0xFE4F then
            Just PunctuationConnector

        else if e 0xFE58 then
            Just PunctuationDash

        else
            Nothing

    else if l 0x00011942 then
        if l 0x00010F54 then
            if l 0x000103CF then
                if l 0xFF9D then
                    if l 0xFF0F then
                        if l 0xFE6F then
                            if e 0xFE5A || e 0xFE5C || e 0xFE5E then
                                Just PunctuationClose

                            else if e 0xFE5B || e 0xFE5D then
                                Just PunctuationOpen

                            else if r 0xFE5F 0xFE61 || e 0xFE68 || r 0xFE6A 0xFE6B then
                                Just PunctuationOther

                            else if e 0xFE62 || r 0xFE64 0xFE66 then
                                Just SymbolMath

                            else if e 0xFE63 then
                                Just PunctuationDash

                            else if e 0xFE69 then
                                Just SymbolCurrency

                            else
                                Nothing

                        else if l 0xFF07 then
                            if r 0xFE70 0xFE74 || r 0xFE76 0xFEFC then
                                Just LetterOther

                            else if e 0xFEFF then
                                Just OtherFormat

                            else if r 0xFF01 0xFF03 || r 0xFF05 0xFF06 then
                                Just PunctuationOther

                            else if e 0xFF04 then
                                Just SymbolCurrency

                            else
                                Nothing

                        else if e 0xFF07 || e 0xFF0A || e 0xFF0C || e 0xFF0E then
                            Just PunctuationOther

                        else if e 0xFF08 then
                            Just PunctuationOpen

                        else if e 0xFF09 then
                            Just PunctuationClose

                        else if e 0xFF0B then
                            Just SymbolMath

                        else if e 0xFF0D then
                            Just PunctuationDash

                        else
                            Nothing

                    else if l 0xFF5A then
                        if l 0xFF3A then
                            if e 0xFF0F || r 0xFF1A 0xFF1B || r 0xFF1F 0xFF20 then
                                Just PunctuationOther

                            else if r 0xFF10 0xFF19 then
                                Just NumberDecimalDigit

                            else if r 0xFF1C 0xFF1E then
                                Just SymbolMath

                            else if r 0xFF21 0xFF39 then
                                Just LetterUppercase

                            else
                                Nothing

                        else if e 0xFF3A then
                            Just LetterUppercase

                        else if e 0xFF3B then
                            Just PunctuationOpen

                        else if e 0xFF3C then
                            Just PunctuationOther

                        else if e 0xFF3D then
                            Just PunctuationClose

                        else if e 0xFF3E || e 0xFF40 then
                            Just SymbolModifier

                        else if e 0xFF3F then
                            Just PunctuationConnector

                        else if r 0xFF41 0xFF59 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if l 0xFF60 then
                        if e 0xFF5A then
                            Just LetterLowercase

                        else if e 0xFF5B || e 0xFF5F then
                            Just PunctuationOpen

                        else if e 0xFF5C || e 0xFF5E then
                            Just SymbolMath

                        else if e 0xFF5D then
                            Just PunctuationClose

                        else
                            Nothing

                    else if e 0xFF60 || e 0xFF63 then
                        Just PunctuationClose

                    else if e 0xFF61 || r 0xFF64 0xFF65 then
                        Just PunctuationOther

                    else if e 0xFF62 then
                        Just PunctuationOpen

                    else if r 0xFF66 0xFF6F || r 0xFF71 0xFF9C then
                        Just LetterOther

                    else if e 0xFF70 then
                        Just LetterModifier

                    else
                        Nothing

                else if l 0x00010136 then
                    if l 0xFFE8 then
                        if e 0xFF9D || r 0xFFA0 0xFFBE || r 0xFFC2 0xFFC7 || r 0xFFCA 0xFFCF || r 0xFFD2 0xFFD7 || r 0xFFDA 0xFFDC then
                            Just LetterOther

                        else if r 0xFF9E 0xFF9F then
                            Just LetterModifier

                        else if r 0xFFE0 0xFFE1 || r 0xFFE5 0xFFE6 then
                            Just SymbolCurrency

                        else if e 0xFFE2 then
                            Just SymbolMath

                        else if e 0xFFE3 then
                            Just SymbolModifier

                        else if e 0xFFE4 then
                            Just SymbolOther

                        else
                            Nothing

                    else if l 0x00010027 then
                        if e 0xFFE8 || r 0xFFED 0xFFEE || r 0xFFFC 0xFFFD then
                            Just SymbolOther

                        else if r 0xFFE9 0xFFEC then
                            Just SymbolMath

                        else if r 0xFFF9 0xFFFB then
                            Just OtherFormat

                        else if r 0x00010000 0x0001000B || r 0x0001000D 0x00010026 then
                            Just LetterOther

                        else
                            Nothing

                    else if r 0x00010028 0x0001003A || r 0x0001003C 0x0001003D || r 0x0001003F 0x0001004D || r 0x00010050 0x0001005D || r 0x00010080 0x000100FA then
                        Just LetterOther

                    else if r 0x00010100 0x00010102 then
                        Just PunctuationOther

                    else if r 0x00010107 0x00010133 then
                        Just NumberOther

                    else
                        Nothing

                else if l 0x000102E0 then
                    if r 0x00010137 0x0001013F || r 0x00010179 0x00010189 || r 0x0001018C 0x0001018E || r 0x00010190 0x0001019C || e 0x000101A0 || r 0x000101D0 0x000101FC then
                        Just SymbolOther

                    else if r 0x00010140 0x00010174 then
                        Just NumberLetter

                    else if r 0x00010175 0x00010178 || r 0x0001018A 0x0001018B then
                        Just NumberOther

                    else if e 0x000101FD then
                        Just MarkNonSpacing

                    else if r 0x00010280 0x0001029C || r 0x000102A0 0x000102D0 then
                        Just LetterOther

                    else
                        Nothing

                else if l 0x00010349 then
                    if e 0x000102E0 then
                        Just MarkNonSpacing

                    else if r 0x000102E1 0x000102FB || r 0x00010320 0x00010323 then
                        Just NumberOther

                    else if r 0x00010300 0x0001031F || r 0x0001032D 0x00010340 || r 0x00010342 0x00010348 then
                        Just LetterOther

                    else if e 0x00010341 then
                        Just NumberLetter

                    else
                        Nothing

                else if e 0x00010349 || r 0x00010350 0x00010375 || r 0x00010380 0x0001039D || r 0x000103A0 0x000103C3 || r 0x000103C8 0x000103CE then
                    Just LetterOther

                else if e 0x0001034A then
                    Just NumberLetter

                else if r 0x00010376 0x0001037A then
                    Just MarkNonSpacing

                else if e 0x0001039F then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x000109D1 then
                if l 0x000107B1 then
                    if l 0x0001056F then
                        if e 0x000103CF || r 0x00010450 0x0001049D || r 0x00010500 0x00010527 || r 0x00010530 0x00010563 then
                            Just LetterOther

                        else if e 0x000103D0 then
                            Just PunctuationOther

                        else if r 0x000103D1 0x000103D5 then
                            Just NumberLetter

                        else if r 0x00010400 0x00010427 || r 0x000104B0 0x000104D3 then
                            Just LetterUppercase

                        else if r 0x00010428 0x0001044F || r 0x000104D8 0x000104FB then
                            Just LetterLowercase

                        else if r 0x000104A0 0x000104A9 then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if l 0x000105B2 then
                        if e 0x0001056F then
                            Just PunctuationOther

                        else if r 0x00010570 0x0001057A || r 0x0001057C 0x0001058A || r 0x0001058C 0x00010592 || r 0x00010594 0x00010595 then
                            Just LetterUppercase

                        else if r 0x00010597 0x000105A1 || r 0x000105A3 0x000105B1 then
                            Just LetterLowercase

                        else
                            Nothing

                    else if r 0x000105B3 0x000105B9 || r 0x000105BB 0x000105BC then
                        Just LetterLowercase

                    else if r 0x00010600 0x00010736 || r 0x00010740 0x00010755 || r 0x00010760 0x00010767 then
                        Just LetterOther

                    else if r 0x00010780 0x00010785 || r 0x00010787 0x000107B0 then
                        Just LetterModifier

                    else
                        Nothing

                else if l 0x000108A6 then
                    if l 0x0001083E then
                        if r 0x000107B2 0x000107BA then
                            Just LetterModifier

                        else if r 0x00010800 0x00010805 || e 0x00010808 || r 0x0001080A 0x00010835 || r 0x00010837 0x00010838 || e 0x0001083C then
                            Just LetterOther

                        else
                            Nothing

                    else if r 0x0001083F 0x00010855 || r 0x00010860 0x00010876 || r 0x00010880 0x0001089E then
                        Just LetterOther

                    else if e 0x00010857 then
                        Just PunctuationOther

                    else if r 0x00010858 0x0001085F || r 0x00010879 0x0001087F then
                        Just NumberOther

                    else if r 0x00010877 0x00010878 then
                        Just SymbolOther

                    else
                        Nothing

                else if l 0x0001091E then
                    if r 0x000108A7 0x000108AF || r 0x000108FB 0x000108FF || r 0x00010916 0x0001091B then
                        Just NumberOther

                    else if r 0x000108E0 0x000108F2 || r 0x000108F4 0x000108F5 || r 0x00010900 0x00010915 then
                        Just LetterOther

                    else
                        Nothing

                else if e 0x0001091F || e 0x0001093F then
                    Just PunctuationOther

                else if r 0x00010920 0x00010939 || r 0x00010980 0x000109B7 || r 0x000109BE 0x000109BF then
                    Just LetterOther

                else if r 0x000109BC 0x000109BD || r 0x000109C0 0x000109CF then
                    Just NumberOther

                else
                    Nothing

            else if l 0x00010B3F then
                if l 0x00010A5F then
                    if r 0x000109D2 0x000109FF || r 0x00010A40 0x00010A48 then
                        Just NumberOther

                    else if e 0x00010A00 || r 0x00010A10 0x00010A13 || r 0x00010A15 0x00010A17 || r 0x00010A19 0x00010A35 then
                        Just LetterOther

                    else if r 0x00010A01 0x00010A03 || r 0x00010A05 0x00010A06 || r 0x00010A0C 0x00010A0F || r 0x00010A38 0x00010A3A || e 0x00010A3F then
                        Just MarkNonSpacing

                    else if r 0x00010A50 0x00010A58 then
                        Just PunctuationOther

                    else
                        Nothing

                else if l 0x00010AC7 then
                    if r 0x00010A60 0x00010A7C || r 0x00010A80 0x00010A9C || r 0x00010AC0 0x00010AC6 then
                        Just LetterOther

                    else if r 0x00010A7D 0x00010A7E || r 0x00010A9D 0x00010A9F then
                        Just NumberOther

                    else if e 0x00010A7F then
                        Just PunctuationOther

                    else
                        Nothing

                else if e 0x00010AC7 || r 0x00010AC9 0x00010AE4 || r 0x00010B00 0x00010B35 then
                    Just LetterOther

                else if e 0x00010AC8 then
                    Just SymbolOther

                else if r 0x00010AE5 0x00010AE6 then
                    Just MarkNonSpacing

                else if r 0x00010AEB 0x00010AEF then
                    Just NumberOther

                else if r 0x00010AF0 0x00010AF6 || r 0x00010B39 0x00010B3E then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x00010D23 then
                if l 0x00010B98 then
                    if e 0x00010B3F then
                        Just PunctuationOther

                    else if r 0x00010B40 0x00010B55 || r 0x00010B60 0x00010B72 || r 0x00010B80 0x00010B91 then
                        Just LetterOther

                    else if r 0x00010B58 0x00010B5F || r 0x00010B78 0x00010B7F then
                        Just NumberOther

                    else
                        Nothing

                else if r 0x00010B99 0x00010B9C then
                    Just PunctuationOther

                else if r 0x00010BA9 0x00010BAF || r 0x00010CFA 0x00010CFF then
                    Just NumberOther

                else if r 0x00010C00 0x00010C48 || r 0x00010D00 0x00010D22 then
                    Just LetterOther

                else if r 0x00010C80 0x00010CB2 then
                    Just LetterUppercase

                else if r 0x00010CC0 0x00010CF2 then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0x00010EAF then
                if e 0x00010D23 || r 0x00010E80 0x00010EA9 then
                    Just LetterOther

                else if r 0x00010D24 0x00010D27 || r 0x00010EAB 0x00010EAC then
                    Just MarkNonSpacing

                else if r 0x00010D30 0x00010D39 then
                    Just NumberDecimalDigit

                else if r 0x00010E60 0x00010E7E then
                    Just NumberOther

                else if e 0x00010EAD then
                    Just PunctuationDash

                else
                    Nothing

            else if r 0x00010EB0 0x00010EB1 || r 0x00010F00 0x00010F1C || e 0x00010F27 || r 0x00010F30 0x00010F45 then
                Just LetterOther

            else if r 0x00010EFD 0x00010EFF || r 0x00010F46 0x00010F50 then
                Just MarkNonSpacing

            else if r 0x00010F1D 0x00010F26 || r 0x00010F51 0x00010F53 then
                Just NumberOther

            else
                Nothing

        else if l 0x00011340 then
            if l 0x00011182 then
                if l 0x000110B2 then
                    if l 0x00011037 then
                        if e 0x00010F54 || r 0x00010FC5 0x00010FCB then
                            Just NumberOther

                        else if r 0x00010F55 0x00010F59 || r 0x00010F86 0x00010F89 then
                            Just PunctuationOther

                        else if r 0x00010F70 0x00010F81 || r 0x00010FB0 0x00010FC4 || r 0x00010FE0 0x00010FF6 || r 0x00011003 0x00011036 then
                            Just LetterOther

                        else if r 0x00010F82 0x00010F85 || e 0x00011001 then
                            Just MarkNonSpacing

                        else if e 0x00011000 || e 0x00011002 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if l 0x00011070 then
                        if e 0x00011037 then
                            Just LetterOther

                        else if r 0x00011038 0x00011046 then
                            Just MarkNonSpacing

                        else if r 0x00011047 0x0001104D then
                            Just PunctuationOther

                        else if r 0x00011052 0x00011065 then
                            Just NumberOther

                        else if r 0x00011066 0x0001106F then
                            Just NumberDecimalDigit

                        else
                            Nothing

                    else if e 0x00011070 || r 0x00011073 0x00011074 || r 0x0001107F 0x00011081 then
                        Just MarkNonSpacing

                    else if r 0x00011071 0x00011072 || e 0x00011075 || r 0x00011083 0x000110AF then
                        Just LetterOther

                    else if e 0x00011082 || r 0x000110B0 0x000110B1 then
                        Just MarkSpacingCombining

                    else
                        Nothing

                else if l 0x00011126 then
                    if l 0x000110BD then
                        if e 0x000110B2 || r 0x000110B7 0x000110B8 then
                            Just MarkSpacingCombining

                        else if r 0x000110B3 0x000110B6 || r 0x000110B9 0x000110BA then
                            Just MarkNonSpacing

                        else if r 0x000110BB 0x000110BC then
                            Just PunctuationOther

                        else
                            Nothing

                    else if e 0x000110BD || e 0x000110CD then
                        Just OtherFormat

                    else if r 0x000110BE 0x000110C1 then
                        Just PunctuationOther

                    else if e 0x000110C2 || r 0x00011100 0x00011102 then
                        Just MarkNonSpacing

                    else if r 0x000110D0 0x000110E8 || r 0x00011103 0x00011125 then
                        Just LetterOther

                    else if r 0x000110F0 0x000110F9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0x00011144 then
                    if e 0x00011126 then
                        Just LetterOther

                    else if r 0x00011127 0x0001112B || r 0x0001112D 0x00011134 then
                        Just MarkNonSpacing

                    else if e 0x0001112C then
                        Just MarkSpacingCombining

                    else if r 0x00011136 0x0001113F then
                        Just NumberDecimalDigit

                    else if r 0x00011140 0x00011143 then
                        Just PunctuationOther

                    else
                        Nothing

                else if e 0x00011144 || e 0x00011147 || r 0x00011150 0x00011172 || e 0x00011176 then
                    Just LetterOther

                else if r 0x00011145 0x00011146 then
                    Just MarkSpacingCombining

                else if e 0x00011173 || r 0x00011180 0x00011181 then
                    Just MarkNonSpacing

                else if r 0x00011174 0x00011175 then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x00011237 then
                if l 0x000111D9 then
                    if e 0x00011182 || r 0x000111B3 0x000111B5 || r 0x000111BF 0x000111C0 || e 0x000111CE then
                        Just MarkSpacingCombining

                    else if r 0x00011183 0x000111B2 || r 0x000111C1 0x000111C4 then
                        Just LetterOther

                    else if r 0x000111B6 0x000111BE || r 0x000111C9 0x000111CC || e 0x000111CF then
                        Just MarkNonSpacing

                    else if r 0x000111C5 0x000111C8 || e 0x000111CD then
                        Just PunctuationOther

                    else if r 0x000111D0 0x000111D8 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if l 0x00011212 then
                    if e 0x000111D9 then
                        Just NumberDecimalDigit

                    else if e 0x000111DA || e 0x000111DC || r 0x00011200 0x00011211 then
                        Just LetterOther

                    else if e 0x000111DB || r 0x000111DD 0x000111DF then
                        Just PunctuationOther

                    else if r 0x000111E1 0x000111F4 then
                        Just NumberOther

                    else
                        Nothing

                else if r 0x00011213 0x0001122B then
                    Just LetterOther

                else if r 0x0001122C 0x0001122E || r 0x00011232 0x00011233 || e 0x00011235 then
                    Just MarkSpacingCombining

                else if r 0x0001122F 0x00011231 || e 0x00011234 || e 0x00011236 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if l 0x000112DF then
                if e 0x00011237 || e 0x0001123E || e 0x00011241 then
                    Just MarkNonSpacing

                else if r 0x00011238 0x0001123D || e 0x000112A9 then
                    Just PunctuationOther

                else if r 0x0001123F 0x00011240 || r 0x00011280 0x00011286 || e 0x00011288 || r 0x0001128A 0x0001128D || r 0x0001128F 0x0001129D || r 0x0001129F 0x000112A8 || r 0x000112B0 0x000112DE then
                    Just LetterOther

                else
                    Nothing

            else if l 0x0001130E then
                if e 0x000112DF || r 0x000112E3 0x000112EA || r 0x00011300 0x00011301 then
                    Just MarkNonSpacing

                else if r 0x000112E0 0x000112E2 || r 0x00011302 0x00011303 then
                    Just MarkSpacingCombining

                else if r 0x000112F0 0x000112F9 then
                    Just NumberDecimalDigit

                else if r 0x00011305 0x0001130C then
                    Just LetterOther

                else
                    Nothing

            else if r 0x0001130F 0x00011310 || r 0x00011313 0x00011328 || r 0x0001132A 0x00011330 || r 0x00011332 0x00011333 || r 0x00011335 0x00011339 || e 0x0001133D then
                Just LetterOther

            else if r 0x0001133B 0x0001133C then
                Just MarkNonSpacing

            else if r 0x0001133E 0x0001133F then
                Just MarkSpacingCombining

            else
                Nothing

        else if l 0x0001163C then
            if l 0x000114AF then
                if l 0x00011437 then
                    if e 0x00011340 || r 0x00011366 0x0001136C || r 0x00011370 0x00011374 then
                        Just MarkNonSpacing

                    else if r 0x00011341 0x00011344 || r 0x00011347 0x00011348 || r 0x0001134B 0x0001134D || e 0x00011357 || r 0x00011362 0x00011363 || r 0x00011435 0x00011436 then
                        Just MarkSpacingCombining

                    else if e 0x00011350 || r 0x0001135D 0x00011361 || r 0x00011400 0x00011434 then
                        Just LetterOther

                    else
                        Nothing

                else if l 0x0001144A then
                    if e 0x00011437 || r 0x00011440 0x00011441 || e 0x00011445 then
                        Just MarkSpacingCombining

                    else if r 0x00011438 0x0001143F || r 0x00011442 0x00011444 || e 0x00011446 then
                        Just MarkNonSpacing

                    else if r 0x00011447 0x00011449 then
                        Just LetterOther

                    else
                        Nothing

                else if e 0x0001144A || r 0x0001145F 0x00011461 || r 0x00011480 0x000114AE then
                    Just LetterOther

                else if r 0x0001144B 0x0001144F || r 0x0001145A 0x0001145B || e 0x0001145D then
                    Just PunctuationOther

                else if r 0x00011450 0x00011459 then
                    Just NumberDecimalDigit

                else if e 0x0001145E then
                    Just MarkNonSpacing

                else
                    Nothing

            else if l 0x0001157F then
                if l 0x000114BE then
                    if e 0x000114AF then
                        Just LetterOther

                    else if r 0x000114B0 0x000114B2 || e 0x000114B9 || r 0x000114BB 0x000114BD then
                        Just MarkSpacingCombining

                    else if r 0x000114B3 0x000114B8 || e 0x000114BA then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if e 0x000114BE || e 0x000114C1 then
                    Just MarkSpacingCombining

                else if r 0x000114BF 0x000114C0 || r 0x000114C2 0x000114C3 then
                    Just MarkNonSpacing

                else if r 0x000114C4 0x000114C5 || e 0x000114C7 then
                    Just LetterOther

                else if e 0x000114C6 then
                    Just PunctuationOther

                else if r 0x000114D0 0x000114D9 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if l 0x000115C0 then
                if r 0x00011580 0x000115AE then
                    Just LetterOther

                else if r 0x000115AF 0x000115B1 || r 0x000115B8 0x000115BB || e 0x000115BE then
                    Just MarkSpacingCombining

                else if r 0x000115B2 0x000115B5 || r 0x000115BC 0x000115BD || e 0x000115BF then
                    Just MarkNonSpacing

                else
                    Nothing

            else if e 0x000115C0 || r 0x000115DC 0x000115DD || r 0x00011633 0x0001163A then
                Just MarkNonSpacing

            else if r 0x000115C1 0x000115D7 then
                Just PunctuationOther

            else if r 0x000115D8 0x000115DB || r 0x00011600 0x0001162F then
                Just LetterOther

            else if r 0x00011630 0x00011632 || e 0x0001163B then
                Just MarkSpacingCombining

            else
                Nothing

        else if l 0x00011739 then
            if l 0x000116AF then
                if l 0x0001164F then
                    if e 0x0001163C || e 0x0001163E then
                        Just MarkSpacingCombining

                    else if e 0x0001163D || r 0x0001163F 0x00011640 then
                        Just MarkNonSpacing

                    else if r 0x00011641 0x00011643 then
                        Just PunctuationOther

                    else if e 0x00011644 then
                        Just LetterOther

                    else
                        Nothing

                else if r 0x00011650 0x00011659 then
                    Just NumberDecimalDigit

                else if r 0x00011660 0x0001166C then
                    Just PunctuationOther

                else if r 0x00011680 0x000116AA then
                    Just LetterOther

                else if e 0x000116AB || e 0x000116AD then
                    Just MarkNonSpacing

                else if e 0x000116AC || e 0x000116AE then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if l 0x000116FF then
                if e 0x000116AF || e 0x000116B6 then
                    Just MarkSpacingCombining

                else if r 0x000116B0 0x000116B5 || e 0x000116B7 then
                    Just MarkNonSpacing

                else if e 0x000116B8 then
                    Just LetterOther

                else if e 0x000116B9 then
                    Just PunctuationOther

                else if r 0x000116C0 0x000116C9 then
                    Just NumberDecimalDigit

                else
                    Nothing

            else if r 0x00011700 0x0001171A then
                Just LetterOther

            else if r 0x0001171D 0x0001171F || r 0x00011722 0x00011725 || r 0x00011727 0x0001172B then
                Just MarkNonSpacing

            else if r 0x00011720 0x00011721 || e 0x00011726 then
                Just MarkSpacingCombining

            else if r 0x00011730 0x00011738 then
                Just NumberDecimalDigit

            else
                Nothing

        else if l 0x000118E9 then
            if l 0x0001182E then
                if e 0x00011739 then
                    Just NumberDecimalDigit

                else if r 0x0001173A 0x0001173B then
                    Just NumberOther

                else if r 0x0001173C 0x0001173E then
                    Just PunctuationOther

                else if e 0x0001173F then
                    Just SymbolOther

                else if r 0x00011740 0x00011746 || r 0x00011800 0x0001182B then
                    Just LetterOther

                else if r 0x0001182C 0x0001182D then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if e 0x0001182E || e 0x00011838 then
                Just MarkSpacingCombining

            else if r 0x0001182F 0x00011837 || r 0x00011839 0x0001183A then
                Just MarkNonSpacing

            else if e 0x0001183B then
                Just PunctuationOther

            else if r 0x000118A0 0x000118BF then
                Just LetterUppercase

            else if r 0x000118C0 0x000118DF then
                Just LetterLowercase

            else if r 0x000118E0 0x000118E8 then
                Just NumberDecimalDigit

            else
                Nothing

        else if l 0x0001192F then
            if e 0x000118E9 then
                Just NumberDecimalDigit

            else if r 0x000118EA 0x000118F2 then
                Just NumberOther

            else if r 0x000118FF 0x00011906 || e 0x00011909 || r 0x0001190C 0x00011913 || r 0x00011915 0x00011916 || r 0x00011918 0x0001192E then
                Just LetterOther

            else
                Nothing

        else if e 0x0001192F || e 0x0001193F || e 0x00011941 then
            Just LetterOther

        else if r 0x00011930 0x00011935 || r 0x00011937 0x00011938 || e 0x0001193D || e 0x00011940 then
            Just MarkSpacingCombining

        else if r 0x0001193B 0x0001193C || e 0x0001193E then
            Just MarkNonSpacing

        else
            Nothing

    else if l 0x0001D4A1 then
        if l 0x00011FFF then
            if l 0x00011C91 then
                if l 0x00011A50 then
                    if l 0x000119E1 then
                        if e 0x00011942 || r 0x000119D1 0x000119D3 || r 0x000119DC 0x000119DF then
                            Just MarkSpacingCombining

                        else if e 0x00011943 || r 0x000119D4 0x000119D7 || r 0x000119DA 0x000119DB || e 0x000119E0 then
                            Just MarkNonSpacing

                        else if r 0x00011944 0x00011946 then
                            Just PunctuationOther

                        else if r 0x00011950 0x00011959 then
                            Just NumberDecimalDigit

                        else if r 0x000119A0 0x000119A7 || r 0x000119AA 0x000119D0 then
                            Just LetterOther

                        else
                            Nothing

                    else if l 0x00011A0A then
                        if e 0x000119E1 || e 0x000119E3 || e 0x00011A00 then
                            Just LetterOther

                        else if e 0x000119E2 then
                            Just PunctuationOther

                        else if e 0x000119E4 then
                            Just MarkSpacingCombining

                        else if r 0x00011A01 0x00011A09 then
                            Just MarkNonSpacing

                        else
                            Nothing

                    else if e 0x00011A0A || r 0x00011A33 0x00011A38 || r 0x00011A3B 0x00011A3E || e 0x00011A47 then
                        Just MarkNonSpacing

                    else if r 0x00011A0B 0x00011A32 || e 0x00011A3A then
                        Just LetterOther

                    else if e 0x00011A39 then
                        Just MarkSpacingCombining

                    else if r 0x00011A3F 0x00011A46 then
                        Just PunctuationOther

                    else
                        Nothing

                else if l 0x00011BFF then
                    if l 0x00011A96 then
                        if e 0x00011A50 || r 0x00011A5C 0x00011A89 then
                            Just LetterOther

                        else if r 0x00011A51 0x00011A56 || r 0x00011A59 0x00011A5B || r 0x00011A8A 0x00011A95 then
                            Just MarkNonSpacing

                        else if r 0x00011A57 0x00011A58 then
                            Just MarkSpacingCombining

                        else
                            Nothing

                    else if e 0x00011A96 || r 0x00011A98 0x00011A99 then
                        Just MarkNonSpacing

                    else if e 0x00011A97 then
                        Just MarkSpacingCombining

                    else if r 0x00011A9A 0x00011A9C || r 0x00011A9E 0x00011AA2 || r 0x00011B00 0x00011B09 then
                        Just PunctuationOther

                    else if e 0x00011A9D || r 0x00011AB0 0x00011AF8 then
                        Just LetterOther

                    else
                        Nothing

                else if l 0x00011C3E then
                    if r 0x00011C00 0x00011C08 || r 0x00011C0A 0x00011C2E then
                        Just LetterOther

                    else if e 0x00011C2F then
                        Just MarkSpacingCombining

                    else if r 0x00011C30 0x00011C36 || r 0x00011C38 0x00011C3D then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if e 0x00011C3E then
                    Just MarkSpacingCombining

                else if e 0x00011C3F then
                    Just MarkNonSpacing

                else if e 0x00011C40 || r 0x00011C72 0x00011C8F then
                    Just LetterOther

                else if r 0x00011C41 0x00011C45 || r 0x00011C70 0x00011C71 then
                    Just PunctuationOther

                else if r 0x00011C50 0x00011C59 then
                    Just NumberDecimalDigit

                else if r 0x00011C5A 0x00011C6C then
                    Just NumberOther

                else
                    Nothing

            else if l 0x00011D96 then
                if l 0x00011D3B then
                    if r 0x00011C92 0x00011CA7 || r 0x00011CAA 0x00011CB0 || r 0x00011CB2 0x00011CB3 || r 0x00011CB5 0x00011CB6 || r 0x00011D31 0x00011D36 || e 0x00011D3A then
                        Just MarkNonSpacing

                    else if e 0x00011CA9 || e 0x00011CB1 || e 0x00011CB4 then
                        Just MarkSpacingCombining

                    else if r 0x00011D00 0x00011D06 || r 0x00011D08 0x00011D09 || r 0x00011D0B 0x00011D30 then
                        Just LetterOther

                    else
                        Nothing

                else if r 0x00011D3C 0x00011D3D || r 0x00011D3F 0x00011D45 || e 0x00011D47 || r 0x00011D90 0x00011D91 || e 0x00011D95 then
                    Just MarkNonSpacing

                else if e 0x00011D46 || r 0x00011D60 0x00011D65 || r 0x00011D67 0x00011D68 || r 0x00011D6A 0x00011D89 then
                    Just LetterOther

                else if r 0x00011D50 0x00011D59 then
                    Just NumberDecimalDigit

                else if r 0x00011D8A 0x00011D8E || r 0x00011D93 0x00011D94 then
                    Just MarkSpacingCombining

                else
                    Nothing

            else if l 0x00011F33 then
                if l 0x00011EF4 then
                    if e 0x00011D96 then
                        Just MarkSpacingCombining

                    else if e 0x00011D97 || e 0x00011EF3 then
                        Just MarkNonSpacing

                    else if e 0x00011D98 || r 0x00011EE0 0x00011EF2 then
                        Just LetterOther

                    else if r 0x00011DA0 0x00011DA9 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if e 0x00011EF4 || r 0x00011F00 0x00011F01 then
                    Just MarkNonSpacing

                else if r 0x00011EF5 0x00011EF6 || e 0x00011F03 then
                    Just MarkSpacingCombining

                else if r 0x00011EF7 0x00011EF8 then
                    Just PunctuationOther

                else if e 0x00011F02 || r 0x00011F04 0x00011F10 || r 0x00011F12 0x00011F32 then
                    Just LetterOther

                else
                    Nothing

            else if l 0x00011F42 then
                if e 0x00011F33 then
                    Just LetterOther

                else if r 0x00011F34 0x00011F35 || r 0x00011F3E 0x00011F3F || e 0x00011F41 then
                    Just MarkSpacingCombining

                else if r 0x00011F36 0x00011F3A || e 0x00011F40 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if e 0x00011F42 then
                Just MarkNonSpacing

            else if r 0x00011F43 0x00011F4F then
                Just PunctuationOther

            else if r 0x00011F50 0x00011F59 then
                Just NumberDecimalDigit

            else if e 0x00011FB0 then
                Just LetterOther

            else if r 0x00011FC0 0x00011FD4 then
                Just NumberOther

            else if r 0x00011FD5 0x00011FDC || r 0x00011FE1 0x00011FF1 then
                Just SymbolOther

            else if r 0x00011FDD 0x00011FE0 then
                Just SymbolCurrency

            else
                Nothing

        else if l 0x00018CFF then
            if l 0x00016B3B then
                if l 0x000143FF then
                    if e 0x00011FFF || r 0x00012470 0x00012474 || r 0x00012FF1 0x00012FF2 then
                        Just PunctuationOther

                    else if r 0x00012000 0x00012399 || r 0x00012480 0x00012543 || r 0x00012F90 0x00012FF0 || r 0x00013000 0x0001342F || r 0x00013441 0x00013446 then
                        Just LetterOther

                    else if r 0x00012400 0x0001246E then
                        Just NumberLetter

                    else if r 0x00013430 0x0001343F then
                        Just OtherFormat

                    else if e 0x00013440 || r 0x00013447 0x00013455 then
                        Just MarkNonSpacing

                    else
                        Nothing

                else if l 0x00016ABF then
                    if r 0x00014400 0x00014646 || r 0x00016800 0x00016A38 || r 0x00016A40 0x00016A5E || r 0x00016A70 0x00016ABE then
                        Just LetterOther

                    else if r 0x00016A60 0x00016A69 then
                        Just NumberDecimalDigit

                    else if r 0x00016A6E 0x00016A6F then
                        Just PunctuationOther

                    else
                        Nothing

                else if r 0x00016AC0 0x00016AC9 then
                    Just NumberDecimalDigit

                else if r 0x00016AD0 0x00016AED || r 0x00016B00 0x00016B2F then
                    Just LetterOther

                else if r 0x00016AF0 0x00016AF4 || r 0x00016B30 0x00016B36 then
                    Just MarkNonSpacing

                else if e 0x00016AF5 || r 0x00016B37 0x00016B3A then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x00016EFF then
                if l 0x00016B5A then
                    if e 0x00016B3B || e 0x00016B44 then
                        Just PunctuationOther

                    else if r 0x00016B3C 0x00016B3F || e 0x00016B45 then
                        Just SymbolOther

                    else if r 0x00016B40 0x00016B43 then
                        Just LetterModifier

                    else if r 0x00016B50 0x00016B59 then
                        Just NumberDecimalDigit

                    else
                        Nothing

                else if r 0x00016B5B 0x00016B61 || r 0x00016E80 0x00016E96 then
                    Just NumberOther

                else if r 0x00016B63 0x00016B77 || r 0x00016B7D 0x00016B8F then
                    Just LetterOther

                else if r 0x00016E40 0x00016E5F then
                    Just LetterUppercase

                else if r 0x00016E60 0x00016E7F then
                    Just LetterLowercase

                else if r 0x00016E97 0x00016E9A then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x00016FE1 then
                if r 0x00016F00 0x00016F4A || e 0x00016F50 then
                    Just LetterOther

                else if e 0x00016F4F || r 0x00016F8F 0x00016F92 then
                    Just MarkNonSpacing

                else if r 0x00016F51 0x00016F87 then
                    Just MarkSpacingCombining

                else if r 0x00016F93 0x00016F9F || e 0x00016FE0 then
                    Just LetterModifier

                else
                    Nothing

            else if e 0x00016FE1 || e 0x00016FE3 then
                Just LetterModifier

            else if e 0x00016FE2 then
                Just PunctuationOther

            else if e 0x00016FE4 then
                Just MarkNonSpacing

            else if r 0x00016FF0 0x00016FF1 then
                Just MarkSpacingCombining

            else if e 0x00017000 || e 0x000187F7 || r 0x00018800 0x00018CD5 then
                Just LetterOther

            else
                Nothing

        else if l 0x0001D166 then
            if l 0x0001BC7F then
                if l 0x0001B131 then
                    if e 0x00018D00 || e 0x00018D08 || r 0x0001B000 0x0001B122 then
                        Just LetterOther

                    else if r 0x0001AFF0 0x0001AFF3 || r 0x0001AFF5 0x0001AFFB || r 0x0001AFFD 0x0001AFFE then
                        Just LetterModifier

                    else
                        Nothing

                else if e 0x0001B132 || r 0x0001B150 0x0001B152 || e 0x0001B155 || r 0x0001B164 0x0001B167 || r 0x0001B170 0x0001B2FB || r 0x0001BC00 0x0001BC6A || r 0x0001BC70 0x0001BC7C then
                    Just LetterOther

                else
                    Nothing

            else if l 0x0001CEFF then
                if r 0x0001BC80 0x0001BC88 || r 0x0001BC90 0x0001BC99 then
                    Just LetterOther

                else if e 0x0001BC9C then
                    Just SymbolOther

                else if r 0x0001BC9D 0x0001BC9E then
                    Just MarkNonSpacing

                else if e 0x0001BC9F then
                    Just PunctuationOther

                else if r 0x0001BCA0 0x0001BCA3 then
                    Just OtherFormat

                else
                    Nothing

            else if r 0x0001CF00 0x0001CF2D || r 0x0001CF30 0x0001CF46 then
                Just MarkNonSpacing

            else if r 0x0001CF50 0x0001CFC3 || r 0x0001D000 0x0001D0F5 || r 0x0001D100 0x0001D126 || r 0x0001D129 0x0001D164 then
                Just SymbolOther

            else if e 0x0001D165 then
                Just MarkSpacingCombining

            else
                Nothing

        else if l 0x0001D244 then
            if l 0x0001D182 then
                if e 0x0001D166 || r 0x0001D16D 0x0001D172 then
                    Just MarkSpacingCombining

                else if r 0x0001D167 0x0001D169 || r 0x0001D17B 0x0001D181 then
                    Just MarkNonSpacing

                else if r 0x0001D16A 0x0001D16C then
                    Just SymbolOther

                else if r 0x0001D173 0x0001D17A then
                    Just OtherFormat

                else
                    Nothing

            else if e 0x0001D182 || r 0x0001D185 0x0001D18B || r 0x0001D1AA 0x0001D1AD || r 0x0001D242 0x0001D243 then
                Just MarkNonSpacing

            else if r 0x0001D183 0x0001D184 || r 0x0001D18C 0x0001D1A9 || r 0x0001D1AE 0x0001D1EA || r 0x0001D200 0x0001D241 then
                Just SymbolOther

            else
                Nothing

        else if l 0x0001D419 then
            if e 0x0001D244 then
                Just MarkNonSpacing

            else if e 0x0001D245 || r 0x0001D300 0x0001D356 then
                Just SymbolOther

            else if r 0x0001D2C0 0x0001D2D3 || r 0x0001D2E0 0x0001D2F3 || r 0x0001D360 0x0001D378 then
                Just NumberOther

            else if r 0x0001D400 0x0001D418 then
                Just LetterUppercase

            else
                Nothing

        else if e 0x0001D419 || r 0x0001D434 0x0001D44D || r 0x0001D468 0x0001D481 || e 0x0001D49C || r 0x0001D49E 0x0001D49F then
            Just LetterUppercase

        else if r 0x0001D41A 0x0001D433 || r 0x0001D44E 0x0001D454 || r 0x0001D456 0x0001D467 || r 0x0001D482 0x0001D49B then
            Just LetterLowercase

        else
            Nothing

    else if l 0x0001E7EC then
        if l 0x0001D788 then
            if l 0x0001D5D3 then
                if l 0x0001D50C then
                    if e 0x0001D4A2 || r 0x0001D4A5 0x0001D4A6 || r 0x0001D4A9 0x0001D4AC || r 0x0001D4AE 0x0001D4B5 || r 0x0001D4D0 0x0001D4E9 || r 0x0001D504 0x0001D505 || r 0x0001D507 0x0001D50A then
                        Just LetterUppercase

                    else if r 0x0001D4B6 0x0001D4B9 || e 0x0001D4BB || r 0x0001D4BD 0x0001D4C3 || r 0x0001D4C5 0x0001D4CF || r 0x0001D4EA 0x0001D503 then
                        Just LetterLowercase

                    else
                        Nothing

                else if l 0x0001D545 then
                    if r 0x0001D50D 0x0001D514 || r 0x0001D516 0x0001D51C || r 0x0001D538 0x0001D539 || r 0x0001D53B 0x0001D53E || r 0x0001D540 0x0001D544 then
                        Just LetterUppercase

                    else if r 0x0001D51E 0x0001D537 then
                        Just LetterLowercase

                    else
                        Nothing

                else if e 0x0001D546 || r 0x0001D54A 0x0001D550 || r 0x0001D56C 0x0001D585 || r 0x0001D5A0 0x0001D5B9 then
                    Just LetterUppercase

                else if r 0x0001D552 0x0001D56B || r 0x0001D586 0x0001D59F || r 0x0001D5BA 0x0001D5D2 then
                    Just LetterLowercase

                else
                    Nothing

            else if l 0x0001D6DB then
                if e 0x0001D5D3 || r 0x0001D5EE 0x0001D607 || r 0x0001D622 0x0001D63B || r 0x0001D656 0x0001D66F || r 0x0001D68A 0x0001D6A5 || r 0x0001D6C2 0x0001D6DA then
                    Just LetterLowercase

                else if r 0x0001D5D4 0x0001D5ED || r 0x0001D608 0x0001D621 || r 0x0001D63C 0x0001D655 || r 0x0001D670 0x0001D689 || r 0x0001D6A8 0x0001D6C0 then
                    Just LetterUppercase

                else if e 0x0001D6C1 then
                    Just SymbolMath

                else
                    Nothing

            else if l 0x0001D71B then
                if e 0x0001D6DB || e 0x0001D6FB || e 0x0001D715 then
                    Just SymbolMath

                else if r 0x0001D6DC 0x0001D6E1 || r 0x0001D6FC 0x0001D714 || r 0x0001D716 0x0001D71A then
                    Just LetterLowercase

                else if r 0x0001D6E2 0x0001D6FA then
                    Just LetterUppercase

                else
                    Nothing

            else if e 0x0001D71B || r 0x0001D736 0x0001D74E || r 0x0001D750 0x0001D755 || r 0x0001D770 0x0001D787 then
                Just LetterLowercase

            else if r 0x0001D71C 0x0001D734 || r 0x0001D756 0x0001D76E then
                Just LetterUppercase

            else if e 0x0001D735 || e 0x0001D74F || e 0x0001D76F then
                Just SymbolMath

            else
                Nothing

        else if l 0x0001DF24 then
            if l 0x0001DA36 then
                if l 0x0001D7C2 then
                    if e 0x0001D788 || r 0x0001D78A 0x0001D78F || r 0x0001D7AA 0x0001D7C1 then
                        Just LetterLowercase

                    else if e 0x0001D789 || e 0x0001D7A9 then
                        Just SymbolMath

                    else if r 0x0001D790 0x0001D7A8 then
                        Just LetterUppercase

                    else
                        Nothing

                else if e 0x0001D7C2 || r 0x0001D7C4 0x0001D7C9 || e 0x0001D7CB then
                    Just LetterLowercase

                else if e 0x0001D7C3 then
                    Just SymbolMath

                else if e 0x0001D7CA then
                    Just LetterUppercase

                else if r 0x0001D7CE 0x0001D7FF then
                    Just NumberDecimalDigit

                else if r 0x0001D800 0x0001D9FF then
                    Just SymbolOther

                else if r 0x0001DA00 0x0001DA35 then
                    Just MarkNonSpacing

                else
                    Nothing

            else if l 0x0001DA84 then
                if e 0x0001DA36 || r 0x0001DA3B 0x0001DA6C || e 0x0001DA75 then
                    Just MarkNonSpacing

                else if r 0x0001DA37 0x0001DA3A || r 0x0001DA6D 0x0001DA74 || r 0x0001DA76 0x0001DA83 then
                    Just SymbolOther

                else
                    Nothing

            else if e 0x0001DA84 || r 0x0001DA9B 0x0001DA9F || r 0x0001DAA1 0x0001DAAF then
                Just MarkNonSpacing

            else if r 0x0001DA85 0x0001DA86 then
                Just SymbolOther

            else if r 0x0001DA87 0x0001DA8B then
                Just PunctuationOther

            else if r 0x0001DF00 0x0001DF09 || r 0x0001DF0B 0x0001DF1E then
                Just LetterLowercase

            else if e 0x0001DF0A then
                Just LetterOther

            else
                Nothing

        else if l 0x0001E14E then
            if r 0x0001DF25 0x0001DF2A then
                Just LetterLowercase

            else if r 0x0001E000 0x0001E006 || r 0x0001E008 0x0001E018 || r 0x0001E01B 0x0001E021 || r 0x0001E023 0x0001E024 || r 0x0001E026 0x0001E02A || e 0x0001E08F || r 0x0001E130 0x0001E136 then
                Just MarkNonSpacing

            else if r 0x0001E030 0x0001E06D || r 0x0001E137 0x0001E13D then
                Just LetterModifier

            else if r 0x0001E100 0x0001E12C then
                Just LetterOther

            else if r 0x0001E140 0x0001E149 then
                Just NumberDecimalDigit

            else
                Nothing

        else if l 0x0001E2FE then
            if e 0x0001E14E || r 0x0001E290 0x0001E2AD || r 0x0001E2C0 0x0001E2EB then
                Just LetterOther

            else if e 0x0001E14F then
                Just SymbolOther

            else if e 0x0001E2AE || r 0x0001E2EC 0x0001E2EF then
                Just MarkNonSpacing

            else if r 0x0001E2F0 0x0001E2F9 then
                Just NumberDecimalDigit

            else
                Nothing

        else if e 0x0001E2FF then
            Just SymbolCurrency

        else if r 0x0001E4D0 0x0001E4EA || r 0x0001E7E0 0x0001E7E6 || r 0x0001E7E8 0x0001E7EB then
            Just LetterOther

        else if e 0x0001E4EB then
            Just LetterModifier

        else if r 0x0001E4EC 0x0001E4EF then
            Just MarkNonSpacing

        else if r 0x0001E4F0 0x0001E4F9 then
            Just NumberDecimalDigit

        else
            Nothing

    else if l 0x0001F1E5 then
        if l 0x0001EE33 then
            if l 0x0001ECAB then
                if r 0x0001E7ED 0x0001E7EE || r 0x0001E7F0 0x0001E7FE || r 0x0001E800 0x0001E8C4 then
                    Just LetterOther

                else if r 0x0001E8C7 0x0001E8CF || r 0x0001EC71 0x0001ECAA then
                    Just NumberOther

                else if r 0x0001E8D0 0x0001E8D6 || r 0x0001E944 0x0001E94A then
                    Just MarkNonSpacing

                else if r 0x0001E900 0x0001E921 then
                    Just LetterUppercase

                else if r 0x0001E922 0x0001E943 then
                    Just LetterLowercase

                else if e 0x0001E94B then
                    Just LetterModifier

                else if r 0x0001E950 0x0001E959 then
                    Just NumberDecimalDigit

                else if r 0x0001E95E 0x0001E95F then
                    Just PunctuationOther

                else
                    Nothing

            else if l 0x0001ED2E then
                if e 0x0001ECAB || r 0x0001ECAD 0x0001ECAF || r 0x0001ECB1 0x0001ECB4 || r 0x0001ED01 0x0001ED2D then
                    Just NumberOther

                else if e 0x0001ECAC then
                    Just SymbolOther

                else if e 0x0001ECB0 then
                    Just SymbolCurrency

                else
                    Nothing

            else if e 0x0001ED2E then
                Just SymbolOther

            else if r 0x0001ED2F 0x0001ED3D then
                Just NumberOther

            else if r 0x0001EE00 0x0001EE03 || r 0x0001EE05 0x0001EE1F || r 0x0001EE21 0x0001EE22 || e 0x0001EE24 || e 0x0001EE27 || r 0x0001EE29 0x0001EE32 then
                Just LetterOther

            else
                Nothing

        else if l 0x0001EE8A then
            if l 0x0001EE63 then
                if r 0x0001EE34 0x0001EE37 || e 0x0001EE42 || r 0x0001EE4D 0x0001EE4F || r 0x0001EE51 0x0001EE52 || e 0x0001EE54 || r 0x0001EE61 0x0001EE62 || ((modBy 2 code == 1) && (r 0x0001EE39 0x0001EE3B || r 0x0001EE47 0x0001EE4B || r 0x0001EE57 0x0001EE5F)) then
                    Just LetterOther

                else
                    Nothing

            else if e 0x0001EE64 || r 0x0001EE67 0x0001EE6A || r 0x0001EE6C 0x0001EE72 || r 0x0001EE74 0x0001EE77 || r 0x0001EE79 0x0001EE7C || e 0x0001EE7E || r 0x0001EE80 0x0001EE89 then
                Just LetterOther

            else
                Nothing

        else if l 0x0001F02F then
            if r 0x0001EE8B 0x0001EE9B || r 0x0001EEA1 0x0001EEA3 || r 0x0001EEA5 0x0001EEA9 || r 0x0001EEAB 0x0001EEBB then
                Just LetterOther

            else if r 0x0001EEF0 0x0001EEF1 then
                Just SymbolMath

            else if r 0x0001F000 0x0001F02B then
                Just SymbolOther

            else
                Nothing

        else if r 0x0001F030 0x0001F093 || r 0x0001F0A0 0x0001F0AE || r 0x0001F0B1 0x0001F0BF || r 0x0001F0C1 0x0001F0CF || r 0x0001F0D1 0x0001F0F5 || r 0x0001F10D 0x0001F1AD then
            Just SymbolOther

        else if r 0x0001F100 0x0001F10C then
            Just NumberOther

        else
            Nothing

    else if l 0x0001FABE then
        if l 0x0001F7DF then
            if r 0x0001F1E6 0x0001F202 || r 0x0001F210 0x0001F23B || r 0x0001F240 0x0001F248 || r 0x0001F250 0x0001F251 || r 0x0001F260 0x0001F265 || r 0x0001F300 0x0001F3FA || r 0x0001F400 0x0001F6D7 || r 0x0001F6DC 0x0001F6EC || r 0x0001F6F0 0x0001F6FC || r 0x0001F700 0x0001F776 || r 0x0001F77B 0x0001F7D9 then
                Just SymbolOther

            else if r 0x0001F3FB 0x0001F3FF then
                Just SymbolModifier

            else
                Nothing

        else if l 0x0001F88F then
            if r 0x0001F7E0 0x0001F7EB || e 0x0001F7F0 || r 0x0001F800 0x0001F80B || r 0x0001F810 0x0001F847 || r 0x0001F850 0x0001F859 || r 0x0001F860 0x0001F887 then
                Just SymbolOther

            else
                Nothing

        else if r 0x0001F890 0x0001F8AD || r 0x0001F8B0 0x0001F8B1 || r 0x0001F900 0x0001FA53 || r 0x0001FA60 0x0001FA6D || r 0x0001FA70 0x0001FA7C || r 0x0001FA80 0x0001FA88 || r 0x0001FA90 0x0001FABD then
            Just SymbolOther

        else
            Nothing

    else if l 0x0002B81F then
        if l 0x0001FBEF then
            if r 0x0001FABF 0x0001FAC5 || r 0x0001FACE 0x0001FADB || r 0x0001FAE0 0x0001FAE8 || r 0x0001FAF0 0x0001FAF8 || r 0x0001FB00 0x0001FB92 || r 0x0001FB94 0x0001FBCA then
                Just SymbolOther

            else
                Nothing

        else if r 0x0001FBF0 0x0001FBF9 then
            Just NumberDecimalDigit

        else if e 0x00020000 || e 0x0002A6DF || e 0x0002A700 || e 0x0002B739 || e 0x0002B740 || e 0x0002B81D then
            Just LetterOther

        else
            Nothing

    else if l 0x00031349 then
        if e 0x0002B820 || e 0x0002CEA1 || e 0x0002CEB0 || e 0x0002EBE0 || r 0x0002F800 0x0002FA1D || e 0x00030000 then
            Just LetterOther

        else
            Nothing

    else if e 0x0003134A || e 0x00031350 || e 0x000323AF then
        Just LetterOther

    else if e 0x000E0001 || r 0x000E0020 0x000E007F then
        Just OtherFormat

    else if r 0x000E0100 0x000E01EF then
        Just MarkNonSpacing

    else if r 0x000F0000 0x0010FFFD then
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
