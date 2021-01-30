module Internal exposing (isAlpha, isAlphanum, isDigit, isLower, isUpper)

{-| Detect upper case characters (UTF-8 category Lu)
-}


isUpper : Char -> Bool
isUpper c =
    let
        code =
            Char.toCode c
    in
    if code < 0x1FC7 then
        if code < 0x0239 then
            if code < 0x01A5 then
                if code < 0x0185 then
                    (code >= 0x41 && code <= 0x5A)
                        || (code >= 0xC0 && code <= 0xD6)
                        || (code >= 0xD8 && code <= 0xDE)
                        || (code >= 0x0178 && code <= 0x0179)
                        || (code >= 0x0181 && code <= 0x0182)
                        || (code == 0x0184)
                        || (if modBy 2 code == 0 then
                                (code >= 0x0100 && code <= 0x0136) || (code >= 0x014A && code <= 0x0176)

                            else
                                (code >= 0x0139 && code <= 0x0147) || (code >= 0x017B && code <= 0x017D)
                           )

                else
                    (code >= 0x0186 && code <= 0x0187)
                        || (code >= 0x0189 && code <= 0x018B)
                        || (code >= 0x018E && code <= 0x0191)
                        || (code >= 0x0193 && code <= 0x0194)
                        || (code >= 0x0196 && code <= 0x0198)
                        || (code >= 0x019C && code <= 0x019D)
                        || (code >= 0x019F && code <= 0x01A0)
                        || (modBy 2 code == 0 && (code >= 0x01A2 && code <= 0x01A4))

            else if code < 0x01BB then
                (code >= 0x01A6 && code <= 0x01A7)
                    || (code == 0x01A9)
                    || (code == 0x01AC)
                    || (code >= 0x01AE && code <= 0x01AF)
                    || (code >= 0x01B1 && code <= 0x01B3)
                    || (code == 0x01B5)
                    || (code >= 0x01B7 && code <= 0x01B8)

            else
                (code == 0x01BC)
                    || (code == 0x01C4)
                    || (code == 0x01C7)
                    || (code == 0x01CA)
                    || (code == 0x01F1)
                    || (code == 0x01F4)
                    || (code >= 0x01F6 && code <= 0x01F8)
                    || (if modBy 2 code == 0 then
                            (code >= 0x01DE && code <= 0x01EE) || (code >= 0x01FA && code <= 0x0232)

                        else
                            code >= 0x01CD && code <= 0x01DB
                       )

        else if code < 0x03FC then
            if code < 0x0387 then
                (code >= 0x023A && code <= 0x023B)
                    || (code >= 0x023D && code <= 0x023E)
                    || (code == 0x0241)
                    || (code >= 0x0243 && code <= 0x0246)
                    || (code == 0x0376)
                    || (code == 0x037F)
                    || (code == 0x0386)
                    || (modBy 2 code == 0 && (code >= 0x0248 && code <= 0x024E) || (code >= 0x0370 && code <= 0x0372))

            else
                (code >= 0x0388 && code <= 0x038F)
                    || (code >= 0x0391 && code <= 0x03AB)
                    || (code == 0x03CF)
                    || (code >= 0x03D2 && code <= 0x03D4)
                    || (code == 0x03F4)
                    || (code == 0x03F7)
                    || (code >= 0x03F9 && code <= 0x03FA)
                    || (modBy 2 code == 0 && (code >= 0x03D8 && code <= 0x03EE))

        else if code < 0x1F17 then
            (code >= 0x03FD && code <= 0x042F)
                || (code >= 0x04C0 && code <= 0x04C1)
                || (code >= 0x0531 && code <= 0x0556)
                || (code >= 0x10A0 && code <= 0x10CD)
                || (code >= 0x13A0 && code <= 0x13F5)
                || (code >= 0x1C90 && code <= 0x1CBF)
                || (code >= 0x1F08 && code <= 0x1F0F)
                || (if modBy 2 code == 0 then
                        (code >= 0x0460 && code <= 0x0480)
                            || (code >= 0x048A && code <= 0x04BE)
                            || (code >= 0x04D0 && code <= 0x052E)
                            || (code >= 0x1E00 && code <= 0x1E94)
                            || (code >= 0x1E9E && code <= 0x1EFE)

                    else
                        code >= 0x04C3 && code <= 0x04CD
                   )

        else
            (code >= 0x1F18 && code <= 0x1F1D)
                || (code >= 0x1F28 && code <= 0x1F2F)
                || (code >= 0x1F38 && code <= 0x1F3F)
                || (code >= 0x1F48 && code <= 0x1F4D)
                || (code >= 0x1F59 && code <= 0x1F5F)
                || (code >= 0x1F68 && code <= 0x1F6F)
                || (code >= 0x1FB8 && code <= 0x1FBB)

    else if code < 0xA7C8 then
        if code < 0x2182 then
            if code < 0x210F then
                (code >= 0x1FC8 && code <= 0x1FCB)
                    || (code >= 0x1FD8 && code <= 0x1FDB)
                    || (code >= 0x1FE8 && code <= 0x1FEC)
                    || (code >= 0x1FF8 && code <= 0x1FFB)
                    || (code == 0x2102)
                    || (code == 0x2107)
                    || (code >= 0x210B && code <= 0x210D)

            else
                (code >= 0x2110 && code <= 0x2112)
                    || (code == 0x2115)
                    || (code >= 0x2119 && code <= 0x211D)
                    || (code >= 0x212A && code <= 0x212D)
                    || (code >= 0x2130 && code <= 0x2133)
                    || (code >= 0x213E && code <= 0x213F)
                    || (code == 0x2145)
                    || (modBy 2 code == 0 && (code >= 0x2124 && code <= 0x2128))

        else if code < 0x2C7D then
            (code == 0x2183)
                || (code >= 0x2C00 && code <= 0x2C2E)
                || (code == 0x2C60)
                || (code >= 0x2C62 && code <= 0x2C64)
                || (code >= 0x2C6D && code <= 0x2C70)
                || (code == 0x2C72)
                || (code == 0x2C75)
                || (modBy 2 code == 1 && (code >= 0x2C67 && code <= 0x2C6B))

        else
            (code >= 0x2C7E && code <= 0x2C80)
                || (code == 0x2CF2)
                || (code >= 0xA77D && code <= 0xA77E)
                || (code >= 0xA7AA && code <= 0xA7AE)
                || (code >= 0xA7B0 && code <= 0xA7B4)
                || (code == 0xA7C2)
                || (code >= 0xA7C4 && code <= 0xA7C7)
                || (if modBy 2 code == 0 then
                        (code >= 0x2C82 && code <= 0x2CE2)
                            || (code >= 0xA640 && code <= 0xA66C)
                            || (code >= 0xA680 && code <= 0xA69A)
                            || (code >= 0xA722 && code <= 0xA72E)
                            || (code >= 0xA732 && code <= 0xA76E)
                            || (code >= 0xA780 && code <= 0xA786)
                            || (code >= 0xA790 && code <= 0xA792)
                            || (code >= 0xA796 && code <= 0xA7A8)
                            || (code >= 0xA7B6 && code <= 0xA7BE)

                    else
                        (code >= 0x2CEB && code <= 0x2CED)
                            || (code >= 0xA779 && code <= 0xA77B)
                            || (code >= 0xA78B && code <= 0xA78D)
                   )

    else if code < 0x0001D537 then
        if code < 0x00016E3F then
            (code == 0xA7C9)
                || (code == 0xA7F5)
                || (code >= 0xFF21 && code <= 0xFF3A)
                || (code >= 0x00010400 && code <= 0x00010427)
                || (code >= 0x000104B0 && code <= 0x000104D3)
                || (code >= 0x00010C80 && code <= 0x00010CB2)
                || (code >= 0x000118A0 && code <= 0x000118BF)

        else
            (code >= 0x00016E40 && code <= 0x00016E5F)
                || (code >= 0x0001D400 && code <= 0x0001D419)
                || (code >= 0x0001D434 && code <= 0x0001D44D)
                || (code >= 0x0001D468 && code <= 0x0001D481)
                || (code >= 0x0001D49C && code <= 0x0001D4B5)
                || (code >= 0x0001D4D0 && code <= 0x0001D4E9)
                || (code >= 0x0001D504 && code <= 0x0001D51C)

    else if code < 0x0001D6A7 then
        (code >= 0x0001D538 && code <= 0x0001D550)
            || (code >= 0x0001D56C && code <= 0x0001D585)
            || (code >= 0x0001D5A0 && code <= 0x0001D5B9)
            || (code >= 0x0001D5D4 && code <= 0x0001D5ED)
            || (code >= 0x0001D608 && code <= 0x0001D621)
            || (code >= 0x0001D63C && code <= 0x0001D655)
            || (code >= 0x0001D670 && code <= 0x0001D689)

    else
        (code >= 0x0001D6A8 && code <= 0x0001D6C0)
            || (code >= 0x0001D6E2 && code <= 0x0001D6FA)
            || (code >= 0x0001D71C && code <= 0x0001D734)
            || (code >= 0x0001D756 && code <= 0x0001D76E)
            || (code >= 0x0001D790 && code <= 0x0001D7A8)
            || (code == 0x0001D7CA)
            || (code >= 0x0001E900 && code <= 0x0001E921)


{-| Detect lower case characters (UTF-8 category Lo)
-}
isLower : Char -> Bool
isLower c =
    let
        code =
            Char.toCode c
    in
    if code < 0x1FDF then
        if code < 0x03AB then
            if code < 0x01AF then
                if code < 0x018B then
                    (code >= 0x61 && code <= 0x7A)
                        || (code == 0xB5)
                        || (code >= 0xDF && code <= 0xF6)
                        || (code >= 0xF8 && code <= 0xFF)
                        || (code >= 0x0137 && code <= 0x0138)
                        || (code >= 0x0148 && code <= 0x0149)
                        || (code >= 0x017E && code <= 0x0180)
                        || (code == 0x0188)
                        || (if modBy 2 code == 0 then
                                (code >= 0x013A && code <= 0x0146) || (code >= 0x017A && code <= 0x017C)

                            else
                                (code >= 0x0101 && code <= 0x0135)
                                    || (code >= 0x014B && code <= 0x0177)
                                    || (code >= 0x0183 && code <= 0x0185)
                           )

                else
                    (code >= 0x018C && code <= 0x018D)
                        || (code == 0x0192)
                        || (code == 0x0195)
                        || (code >= 0x0199 && code <= 0x019B)
                        || (code == 0x019E)
                        || (code == 0x01A8)
                        || (code >= 0x01AA && code <= 0x01AB)
                        || (code == 0x01AD)
                        || (modBy 2 code == 1 && (code >= 0x01A1 && code <= 0x01A5))

            else if code < 0x023B then
                (code == 0x01B0)
                    || (code >= 0x01B9 && code <= 0x01BA)
                    || (code >= 0x01BD && code <= 0x01BF)
                    || (code == 0x01C6)
                    || (code == 0x01C9)
                    || (code >= 0x01DC && code <= 0x01DD)
                    || (code >= 0x01EF && code <= 0x01F0)
                    || (code >= 0x0233 && code <= 0x0239)
                    || (if modBy 2 code == 0 then
                            (code >= 0x01B4 && code <= 0x01B6) || (code >= 0x01CC && code <= 0x01DA)

                        else
                            (code >= 0x01DF && code <= 0x01ED)
                                || (code >= 0x01F3 && code <= 0x01F5)
                                || (code >= 0x01F9 && code <= 0x0231)
                       )

            else
                (code == 0x023C)
                    || (code >= 0x023F && code <= 0x0240)
                    || (code == 0x0242)
                    || (code >= 0x024F && code <= 0x0293)
                    || (code >= 0x0295 && code <= 0x02AF)
                    || (code == 0x0377)
                    || (code >= 0x037B && code <= 0x037D)
                    || (code == 0x0390)
                    || (modBy 2 code == 1 && (code >= 0x0247 && code <= 0x024D) || (code >= 0x0371 && code <= 0x0373))

        else if code < 0x1D78 then
            if code < 0x04CD then
                (code >= 0x03AC && code <= 0x03CE)
                    || (code >= 0x03D0 && code <= 0x03D1)
                    || (code >= 0x03D5 && code <= 0x03D7)
                    || (code >= 0x03EF && code <= 0x03F3)
                    || (code == 0x03F5)
                    || (code == 0x03F8)
                    || (code >= 0x03FB && code <= 0x03FC)
                    || (code >= 0x0430 && code <= 0x045F)
                    || (if modBy 2 code == 0 then
                            code >= 0x04C2 && code <= 0x04CC

                        else
                            (code >= 0x03D9 && code <= 0x03ED)
                                || (code >= 0x0461 && code <= 0x0481)
                                || (code >= 0x048B && code <= 0x04BF)
                       )

            else
                (code >= 0x04CE && code <= 0x04CF)
                    || (code >= 0x0560 && code <= 0x0588)
                    || (code >= 0x10D0 && code <= 0x10FA)
                    || (code >= 0x10FD && code <= 0x10FF)
                    || (code >= 0x13F8 && code <= 0x13FD)
                    || (code >= 0x1C80 && code <= 0x1C88)
                    || (code >= 0x1D00 && code <= 0x1D2B)
                    || (code >= 0x1D6B && code <= 0x1D77)
                    || (modBy 2 code == 1 && (code >= 0x04D1 && code <= 0x052F))

        else if code < 0x1F5F then
            (code >= 0x1D79 && code <= 0x1D9A)
                || (code >= 0x1E95 && code <= 0x1E9D)
                || (code >= 0x1EFF && code <= 0x1F07)
                || (code >= 0x1F10 && code <= 0x1F15)
                || (code >= 0x1F20 && code <= 0x1F27)
                || (code >= 0x1F30 && code <= 0x1F37)
                || (code >= 0x1F40 && code <= 0x1F45)
                || (code >= 0x1F50 && code <= 0x1F57)
                || (modBy 2 code == 1 && (code >= 0x1E01 && code <= 0x1E93) || (code >= 0x1E9F && code <= 0x1EFD))

        else
            (code >= 0x1F60 && code <= 0x1F67)
                || (code >= 0x1F70 && code <= 0x1F87)
                || (code >= 0x1F90 && code <= 0x1F97)
                || (code >= 0x1FA0 && code <= 0x1FA7)
                || (code >= 0x1FB0 && code <= 0x1FB7)
                || (code == 0x1FBE)
                || (code >= 0x1FC2 && code <= 0x1FC7)
                || (code >= 0x1FD0 && code <= 0x1FD7)

    else if code < 0xFAFF then
        if code < 0x2C72 then
            if code < 0x213B then
                (code >= 0x1FE0 && code <= 0x1FE7)
                    || (code >= 0x1FF2 && code <= 0x1FF7)
                    || (code == 0x210A)
                    || (code >= 0x210E && code <= 0x210F)
                    || (code == 0x2113)
                    || (code == 0x212F)
                    || (code == 0x2134)
                    || (code == 0x2139)

            else
                (code >= 0x213C && code <= 0x213D)
                    || (code >= 0x2146 && code <= 0x2149)
                    || (code == 0x214E)
                    || (code == 0x2184)
                    || (code >= 0x2C30 && code <= 0x2C5E)
                    || (code == 0x2C61)
                    || (code >= 0x2C65 && code <= 0x2C66)
                    || (code == 0x2C71)
                    || (modBy 2 code == 0 && (code >= 0x2C68 && code <= 0x2C6C))

        else if code < 0xA792 then
            (code >= 0x2C73 && code <= 0x2C74)
                || (code >= 0x2C76 && code <= 0x2C7B)
                || (code >= 0x2CE3 && code <= 0x2CE4)
                || (code == 0x2CF3)
                || (code >= 0x2D00 && code <= 0x2D2D)
                || (code >= 0xA72F && code <= 0xA731)
                || (code >= 0xA771 && code <= 0xA778)
                || (code == 0xA791)
                || (if modBy 2 code == 0 then
                        (code >= 0x2CEC && code <= 0x2CEE)
                            || (code >= 0xA77A && code <= 0xA77C)
                            || (code >= 0xA78C && code <= 0xA78E)

                    else
                        (code >= 0x2C81 && code <= 0x2CE1)
                            || (code >= 0xA641 && code <= 0xA66D)
                            || (code >= 0xA681 && code <= 0xA69B)
                            || (code >= 0xA723 && code <= 0xA72D)
                            || (code >= 0xA733 && code <= 0xA76F)
                            || (code >= 0xA77F && code <= 0xA787)
                   )

        else
            (code >= 0xA793 && code <= 0xA795)
                || (code == 0xA7AF)
                || (code == 0xA7C3)
                || (code == 0xA7F6)
                || (code == 0xA7FA)
                || (code >= 0xAB30 && code <= 0xAB5A)
                || (code >= 0xAB60 && code <= 0xAB68)
                || (code >= 0xAB70 && code <= 0xABBF)
                || (if modBy 2 code == 0 then
                        code >= 0xA7C8 && code <= 0xA7CA

                    else
                        (code >= 0xA797 && code <= 0xA7A9) || (code >= 0xA7B5 && code <= 0xA7BF)
                   )

    else if code < 0x0001D5ED then
        if code < 0x0001D44D then
            (code >= 0xFB00 && code <= 0xFB17)
                || (code >= 0xFF41 && code <= 0xFF5A)
                || (code >= 0x00010428 && code <= 0x0001044F)
                || (code >= 0x000104D8 && code <= 0x000104FB)
                || (code >= 0x00010CC0 && code <= 0x00010CF2)
                || (code >= 0x000118C0 && code <= 0x000118DF)
                || (code >= 0x00016E60 && code <= 0x00016E7F)
                || (code >= 0x0001D41A && code <= 0x0001D433)

        else
            (code >= 0x0001D44E && code <= 0x0001D467)
                || (code >= 0x0001D482 && code <= 0x0001D49B)
                || (code >= 0x0001D4B6 && code <= 0x0001D4CF)
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
            || (code >= 0x0001E922 && code <= 0x0001E943)


{-| Detect letters (UTF-8 categories Lu, Ll, Lt, Lm, Lo)
-}
isAlpha : Char -> Bool
isAlpha c =
    let
        code =
            Char.toCode c
    in
    if code < 0x319F then
        if code < 0x0F87 then
            if code < 0x094F then
                if code < 0x061F then
                    if code < 0x0375 then
                        (code >= 0x41 && code <= 0x5A)
                            || (code >= 0x61 && code <= 0x7A)
                            || (code == 0xAA)
                            || (code == 0xB5)
                            || (code == 0xBA)
                            || (code >= 0xC0 && code <= 0xD6)
                            || (code >= 0xD8 && code <= 0xF6)
                            || (code >= 0xF8 && code <= 0x02C1)
                            || (code >= 0x02C6 && code <= 0x02D1)
                            || (code >= 0x02E0 && code <= 0x02E4)
                            || (code >= 0x0370 && code <= 0x0374)
                            || (modBy 2 code == 0 && (code >= 0x02EC && code <= 0x02EE))

                    else
                        (code >= 0x0376 && code <= 0x0377)
                            || (code >= 0x037A && code <= 0x037D)
                            || (code == 0x037F)
                            || (code == 0x0386)
                            || (code >= 0x0388 && code <= 0x03F5)
                            || (code >= 0x03F7 && code <= 0x0481)
                            || (code >= 0x048A && code <= 0x052F)
                            || (code >= 0x0531 && code <= 0x0556)
                            || (code == 0x0559)
                            || (code >= 0x0560 && code <= 0x0588)
                            || (code >= 0x05D0 && code <= 0x05F2)

                else if code < 0x07B0 then
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
                        || (code >= 0x074D && code <= 0x07A5)

                else
                    (code == 0x07B1)
                        || (code >= 0x07CA && code <= 0x07EA)
                        || (code >= 0x07F4 && code <= 0x07F5)
                        || (code == 0x07FA)
                        || (code >= 0x0800 && code <= 0x0815)
                        || (code == 0x081A)
                        || (code == 0x0824)
                        || (code == 0x0828)
                        || (code >= 0x0840 && code <= 0x0858)
                        || (code >= 0x0860 && code <= 0x08C7)
                        || (code >= 0x0904 && code <= 0x0939)
                        || (code == 0x093D)

            else if code < 0x0C04 then
                if code < 0x0A71 then
                    (code == 0x0950)
                        || (code >= 0x0958 && code <= 0x0961)
                        || (code >= 0x0971 && code <= 0x0980)
                        || (code >= 0x0985 && code <= 0x09B9)
                        || (code == 0x09BD)
                        || (code == 0x09CE)
                        || (code >= 0x09DC && code <= 0x09E1)
                        || (code >= 0x09F0 && code <= 0x09F1)
                        || (code == 0x09FC)
                        || (code >= 0x0A05 && code <= 0x0A39)
                        || (code >= 0x0A59 && code <= 0x0A5E)

                else
                    (code >= 0x0A72 && code <= 0x0A74)
                        || (code >= 0x0A85 && code <= 0x0AB9)
                        || (code == 0x0ABD)
                        || (code >= 0x0AD0 && code <= 0x0AE1)
                        || (code == 0x0AF9)
                        || (code >= 0x0B05 && code <= 0x0B39)
                        || (code == 0x0B3D)
                        || (code >= 0x0B5C && code <= 0x0B61)
                        || (code == 0x0B71)
                        || (code >= 0x0B83 && code <= 0x0BB9)
                        || (code == 0x0BD0)

            else if code < 0x0D5E then
                (code >= 0x0C05 && code <= 0x0C3D)
                    || (code >= 0x0C58 && code <= 0x0C61)
                    || (code == 0x0C80)
                    || (code >= 0x0C85 && code <= 0x0CB9)
                    || (code == 0x0CBD)
                    || (code >= 0x0CDE && code <= 0x0CE1)
                    || (code >= 0x0CF1 && code <= 0x0CF2)
                    || (code >= 0x0D04 && code <= 0x0D3A)
                    || (code == 0x0D3D)
                    || (code == 0x0D4E)
                    || (code >= 0x0D54 && code <= 0x0D56)

            else
                (code >= 0x0D5F && code <= 0x0D61)
                    || (code >= 0x0D7A && code <= 0x0D7F)
                    || (code >= 0x0D85 && code <= 0x0DC6)
                    || (code >= 0x0E01 && code <= 0x0E30)
                    || (code >= 0x0E32 && code <= 0x0E33)
                    || (code >= 0x0E40 && code <= 0x0E46)
                    || (code >= 0x0E81 && code <= 0x0EB0)
                    || (code >= 0x0EB2 && code <= 0x0EB3)
                    || (code >= 0x0EBD && code <= 0x0EC4)
                    || (code == 0x0EC6)
                    || (code >= 0x0EDC && code <= 0x0F00)
                    || (code >= 0x0F40 && code <= 0x0F6C)

        else if code < 0x1CED then
            if code < 0x173F then
                if code < 0x10CF then
                    (code >= 0x0F88 && code <= 0x0F8C)
                        || (code >= 0x1000 && code <= 0x102A)
                        || (code == 0x103F)
                        || (code >= 0x1050 && code <= 0x1055)
                        || (code >= 0x105A && code <= 0x105D)
                        || (code == 0x1061)
                        || (code >= 0x1065 && code <= 0x1066)
                        || (code >= 0x106E && code <= 0x1070)
                        || (code >= 0x1075 && code <= 0x1081)
                        || (code == 0x108E)
                        || (code >= 0x10A0 && code <= 0x10CD)

                else
                    (code >= 0x10D0 && code <= 0x10FA)
                        || (code >= 0x10FC && code <= 0x135A)
                        || (code >= 0x1380 && code <= 0x138F)
                        || (code >= 0x13A0 && code <= 0x13F5)
                        || (code >= 0x13F8 && code <= 0x13FD)
                        || (code >= 0x1401 && code <= 0x166C)
                        || (code >= 0x166F && code <= 0x167F)
                        || (code >= 0x1681 && code <= 0x169A)
                        || (code >= 0x16A0 && code <= 0x16EA)
                        || (code >= 0x16F1 && code <= 0x1711)
                        || (code >= 0x1720 && code <= 0x1731)

            else if code < 0x1AA6 then
                (code >= 0x1740 && code <= 0x1751)
                    || (code >= 0x1760 && code <= 0x1770)
                    || (code >= 0x1780 && code <= 0x17B3)
                    || (code == 0x17D7)
                    || (code == 0x17DC)
                    || (code >= 0x1820 && code <= 0x1884)
                    || (code >= 0x1887 && code <= 0x18A8)
                    || (code >= 0x18AA && code <= 0x191E)
                    || (code >= 0x1950 && code <= 0x19C9)
                    || (code >= 0x1A00 && code <= 0x1A16)
                    || (code >= 0x1A20 && code <= 0x1A54)

            else
                (code == 0x1AA7)
                    || (code >= 0x1B05 && code <= 0x1B33)
                    || (code >= 0x1B45 && code <= 0x1B4B)
                    || (code >= 0x1B83 && code <= 0x1BA0)
                    || (code >= 0x1BAE && code <= 0x1BAF)
                    || (code >= 0x1BBA && code <= 0x1BE5)
                    || (code >= 0x1C00 && code <= 0x1C23)
                    || (code >= 0x1C4D && code <= 0x1C4F)
                    || (code >= 0x1C5A && code <= 0x1C7D)
                    || (code >= 0x1C80 && code <= 0x1C88)
                    || (code >= 0x1C90 && code <= 0x1CBF)
                    || (code >= 0x1CE9 && code <= 0x1CEC)

        else if code < 0x2129 then
            if code < 0x1FC1 then
                (code >= 0x1CEE && code <= 0x1CF3)
                    || (code >= 0x1CF5 && code <= 0x1CF6)
                    || (code == 0x1CFA)
                    || (code >= 0x1D00 && code <= 0x1DBF)
                    || (code >= 0x1E00 && code <= 0x1F15)
                    || (code >= 0x1F18 && code <= 0x1F1D)
                    || (code >= 0x1F20 && code <= 0x1F45)
                    || (code >= 0x1F48 && code <= 0x1F4D)
                    || (code >= 0x1F50 && code <= 0x1F57)
                    || (code >= 0x1F59 && code <= 0x1FBC)
                    || (code == 0x1FBE)

            else
                (code >= 0x1FC2 && code <= 0x1FCC)
                    || (code >= 0x1FD0 && code <= 0x1FDB)
                    || (code >= 0x1FE0 && code <= 0x1FEC)
                    || (code >= 0x1FF2 && code <= 0x1FFC)
                    || (code == 0x2071)
                    || (code == 0x207F)
                    || (code >= 0x2090 && code <= 0x209C)
                    || (code == 0x2102)
                    || (code == 0x2107)
                    || (code >= 0x210A && code <= 0x2113)
                    || (code == 0x2115)
                    || (code >= 0x2119 && code <= 0x211D)
                    || (modBy 2 code == 0 && (code >= 0x2124 && code <= 0x2128))

        else if code < 0x2CFF then
            (code >= 0x212A && code <= 0x212D)
                || (code >= 0x212F && code <= 0x2139)
                || (code >= 0x213C && code <= 0x213F)
                || (code >= 0x2145 && code <= 0x2149)
                || (code == 0x214E)
                || (code >= 0x2183 && code <= 0x2184)
                || (code >= 0x2C00 && code <= 0x2C2E)
                || (code >= 0x2C30 && code <= 0x2C5E)
                || (code >= 0x2C60 && code <= 0x2CE4)
                || (code >= 0x2CEB && code <= 0x2CEE)
                || (code >= 0x2CF2 && code <= 0x2CF3)

        else
            (code >= 0x2D00 && code <= 0x2D2D)
                || (code >= 0x2D30 && code <= 0x2D67)
                || (code == 0x2D6F)
                || (code >= 0x2D80 && code <= 0x2DDE)
                || (code == 0x2E2F)
                || (code >= 0x3005 && code <= 0x3006)
                || (code >= 0x3031 && code <= 0x3035)
                || (code >= 0x303B && code <= 0x303C)
                || (code >= 0x3041 && code <= 0x3096)
                || (code >= 0x309D && code <= 0x309F)
                || (code >= 0x30A1 && code <= 0x30FA)
                || (code >= 0x30FC && code <= 0x318E)

    else if code < 0x00010C7F then
        if code < 0xAB2F then
            if code < 0xA8FA then
                if code < 0xA716 then
                    (code >= 0x31A0 && code <= 0x31BF)
                        || (code >= 0x31F0 && code <= 0x31FF)
                        || (code >= 0x3400 && code <= 0x4DBF)
                        || (code >= 0x4E00 && code <= 0xA48C)
                        || (code >= 0xA4D0 && code <= 0xA4FD)
                        || (code >= 0xA500 && code <= 0xA60C)
                        || (code >= 0xA610 && code <= 0xA61F)
                        || (code >= 0xA62A && code <= 0xA62B)
                        || (code >= 0xA640 && code <= 0xA66E)
                        || (code >= 0xA67F && code <= 0xA69D)
                        || (code >= 0xA6A0 && code <= 0xA6E5)

                else
                    (code >= 0xA717 && code <= 0xA71F)
                        || (code >= 0xA722 && code <= 0xA788)
                        || (code >= 0xA78B && code <= 0xA7BF)
                        || (code >= 0xA7C2 && code <= 0xA7CA)
                        || (code >= 0xA7F5 && code <= 0xA801)
                        || (code >= 0xA803 && code <= 0xA805)
                        || (code >= 0xA807 && code <= 0xA80A)
                        || (code >= 0xA80C && code <= 0xA822)
                        || (code >= 0xA840 && code <= 0xA873)
                        || (code >= 0xA882 && code <= 0xA8B3)
                        || (code >= 0xA8F2 && code <= 0xA8F7)

            else if code < 0xAA43 then
                (code == 0xA8FB)
                    || (code >= 0xA8FD && code <= 0xA8FE)
                    || (code >= 0xA90A && code <= 0xA925)
                    || (code >= 0xA930 && code <= 0xA946)
                    || (code >= 0xA960 && code <= 0xA97C)
                    || (code >= 0xA984 && code <= 0xA9B2)
                    || (code == 0xA9CF)
                    || (code >= 0xA9E0 && code <= 0xA9E4)
                    || (code >= 0xA9E6 && code <= 0xA9EF)
                    || (code >= 0xA9FA && code <= 0xAA28)
                    || (code >= 0xAA40 && code <= 0xAA42)

            else
                (code >= 0xAA44 && code <= 0xAA4B)
                    || (code >= 0xAA60 && code <= 0xAA76)
                    || (code == 0xAA7A)
                    || (code >= 0xAA7E && code <= 0xAAAF)
                    || (code == 0xAAB1)
                    || (code >= 0xAAB5 && code <= 0xAAB6)
                    || (code >= 0xAAB9 && code <= 0xAABD)
                    || (code == 0xAAC0)
                    || (code >= 0xAAC2 && code <= 0xAADD)
                    || (code >= 0xAAE0 && code <= 0xAAEA)
                    || (code >= 0xAAF2 && code <= 0xAAF4)
                    || (code >= 0xAB01 && code <= 0xAB2E)

        else if code < 0x000103FF then
            if code < 0xFE6F then
                (code >= 0xAB30 && code <= 0xAB5A)
                    || (code >= 0xAB5C && code <= 0xAB69)
                    || (code >= 0xAB70 && code <= 0xABE2)
                    || (code >= 0xAC00 && code <= 0xD7FB)
                    || (code >= 0xF900 && code <= 0xFAD9)
                    || (code >= 0xFB00 && code <= 0xFB17)
                    || (code == 0xFB1D)
                    || (code >= 0xFB1F && code <= 0xFB28)
                    || (code >= 0xFB2A && code <= 0xFBB1)
                    || (code >= 0xFBD3 && code <= 0xFD3D)
                    || (code >= 0xFD50 && code <= 0xFDFB)

            else
                (code >= 0xFE70 && code <= 0xFEFC)
                    || (code >= 0xFF21 && code <= 0xFF3A)
                    || (code >= 0xFF41 && code <= 0xFF5A)
                    || (code >= 0xFF66 && code <= 0xFFDC)
                    || (code >= 0x00010000 && code <= 0x000100FA)
                    || (code >= 0x00010280 && code <= 0x000102D0)
                    || (code >= 0x00010300 && code <= 0x0001031F)
                    || (code >= 0x0001032D && code <= 0x00010340)
                    || (code >= 0x00010342 && code <= 0x00010349)
                    || (code >= 0x00010350 && code <= 0x00010375)
                    || (code >= 0x00010380 && code <= 0x0001039D)
                    || (code >= 0x000103A0 && code <= 0x000103CF)

        else if code < 0x000109BD then
            (code >= 0x00010400 && code <= 0x0001049D)
                || (code >= 0x000104B0 && code <= 0x000104D3)
                || (code >= 0x000104D8 && code <= 0x000104FB)
                || (code >= 0x00010500 && code <= 0x00010563)
                || (code >= 0x00010600 && code <= 0x00010855)
                || (code >= 0x00010860 && code <= 0x00010876)
                || (code >= 0x00010880 && code <= 0x0001089E)
                || (code >= 0x000108E0 && code <= 0x000108F5)
                || (code >= 0x00010900 && code <= 0x00010915)
                || (code >= 0x00010920 && code <= 0x00010939)
                || (code >= 0x00010980 && code <= 0x000109B7)

        else
            (code >= 0x000109BE && code <= 0x000109BF)
                || (code == 0x00010A00)
                || (code >= 0x00010A10 && code <= 0x00010A35)
                || (code >= 0x00010A60 && code <= 0x00010A7C)
                || (code >= 0x00010A80 && code <= 0x00010A9C)
                || (code >= 0x00010AC0 && code <= 0x00010AC7)
                || (code >= 0x00010AC9 && code <= 0x00010AE4)
                || (code >= 0x00010B00 && code <= 0x00010B35)
                || (code >= 0x00010B40 && code <= 0x00010B55)
                || (code >= 0x00010B60 && code <= 0x00010B72)
                || (code >= 0x00010B80 && code <= 0x00010B91)
                || (code >= 0x00010C00 && code <= 0x00010C48)

    else if code < 0x00011A9C then
        if code < 0x0001134F then
            if code < 0x00011102 then
                (code >= 0x00010C80 && code <= 0x00010CB2)
                    || (code >= 0x00010CC0 && code <= 0x00010CF2)
                    || (code >= 0x00010D00 && code <= 0x00010D23)
                    || (code >= 0x00010E80 && code <= 0x00010EA9)
                    || (code >= 0x00010EB0 && code <= 0x00010F1C)
                    || (code >= 0x00010F27 && code <= 0x00010F45)
                    || (code >= 0x00010FB0 && code <= 0x00010FC4)
                    || (code >= 0x00010FE0 && code <= 0x00010FF6)
                    || (code >= 0x00011003 && code <= 0x00011037)
                    || (code >= 0x00011083 && code <= 0x000110AF)
                    || (code >= 0x000110D0 && code <= 0x000110E8)

            else
                (code >= 0x00011103 && code <= 0x00011126)
                    || (code == 0x00011144)
                    || (code >= 0x00011147 && code <= 0x00011172)
                    || (code == 0x00011176)
                    || (code >= 0x00011183 && code <= 0x000111B2)
                    || (code >= 0x000111C1 && code <= 0x000111C4)
                    || (code >= 0x00011200 && code <= 0x0001122B)
                    || (code >= 0x00011280 && code <= 0x000112A8)
                    || (code >= 0x000112B0 && code <= 0x000112DE)
                    || (code >= 0x00011305 && code <= 0x00011339)
                    || (code == 0x0001133D)
                    || (modBy 2 code == 0 && (code >= 0x000111DA && code <= 0x000111DC))

        else if code < 0x0001167F then
            (code == 0x00011350)
                || (code >= 0x0001135D && code <= 0x00011361)
                || (code >= 0x00011400 && code <= 0x00011434)
                || (code >= 0x00011447 && code <= 0x0001144A)
                || (code >= 0x0001145F && code <= 0x000114AF)
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
                || (code >= 0x00011800 && code <= 0x0001182B)
                || (code >= 0x000118A0 && code <= 0x000118DF)
                || (code >= 0x000118FF && code <= 0x0001192F)
                || (code >= 0x000119A0 && code <= 0x000119D0)
                || (code == 0x00011A00)
                || (code >= 0x00011A0B && code <= 0x00011A32)
                || (code == 0x00011A3A)
                || (code == 0x00011A50)
                || (code >= 0x00011A5C && code <= 0x00011A89)
                || (modBy 2 code
                        == 1
                        && (code >= 0x0001193F && code <= 0x00011941)
                        || (code >= 0x000119E1 && code <= 0x000119E3)
                   )

    else if code < 0x0001D3FF then
        if code < 0x0001247F then
            (code == 0x00011A9D)
                || (code >= 0x00011AC0 && code <= 0x00011C2E)
                || (code == 0x00011C40)
                || (code >= 0x00011C72 && code <= 0x00011C8F)
                || (code >= 0x00011D00 && code <= 0x00011D30)
                || (code == 0x00011D46)
                || (code >= 0x00011D60 && code <= 0x00011D89)
                || (code == 0x00011D98)
                || (code >= 0x00011EE0 && code <= 0x00011EF2)
                || (code == 0x00011FB0)
                || (code >= 0x00012000 && code <= 0x00012399)

        else
            (code >= 0x00012480 && code <= 0x0001342E)
                || (code >= 0x00014400 && code <= 0x00016A5E)
                || (code >= 0x00016AD0 && code <= 0x00016AED)
                || (code >= 0x00016B00 && code <= 0x00016B2F)
                || (code >= 0x00016B40 && code <= 0x00016B43)
                || (code >= 0x00016B63 && code <= 0x00016B8F)
                || (code >= 0x00016E40 && code <= 0x00016E7F)
                || (code >= 0x00016F00 && code <= 0x00016F4A)
                || (code == 0x00016F50)
                || (code >= 0x00016F93 && code <= 0x00016FE1)
                || (code == 0x00016FE3)
                || (code >= 0x00017000 && code <= 0x0001BC99)

    else if code < 0x0001D789 then
        (code >= 0x0001D400 && code <= 0x0001D51C)
            || (code >= 0x0001D51E && code <= 0x0001D550)
            || (code >= 0x0001D552 && code <= 0x0001D6A5)
            || (code >= 0x0001D6A8 && code <= 0x0001D6C0)
            || (code >= 0x0001D6C2 && code <= 0x0001D6DA)
            || (code >= 0x0001D6DC && code <= 0x0001D6FA)
            || (code >= 0x0001D6FC && code <= 0x0001D714)
            || (code >= 0x0001D716 && code <= 0x0001D734)
            || (code >= 0x0001D736 && code <= 0x0001D74E)
            || (code >= 0x0001D750 && code <= 0x0001D76E)
            || (code >= 0x0001D770 && code <= 0x0001D788)

    else
        (code >= 0x0001D78A && code <= 0x0001D7A8)
            || (code >= 0x0001D7AA && code <= 0x0001D7C2)
            || (code >= 0x0001D7C4 && code <= 0x0001D7CB)
            || (code >= 0x0001E100 && code <= 0x0001E12C)
            || (code >= 0x0001E137 && code <= 0x0001E13D)
            || (code == 0x0001E14E)
            || (code >= 0x0001E2C0 && code <= 0x0001E2EB)
            || (code >= 0x0001E800 && code <= 0x0001E8C4)
            || (code >= 0x0001E900 && code <= 0x0001E943)
            || (code == 0x0001E94B)
            || (code >= 0x0001EE00 && code <= 0x0001EEBB)
            || (code >= 0x00020000 && code <= 0x0003134A)


{-| Detect digits (UTF-8 category Nd)
-}
isDigit : Char -> Bool
isDigit c =
    let
        code =
            Char.toCode c
    in
    if code < 0xA8FF then
        if code < 0x0ECF then
            if code < 0x0AE5 then
                (code >= 0x30 && code <= 0x39)
                    || (code >= 0x0660 && code <= 0x0669)
                    || (code >= 0x06F0 && code <= 0x06F9)
                    || (code >= 0x07C0 && code <= 0x07C9)
                    || (code >= 0x0966 && code <= 0x096F)
                    || (code >= 0x09E6 && code <= 0x09EF)
                    || (code >= 0x0A66 && code <= 0x0A6F)

            else
                (code >= 0x0AE6 && code <= 0x0AEF)
                    || (code >= 0x0B66 && code <= 0x0B6F)
                    || (code >= 0x0BE6 && code <= 0x0BEF)
                    || (code >= 0x0C66 && code <= 0x0C6F)
                    || (code >= 0x0CE6 && code <= 0x0CEF)
                    || (code >= 0x0D66 && code <= 0x0D6F)
                    || (code >= 0x0DE6 && code <= 0x0DEF)
                    || (code >= 0x0E50 && code <= 0x0E59)

        else if code < 0x19CF then
            (code >= 0x0ED0 && code <= 0x0ED9)
                || (code >= 0x0F20 && code <= 0x0F29)
                || (code >= 0x1040 && code <= 0x1049)
                || (code >= 0x1090 && code <= 0x1099)
                || (code >= 0x17E0 && code <= 0x17E9)
                || (code >= 0x1810 && code <= 0x1819)
                || (code >= 0x1946 && code <= 0x194F)

        else
            (code >= 0x19D0 && code <= 0x19D9)
                || (code >= 0x1A80 && code <= 0x1A99)
                || (code >= 0x1B50 && code <= 0x1B59)
                || (code >= 0x1BB0 && code <= 0x1BB9)
                || (code >= 0x1C40 && code <= 0x1C49)
                || (code >= 0x1C50 && code <= 0x1C59)
                || (code >= 0xA620 && code <= 0xA629)
                || (code >= 0xA8D0 && code <= 0xA8D9)

    else if code < 0x0001164F then
        if code < 0x00010D2F then
            (code >= 0xA900 && code <= 0xA909)
                || (code >= 0xA9D0 && code <= 0xA9D9)
                || (code >= 0xA9F0 && code <= 0xA9F9)
                || (code >= 0xAA50 && code <= 0xAA59)
                || (code >= 0xABF0 && code <= 0xABF9)
                || (code >= 0xFF10 && code <= 0xFF19)
                || (code >= 0x000104A0 && code <= 0x000104A9)

        else
            (code >= 0x00010D30 && code <= 0x00010D39)
                || (code >= 0x00011066 && code <= 0x0001106F)
                || (code >= 0x000110F0 && code <= 0x000110F9)
                || (code >= 0x00011136 && code <= 0x0001113F)
                || (code >= 0x000111D0 && code <= 0x000111D9)
                || (code >= 0x000112F0 && code <= 0x000112F9)
                || (code >= 0x00011450 && code <= 0x00011459)
                || (code >= 0x000114D0 && code <= 0x000114D9)

    else if code < 0x00011D9F then
        (code >= 0x00011650 && code <= 0x00011659)
            || (code >= 0x000116C0 && code <= 0x000116C9)
            || (code >= 0x00011730 && code <= 0x00011739)
            || (code >= 0x000118E0 && code <= 0x000118E9)
            || (code >= 0x00011950 && code <= 0x00011959)
            || (code >= 0x00011C50 && code <= 0x00011C59)
            || (code >= 0x00011D50 && code <= 0x00011D59)

    else
        (code >= 0x00011DA0 && code <= 0x00011DA9)
            || (code >= 0x00016A60 && code <= 0x00016A69)
            || (code >= 0x00016B50 && code <= 0x00016B59)
            || (code >= 0x0001D7CE && code <= 0x0001D7FF)
            || (code >= 0x0001E140 && code <= 0x0001E149)
            || (code >= 0x0001E2F0 && code <= 0x0001E2F9)
            || (code >= 0x0001E950 && code <= 0x0001E959)
            || (code >= 0x0001FBF0 && code <= 0x0001FBF9)


{-| Detect letters or digits (UTF-8 categories Lu, Ll, Lt, Lm, Lo, Nd)
-}
isAlphaNum : Char -> Bool
isAlphaNum c =
    let
        code =
            Char.toCode c
    in
    if code < 0xA4CF then
        if code < 0x0F1F then
            if code < 0x0984 then
                if code < 0x066D then
                    if code < 0x0375 then
                        (code >= 0x30 && code <= 0x39)
                            || (code >= 0x41 && code <= 0x5A)
                            || (code >= 0x61 && code <= 0x7A)
                            || (code == 0xAA)
                            || (code == 0xB5)
                            || (code == 0xBA)
                            || (code >= 0xC0 && code <= 0xD6)
                            || (code >= 0xD8 && code <= 0xF6)
                            || (code >= 0xF8 && code <= 0x02C1)
                            || (code >= 0x02C6 && code <= 0x02D1)
                            || (code >= 0x02E0 && code <= 0x02E4)
                            || (code >= 0x0370 && code <= 0x0374)
                            || (modBy 2 code == 0 && (code >= 0x02EC && code <= 0x02EE))

                    else if code < 0x0489 then
                        (code >= 0x0376 && code <= 0x0377)
                            || (code >= 0x037A && code <= 0x037D)
                            || (code == 0x037F)
                            || (code == 0x0386)
                            || (code >= 0x0388 && code <= 0x03F5)
                            || (code >= 0x03F7 && code <= 0x0481)

                    else
                        (code >= 0x048A && code <= 0x052F)
                            || (code >= 0x0531 && code <= 0x0556)
                            || (code == 0x0559)
                            || (code >= 0x0560 && code <= 0x0588)
                            || (code >= 0x05D0 && code <= 0x05F2)
                            || (code >= 0x0620 && code <= 0x064A)
                            || (code >= 0x0660 && code <= 0x0669)

                else if code < 0x07F9 then
                    (code >= 0x066E && code <= 0x066F)
                        || (code >= 0x0671 && code <= 0x06D3)
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

                else if code < 0x085F then
                    (code == 0x07FA)
                        || (code >= 0x0800 && code <= 0x0815)
                        || (code == 0x081A)
                        || (code == 0x0824)
                        || (code == 0x0828)
                        || (code >= 0x0840 && code <= 0x0858)

                else
                    (code >= 0x0860 && code <= 0x08C7)
                        || (code >= 0x0904 && code <= 0x0939)
                        || (code == 0x093D)
                        || (code == 0x0950)
                        || (code >= 0x0958 && code <= 0x0961)
                        || (code >= 0x0966 && code <= 0x096F)
                        || (code >= 0x0971 && code <= 0x0980)

            else if code < 0x0C65 then
                if code < 0x0ACF then
                    (code >= 0x0985 && code <= 0x09B9)
                        || (code == 0x09BD)
                        || (code == 0x09CE)
                        || (code >= 0x09DC && code <= 0x09E1)
                        || (code >= 0x09E6 && code <= 0x09F1)
                        || (code == 0x09FC)
                        || (code >= 0x0A05 && code <= 0x0A39)
                        || (code >= 0x0A59 && code <= 0x0A5E)
                        || (code >= 0x0A66 && code <= 0x0A6F)
                        || (code >= 0x0A72 && code <= 0x0A74)
                        || (code >= 0x0A85 && code <= 0x0AB9)
                        || (code == 0x0ABD)

                else if code < 0x0B65 then
                    (code >= 0x0AD0 && code <= 0x0AE1)
                        || (code >= 0x0AE6 && code <= 0x0AEF)
                        || (code == 0x0AF9)
                        || (code >= 0x0B05 && code <= 0x0B39)
                        || (code == 0x0B3D)
                        || (code >= 0x0B5C && code <= 0x0B61)

                else
                    (code >= 0x0B66 && code <= 0x0B6F)
                        || (code == 0x0B71)
                        || (code >= 0x0B83 && code <= 0x0BB9)
                        || (code == 0x0BD0)
                        || (code >= 0x0BE6 && code <= 0x0BEF)
                        || (code >= 0x0C05 && code <= 0x0C3D)
                        || (code >= 0x0C58 && code <= 0x0C61)

            else if code < 0x0D79 then
                if code < 0x0CF0 then
                    (code >= 0x0C66 && code <= 0x0C6F)
                        || (code == 0x0C80)
                        || (code >= 0x0C85 && code <= 0x0CB9)
                        || (code == 0x0CBD)
                        || (code >= 0x0CDE && code <= 0x0CE1)
                        || (code >= 0x0CE6 && code <= 0x0CEF)

                else
                    (code >= 0x0CF1 && code <= 0x0CF2)
                        || (code >= 0x0D04 && code <= 0x0D3A)
                        || (code == 0x0D3D)
                        || (code == 0x0D4E)
                        || (code >= 0x0D54 && code <= 0x0D56)
                        || (code >= 0x0D5F && code <= 0x0D61)
                        || (code >= 0x0D66 && code <= 0x0D6F)

            else if code < 0x0E4F then
                (code >= 0x0D7A && code <= 0x0D7F)
                    || (code >= 0x0D85 && code <= 0x0DC6)
                    || (code >= 0x0DE6 && code <= 0x0DEF)
                    || (code >= 0x0E01 && code <= 0x0E30)
                    || (code >= 0x0E32 && code <= 0x0E33)
                    || (code >= 0x0E40 && code <= 0x0E46)

            else
                (code >= 0x0E50 && code <= 0x0E59)
                    || (code >= 0x0E81 && code <= 0x0EB0)
                    || (code >= 0x0EB2 && code <= 0x0EB3)
                    || (code >= 0x0EBD && code <= 0x0EC4)
                    || (code == 0x0EC6)
                    || (code >= 0x0ED0 && code <= 0x0ED9)
                    || (code >= 0x0EDC && code <= 0x0F00)

        else if code < 0x1CE8 then
            if code < 0x173F then
                if code < 0x108F then
                    (code >= 0x0F20 && code <= 0x0F29)
                        || (code >= 0x0F40 && code <= 0x0F6C)
                        || (code >= 0x0F88 && code <= 0x0F8C)
                        || (code >= 0x1000 && code <= 0x102A)
                        || (code >= 0x103F && code <= 0x1049)
                        || (code >= 0x1050 && code <= 0x1055)
                        || (code >= 0x105A && code <= 0x105D)
                        || (code == 0x1061)
                        || (code >= 0x1065 && code <= 0x1066)
                        || (code >= 0x106E && code <= 0x1070)
                        || (code >= 0x1075 && code <= 0x1081)
                        || (code == 0x108E)

                else if code < 0x13F7 then
                    (code >= 0x1090 && code <= 0x1099)
                        || (code >= 0x10A0 && code <= 0x10CD)
                        || (code >= 0x10D0 && code <= 0x10FA)
                        || (code >= 0x10FC && code <= 0x135A)
                        || (code >= 0x1380 && code <= 0x138F)
                        || (code >= 0x13A0 && code <= 0x13F5)

                else
                    (code >= 0x13F8 && code <= 0x13FD)
                        || (code >= 0x1401 && code <= 0x166C)
                        || (code >= 0x166F && code <= 0x167F)
                        || (code >= 0x1681 && code <= 0x169A)
                        || (code >= 0x16A0 && code <= 0x16EA)
                        || (code >= 0x16F1 && code <= 0x1711)
                        || (code >= 0x1720 && code <= 0x1731)

            else if code < 0x1A1F then
                if code < 0x180F then
                    (code >= 0x1740 && code <= 0x1751)
                        || (code >= 0x1760 && code <= 0x1770)
                        || (code >= 0x1780 && code <= 0x17B3)
                        || (code == 0x17D7)
                        || (code == 0x17DC)
                        || (code >= 0x17E0 && code <= 0x17E9)

                else
                    (code >= 0x1810 && code <= 0x1819)
                        || (code >= 0x1820 && code <= 0x1884)
                        || (code >= 0x1887 && code <= 0x18A8)
                        || (code >= 0x18AA && code <= 0x191E)
                        || (code >= 0x1946 && code <= 0x19C9)
                        || (code >= 0x19D0 && code <= 0x19D9)
                        || (code >= 0x1A00 && code <= 0x1A16)

            else if code < 0x1B82 then
                (code >= 0x1A20 && code <= 0x1A54)
                    || (code >= 0x1A80 && code <= 0x1A99)
                    || (code == 0x1AA7)
                    || (code >= 0x1B05 && code <= 0x1B33)
                    || (code >= 0x1B45 && code <= 0x1B4B)
                    || (code >= 0x1B50 && code <= 0x1B59)

            else
                (code >= 0x1B83 && code <= 0x1BA0)
                    || (code >= 0x1BAE && code <= 0x1BE5)
                    || (code >= 0x1C00 && code <= 0x1C23)
                    || (code >= 0x1C40 && code <= 0x1C49)
                    || (code >= 0x1C4D && code <= 0x1C7D)
                    || (code >= 0x1C80 && code <= 0x1C88)
                    || (code >= 0x1C90 && code <= 0x1CBF)

        else if code < 0x212E then
            if code < 0x1FC1 then
                (code >= 0x1CE9 && code <= 0x1CEC)
                    || (code >= 0x1CEE && code <= 0x1CF3)
                    || (code >= 0x1CF5 && code <= 0x1CF6)
                    || (code == 0x1CFA)
                    || (code >= 0x1D00 && code <= 0x1DBF)
                    || (code >= 0x1E00 && code <= 0x1F15)
                    || (code >= 0x1F18 && code <= 0x1F1D)
                    || (code >= 0x1F20 && code <= 0x1F45)
                    || (code >= 0x1F48 && code <= 0x1F4D)
                    || (code >= 0x1F50 && code <= 0x1F57)
                    || (code >= 0x1F59 && code <= 0x1FBC)
                    || (code == 0x1FBE)

            else if code < 0x208F then
                (code >= 0x1FC2 && code <= 0x1FCC)
                    || (code >= 0x1FD0 && code <= 0x1FDB)
                    || (code >= 0x1FE0 && code <= 0x1FEC)
                    || (code >= 0x1FF2 && code <= 0x1FFC)
                    || (code == 0x2071)
                    || (code == 0x207F)

            else
                (code >= 0x2090 && code <= 0x209C)
                    || (code == 0x2102)
                    || (code == 0x2107)
                    || (code >= 0x210A && code <= 0x2113)
                    || (code == 0x2115)
                    || (code >= 0x2119 && code <= 0x211D)
                    || (code >= 0x212A && code <= 0x212D)
                    || (modBy 2 code == 0 && (code >= 0x2124 && code <= 0x2128))

        else if code < 0x2D7F then
            if code < 0x2C2F then
                (code >= 0x212F && code <= 0x2139)
                    || (code >= 0x213C && code <= 0x213F)
                    || (code >= 0x2145 && code <= 0x2149)
                    || (code == 0x214E)
                    || (code >= 0x2183 && code <= 0x2184)
                    || (code >= 0x2C00 && code <= 0x2C2E)

            else
                (code >= 0x2C30 && code <= 0x2C5E)
                    || (code >= 0x2C60 && code <= 0x2CE4)
                    || (code >= 0x2CEB && code <= 0x2CEE)
                    || (code >= 0x2CF2 && code <= 0x2CF3)
                    || (code >= 0x2D00 && code <= 0x2D2D)
                    || (code >= 0x2D30 && code <= 0x2D67)
                    || (code == 0x2D6F)

        else if code < 0x309C then
            (code >= 0x2D80 && code <= 0x2DDE)
                || (code == 0x2E2F)
                || (code >= 0x3005 && code <= 0x3006)
                || (code >= 0x3031 && code <= 0x3035)
                || (code >= 0x303B && code <= 0x303C)
                || (code >= 0x3041 && code <= 0x3096)

        else
            (code >= 0x309D && code <= 0x309F)
                || (code >= 0x30A1 && code <= 0x30FA)
                || (code >= 0x30FC && code <= 0x318E)
                || (code >= 0x31A0 && code <= 0x31BF)
                || (code >= 0x31F0 && code <= 0x31FF)
                || (code >= 0x3400 && code <= 0x4DBF)
                || (code >= 0x4E00 && code <= 0xA48C)

    else if code < 0x000110CF then
        if code < 0xFBD2 then
            if code < 0xA9DF then
                if code < 0xA806 then
                    (code >= 0xA4D0 && code <= 0xA4FD)
                        || (code >= 0xA500 && code <= 0xA60C)
                        || (code >= 0xA610 && code <= 0xA62B)
                        || (code >= 0xA640 && code <= 0xA66E)
                        || (code >= 0xA67F && code <= 0xA69D)
                        || (code >= 0xA6A0 && code <= 0xA6E5)
                        || (code >= 0xA717 && code <= 0xA71F)
                        || (code >= 0xA722 && code <= 0xA788)
                        || (code >= 0xA78B && code <= 0xA7BF)
                        || (code >= 0xA7C2 && code <= 0xA7CA)
                        || (code >= 0xA7F5 && code <= 0xA801)
                        || (code >= 0xA803 && code <= 0xA805)

                else if code < 0xA8FA then
                    (code >= 0xA807 && code <= 0xA80A)
                        || (code >= 0xA80C && code <= 0xA822)
                        || (code >= 0xA840 && code <= 0xA873)
                        || (code >= 0xA882 && code <= 0xA8B3)
                        || (code >= 0xA8D0 && code <= 0xA8D9)
                        || (code >= 0xA8F2 && code <= 0xA8F7)

                else
                    (code == 0xA8FB)
                        || (code >= 0xA8FD && code <= 0xA8FE)
                        || (code >= 0xA900 && code <= 0xA925)
                        || (code >= 0xA930 && code <= 0xA946)
                        || (code >= 0xA960 && code <= 0xA97C)
                        || (code >= 0xA984 && code <= 0xA9B2)
                        || (code >= 0xA9CF && code <= 0xA9D9)

            else if code < 0xAADF then
                if code < 0xAA79 then
                    (code >= 0xA9E0 && code <= 0xA9E4)
                        || (code >= 0xA9E6 && code <= 0xAA28)
                        || (code >= 0xAA40 && code <= 0xAA42)
                        || (code >= 0xAA44 && code <= 0xAA4B)
                        || (code >= 0xAA50 && code <= 0xAA59)
                        || (code >= 0xAA60 && code <= 0xAA76)

                else
                    (code == 0xAA7A)
                        || (code >= 0xAA7E && code <= 0xAAAF)
                        || (code == 0xAAB1)
                        || (code >= 0xAAB5 && code <= 0xAAB6)
                        || (code >= 0xAAB9 && code <= 0xAABD)
                        || (code == 0xAAC0)
                        || (code >= 0xAAC2 && code <= 0xAADD)

            else if code < 0xABEF then
                (code >= 0xAAE0 && code <= 0xAAEA)
                    || (code >= 0xAAF2 && code <= 0xAAF4)
                    || (code >= 0xAB01 && code <= 0xAB2E)
                    || (code >= 0xAB30 && code <= 0xAB5A)
                    || (code >= 0xAB5C && code <= 0xAB69)
                    || (code >= 0xAB70 && code <= 0xABE2)

            else
                (code >= 0xABF0 && code <= 0xABF9)
                    || (code >= 0xAC00 && code <= 0xD7FB)
                    || (code >= 0xF900 && code <= 0xFAD9)
                    || (code >= 0xFB00 && code <= 0xFB17)
                    || (code == 0xFB1D)
                    || (code >= 0xFB1F && code <= 0xFB28)
                    || (code >= 0xFB2A && code <= 0xFBB1)

        else if code < 0x0001091F then
            if code < 0x0001034F then
                (code >= 0xFBD3 && code <= 0xFD3D)
                    || (code >= 0xFD50 && code <= 0xFDFB)
                    || (code >= 0xFE70 && code <= 0xFEFC)
                    || (code >= 0xFF10 && code <= 0xFF19)
                    || (code >= 0xFF21 && code <= 0xFF3A)
                    || (code >= 0xFF41 && code <= 0xFF5A)
                    || (code >= 0xFF66 && code <= 0xFFDC)
                    || (code >= 0x00010000 && code <= 0x000100FA)
                    || (code >= 0x00010280 && code <= 0x000102D0)
                    || (code >= 0x00010300 && code <= 0x0001031F)
                    || (code >= 0x0001032D && code <= 0x00010340)
                    || (code >= 0x00010342 && code <= 0x00010349)

            else if code < 0x000104D7 then
                (code >= 0x00010350 && code <= 0x00010375)
                    || (code >= 0x00010380 && code <= 0x0001039D)
                    || (code >= 0x000103A0 && code <= 0x000103CF)
                    || (code >= 0x00010400 && code <= 0x0001049D)
                    || (code >= 0x000104A0 && code <= 0x000104A9)
                    || (code >= 0x000104B0 && code <= 0x000104D3)

            else
                (code >= 0x000104D8 && code <= 0x000104FB)
                    || (code >= 0x00010500 && code <= 0x00010563)
                    || (code >= 0x00010600 && code <= 0x00010855)
                    || (code >= 0x00010860 && code <= 0x00010876)
                    || (code >= 0x00010880 && code <= 0x0001089E)
                    || (code >= 0x000108E0 && code <= 0x000108F5)
                    || (code >= 0x00010900 && code <= 0x00010915)

        else if code < 0x00010BFF then
            if code < 0x00010A7F then
                (code >= 0x00010920 && code <= 0x00010939)
                    || (code >= 0x00010980 && code <= 0x000109B7)
                    || (code >= 0x000109BE && code <= 0x000109BF)
                    || (code == 0x00010A00)
                    || (code >= 0x00010A10 && code <= 0x00010A35)
                    || (code >= 0x00010A60 && code <= 0x00010A7C)

            else
                (code >= 0x00010A80 && code <= 0x00010A9C)
                    || (code >= 0x00010AC0 && code <= 0x00010AC7)
                    || (code >= 0x00010AC9 && code <= 0x00010AE4)
                    || (code >= 0x00010B00 && code <= 0x00010B35)
                    || (code >= 0x00010B40 && code <= 0x00010B55)
                    || (code >= 0x00010B60 && code <= 0x00010B72)
                    || (code >= 0x00010B80 && code <= 0x00010B91)

        else if code < 0x00010EAF then
            (code >= 0x00010C00 && code <= 0x00010C48)
                || (code >= 0x00010C80 && code <= 0x00010CB2)
                || (code >= 0x00010CC0 && code <= 0x00010CF2)
                || (code >= 0x00010D00 && code <= 0x00010D23)
                || (code >= 0x00010D30 && code <= 0x00010D39)
                || (code >= 0x00010E80 && code <= 0x00010EA9)

        else
            (code >= 0x00010EB0 && code <= 0x00010F1C)
                || (code >= 0x00010F27 && code <= 0x00010F45)
                || (code >= 0x00010FB0 && code <= 0x00010FC4)
                || (code >= 0x00010FE0 && code <= 0x00010FF6)
                || (code >= 0x00011003 && code <= 0x00011037)
                || (code >= 0x00011066 && code <= 0x0001106F)
                || (code >= 0x00011083 && code <= 0x000110AF)

    else if code < 0x00011CFF then
        if code < 0x000114CF then
            if code < 0x0001127F then
                (code >= 0x000110D0 && code <= 0x000110E8)
                    || (code >= 0x000110F0 && code <= 0x000110F9)
                    || (code >= 0x00011103 && code <= 0x00011126)
                    || (code >= 0x00011136 && code <= 0x0001113F)
                    || (code == 0x00011144)
                    || (code >= 0x00011147 && code <= 0x00011172)
                    || (code == 0x00011176)
                    || (code >= 0x00011183 && code <= 0x000111B2)
                    || (code >= 0x000111C1 && code <= 0x000111C4)
                    || (code >= 0x000111D0 && code <= 0x000111DA)
                    || (code == 0x000111DC)
                    || (code >= 0x00011200 && code <= 0x0001122B)

            else if code < 0x0001135C then
                (code >= 0x00011280 && code <= 0x000112A8)
                    || (code >= 0x000112B0 && code <= 0x000112DE)
                    || (code >= 0x000112F0 && code <= 0x000112F9)
                    || (code >= 0x00011305 && code <= 0x00011339)
                    || (code == 0x0001133D)
                    || (code == 0x00011350)

            else
                (code >= 0x0001135D && code <= 0x00011361)
                    || (code >= 0x00011400 && code <= 0x00011434)
                    || (code >= 0x00011447 && code <= 0x0001144A)
                    || (code >= 0x00011450 && code <= 0x00011459)
                    || (code >= 0x0001145F && code <= 0x000114AF)
                    || (code >= 0x000114C4 && code <= 0x000114C5)
                    || (code == 0x000114C7)

        else if code < 0x000118FE then
            if code < 0x0001167F then
                (code >= 0x000114D0 && code <= 0x000114D9)
                    || (code >= 0x00011580 && code <= 0x000115AE)
                    || (code >= 0x000115D8 && code <= 0x000115DB)
                    || (code >= 0x00011600 && code <= 0x0001162F)
                    || (code == 0x00011644)
                    || (code >= 0x00011650 && code <= 0x00011659)

            else
                (code >= 0x00011680 && code <= 0x000116AA)
                    || (code == 0x000116B8)
                    || (code >= 0x000116C0 && code <= 0x000116C9)
                    || (code >= 0x00011700 && code <= 0x0001171A)
                    || (code >= 0x00011730 && code <= 0x00011739)
                    || (code >= 0x00011800 && code <= 0x0001182B)
                    || (code >= 0x000118A0 && code <= 0x000118E9)

        else if code < 0x00011A4F then
            (code >= 0x000118FF && code <= 0x0001192F)
                || (code >= 0x00011950 && code <= 0x00011959)
                || (code >= 0x000119A0 && code <= 0x000119D0)
                || (code == 0x00011A00)
                || (code >= 0x00011A0B && code <= 0x00011A32)
                || (code == 0x00011A3A)
                || (modBy 2 code
                        == 1
                        && (code >= 0x0001193F && code <= 0x00011941)
                        || (code >= 0x000119E1 && code <= 0x000119E3)
                   )

        else
            (code == 0x00011A50)
                || (code >= 0x00011A5C && code <= 0x00011A89)
                || (code == 0x00011A9D)
                || (code >= 0x00011AC0 && code <= 0x00011C2E)
                || (code == 0x00011C40)
                || (code >= 0x00011C50 && code <= 0x00011C59)
                || (code >= 0x00011C72 && code <= 0x00011C8F)

    else if code < 0x0001D551 then
        if code < 0x00016ACF then
            (code >= 0x00011D00 && code <= 0x00011D30)
                || (code == 0x00011D46)
                || (code >= 0x00011D50 && code <= 0x00011D59)
                || (code >= 0x00011D60 && code <= 0x00011D89)
                || (code == 0x00011D98)
                || (code >= 0x00011DA0 && code <= 0x00011DA9)
                || (code >= 0x00011EE0 && code <= 0x00011EF2)
                || (code == 0x00011FB0)
                || (code >= 0x00012000 && code <= 0x00012399)
                || (code >= 0x00012480 && code <= 0x0001342E)
                || (code >= 0x00014400 && code <= 0x00016A5E)
                || (code >= 0x00016A60 && code <= 0x00016A69)

        else if code < 0x00016EFF then
            (code >= 0x00016AD0 && code <= 0x00016AED)
                || (code >= 0x00016B00 && code <= 0x00016B2F)
                || (code >= 0x00016B40 && code <= 0x00016B43)
                || (code >= 0x00016B50 && code <= 0x00016B59)
                || (code >= 0x00016B63 && code <= 0x00016B8F)
                || (code >= 0x00016E40 && code <= 0x00016E7F)

        else
            (code >= 0x00016F00 && code <= 0x00016F4A)
                || (code == 0x00016F50)
                || (code >= 0x00016F93 && code <= 0x00016FE1)
                || (code == 0x00016FE3)
                || (code >= 0x00017000 && code <= 0x0001BC99)
                || (code >= 0x0001D400 && code <= 0x0001D51C)
                || (code >= 0x0001D51E && code <= 0x0001D550)

    else if code < 0x0001E0FF then
        if code < 0x0001D735 then
            (code >= 0x0001D552 && code <= 0x0001D6A5)
                || (code >= 0x0001D6A8 && code <= 0x0001D6C0)
                || (code >= 0x0001D6C2 && code <= 0x0001D6DA)
                || (code >= 0x0001D6DC && code <= 0x0001D6FA)
                || (code >= 0x0001D6FC && code <= 0x0001D714)
                || (code >= 0x0001D716 && code <= 0x0001D734)

        else
            (code >= 0x0001D736 && code <= 0x0001D74E)
                || (code >= 0x0001D750 && code <= 0x0001D76E)
                || (code >= 0x0001D770 && code <= 0x0001D788)
                || (code >= 0x0001D78A && code <= 0x0001D7A8)
                || (code >= 0x0001D7AA && code <= 0x0001D7C2)
                || (code >= 0x0001D7C4 && code <= 0x0001D7CB)
                || (code >= 0x0001D7CE && code <= 0x0001D7FF)

    else if code < 0x0001E7FF then
        (code >= 0x0001E100 && code <= 0x0001E12C)
            || (code >= 0x0001E137 && code <= 0x0001E13D)
            || (code >= 0x0001E140 && code <= 0x0001E149)
            || (code == 0x0001E14E)
            || (code >= 0x0001E2C0 && code <= 0x0001E2EB)
            || (code >= 0x0001E2F0 && code <= 0x0001E2F9)

    else
        (code >= 0x0001E800 && code <= 0x0001E8C4)
            || (code >= 0x0001E900 && code <= 0x0001E943)
            || (code == 0x0001E94B)
            || (code >= 0x0001E950 && code <= 0x0001E959)
            || (code >= 0x0001EE00 && code <= 0x0001EEBB)
            || (code >= 0x0001FBF0 && code <= 0x0001FBF9)
            || (code >= 0x00020000 && code <= 0x0003134A)
