module Internal exposing (isLower)

{-| Detect lower case characters
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
