# elm-unicode
This package is meant to augment the `Char` one in `elm/core`, by offering Unicode-aware `isLower`/`isUpper`/`isDigit`/... functions.

# Tests #
The package can be tested using elm-test. Warning: the test will take a long time and probably crash `elm-test`.

# Updating `Unicode.elm` #
Just run `make` while inside the `generator` directory. It will download raw data from the Unicode website and build the `.elm` file
