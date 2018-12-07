module Internal.Color exposing
    ( Scheme
    , darken
    , get
    , lessEqualBinSearch
    , toStr
    )

{-| Provides functions and data types for colors.
-}

import Array
import Color


{-| Defines a color scheme for the Heatmap.

The "float" entry contains a list of (floating-point value, color) tuples sorted by floating-point value. A floating
point value X in the heatmap is assigned the color of the smallest element Y in the list such that X <= Y, unless X is
larger than the largest element Z, in which case it gets assigned the color of Z.

The "nan" Color is assigned to NaN entries.

The "empty" Color is assigned to empty cells (for the sparse matrix view).

-}
type alias Scheme =
    { float : Array.Array ( Float, Color.Color )
    , nan : Color.Color
    , empty : Color.Color
    }


{-| Computes the color corresponding to a value.

If scheme.float is an empty array, scheme.nan is returned.

    import Color

    scheme : Scheme
    scheme =
        { float = Array.fromList [(0, Color.red), (5, Color.yellow)]
        , nan = Color.blue
        , empty = Color.black
        }

    -- minus infinity
    get scheme -(1/0)
    --> Color.red

    get scheme 0
    --> Color.red

    get scheme 3
    --> Color.yellow

    get scheme 10
    --> Color.yellow

    -- infinity
    get scheme (1/0)
    --> Color.yellow

    -- NaN
    get scheme (0/0)
    --> Color.blue

-}
get : Scheme -> Float -> Color.Color
get colorScheme cellValue =
    if isNaN cellValue then
        colorScheme.nan

    else
        let
            floats =
                Array.map Tuple.first colorScheme.float

            index =
                lessEqualBinSearch floats cellValue
        in
        case index of
            (-2) ->
                colorScheme.empty

            (-1) ->
                colorScheme.nan

            _ ->
                Array.get index colorScheme.float
                    |> Maybe.withDefault ( 0, colorScheme.empty )
                    |> Tuple.second


{-| Returns the RGB description of a `Color`.

    import Color

    toStr Color.black
    --> "rgb(0, 0, 0)"

    toStr Color.white
    --> "rgb(255, 255, 255)"

-}
toStr : Color.Color -> String
toStr color =
    Color.toRgb color
        |> (\c -> "rgb(" ++ toString c.red ++ ", " ++ toString c.green ++ ", " ++ toString c.blue ++ ")")


{-| Darkens the hue of a color by an offset between `0` and `1`.

    import Color

    darken 1 Color.red
    --> Color.red

    darken 0 Color.blue
    --> Color.black

-}
darken : Float -> Color.Color -> Color.Color
darken offset cl =
    let
        rgb =
            Color.toRgb cl

        dark color =
            floor <| toFloat color * offset
    in
    Color.rgb (dark rgb.red) (dark rgb.green) (dark rgb.blue)


{-| Performs a binary search in a sorted list of numbers.

It searches for the index corresponding to the smallest value larger or equal than the input.

  - If the input is larger than the largest element in the list, it returns the index of the last element in the list.

  - If the input is NaN, it returns -1.

  - If the list is empty, it returns -2.

    import Array

    lessEqualBinSearch (Array.fromList [0, 2, 4, 5]) 3
    --> 2

    lessEqualBinSearch (Array.fromList [0, 2, 4, 5]) -1
    --> 0

    lessEqualBinSearch (Array.fromList [0, 2, 4, 5]) 6
    --> 3

    lessEqualBinSearch (Array.fromList []) 3
    --> -2

    lessEqualBinSearch (Array.fromList [0, 2, 4, 5]) (0/0)
    --> -1

-}
lessEqualBinSearch : Array.Array Float -> Float -> Int
lessEqualBinSearch listOfNumbers searchValue =
    let
        len =
            Array.length listOfNumbers

        first =
            Array.get 0 listOfNumbers
                |> Maybe.withDefault 0

        last =
            Array.get (len - 1) listOfNumbers
                |> Maybe.withDefault first
    in
    if isNaN searchValue then
        -1

    else if len == 0 then
        -2

    else if searchValue > last then
        len - 1

    else if searchValue <= first then
        0

    else
        recursiveLEBinSrc listOfNumbers searchValue 0 (len - 1) len


{-| Performs a recursive binary search.

It searches for the index corresponding to
the smallest value larger or equal than the input. It is guaranteed to return a
valid index of the list, as the caller of this function deals with corner cases
before calling it.

-}
recursiveLEBinSrc : Array.Array Float -> Float -> Int -> Int -> Int -> Int
recursiveLEBinSrc listOfNumbers searchValue lo hi len =
    if lo > hi then
        -1

    else
        let
            mid =
                lo + (hi - lo) // 2

            midValue =
                Array.get mid listOfNumbers

            prevValue =
                Array.get (mid - 1) listOfNumbers
        in
        case midValue of
            Nothing ->
                -1

            Just value ->
                case prevValue of
                    Just prev ->
                        if searchValue < value then
                            if prev <= searchValue then
                                mid

                            else
                                recursiveLEBinSrc listOfNumbers searchValue lo (mid - 1) len

                        else if searchValue > value then
                            recursiveLEBinSrc listOfNumbers searchValue (mid + 1) hi len

                        else
                            mid

                    Nothing ->
                        if searchValue > value then
                            mid + 1

                        else
                            mid
