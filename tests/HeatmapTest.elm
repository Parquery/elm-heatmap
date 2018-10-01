module HeatmapTest exposing (binarySearch, getColor)

import Array
import Color
import Expect
import Heatmap
import Internal.Color
import Test
import Time


getColor : Test.Test
getColor =
    let
        scheme : Internal.Color.Scheme
        scheme =
            { float = Array.fromList [ ( -5, Color.blue ), ( 3, Color.lightBlue ), ( 13, Color.white ) ]
            , nan = Color.red
            , empty = Color.black
            }

        schemeSizeOne : Internal.Color.Scheme
        schemeSizeOne =
            { scheme | float = Array.fromList [ ( 0, Color.blue ) ] }

        schemeSizeZero : Internal.Color.Scheme
        schemeSizeZero =
            { scheme | float = Array.empty }
    in
    Test.describe "Test the 'get' function in the heatmap."
        [ Test.test "Test special cases in normal colorscheme." <|
            \() ->
                let
                    specialVals =
                        [ -1 / 0 -- negative infinity
                        , 0 / 0 -- NaN
                        , 1 / 0 -- positive infinity
                        ]
                in
                Expect.equal
                    [ Color.blue
                    , scheme.nan
                    , Color.white
                    ]
                    (List.map (Internal.Color.get scheme) specialVals)
        , Test.test "Test special cases in scheme with only one color." <|
            \() ->
                let
                    specialVals =
                        [ -1 / 0 -- negative infinity
                        , 0 / 0 -- NaN
                        , 1 / 0 -- positive infinity
                        ]
                in
                Expect.equal
                    [ Color.blue
                    , schemeSizeOne.nan
                    , Color.blue
                    ]
                    (List.map (Internal.Color.get schemeSizeOne) specialVals)
        , Test.test "Test special cases in empty scheme." <|
            \() ->
                let
                    specialVals =
                        [ -1 / 0 -- negative infinity
                        , 0 / 0 -- NaN
                        , 1 / 0 -- positive infinity
                        ]
                in
                Expect.equal
                    [ schemeSizeZero.empty
                    , schemeSizeZero.nan
                    , schemeSizeZero.empty
                    ]
                    (List.map (Internal.Color.get schemeSizeZero) specialVals)
        , Test.test "Test a range of floats in normal scheme." <|
            \() ->
                let
                    floatVals =
                        [ -20
                        , -8
                        , -3
                        , 0
                        , 5
                        , 198
                        ]
                in
                Expect.equal
                    [ Color.blue
                    , Color.blue
                    , Color.lightBlue
                    , Color.lightBlue
                    , Color.white
                    , Color.white
                    ]
                    (List.map (Internal.Color.get scheme) floatVals)
        , Test.test "Test a range of floats in scheme with only one color." <|
            \() ->
                let
                    floatVals =
                        [ -20
                        , -8
                        , -3
                        , 0
                        , 5
                        , 198
                        ]
                in
                Expect.equal
                    [ Color.blue
                    , Color.blue
                    , Color.blue
                    , Color.blue
                    , Color.blue
                    , Color.blue
                    ]
                    (List.map (Internal.Color.get schemeSizeOne) floatVals)
        , Test.test "Test a range of floats in empty scheme." <|
            \() ->
                let
                    floatVals =
                        [ -20
                        , -8
                        , -3
                        , 0
                        , 5
                        , 198
                        ]
                in
                Expect.equal
                    [ schemeSizeZero.empty
                    , schemeSizeZero.empty
                    , schemeSizeZero.empty
                    , schemeSizeZero.empty
                    , schemeSizeZero.empty
                    , schemeSizeZero.empty
                    ]
                    (List.map (Internal.Color.get schemeSizeZero) floatVals)
        ]


binarySearch : Test.Test
binarySearch =
    Test.describe "Test the binary search in the heatmap."
        [ Test.test "Test binary search with empty array." <|
            \() ->
                Expect.equal
                    -2
                    (Internal.Color.lessEqualBinSearch Array.empty 21.0)
        , Test.test "Test binary search with element smaller than all of the list's." <|
            \() ->
                Expect.equal
                    0
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) 1.0)
        , Test.test "Test binary search with NaN." <|
            \() ->
                Expect.equal
                    -1
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) (0 / 0))
        , Test.test "Test binary search with infinity." <|
            \() ->
                Expect.equal
                    2
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) (1 / 0))
        , Test.test "Test binary search with negative infinity." <|
            \() ->
                Expect.equal
                    0
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) (-1 / 0))
        , Test.test "Test binary search with element larger than all of the list's." <|
            \() ->
                Expect.equal
                    2
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) 27.0)
        , Test.test "Test binary search with element somewhere in the list (1)." <|
            \() ->
                Expect.equal
                    2
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) 4.0)
        , Test.test "Test binary search with element somewhere in the list (2)." <|
            \() ->
                Expect.equal
                    2
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) 4.5)
        , Test.test "Test binary search with element somewhere in the list (3)." <|
            \() ->
                Expect.equal
                    0
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ -2.3, 0, 1, 124.5 ]) -2.3)
        , Test.test "Test binary search with element somewhere in the list (4)." <|
            \() ->
                Expect.equal
                    1
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 2.3, 3.4, 4.5 ]) 3.0)
        , Test.test "Test binary search with element somewhere in the list (5)." <|
            \() ->
                Expect.equal
                    1
                    (Internal.Color.lessEqualBinSearch (Array.fromList [ 0, 5 ]) 3.0)
        , Test.test "Test binary search with element somewhere in the long list." <|
            \() ->
                Expect.equal
                    20
                    (let
                        longList =
                            List.range 1 1000
                                |> List.map toFloat
                                |> Array.fromList
                     in
                     Internal.Color.lessEqualBinSearch longList 20
                    )
        ]
