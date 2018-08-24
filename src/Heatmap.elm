module Heatmap
    exposing
        ( Cell
        , CellWithPosition
        , Config
        , DataWithPosition
        , Msg
        , State
        , config
        , state
        , update
        , view
        , viewSparse
        , withColumnLabels
        , withDarken
        , withHoverMessage
        , withPadding
        , withRange
        , withRowLabels
        )

{-| HTML-based [heatmaps](https://en.wikipedia.org/wiki/Heat_map) in pure Elm.

The heatmaps allow you to display two-dimensional histogram data intuitively as a shading matrix. The buckets
correspond to a matrix whose individual cells are colored according to the respective bucket value.

The view is done in pure HTML and CSS and is configured to scale seamlessly (all sizes are defined in percentage value).


# Types

@docs Cell, CellWithPosition, DataWithPosition, State, Config, Msg


# Configuration options

@docs config, withDarken, withHoverMessage, withPadding, withRange, withRowLabels, withColumnLabels


# State

@docs state


# View

@docs view, viewSparse


# Update

@docs update

-}

import Array
import Color exposing (Color)
import Dict
import Html
import Html.Attributes
import Html.Events
import Json.Encode


-- CONFIG


{-| Is an opaque type holding the configuration.
-}
type Config data
    = Config (ConfigInternal data)


{-| Stores the state of the Heatmap.
-}
type alias State =
    { selected : Maybe CellWithPosition
    }


type alias ConfigInternal data =
    { toCell : data -> Cell
    , id : String
    , colorScheme : Array.Array Color.Color
    , padding : EmptyCell
    , range : Maybe ValueRange
    , columnLabels : Maybe (List String)
    , rowLabels : Maybe (List String)
    , message : Bool
    , darken : Bool
    }


{-| Is a `Cell` in the Heatmap.

`Cell`s lives in a 3-dimensional space: its 2-dimensional position on the Heatmap is determined by its row and column
in the data list, while the (non-unique) value determines its color.

If `withHoverMessage` is set, the message is shown in a tooltip when the `Cell` is hovered by the mouse.

-}
type alias Cell =
    { value : Float
    , message : String
    }


{-| Is a `Cell` in the Heatmap with its position.
-}
type alias CellWithPosition =
    { cell : Cell
    , row : Int
    , column : Int
    }


{-| Is a placehodler for a missing `Cell` in the Heatmap.
-}
type alias EmptyCell =
    { message : String
    , color : Color.Color
    }


{-| Is an optional range of values for the `Cell` values to be colored according to.

`Cell`s with value larger than `upper` are colored with the color assigned to the upper value, and likewise `Cell`s with
value smaller than `lower` are colored with the color assigned to the lower value.

-}
type alias ValueRange =
    { lower : Float
    , upper : Float
    }


{-| Holds the internap representation of the Heatmap with empty values.

Entries equals to `Nothing` are drawn on the Heatmap according to `config.padding` (see `withPadding`).

-}
type alias CellMatrix =
    List (List (Maybe Cell))


{-| Is a piece of `data` with its position in the heatmap.

The function `config.toCell` converts it to a `CellWithPosition` upon render.

-}
type alias DataWithPosition data =
    { row : Int
    , column : Int
    , data : data
    }


{-| Encapsulates the actions on the Heatmap.

While in most cases you would simply route "down" a Heatmap `Msg`, it is exported for allowing further customization.

-}
type
    Msg
    -- when a Cell is hovered by the mouse
    = OnHover CellWithPosition
      -- when the mouse leaves a Cell
    | OnLeave


{-| Creates the `Config` for a Heatmap. This takes:

  - `toCell`: A function converting the data to a Heatmap `Cell`

  - `id`: A unique identifier for the Html div containing the table

  - `colorScheme`: A color scheme (`Array` containing at least two `Color`s) for the colors displayed in the Heatmap

```
Heatmap.config { toCell = identity
             , id = "a-nice-heatmap"
             , colorScheme = Array.fromList [ Color.rgb 255 0 0
                                         , Color.rgb 255 255 255
                                         ]
             }
```

-}
config :
    { toCell : data -> Cell
    , id : String
    , colorScheme : Array.Array Color.Color
    }
    -> Config data
config { toCell, id, colorScheme } =
    Config
        { toCell = toCell
        , id = id
        , colorScheme = colorScheme
        , padding = { color = Color.black, message = "" }
        , range = Nothing
        , columnLabels = Nothing
        , rowLabels = Nothing
        , message = False
        , darken = False
        }


{-| Allows to specify labels for the Heatmap's X-axis.

    Heatmap.withColumnLabels [ "column 1", "column 2", "column 3" ] heatmapConfig

-}
withColumnLabels : List String -> Config data -> Config data
withColumnLabels columnLabels (Config configInternal) =
    Config
        { configInternal | columnLabels = Just columnLabels }


{-| Allows to specify labels for the Heatmap's Y-axis.

    Heatmap.withRowLabels [ "row 1", "row 2", "row 3" ] heatmapConfig

-}
withRowLabels : List String -> Config data -> Config data
withRowLabels rowLabels (Config configInternal) =
    Config
        { configInternal | rowLabels = Just rowLabels }


{-| Allows to specify an optional range for the Cell values.

If set, the colors are adapted to fit the range as opposed to fitting the minimum and maximum value of the data set:
`Cell`s with value larger than `upper` are colored with the color assigned to the upper value, and likewise `Cell`s
with value smaller than `lower` are colored with the color assigned to the lower value.

    Heatmap.withRange (0, 100) heatmapConfig

-}
withRange : ( Float, Float ) -> Config data -> Config data
withRange ( min, max ) (Config configInternal) =
    Config
        { configInternal
            | range = Just { lower = min, upper = max }
        }


{-| Adds the responsive behavior of the `Cell`s of showing their `message` on hover as a tooltip.

    Heatmap.withHoverMessage heatmapConfig

-}
withHoverMessage : Config data -> Config data
withHoverMessage (Config configInternal) =
    Config
        { configInternal | message = True }


{-| Adds the responsive behavior of the `Cell`s of darkening their color on hover.

    Heatmap.withDarken heatmapConfig

-}
withDarken : Config data -> Config data
withDarken (Config configInternal) =
    Config
        { configInternal | darken = True }


{-| Sets a custom color and message for the missing `Cell`s of the heatmap. The default values are (black, "").

    Heatmap.withPadding (Color.red, "no data available.") heatmapConfig

-}
withPadding : ( Color.Color, String ) -> Config data -> Config data
withPadding ( color, message ) (Config configInternal) =
    Config
        { configInternal | padding = { color = color, message = message } }


{-| Creates an intial (empty) `State`.
-}
state : State
state =
    { selected = Nothing
    }



--UPDATE


{-| Updates the component `State`.

    HeatmapMsg subMsg ->
        let
            updated =
                Heatmap.update subMsg model.heatmapState
        in
            { model | heatmapState = updated }

-}
update : Msg -> State -> State
update msg state =
    case msg of
        OnHover cellWithPosition ->
            { state | selected = Just cellWithPosition }

        OnLeave ->
            { state | selected = Nothing }



-- VIEW


{-| Takes a `Config`, a `State` and a `List (List data)` and turns it into an HTML based Heatmap.

If needed, the rows are padded (with cells having color and message specified in config.padding) to all have the same
length (the length of the longest row).

    Html.map HeatmapMsg (Heatmap.view heatmapConfig model.heatmapState model.cells)

-}
view : Config data -> State -> List (List data) -> Html.Html Msg
view ((Config { toCell, id, colorScheme, range }) as config) state data =
    let
        cellMatrix =
            data
                |> List.map (List.map toCell)

        ( minValue, maxValue ) =
            case range of
                Just rng ->
                    ( rng.lower, rng.upper )

                Nothing ->
                    cellMatrix
                        |> List.foldr (++) []
                        |> List.map .value
                        |> (\list ->
                                ( Maybe.withDefault 0 (List.minimum list)
                                , Maybe.withDefault 0 (List.maximum list)
                                )
                           )

        maxRowLength =
            cellMatrix
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 1

        paddedCellMatrix =
            cellMatrix
                |> List.map (List.map (\cell -> Just cell))
                |> List.map
                    (\cellRow ->
                        let
                            missingCellCount =
                                maxRowLength - List.length cellRow
                        in
                        if missingCellCount > 0 then
                            List.append cellRow <| List.repeat missingCellCount Nothing
                        else
                            cellRow
                    )

        colorFunction =
            colorScale colorScheme minValue maxValue
    in
    Html.div [ Html.Attributes.id id, Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ drawCells paddedCellMatrix config colorFunction state
        ]


{-| Takes a `Config`, a `State` and a `List (Int, Int, data)` and turns it into an HTML based Heatmap.

The sparse data is given as a list of (row index, column index, data). In case of conflict in (row, column),
an arbitrary conflicting item will be taken for that (row, column).

The Heatmap will have row count corresponding to the maximum row value in the list, column count corresponding
to the maximum column value in the list, and the missing values will be rendered as padding cells (see
`withPadding`).

    Html.map HeatmapMsg (Heatmap.viewSparse heatmapConfig model.heatmapState model.sparsePositionedCells)

-}
viewSparse : Config data -> State -> List (DataWithPosition data) -> Html.Html Msg
viewSparse ((Config { toCell, id, colorScheme, range }) as config) state data =
    let
        dataZeroBased : List (DataWithPosition data)
        dataZeroBased =
            let
                rowOffset =
                    data
                        |> List.map .row
                        |> List.minimum
                        |> Maybe.withDefault 0

                colOffset =
                    data
                        |> List.map .column
                        |> List.minimum
                        |> Maybe.withDefault 0
            in
            data
                |> List.map
                    (\dataWithPos ->
                        { row = dataWithPos.row - rowOffset
                        , column = dataWithPos.column - colOffset
                        , data = dataWithPos.data
                        }
                    )

        positionedCellList : List CellWithPosition
        positionedCellList =
            dataZeroBased
                |> List.map
                    (\dataWithPos ->
                        { row = dataWithPos.row
                        , column = dataWithPos.column
                        , cell = toCell dataWithPos.data
                        }
                    )

        ( minValue, maxValue ) =
            case range of
                Just rng ->
                    ( rng.lower, rng.upper )

                Nothing ->
                    positionedCellList
                        |> List.map .cell
                        |> List.map .value
                        |> (\list ->
                                ( Maybe.withDefault 0 (List.minimum list)
                                , Maybe.withDefault 0 (List.maximum list)
                                )
                           )

        maxRowLength =
            dataZeroBased
                |> List.map (\dataWP -> dataWP.row + 1)
                |> List.maximum
                |> Maybe.withDefault 1

        maxColLength =
            dataZeroBased
                |> List.map (\dataWP -> dataWP.column + 1)
                |> List.maximum
                |> Maybe.withDefault 1

        colorFunction =
            colorScale colorScheme minValue maxValue

        paddedCellMatrix =
            let
                emptyMatrix =
                    List.repeat maxRowLength (List.repeat maxColLength Nothing)

                valueDict =
                    positionedCellList
                        |> List.map (\cellWP -> ( ( cellWP.row, cellWP.column ), cellWP.cell ))
                        |> Dict.fromList

                mapCells row cellList =
                    cellList
                        |> List.indexedMap
                            (\column ->
                                \_ ->
                                    Dict.get ( row, column ) valueDict
                            )
            in
            emptyMatrix
                |> List.indexedMap mapCells
    in
    Html.div [ Html.Attributes.id id, Html.Attributes.style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
        [ drawCells paddedCellMatrix config colorFunction state
        ]


{-| Draws the `Cell`s as an HTML table.

The top row contains the (optional) x-axis labels, the leftmost column contains the (optional) y-axis labels, and the
rest consists in the colored `Cell`s. The rows in the `CellMatrix` all have the same length.

-}
drawCells : CellMatrix -> Config data -> (Float -> Color) -> State -> Html.Html Msg
drawCells cellMatrix (Config { message, darken, rowLabels, columnLabels, padding }) colorFunction state =
    let
        maxRowLength =
            cellMatrix
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 1

        -- standard placeholder for empty HTML table entry
        emptyCell =
            Html.div [ Html.Attributes.style [ ( "height", "1px" ) ] ]
                [ Html.span [ Html.Attributes.property "innerHTML" (Json.Encode.string "&nbsp;") ] []
                ]

        cellHei =
            let
                columns =
                    if columnLabels == Nothing then
                        List.length cellMatrix
                    else
                        1 + List.length cellMatrix
            in
            columns
                |> toFloat
                |> (\hei ->
                        100
                            / hei
                            |> toString
                            |> (\h -> h ++ "%")
                   )

        cellWid =
            let
                availableWidth =
                    if rowLabels == Nothing then
                        100
                    else
                        90
            in
            availableWidth
                / toFloat maxRowLength
                |> toString
                |> (\w -> w ++ "%")

        fillColor row col cell =
            let
                color =
                    colorFunction cell.value
            in
            (case state.selected of
                Just cellWithPosition ->
                    if row == cellWithPosition.row && col == cellWithPosition.column && darken then
                        darkenColor 0.7 color
                    else
                        color

                Nothing ->
                    color
            )
                |> colorToString

        firstRowLabels labels =
            let
                labelList =
                    if rowLabels /= Nothing then
                        "" :: labels
                    else
                        labels
            in
            Html.tr
                [ Html.Attributes.style
                    [ ( "width", "100%" )
                    , ( "line-height", cellHei )
                    , ( "max-height", cellHei )
                    ]
                ]
                (List.map yLabel labelList)

        paddingCell =
            let
                toolTip =
                    if message then
                        [ Html.Attributes.title padding.message ]
                    else
                        []
            in
            Html.td
                (List.append
                    [ Html.Attributes.style
                        [ ( "background-color", colorToString padding.color )
                        , ( "width", cellWid )
                        , ( "border", "2px solid black" )
                        ]
                    ]
                    toolTip
                )
                [ emptyCell ]

        toTableRow rowIndex cellList =
            let
                prepended =
                    case rowLabels of
                        Just labels ->
                            let
                                labelText =
                                    labels
                                        |> List.drop rowIndex
                                        |> List.head
                                        |> Maybe.withDefault ""
                            in
                            [ xLabel labelText ]

                        Nothing ->
                            []
            in
            Html.tr
                [ Html.Attributes.style
                    [ ( "width", "100%" )
                    , ( "line-height", cellHei )
                    , ( "max-height", cellHei )
                    ]
                ]
                (prepended
                    ++ List.indexedMap
                        (\col ->
                            \maybeCell ->
                                case maybeCell of
                                    Just cell ->
                                        Html.td (cellAttributes rowIndex col cell) [ emptyCell ]

                                    Nothing ->
                                        paddingCell
                        )
                        cellList
                )

        rows =
            cellMatrix
                |> List.indexedMap toTableRow
                |> (\tableRows ->
                        case columnLabels of
                            Just labels ->
                                firstRowLabels labels :: tableRows

                            Nothing ->
                                tableRows
                   )

        cellAttributes row col cell =
            let
                toolTip =
                    if message then
                        [ Html.Attributes.title cell.message ]
                    else
                        []
            in
            [ Html.Attributes.style
                [ ( "background-color", fillColor row col cell )
                , ( "width", cellWid )
                , ( "border", "2px solid black" )
                ]
            , Html.Events.onMouseOver (OnHover { cell = cell, row = row, column = col })
            , Html.Events.onMouseLeave OnLeave
            ]
                ++ toolTip

        xLabel text =
            Html.td
                [ Html.Attributes.style
                    [ ( "background-color", "white" )
                    , ( "font-size", "60%" )
                    , ( "text-align", "right" )
                    , ( "white-space", "nowrap" )
                    , ( "border-top", "2px solid white" )
                    , ( "border-bottom", "2px solid white" )
                    , ( "border-left", "2px solid white" )
                    , ( "width", "10%" )
                    ]
                ]
                [ Html.text text ]

        yLabel text =
            Html.td
                [ Html.Attributes.style
                    [ ( "background-color", "white" )
                    , ( "font-size", "60%" )
                    , ( "text-align", "center" )
                    , ( "white-space", "nowrap" )
                    , ( "overflow", "hidden" )
                    , ( "border-top", "2px solid white" )
                    , ( "border-right", "2px solid white" )
                    , ( "border-left", "2px solid white" )
                    , ( "width", cellWid )
                    , ( "max-width", cellWid )
                    , ( "line-width", cellWid )
                    ]
                ]
                [ Html.text text ]
    in
    Html.table
        [ Html.Attributes.style
            [ ( "background-color", "black" )
            , ( "border-collapse", "collapse" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ Html.tbody
            [ Html.Attributes.style
                [ ( "width", "100%" )
                , ( "height", "100%" )
                ]
            ]
            rows
        ]


{-| Computes the color corresponding to a value by interpolating linearly between the given min and max values.

    import Color
    import Array

    colorScale (Array.fromList [Color.rgb 0 0 0, Color.rgb 255 255 255]) 0 1 0.5
    --> Color.rgb 122 122 122

-}
colorScale : Array.Array Color.Color -> Float -> Float -> Float -> Color
colorScale colorScheme min max cellValue =
    let
        step =
            (max - min) / (toFloat <| Array.length colorScheme)

        pos =
            (cellValue - min) / step |> floor

        valueAsZeroToOne =
            (cellValue - min) / (max - min)

        nextPos =
            Basics.min (Array.length colorScheme - 1) (pos + 1)

        lastColor =
            colorScheme
                |> Array.toList
                |> List.reverse
                |> List.head
                |> Maybe.withDefault Color.black

        getColor index =
            Array.get index colorScheme
                |> Maybe.withDefault lastColor
    in
    interpolate (getColor pos) (getColor nextPos) valueAsZeroToOne


{-| Returns the RGB description of a `Color`.
-}
colorToString : Color -> String
colorToString color =
    Color.toRgb color
        |> (\c -> "rgb(" ++ toString c.red ++ ", " ++ toString c.green ++ ", " ++ toString c.blue ++ ")")


{-| Darkens the hue of a color by an offset between `0` and `1`.
-}
darkenColor : Float -> Color.Color -> Color.Color
darkenColor offset cl =
    let
        rgb =
            Color.toRgb cl

        dark color =
            floor <| toFloat color * offset
    in
    Color.rgb (dark rgb.red) (dark rgb.green) (dark rgb.blue)


{-| Computes the linear interpolation of two colors by a factor between `0` and `1`.
-}
interpolate : Color.Color -> Color.Color -> Float -> Color.Color
interpolate cl1 cl2 t =
    let
        linear : Float -> Float -> Float -> Float
        linear val i1 i2 =
            i1 + (i2 - i1) * val

        i =
            linear t
    in
    let
        cl1_ =
            Color.toRgb cl1

        cl2_ =
            Color.toRgb cl2
    in
    Color.rgba (round (i (toFloat cl1_.red) (toFloat cl2_.red)))
        (round (i (toFloat cl1_.green) (toFloat cl2_.green)))
        (round (i (toFloat cl1_.blue) (toFloat cl2_.blue)))
        (i cl1_.alpha cl2_.alpha)
