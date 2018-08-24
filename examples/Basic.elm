module Basic exposing (..)

import Array
import Color
import Dict exposing (Dict)
import Heatmap
import Html exposing (Html)
import Html.Attributes
import Html.Events


{- Example Heatmap page. To run the example cd to the folder containing this module and run elm-reactor. -}


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }


type alias Model =
    { cells : List (List Heatmap.Cell)
    , sparseCells : List (Heatmap.DataWithPosition Heatmap.Cell)
    , colorScheme : Array.Array Color.Color
    , text : String
    , state : Heatmap.State
    , sparseState : Heatmap.State
    , width : Int
    , height : Int
    , darken : Bool
    , columnLabels : Bool
    , rowLabels : Bool
    , hoverMessage : Bool
    }


type Msg
    = NoOp
    | HeatmapMsg Heatmap.Msg
    | HeatmapSparseMsg Heatmap.Msg
    | InsertCell
    | UpdateWidth String
    | UpdateHeight String
    | ToggleDarken
    | ToggleColumnLabels
    | ToggleRowLabels
    | ToggleHoverMessage


sunrise : Array.Array Color.Color
sunrise =
    Array.fromList
        [ Color.rgb 255 0 0
        , Color.rgb 255 255 0
        , Color.rgb 255 255 255
        ]


initialModel : Model
initialModel =
    { cells = cellMatrix
    , sparseCells = sparseCellsList
    , colorScheme = sunrise
    , text = "You're not hovering anything"
    , state = Heatmap.state
    , sparseState = Heatmap.state
    , width = 500
    , height = 500
    , darken = True
    , columnLabels = True
    , rowLabels = True
    , hoverMessage = True
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        InsertCell ->
            { model | cells = insertOneCell model.cells }

        ToggleDarken ->
            { model | darken = not model.darken }

        ToggleColumnLabels ->
            { model | columnLabels = not model.columnLabels }

        ToggleRowLabels ->
            { model | rowLabels = not model.rowLabels }

        ToggleHoverMessage ->
            { model | hoverMessage = not model.hoverMessage }

        UpdateWidth wid ->
            { model | width = Result.withDefault 100 <| String.toInt wid }

        UpdateHeight hei ->
            { model | height = Result.withDefault 100 <| String.toInt hei }

        HeatmapMsg msg ->
            let
                state =
                    Heatmap.update msg model.state

                text =
                    Maybe.map
                        (\cellWithPos ->
                            "You're hovering a cell with value "
                                ++ toString cellWithPos.cell.value
                                ++ " in row "
                                ++ toString (cellWithPos.row + 1)
                                ++ ", column "
                                ++ toString (cellWithPos.column + 1)
                                ++ "."
                        )
                        state.selected
                        |> Maybe.withDefault "You're not hovering anything"
            in
            { model
                | text = text
                , state = state
            }

        HeatmapSparseMsg msg ->
            let
                state =
                    Heatmap.update msg model.sparseState
            in
            { model
                | sparseState = state
            }


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "center" ]
        [ Html.p []
            [ Html.text model.text ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "100"
                , Html.Attributes.max "1000"
                , Html.Attributes.value <| toString model.width
                , Html.Events.onInput UpdateWidth
                ]
                []
            , Html.text ("Width: " ++ toString model.width)
            ]
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "100"
                , Html.Attributes.max "1000"
                , Html.Attributes.value <| toString model.height
                , Html.Events.onInput UpdateHeight
                ]
                []
            , Html.text ("Height: " ++ toString model.height)
            ]
        , Html.div []
            [ checkbox ToggleDarken "with darkening" model.darken
            , checkbox ToggleColumnLabels "with x-axis ticks" model.columnLabels
            , checkbox ToggleRowLabels "with y-axis ticks" model.rowLabels
            , checkbox ToggleHoverMessage "with message on hover" model.hoverMessage
            ]
        , Html.div []
            [ Html.button [ Html.Events.onClick InsertCell ] [ Html.text "insert a cell" ]
            ]
        , Html.hr []
            []
        , Html.p [] [ Html.text "Dense table" ]
        , Html.div
            [ Html.Attributes.style
                [ ( "width", toString model.width ++ "px" )
                , ( "height", toString model.height ++ "px" )
                ]
            ]
            [ Html.map HeatmapMsg <| Heatmap.view (heatmapConfig model) model.state model.cells
            ]
        , Html.hr []
            []
        , Html.p [] [ Html.text "Sparse table" ]
        , Html.div
            [ Html.Attributes.style
                [ ( "width", "500px" )
                , ( "height", "200px" )
                ]
            ]
            [ Html.map HeatmapSparseMsg <| Heatmap.viewSparse (heatmapConfigSparse model) model.sparseState model.sparseCells
            ]
        ]


checkbox : Msg -> String -> Bool -> Html Msg
checkbox msg name checked =
    Html.label []
        [ Html.input [ Html.Attributes.type_ "checkbox", Html.Attributes.checked checked, Html.Events.onClick msg ] []
        , Html.text name
        ]


heatmapConfig : Model -> Heatmap.Config Heatmap.Cell
heatmapConfig model =
    let
        cols =
            8

        rows =
            List.length model.cells

        withHover =
            if model.hoverMessage then
                Heatmap.withHoverMessage
            else
                identity

        withDarken =
            if model.darken then
                Heatmap.withDarken
            else
                identity

        withColumnLabels =
            if model.columnLabels then
                Heatmap.withColumnLabels (List.range 1 cols |> List.map (\idx -> "col " ++ toString idx))
            else
                identity

        withRowLabels =
            if model.rowLabels then
                Heatmap.withRowLabels (List.range 1 rows |> List.map (\idx -> "row " ++ toString idx))
            else
                identity
    in
    Heatmap.config
        { toCell = identity
        , id = "a-nice-heatmap"
        , colorScheme = model.colorScheme
        }
        |> withHover
        |> withDarken
        |> Heatmap.withPadding ( Color.black, "no data available" )
        |> withColumnLabels
        |> withRowLabels


heatmapConfigSparse : Model -> Heatmap.Config Heatmap.Cell
heatmapConfigSparse model =
    Heatmap.config
        { toCell = identity
        , id = "a-nice-heatmap"
        , colorScheme = model.colorScheme
        }
        |> Heatmap.withHoverMessage
        |> Heatmap.withDarken
        |> Heatmap.withPadding ( Color.black, "no data available" )


cellMatrix : List (List Heatmap.Cell)
cellMatrix =
    List.range 1 25
        |> List.map
            (\col ->
                List.range 1 8
                    |> List.map
                        (\row ->
                            { value = toFloat <| ((col + row) * 3) % 10
                            , message = "some notes " ++ toString (col + row)
                            }
                        )
            )


sparseCellsList : List (Heatmap.DataWithPosition Heatmap.Cell)
sparseCellsList =
    let
        entry row col =
            { row = row
            , column = col
            , data =
                { value = toFloat <| ((col + row) * 3) % 10
                , message = "cell in row " ++ toString row ++ ", column " ++ toString col
                }
            }
    in
    [ entry 0 0
    , entry 0 1
    , entry 0 2
    , entry 0 4
    , entry 0 8
    , entry 0 12
    , entry 1 0
    , entry 1 4
    , entry 1 8
    , entry 1 9
    , entry 1 11
    , entry 1 12
    , entry 2 0
    , entry 2 1
    , entry 2 2
    , entry 2 4
    , entry 2 8
    , entry 2 10
    , entry 2 12
    , entry 3 0
    , entry 3 4
    , entry 3 8
    , entry 3 12
    , entry 4 0
    , entry 4 1
    , entry 4 2
    , entry 4 4
    , entry 4 5
    , entry 4 6
    , entry 4 8
    , entry 4 12
    ]


insertOneCell : List (List Heatmap.Cell) -> List (List Heatmap.Cell)
insertOneCell matrix =
    let
        aNumber =
            (List.length matrix * 3) % 10

        newCell : Heatmap.Cell
        newCell =
            { value = toFloat <| aNumber
            , message = "some notes " ++ toString aNumber
            }

        lastRow : List Heatmap.Cell
        lastRow =
            matrix
                |> List.reverse
                |> List.head
                |> Maybe.withDefault []
    in
    if List.length lastRow == 8 then
        List.append matrix [ [ newCell ] ]
    else
        matrix
            |> List.take (List.length matrix - 1)
            |> (\lst -> List.append lst [ List.append lastRow [ newCell ] ])
