module Basic exposing (..)

import Array
import Color
import Dict exposing (Dict)
import Heatmap
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Internal.Color


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
    , sparseCells : List (Heatmap.SparseData Heatmap.Cell)
    , colorScheme : Internal.Color.Scheme
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


sunrise : List ( Float, Color.Color )
sunrise =
    [ ( 0, Color.rgb 255 0 0 )
    , ( 5, Color.rgb 255 125 0 )
    , ( 8, Color.rgb 255 125 125 )
    , ( 10, Color.rgb 255 200 125 )
    , ( 13, Color.rgb 255 200 200 )
    , ( 15, Color.rgb 255 255 200 )
    , ( 20, Color.rgb 255 255 255 )
    ]


initialModel : Model
initialModel =
    { cells = cellMatrix
    , sparseCells = sparseCellsList
    , colorScheme = Heatmap.colorSchemeInit { float = sunrise, empty = Color.black, nan = Color.black }
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
            [ Html.map HeatmapSparseMsg <| Heatmap.viewSparse (heatmapConfigSparse model) model.sparseState model.sparseCells (Just ( 7, 15 ))
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


sparseCellsList : List (Heatmap.SparseData Heatmap.Cell)
sparseCellsList =
    let
        entry row col =
            { row = row
            , column = col
            , data =
                { value = toFloat <| ((col + row) * 3) % 10
                , message = "cell in row " ++ toString (row + 1) ++ ", column " ++ toString (col + 1)
                }
            }
    in
    [ entry 1 1
    , entry 1 2
    , entry 1 3
    , entry 1 5
    , entry 1 9
    , entry 1 13
    , entry 2 1
    , entry 2 5
    , entry 2 9
    , entry 2 10
    , entry 2 12
    , entry 2 13
    , entry 3 1
    , entry 3 2
    , entry 3 3
    , entry 3 5
    , entry 3 9
    , entry 3 11
    , entry 3 13
    , entry 4 1
    , entry 4 5
    , entry 4 9
    , entry 4 13
    , entry 5 1
    , entry 5 2
    , entry 5 3
    , entry 5 5
    , entry 5 6
    , entry 5 7
    , entry 5 9
    , entry 5 13
    ]


insertOneCell : List (List Heatmap.Cell) -> List (List Heatmap.Cell)
insertOneCell matrix =
    let
        aNumber =
            (List.length matrix * 3) % 15

        newCell : Heatmap.Cell
        newCell =
            { value = toFloat <| aNumber
            , message = "value is " ++ toString aNumber
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
