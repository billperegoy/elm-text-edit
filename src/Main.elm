port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Mouse


port processSelection : String -> Cmd msg


port selectionResponse : (String -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { selectionState : SelectionState
    , lastSelectedText : String
    }


init : ( Model, Cmd Msg )
init =
    { selectionState = NothingSelected
    , lastSelectedText = ""
    }
        ! []



-- Update


type Msg
    = HandleMouseDown Mouse.Event
    | HandleMouseMove Mouse.Event
    | HandleMouseUp Mouse.Event
    | ProcessSelectionResponse String


type SelectionState
    = NothingSelected
    | Selecting
    | SelectionDone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleMouseDown event ->
            let
                newState =
                    case model.selectionState of
                        NothingSelected ->
                            Selecting

                        Selecting ->
                            Selecting

                        SelectionDone ->
                            SelectionDone
            in
                { model | selectionState = newState } ! []

        HandleMouseMove event ->
            let
                newState =
                    case model.selectionState of
                        NothingSelected ->
                            NothingSelected

                        Selecting ->
                            Selecting

                        SelectionDone ->
                            SelectionDone
            in
                { model | selectionState = newState } ! []

        HandleMouseUp event ->
            let
                newState =
                    case model.selectionState of
                        NothingSelected ->
                            NothingSelected

                        Selecting ->
                            SelectionDone

                        SelectionDone ->
                            SelectionDone

                effect =
                    if model.selectionState == Selecting then
                        processSelection "blah"
                    else
                        Cmd.none
            in
                { model | selectionState = SelectionDone } ! [ effect ]

        ProcessSelectionResponse text ->
            let
                newState =
                    case model.selectionState of
                        NothingSelected ->
                            NothingSelected

                        Selecting ->
                            Selecting

                        SelectionDone ->
                            NothingSelected
            in
                { model
                    | selectionState = newState
                    , lastSelectedText = text
                }
                    ! []



-- View


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ onDown model.selectionState HandleMouseDown
            , onMove model.selectionState HandleMouseMove
            , onUp model.selectionState HandleMouseUp
            , style [ ( "font-size", "16px" ), ( "width", "1000px" ), ( "height", "400px" ) ]
            ]
            [ text "This is a test of how to extract a piece of data and link it to something." ]
        , div [] [ text <| model.lastSelectedText ]
        , div [] [ text <| toString model.selectionState ]
        ]


stopOptions : SelectionState -> Html.Events.Options
stopOptions state =
    let
        preventDefault =
            state == SelectionDone
    in
        { stopPropagation = True
        , preventDefault = preventDefault
        }


onMove : SelectionState -> ((Mouse.Event -> msg) -> Attribute msg)
onMove state =
    Mouse.onWithOptions "mousemove" (stopOptions state)


onUp : SelectionState -> ((Mouse.Event -> msg) -> Attribute msg)
onUp state =
    Mouse.onWithOptions "mouseup" (stopOptions state)


onDown : SelectionState -> ((Mouse.Event -> msg) -> Attribute msg)
onDown state =
    Mouse.onWithOptions "mousedown" (stopOptions state)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    selectionResponse ProcessSelectionResponse
