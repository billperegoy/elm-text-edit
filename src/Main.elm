port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Mouse
import Json.Decode
import Json.Decode.Pipeline


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


type alias SelectResult =
    { text : String
    , id : String
    , startOffset : Int
    , endOffset : Int
    }


decoder : Json.Decode.Decoder SelectResult
decoder =
    Json.Decode.Pipeline.decode SelectResult
        |> Json.Decode.Pipeline.required "text" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "startOffset" Json.Decode.int
        |> Json.Decode.Pipeline.required "endOffset" Json.Decode.int


type alias Model =
    { data : String
    , selectionState : SelectionState
    , lastSelectedText : String
    }


initData : String
initData =
    "Receiving the top-maul from Starbuck, he advanced towards the main-mast with the hammer uplifted in one hand, exhibiting the gold with the other, and with a high raised voice exclaiming: “Whosoever of ye raises me a white-headed whale with a wrinkled brow and a crooked jaw; whosoever of ye raises me that white-headed whale, with three holes punctured in his starboard fluke—look ye, whosoever of ye raises me that same white whale, he shall have this gold ounce, my boys!”\n\n“Huzza! huzza!” cried the seamen, as with swinging tarpaulins they hailed the act of nailing the gold to the mast.\n\n“It’s a white whale, I say,” resumed Ahab, as he threw down the topmaul: “a white whale. Skin your eyes for him, men; look sharp for white water; if ye see but a bubble, sing out.”\n\nAll this while Tashtego, Daggoo, and Queequeg had looked on with even more intense interest and surprise than the rest, and at the mention of the wrinkled brow and crooked jaw they had started as if each was separately touched by some specific recollection.\n\n“Captain Ahab,” said Tashtego, “that white whale must be the same that some call Moby Dick.”\n\n“Moby Dick?” shouted Ahab. “Do ye know the white whale then, Tash?”\n"


init : ( Model, Cmd Msg )
init =
    { data = initData
    , selectionState = NothingSelected
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
                        processSelection ""
                    else
                        Cmd.none
            in
                { model | selectionState = SelectionDone } ! [ effect ]

        ProcessSelectionResponse result ->
            let
                decodeResult =
                    Json.Decode.decodeString decoder result

                decodedResult =
                    case decodeResult of
                        Ok x ->
                            x

                        Err _ ->
                            { text = "", startOffset = 0, endOffset = 0 }

                newData =
                    String.slice 0 decodedResult.startOffset model.data
                        ++ "LINK"
                        ++ String.slice decodedResult.endOffset (String.length model.data) model.data

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
                    | data = newData
                    , selectionState = newState
                    , lastSelectedText = decodedResult.text
                }
                    ! []



-- View


view : Model -> Html Msg
view model =
    div []
        [ div
            [ id "1"
            , style
                [ ( "font-size", "24px" )
                , ( "border", "1px solid black" )
                , ( "width", "800px" )
                , ( "padding", "5px" )
                , ( "margin-bottom", "50px" )
                ]
            , onDown model.selectionState HandleMouseDown
            , onMove model.selectionState HandleMouseMove
            , onUp model.selectionState HandleMouseUp
            ]
            [ text model.data ]
        , div [ style [ ( "font-size", "24px" ) ] ] [ text <| model.lastSelectedText ]
        , div [ style [ ( "font-size", "24px" ) ] ] [ text <| toString model.selectionState ]
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
