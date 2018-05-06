port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Mouse
import Json.Decode
import Paragraph
import SelectResult


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
    { data : List Paragraph.Paragraph
    , nextId : Int
    , selectionState : SelectionState
    , lastSelectedText : String
    }


initData : List Paragraph.Paragraph
initData =
    [ ( 1, "Nulla rhoncus eu justo eget dictum. Praesent scelerisque in orci ut vulputate. Mauris faucibus neque neque, dapibus sollicitudin purus eleifend at. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Praesent sollicitudin facilisis metus tempor ultrices. Integer tincidunt finibus fermentum. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Nullam convallis ante quis turpis consequat facilisis. Suspendisse potenti. Mauris id leo sed velit porttitor fermentum nec ac quam. Phasellus suscipit elit nec ante fermentum condimentum vitae ac justo." )
    , ( 2, "Praesent nisl magna, scelerisque vulputate faucibus at, aliquam eleifend lorem. Nunc lobortis diam sed dictum accumsan. Nulla bibendum aliquet justo, id finibus ipsum. Phasellus turpis justo, dignissim nec finibus at, pellentesque ac metus. Sed leo felis, lobortis non venenatis nec, fringilla et nunc. Ut bibendum mauris pharetra egestas luctus. Duis nec mollis ex. Donec tortor quam, luctus id congue vel, commodo eu dolor. Maecenas tristique, augue et efficitur tempor, tortor nunc lobortis nisi, id faucibus lorem neque ut elit. Nunc purus nunc, porta at massa sit amet, imperdiet ornare leo. Donec dui elit, convallis nec tellus et, dapibus cursus ante. Nam sed felis varius, ultrices lacus ut, rhoncus risus. Sed quis massa diam. Aliquam quis sodales enim. Interdum et malesuada fames ac ante ipsum primis in faucibus." )
    , ( 3, "Nulla semper neque a pulvinar rhoncus. Sed blandit quis tortor eu aliquam. Donec volutpat dolor lorem, non convallis nibh laoreet at. Integer ornare dapibus nisl, non feugiat eros. In euismod lacus eget lacinia ornare. Donec consectetur posuere est, ac semper dui efficitur nec. Donec egestas justo vitae lectus aliquet varius. Curabitur aliquam enim elit, et elementum tellus tincidunt quis. Maecenas nibh risus, ultrices aliquam vehicula eget, feugiat sit amet diam. Donec ante velit, dapibus vel semper sit amet, pellentesque non velit. Praesent eget lacus non diam eleifend faucibus a eu justo. Curabitur a ipsum est. Aliquam quis sodales metus. Nam sapien erat, volutpat et ligula id, vehicula semper quam." )
    , ( 4, "Nam porta odio ac ipsum blandit pretium non sit amet urna. Duis fermentum dui magna, in posuere est posuere ut. Sed congue porttitor turpis, nec egestas nibh elementum non. Mauris ex augue, lobortis ac libero in, blandit consequat nisl. In lectus odio, egestas vel fringilla vestibulum, sagittis eget urna. Sed gravida posuere ex, a molestie elit pretium sit amet. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi nunc sapien, gravida quis molestie vel, pretium sed sapien. Suspendisse tellus elit, porta et bibendum nec, ultricies et elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Pellentesque fringilla nec dui in ultricies. Praesent placerat nulla eu ante ullamcorper, ac rhoncus orci rutrum. Cras tristique eros auctor nibh vehicula placerat. Interdum et malesuada fames ac ante ipsum primis in faucibus. Nam fermentum eu odio non egestas. Nam scelerisque interdum lacus, sit amet elementum tellus." )
    ]


init : ( Model, Cmd Msg )
init =
    { data = initData
    , nextId = 5
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
                    if model.selectionState == NothingSelected then
                        Selecting
                    else
                        model.selectionState
            in
                { model | selectionState = newState } ! []

        HandleMouseMove event ->
            model ! []

        HandleMouseUp event ->
            let
                newState =
                    if model.selectionState == Selecting then
                        SelectionDone
                    else
                        model.selectionState

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
                    Json.Decode.decodeString SelectResult.decoder result

                decodedResult =
                    case decodeResult of
                        Ok res ->
                            res

                        Err _ ->
                            { text = "", id = "", startOffset = 0, endOffset = 0 }

                newData =
                    List.map (addLink decodedResult) model.data

                newState =
                    if model.selectionState == SelectionDone then
                        NothingSelected
                    else
                        model.selectionState
            in
                { model
                    | data = newData
                    , selectionState = newState
                    , lastSelectedText = decodedResult.text
                }
                    ! []



-- View


addLink : SelectResult.SelectResult -> Paragraph.Paragraph -> Paragraph.Paragraph
addLink result paragraph =
    let
        dataId =
            Paragraph.id paragraph |> toString
    in
        if dataId == result.id then
            addLinkText result paragraph
        else
            paragraph


addLinkText : SelectResult.SelectResult -> Paragraph.Paragraph -> Paragraph.Paragraph
addLinkText result paragraph =
    let
        dataValue =
            Paragraph.text paragraph
    in
        if result.startOffset == result.endOffset then
            paragraph
        else
            Paragraph.create
                (Paragraph.id paragraph)
                (insertText result.startOffset result.endOffset dataValue)


insertText : Int -> Int -> String -> String
insertText startOffset endOffset text =
    (String.slice 0 startOffset text
        ++ "LINK"
        ++ String.slice endOffset (String.length text) text
    )


paragraph : SelectionState -> Paragraph.Paragraph -> Html Msg
paragraph selectionState paragraph =
    p
        [ id (Paragraph.id paragraph |> toString)
        , onDown selectionState HandleMouseDown
        , onMove selectionState HandleMouseMove
        , onUp selectionState HandleMouseUp
        ]
        [ text (Paragraph.text paragraph) ]


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style
                [ ( "font-size", "24px" )
                , ( "border", "1px solid black" )
                , ( "width", "800px" )
                , ( "padding", "5px" )
                , ( "margin-bottom", "50px" )
                ]
            ]
            (List.map (paragraph model.selectionState) model.data)
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
