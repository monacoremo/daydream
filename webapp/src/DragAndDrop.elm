module DragAndDrop exposing
    ( Config
    , Msg
    , State
    , draggable
    , droppable
    , init
    , isDragged
    , isDraggedOver
    , update
    )

import Element exposing (Attribute)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode


type State draggable droppable
    = NotDragging
    | Dragging draggable
    | DraggingOver { item : draggable, target : droppable }


init : State draggable droppable
init =
    NotDragging


isDragged : draggable -> State draggable droppable -> Bool
isDragged item state =
    case state of
        NotDragging ->
            False

        Dragging draggedItem ->
            draggedItem == item

        DraggingOver info ->
            info.item == item


isDraggedOver : droppable -> State draggable droppable -> Bool
isDraggedOver target state =
    case state of
        NotDragging ->
            False

        Dragging _ ->
            False

        DraggingOver info ->
            info.target == target


type Msg draggable droppable
    = DragStart draggable
    | DragEnd
    | DragOver
    | DragEnter droppable
    | DragLeave
    | Drop


type alias Config draggable droppable model =
    { mapState :
        (State draggable droppable -> State draggable droppable)
        -> model
        -> model
    , state : model -> State draggable droppable
    , updateDropped : draggable -> droppable -> model -> model
    }


update :
    Config draggable droppable model
    -> Msg draggable droppable
    -> model
    -> model
update config msg model =
    case msg of
        DragStart item ->
            config.mapState (always (Dragging item)) model

        DragOver ->
            model

        DragEnter target ->
            config.mapState
                (\state ->
                    case state of
                        NotDragging ->
                            state

                        Dragging item ->
                            DraggingOver { item = item, target = target }

                        DraggingOver { item } ->
                            DraggingOver { item = item, target = target }
                )
                model

        DragLeave ->
            model

        DragEnd ->
            config.mapState (always NotDragging) model

        Drop ->
            config.mapState (always NotDragging)
                (case config.state model of
                    DraggingOver { item, target } ->
                        config.updateDropped item target model

                    _ ->
                        model
                )


draggable : draggable -> (Msg draggable droppable -> msg) -> List (Attribute msg)
draggable item tagger =
    List.map (Element.htmlAttribute >> Element.mapAttribute tagger)
        [ Html.Attributes.draggable "true"
        , on "dragstart" (DragStart item)
        , on "dragend" DragEnd
        ]


droppable : droppable -> (Msg draggable droppable -> msg) -> List (Attribute msg)
droppable target tagger =
    List.map (Element.htmlAttribute >> Element.mapAttribute tagger)
        [ preventDefaultOn "dragover" DragOver
        , preventDefaultOn "dragenter" (DragEnter target)
        , preventDefaultOn "dragleave" DragLeave
        , preventDefaultOn "drop" Drop
        ]


on : String -> msg -> Html.Attribute msg
on event msg =
    Html.Events.on event (Decode.succeed msg)


preventDefaultOn : String -> msg -> Html.Attribute msg
preventDefaultOn event msg =
    Html.Events.preventDefaultOn event (Decode.succeed ( msg, True ))
