module Api.Types.Command exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Command 
    = CreateTree { rootLabel : String }
    | AppendLeaf { parentId : Int, label : String }
    | DeleteNode { nodeId : Int }
    | MoveNodeAppend { nodeId : Int, parentId : Int }


encoder : Command -> Json.Encode.Value
encoder a =
    case a of
        CreateTree b ->
            Json.Encode.object [ ("command" , Json.Encode.string "create_tree")
            , ("value" , Json.Encode.object [ ("rootLabel" , Json.Encode.string b.rootLabel) ]) ]
        
        AppendLeaf b ->
            Json.Encode.object [ ("command" , Json.Encode.string "append_leaf")
            , ("value" , Json.Encode.object [ ("parentId" , Json.Encode.int b.parentId)
            , ("label" , Json.Encode.string b.label) ]) ]
        
        DeleteNode b ->
            Json.Encode.object [ ("command" , Json.Encode.string "delete_node")
            , ("value" , Json.Encode.object [ ("nodeId" , Json.Encode.int b.nodeId) ]) ]
        
        MoveNodeAppend b ->
            Json.Encode.object [ ("command" , Json.Encode.string "move_node_append")
            , ("value" , Json.Encode.object [ ("nodeId" , Json.Encode.int b.nodeId)
            , ("parentId" , Json.Encode.int b.parentId) ]) ]


decoder : Json.Decode.Decoder Command
decoder =
    Json.Decode.field "command" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "create_tree" ->
            Json.Decode.succeed CreateTree |>
            Json.Decode.Pipeline.required "value" (\b -> { rootLabel = b } |>
            Json.Decode.Pipeline.required "rootLabel" Json.Decode.string)
        
        "append_leaf" ->
            Json.Decode.succeed AppendLeaf |>
            Json.Decode.Pipeline.required "value" (\b c -> { parentId = b
            , label = c } |>
            Json.Decode.Pipeline.required "parentId" Json.Decode.int |>
            Json.Decode.Pipeline.required "label" Json.Decode.string)
        
        "delete_node" ->
            Json.Decode.succeed DeleteNode |>
            Json.Decode.Pipeline.required "value" (\b -> { nodeId = b } |>
            Json.Decode.Pipeline.required "nodeId" Json.Decode.int)
        
        "move_node_append" ->
            Json.Decode.succeed MoveNodeAppend |>
            Json.Decode.Pipeline.required "value" (\b c -> { nodeId = b
            , parentId = c } |>
            Json.Decode.Pipeline.required "nodeId" Json.Decode.int |>
            Json.Decode.Pipeline.required "parentId" Json.Decode.int)
        
        _ ->
            Json.Decode.fail "No matching constructor")