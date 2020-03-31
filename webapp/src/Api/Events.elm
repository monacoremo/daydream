module Api.Events exposing (..)

import Api.Types.Command
import Config
import Http
import Json.Decode


post : Maybe String -> Api.Types.Command.Command -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) String)
post a b =
    Http.request { method = "POST"
    , headers = List.filterMap identity [ Maybe.map (Http.header "Cookie") (Maybe.map identity a) ]
    , url = Config.urlBase
    , body = Http.jsonBody (Api.Types.Command.encoder b)
    , expect = Http.expectStringResponse identity (\c -> case c of
        Http.BadUrl_ d ->
            Err (Http.BadUrl d , Nothing)
        
        Http.Timeout_ ->
            Err (Http.Timeout , Nothing)
        
        Http.NetworkError_ ->
            Err (Http.NetworkError , Nothing)
        
        Http.BadStatus_ d e ->
            Err (Http.BadStatus d.statusCode , Just { metadata = d, body = e })
        
        Http.GoodStatus_ d e ->
            Result.mapError (\f -> (Http.BadBody (Json.Decode.errorToString f) , Just { metadata = d
            , body = e })) (Json.Decode.decodeString Json.Decode.string e))
    , timeout = Nothing
    , tracker = Nothing }