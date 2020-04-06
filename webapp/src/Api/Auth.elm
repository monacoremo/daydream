module Api.Auth exposing
    ( check
    , login
    , logout
    , refresh
    , userInfo
    )

import Api
import Http
import Json.Encode as Encode
import Types.UserInfo as UserInfo exposing (UserInfo)
import Url.Builder as Url


login : String -> String -> Api.Request () msg
login email password tagger =
    Api.postGetSingle
        { url =
            Url.absolute [ "api", "rpc", "login" ] []
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
                )
        , expect = Http.expectWhatever tagger
        , tracker = Nothing
        , headers = []
        }


refresh : Api.Request () msg
refresh tagger =
    Api.post
        { url = Url.absolute [ "api", "rpc", "refresh" ] []
        , body = Http.emptyBody
        , expect = Http.expectWhatever tagger
        }


check : Api.Request () msg
check tagger =
    Api.get
        { url = Url.absolute [ "api", "rpc", "userinfo" ] []
        , expect = Http.expectWhatever tagger
        }


logout : Api.Request () msg
logout tagger =
    Api.post
        { url = Url.absolute [ "api", "rpc", "logout" ] []
        , body = Http.emptyBody
        , expect = Http.expectWhatever tagger
        }


userInfo : Api.Request UserInfo msg
userInfo tagger =
    Api.getSingle
        { url = Url.absolute [ "api", "rpc", "userinfo" ] []
        , expect = Http.expectJson tagger UserInfo.decoder
        }
