module Types.UserInfo exposing (UserInfo, decoder)

import Json.Decode as Decode


type alias UserInfo =
    { userid : Int
    , name : String
    , email : String
    , role : String
    }



-- DECODE


decoder : Decode.Decoder UserInfo
decoder =
    Decode.map4 UserInfo
        (Decode.field "userid" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "role" Decode.string)
