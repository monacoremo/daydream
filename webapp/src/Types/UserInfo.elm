module Types.UserInfo exposing (UserInfo, decoder)

import Json.Decode as Decode


type alias UserInfo =
    { userId : Int
    , name : String
    , email : String
    }



-- DECODE


decoder : Decode.Decoder UserInfo
decoder =
    Decode.map3 UserInfo
        (Decode.field "user_id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
