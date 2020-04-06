module Api exposing
    ( Request
    , get
    , getSingle
    , patch
    , post
    , postGetSingle
    , request
    )

import Http exposing (Body, Expect, Header)


type alias Request data msg =
    (Result Http.Error data -> msg) -> Cmd msg


get :
    { url : String
    , expect : Expect msg
    }
    -> Cmd msg
get r =
    request
        { method = "GET"
        , headers = []
        , url = r.url
        , body = Http.emptyBody
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


getSingle :
    { url : String
    , expect : Expect msg
    }
    -> Cmd msg
getSingle r =
    request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , url = r.url
        , body = Http.emptyBody
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


patch :
    { url : String
    , body : Body
    , expect : Expect msg
    }
    -> Cmd msg
patch r =
    request
        { method = "PATCH"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


post :
    { url : String
    , body : Body
    , expect : Expect msg
    }
    -> Cmd msg
post r =
    request
        { method = "POST"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


postGetSingle :
    { url : String
    , body : Body
    , expect : Expect msg
    , headers : List Http.Header
    , tracker : Maybe String
    }
    -> Cmd msg
postGetSingle r =
    request
        { method = "POST"
        , headers = Http.header "Accept" "application/vnd.pgrst.object+json" :: r.headers
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = r.tracker
        }


request :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request r =
    Http.request
        { method = r.method
        , headers = Http.header "X-Requested-By" "webapp" :: r.headers
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = r.timeout
        , tracker = r.tracker
        }
