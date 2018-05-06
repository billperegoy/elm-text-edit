module SelectResult exposing (..)

import Json.Decode
import Json.Decode.Pipeline


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
