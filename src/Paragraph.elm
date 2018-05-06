module Paragraph exposing (..)


type alias Paragraph =
    ( Int, String )


create : Int -> String -> Paragraph
create id text =
    ( id, text )


id : Paragraph -> Int
id paragraph =
    Tuple.first paragraph


text : Paragraph -> String
text paragraph =
    Tuple.second paragraph
