module DecodeTest exposing (decodeTest)

import Expect
import Json.Decode as Decode
import Test exposing (..)


type alias Node =
    { nodeType : String
    , totalCost : Float
    }


d : Decode.Decoder Node
d =
    Decode.map2 Node (Decode.field "Node Type" Decode.string) (Decode.field "Total Cost" Decode.float)


decodeTest : Test
decodeTest =
    describe "A Decode Suite"
        [ test "should decode well" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString d """{"Node Type": "type1", "Total Cost": 3.2}""")
                    (Ok (Node "type1" 3.2))
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
