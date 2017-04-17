module Decoders exposing (..)

import Main exposing (Dot, Model)
import Json.Decode.Extra as Jextra exposing (..)
import Json.Decode.Pipeline as Pipe exposing (..)
import Json.Decode as DC exposing (..)
import List.Extra as Lextra exposing (..)
import Html exposing (..)


--DECODERS 

type alias GameItem = 
    { id: Int 
    , name: String 
    , data: String
    , game_item: Int
    }

gameItemDecoder: Decoder GameItem 
gameItemDecoder = 
    decode GameItem 
        |> required "id" int 
        |> required "name" string 
        |> required "data" string 
        |> required "game_item" int


--Decode.decodeString (Decode.list user) ""

jsonString: String
jsonString = 
    """[
    {
        "id": 1,
        "name": "left",
        "file": null,
        "data": "a",
        "game": 2,
        "game_item": 6
    },
    {
        "id": 2,
        "name": "left",
        "file": null,
        "data": "b",
        "game": 2,
        "game_item": 4
    },
    {
        "id": 3,
        "name": "left",
        "file": null,
        "data": "c",
        "game": 2,
        "game_item": 5
    },
    {
        "id": 4,
        "name": "right",
        "file": null,
        "data": "b",
        "game": 2,
        "game_item": 2
    }, 
    {   
        "id": 5, 
        "name": "right", 
        "file": null,
        "data": "c",
        "game": 2,
        "game_item": 3
    }, 
    {
        "id": 6, 
        "name": "right", 
        "file": null,
        "data": "a",
        "game": 2,
        "game_item": 1
    }
]"""

listOfGameItems = 
    DC.decodeString (DC.list gameItemDecoder) 

gameItemToDot: GameItem -> Dot 
gameItemToDot gameItem = 
    let 
        newDot: Dot
        newDot = 
            { pk = gameItem.id
            , fk = 0
            , text = gameItem.data 
            , position = (0,0) 
            , lineEndPosition = (0,0)
            }
    in
        newDot

isMemberGroupA: GameItem -> Bool 
isMemberGroupA gameItem = 
    if gameItem.name == "left" then 
        True
    else 
        False

isMemberGroupB: GameItem -> Bool 
isMemberGroupB gameItem = 
    if gameItem.name == "left" then 
        False
    else 
        True


createGroupA: List GameItem -> List Dot 
createGroupA giList = 
    List.map gameItemToDot (List.filter isMemberGroupA giList)

createGroupB: List GameItem -> List Dot 
createGroupB giList = 
    List.map gameItemToDot (List.filter isMemberGroupB giList)

getAnswerTupleFromGameItem: GameItem -> (Int, Int)
getAnswerTupleFromGameItem gameItem = 
    (gameItem.id, gameItem.game_item)
 

createAnswerTuples: List GameItem -> List (Int, Int)
createAnswerTuples giList = 
    let 
        onlyTakeLeftItems = List.filter isMemberGroupA giList 
    in 
        List.map getAnswerTupleFromGameItem onlyTakeLeftItems



createModel: List GameItem -> Model
createModel giList = 
    { groupA = (createGroupA giList)
    , groupB = (createGroupB giList)
    , currentPosition = (0,0)
    , currentPK = 0
    , drawing = False
    , answerList = (createAnswerTuples giList)
    }

--need to assign position somehow 
--need to account for pks being different 
--refactor to deal with maybe types
--grab Json with Ajax???

--Model 

baseModel: Model 
baseModel = 
    createModel listOfGameItems

--Program

main = 
    Html.beginnerProgram 
        { model = baseModel
        , view = view
        , update = update
        }

--View 

view: Model -> (Html Msg)
view model = 
    Html.div [] [
        text (toString Model)
    ]

--Update

type Msg 
  = NoOp


update: Msg -> Model -> Model
update msg model =
  case msg of 
    NoOp ->
      model 
