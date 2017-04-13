module Main exposing (..)

import Html exposing (program)
import Html.Attributes as Htmla exposing (style) 
import Svg exposing (..)
import Svg.Attributes as Sattr exposing (..)
import Svg.Events as Sevents exposing (..)
import Mouse exposing (..)
import List.Extra as Lex exposing (replaceIf) 


--INIT


init : ( Model, Cmd Msg )
init =
    ( baseModel, Cmd.none )





--MODEL

--assumes that answerList is ordered by the first value in the tuple

type alias Model =
    { groupA : List Dot
    , groupB : List Dot
    , currentPosition: Point
    , currentPK: Int
    , drawing: Bool 
    , answerList : List (Int, Int) 
    }


type alias Dot =
    { pk : Int
    , fk : Int
    , text : String
    , position : Point 
    , lineEndPosition : Point
    }


baseModel : Model
baseModel =
    { groupA =
        [ { pk = 1, lineEndPosition = ( 200, 100 ), position = ( 200, 100 ), fk = 0, text = "Daisy" }
        , { pk = 2, lineEndPosition = ( 200, 200 ), position = ( 200, 200 ), fk = 0, text = "Dog" }
        , { pk = 3, lineEndPosition = ( 200, 300 ), position = ( 200, 300 ), fk = 0, text = "Salt" }
        ]
    , groupB =
        [ { pk = 4, lineEndPosition = ( 500, 250 ), position = ( 800, 150 ), fk = 0, text = "Animal" }
        , { pk = 5, lineEndPosition = ( 500, 250 ), position = ( 800, 250 ), fk = 0, text = "Mineral" }
        , { pk = 6, lineEndPosition = ( 500, 250 ), position = ( 800, 350 ), fk = 0, text = "Plant" }
        ]
    , currentPosition = (0,0) 
    , currentPK = 0
    , drawing = False
    , answerList = [(1,6), (2,4), (3,5)]
    }


type alias Point =
    ( Int, Int )


makePoint : Int -> Int -> Int ->  Svg Msg
makePoint x y key =
    Svg.circle
        [ Sattr.cx <| toString x
        , Sattr.cy <| toString y
        , r "12"
        , fill "navyblue"
        , Sevents.onClick (ChangePK key) 
        ]
        []


makeText : Int -> Int -> String -> Svg Msg
makeText x y caption =
    Svg.text_
        [ Sattr.x <| toString x
        , Sattr.y <| toString y
        , Sattr.fontSize "25px"
        , Sattr.fontFamily "sans-serif"
        , fill "black"
        ]
        [ Svg.text caption
        ]


makeTextPoint : Int -> Int -> Int -> String -> Svg Msg
makeTextPoint x y key caption =
    Svg.g
        [ Sevents.onClick (ToggleDrawing)
        ]
        [ makePoint x y key
        , makeText (x + 15) (y - 1) caption
        ]


dotToTextPoint : Dot -> Svg Msg
dotToTextPoint dot =
    makeTextPoint (Tuple.first dot.position) (Tuple.second dot.position) dot.pk dot.text



{--makeLine: Dot -> Maybe (Svg Msg)
makeLine dot =
    case dot.lineEndPosition of
        Just endPoint ->
            Just (Svg.line
                [ Sattr.x1 <|toString <| (Tuple.first dot.position)
                , Sattr.y1 <|toString <| (Tuple.second dot.position)
                , Sattr.x2 <|toString <| (Tuple.first endPoint) +50
                , Sattr.y2 <|toString <| (Tuple.second endPoint) +50
                , Sattr.fill "red"
                ]
                [])
        Nothing ->
            Nothing --}


{-- initLinesAndLanding : Dot -> Svg Msg
initLinesAndLanding dot =

        if List.member dot baseModel.groupA then
            Svg.line
                [ Sattr.x1 <| toString <| (Tuple.first dot.position)
                , Sattr.y1 <| toString <| (Tuple.second dot.position)
                , Sattr.x2 <| toString <| (Tuple.first dot.lineEndPosition)
                , Sattr.y2 <| toString <| (Tuple.second dot.lineEndPosition)
                , Sattr.stroke "red"
                , Sattr.strokeWidth "5"
                , Sattr.z "-1"
                ]
                [ {--text <| toString <| point--} ]

        else
            Svg.circle
                [ Sattr.cx <| toString <| (Tuple.first dot.position)
                , Sattr.cy <| toString <| (Tuple.second dot.position)
                , Sattr.r "20"
                , Sattr.fill "#F7F7F7"
                , Sattr.z "1"
                ]
                [] --} 



--UPDATE


hasPK : Int -> Dot -> Bool
hasPK key dot =
    if dot.pk == key then
        True
    else
        False
        
dotIdentity: Dot 
dotIdentity = 
    { pk = 0
    , fk = 0
    , text = "String"
    , position = (0,0)  
    , lineEndPosition = (0,0) 
    }


dotIdentityFunction: Dot -> Dot -> Dot 
-- use this with foldr to retrieve single dot element w/o having to deal with maybe types 
dotIdentityFunction desiredDot dummyDot = 
    { pk = desiredDot.pk 
    , fk = desiredDot.fk
    , text = desiredDot.text
    , position = desiredDot.position
    , lineEndPosition = desiredDot.lineEndPosition 
    }

getDotWithPK: Int -> Model -> Dot 
getDotWithPK pk model = 
    let
        allDots = model.groupA ++ model.groupB
        intermediaryDotList = List.filter (hasPK pk) allDots
        desiredDot = List.foldr dotIdentityFunction dotIdentity intermediaryDotList 
    in
        desiredDot

getActiveDot: Model -> Dot 
getActiveDot model =
    getDotWithPK (model.currentPK) model

type Msg
    = MouseMove Mouse.Position
    | ChangePK Int 
    | ToggleDrawing
    


-- position to point (this will need to be changed when we embed) 
updateLineEndPosition: Position -> Dot -> Dot
updateLineEndPosition pos dot= 
    {dot | lineEndPosition = (pos.x, pos.y) } 



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove pos ->
            {-- let 
                
                model_ =
                    { model | groupB = [] }
            in
                ( model_, Cmd.none ) --}
            {-- let 
                 intermediaryDotList = List.filter (hasPK model.currentPK) model.groupA 
                 activeDot = List.foldr dotIdentityFunction dotIdentity intermediaryDotList 
                 lineEndPositionToPoint = positionToPoint pos
                 newActiveDot = { activeDot | lineEndPosition = lineEndPositionToPoint } 
                 newGroupA = Lex.replaceIf (hasPK model.currentPK) newActiveDot model.groupA
            in 
            
                if model.drawing == True then       
                     ({model | groupA = newGroupA}, Cmd.none ) 
            else 
                model ! [] --} 
            if model.drawing == True then
                let    
                    newGroupA = Lex.updateIf (hasPK model.currentPK) (updateLineEndPosition pos) model.groupA 
                    model_ = {model | groupA = newGroupA}
                in 
                    (model_, Cmd.none) 
            else 
                model ! []
            
        ChangePK b -> 
            {-- let 
                model_: Model
                model_ = { model | currentPK = b }
            in
                (model_ , Cmd.none ) --}
            let 
                activeDot = getActiveDot model  
            in    
                if List.member activeDot model.groupA == True then     
                     let
                         newActiveDot = { activeDot | fk = b } 
                         newGroupA = Lex.replaceIf (hasPK model.currentPK) newActiveDot model.groupA
                    
                         model_ =
                             { model 
                             | groupA = newGroupA
                             , currentPK = b
                             }
                     in 
                         (model_, Cmd.none) 
                else 
                    let 
                        model_: Model
                        model_ = { model | currentPK = b }
                    in
                        (model_ , Cmd.none )
            
                
        ToggleDrawing -> 
            let 
                activeDot = getActiveDot model  
            in 
                if List.member activeDot model.groupA == True then 
                    ( {model | drawing = True}, Cmd.none ) 
                else 
                    ( {model | drawing = False}, Cmd.none ) 
         
                

-- helper group update functions
--VIEW


view : Model -> Svg Msg
view model = 
    {--svg [ Sattr.viewBox "0 0 1000 500" ]
        [ makeTextPoint 100 100 "Svg"
        , makeTextPoint 100 200 "is so very"
        , makeTextPoint 100 300 "cool!"
        ] --}
    let   
        --this might need to change if the pks are not in order. 
        firstDot = getDotWithPK 1 model
        secondDot = getDotWithPK 2 model
        thirdDot = getDotWithPK 3 model 

    
    in
        Html.div []{--
            [ ("left", )
 
            ] --}
            [
            svg [ Sattr.viewBox "0 0 1000 500" ]
                ({-- (List.map initLinesAndLanding model.groupA)
                    ++ (List.map initLinesAndLanding model.groupB) --}
                    List.singleton (Svg.line
                        [ Sattr.x1 <| toString <| (Tuple.first firstDot.position)
                        , Sattr.y1 <| toString <| (Tuple.second firstDot.position)
                        , Sattr.x2 <| toString <| (Tuple.first firstDot.lineEndPosition)
                        , Sattr.y2 <| toString <| (Tuple.second firstDot.lineEndPosition)
                        , Sattr.stroke (answerLineColor model)
                        , Sattr.strokeWidth "5"
                        , Sattr.z "-1"
                        ] [] )
                    ++ List.singleton (Svg.line
                        [ Sattr.x1 <| toString <| (Tuple.first secondDot.position)
                        , Sattr.y1 <| toString <| (Tuple.second secondDot.position)
                        , Sattr.x2 <| toString <| (Tuple.first secondDot.lineEndPosition)
                        , Sattr.y2 <| toString <| (Tuple.second secondDot.lineEndPosition)
                        , Sattr.stroke (answerLineColor model)
                        , Sattr.strokeWidth "5"
                        , Sattr.z "-1"
                        ] [] )
                    ++ List.singleton (Svg.line
                        [ Sattr.x1 <| toString <| (Tuple.first thirdDot.position)
                        , Sattr.y1 <| toString <| (Tuple.second thirdDot.position)
                        , Sattr.x2 <| toString <| (Tuple.first thirdDot.lineEndPosition)
                        , Sattr.y2 <| toString <| (Tuple.second thirdDot.lineEndPosition)
                        , Sattr.stroke (answerLineColor model)
                        , Sattr.strokeWidth "5"
                        , Sattr.z "-1"
                        ] [] )
                    ++ (List.map dotToTextPoint model.groupA)
                    ++ (List.map dotToTextPoint model.groupB)
                    ), 
            -- Svg.text (toString model.currentPK ++ toString model.drawing),  
            checkAnswers model
        ]
        

type Check = SomeDotsUnconnected | Connections List (Int, Int)



getResponseTuple: Dot -> (Int, Int)
getResponseTuple dot = 
    (dot.pk, dot.fk) 
    
lineHasBeenMade: (Int, Int) -> Bool 
lineHasBeenMade (pk, fk) =
    if fk == 0 then 
        False
    else 
        True 

getResponseList: Model -> List (Int, Int) 
getResponseList model = 
    let 
        responseList = List.map getResponseTuple model.groupA
    in 
        responseList 
        
checkAnswers: Model -> Html.Html msg
checkAnswers model =
    let
        responseList = getResponseList model 
        linesMadeList = List.filter lineHasBeenMade responseList 
    in    
        if (List.length responseList) /= (List.length linesMadeList) then  
            Html.div [] []
        else if getResponseList model == (model.answerList) then 
            Html.div [ 
                Htmla.style 
                    [ ("color", "green") 
                    , ("font-size", "3em ")
                    , ("font-family", "sans-serif")
                    , ("width", "100%")
                    , ("text-align", "center")
                    ]
                ]
                [ text "Great job!" ]
        else 
            Html.div[ 
                Htmla.style 
                    [ ("color", "RED") 
                    , ("font-size", "3em ") 
                    , ("font-family", "sans-serif")
                    , ("width", "100%")
                    , ("text-align", "center")
                    ]
                ]
                [ text "Why don't you try again?" ]

answerLineColor: Model -> String
answerLineColor model = 
    let
        responseList = getResponseList model 
        linesMadeList = List.filter lineHasBeenMade responseList 
    in    
        if (List.length responseList) /= (List.length linesMadeList) then  
            "black"
        else if getResponseList model == (model.answerList) then 
            "green"
        else 
            "red"

        
--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves MouseMove



--MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
