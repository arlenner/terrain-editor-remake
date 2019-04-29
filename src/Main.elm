module Main exposing (..)

import Json.Decode as D
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


---- MODEL ----

type Direction 
    = Up
    | Down
    | Left
    | Right

type Terrain 
    = Grass
    | Water
    | Dirt
    | Snow
    | Stone

type alias Cube =
    { size : Int
    , selected : Bool
    , ht : Int
    , x : Int
    , y : Int
    , terrain : Terrain
    , contents : Maybe Object
    }

type alias Object = 
    { size : Int
    , selected : Bool
    , x : Int
    , y : Int
    , ht : Int
    , tag : String
    }

type alias Model =
    { cubes : List Cube, rot : Int, active : Maybe Cube, gridSize : Int, cubeSize : Int, mSelect : Bool }

-- |> grid/cell construction
cube : Int -> Int -> Int -> Int -> Bool -> Terrain -> Cube
cube x y size ht sel t =
    { x = x, y = y, size = size, ht = ht, selected = sel, terrain = t, contents = Nothing }

cubeWithObj : Object -> Cube -> Cube
cubeWithObj obj c =
    { x = c.x,  y = c.y, size = c.size, ht = c.ht, selected = c.selected, terrain = c.terrain, contents = Just obj}

cubeLine : Int -> Int -> Int -> List Cube
cubeLine y length size =
    List.map (\x -> cube x y size 0 False Grass) (List.range 1 length)

cubeGrid : Int -> Int -> List (List Cube)
cubeGrid gsize csize =
    List.map (\y -> cubeLine y gsize csize) (List.range 1 gsize)

flatGrid : List (List Cube) -> List Cube
flatGrid cubes =
    List.foldl (\a b -> List.append b a) [] cubes

object : Int -> Int -> Int -> Int -> String -> Object
object x y size ht tag =
    { x = x, y = y, size = size, selected = True, ht = ht, tag = tag }

emptyObject : Int -> Int -> Object
emptyObject x y = { x = x, y = y, size = 1, selected = False, ht = 0, tag = "placeholder" }

isEmptyObject : Object -> Bool
isEmptyObject o = if o.tag == "placeholder" then True else False

init : ( Model, Cmd Msg )
init =
    ( { cubes = List.map (\c -> 
        if c.x == 1 && c.y == 1 
        then cubeWithObj (object 1 1 1 0 "me") c
        else c ) (flatGrid (cubeGrid 5 3))
    , rot = 45
    , active = Nothing
    , gridSize = 5
    , cubeSize = 3
    , mSelect = False }, Cmd.none )



---- UPDATE ----


type Msg
    = Select Cube
    | Select2 Object
    | IncHt
    | DecHt
    | RotLeft
    | RotRight
    | SizeUp
    | SizeDown
    | Swap Terrain
    | ToggleMSelect
    | SpawnObject Object
    | MoveObject Direction
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Noop -> (model, Cmd.none)

        Select cb ->
            ({ model | cubes = 
                List.map 
                    (\c -> 
                        if c.x == cb.x && c.y == cb.y 
                        then { x = c.x, y = c.y, size = c.size, ht = c.ht, selected = not c.selected, contents = c.contents, terrain = c.terrain }
                        else { x = c.x, y = c.y, size = c.size, ht = c.ht, contents = c.contents, selected = 
                            (if model.mSelect then c.selected else False), terrain = c.terrain })
                    model.cubes
            , active = Just cb
            }, Cmd.none)

        Select2 obj ->
            ({ model | cubes = model.cubes
                |> List.map (\c -> 
                    case c.contents of
                        Just o ->
                            if o.tag == obj.tag 
                            then { c | contents = Just { o | selected = not o.selected } } 
                            else c
                        Nothing -> c)
            }, Cmd.none)

        IncHt ->
            ({ model | cubes = 
                List.map
                    (\c -> 
                        if c.selected 
                        then case c.contents of
                            Nothing -> { c | ht = c.ht + 1, contents = (Just (emptyObject c.x c.y)) }
                            Just obj -> { c | ht = c.ht + 1, contents = (Just {obj | ht = obj.ht + 1 }) }
                        else c
                    ) model.cubes
            }, Cmd.none)
        
        DecHt ->
            ({ model | cubes = 
                List.map
                    (\c -> 
                        if c.selected 
                        then if c.ht - 1 < 0 
                            then c 
                            else case c.contents of
                                Nothing -> { c | ht = c.ht - 1, contents = (Just (emptyObject c.x c.y)) }
                                Just obj -> { c | ht = c.ht - 1, contents = (Just {obj | ht = obj.ht - 1 }) }
                        else c 
                    ) model.cubes
            }, Cmd.none)

        RotLeft ->
            ({ model | rot = model.rot - 45 }, Cmd.none)

        RotRight ->
            ({model | rot = model.rot + 45 }, Cmd.none)
        
        SizeUp -> 
            ({ model | cubes = 
                flatGrid (cubeGrid (model.gridSize + 1) model.cubeSize )
                , gridSize = model.gridSize + 1
            }, Cmd.none)
        
        SizeDown ->
            ({ model | cubes = 
                flatGrid (cubeGrid (if model.gridSize - 1 < 1 then 1 else model.gridSize - 1) model.cubeSize) 
                , gridSize = (if model.gridSize - 1 < 1 then 1 else model.gridSize - 1)
            }, Cmd.none)

        Swap terr ->
            ({ model | cubes = 
                List.map 
                    (\c -> 
                        (if c.selected 
                        then
                            case c.contents of
                                Nothing -> { c | terrain = terr }
                                Just obj -> { c | contents = Just obj, terrain = terr }
                        else c)) model.cubes }, Cmd.none)
        ToggleMSelect ->
            ({ model | mSelect = not model.mSelect }, Cmd.none)

        SpawnObject o ->
            (model, Cmd.none)
        
        MoveObject dir ->
            let
                sq = round (sqrt (toFloat (List.length model.cubes)))
            in
            case dir of 
                Up -> 
                    let 
                        obj : Maybe Object
                        obj = model.cubes
                            |> List.filter (\c -> 
                                case c.contents of
                                    Nothing -> False
                                    Just o -> if o.selected then True else False)
                            |> List.map (\c -> c.contents)
                            |> (\list -> case (List.head list) of
                                Nothing -> Nothing
                                Just o -> o)
                        

                        ctarg = case obj of
                            Nothing -> Nothing
                            Just o -> model.cubes
                                |> List.filter (\c -> c.x == o.x - 1 && c.y == o.y) 
                                |> List.head
                    in 
                        case ctarg of
                            Nothing -> (model, Cmd.none)
                            Just targ ->
                                ({ model | cubes =
                                    case obj of
                                        Nothing -> model.cubes
                                        Just o ->
                                            model.cubes
                                                |> List.map (\c -> 
                                                    if c.x == o.x && c.y == o.y
                                                    then { c | contents = Nothing }
                                                    else if c.x == targ.x && c.y == targ.y
                                                        then { c | contents = Just { o | x = o.x - 1 } }
                                                    else c)
                                }, Cmd.none)
                Down -> 
                    let 
                        obj : Maybe Object
                        obj = model.cubes
                            |> List.filter (\c -> 
                                case c.contents of
                                    Nothing -> False
                                    Just o -> if o.selected then True else False)
                            |> List.map (\c -> c.contents)
                            |> (\list -> case (List.head list) of
                                Nothing -> Nothing
                                Just o -> o)
                        

                        ctarg = case obj of
                            Nothing -> Nothing
                            Just o -> model.cubes
                                |> List.filter (\c -> c.x == o.x + 1 && c.y == o.y) 
                                |> List.head
                    in 
                        case ctarg of
                            Nothing -> (model, Cmd.none)
                            Just targ ->
                                ({ model | cubes =
                                    case obj of
                                        Nothing -> model.cubes
                                        Just o ->
                                            model.cubes
                                                |> List.map (\c -> 
                                                    if c.x == o.x && c.y == o.y
                                                    then { c | contents = Nothing }
                                                    else if c.x == targ.x && c.y == targ.y
                                                        then { c | contents = Just { o | x = o.x + 1 }}
                                                    else c)
                                }, Cmd.none)
                Left -> 
                    let 
                        obj : Maybe Object
                        obj = model.cubes
                            |> List.filter (\c -> 
                                case c.contents of
                                    Nothing -> False
                                    Just o -> if o.selected then True else False)
                            |> List.map (\c -> c.contents)
                            |> (\list -> case (List.head list) of
                                Nothing -> Nothing
                                Just o -> o)
                        

                        ctarg = case obj of
                            Nothing -> Nothing
                            Just o -> model.cubes
                                |> List.filter (\c -> c.x == o.x && c.y == o.y - 1) 
                                |> List.head
                    in 
                        case ctarg of
                            Nothing -> (model, Cmd.none)
                            Just targ ->
                                ({ model | cubes =
                                    case obj of
                                        Nothing -> model.cubes
                                        Just o ->
                                            model.cubes
                                                |> List.map (\c -> 
                                                    if c.x == o.x && c.y == o.y
                                                    then { c | contents = Nothing }
                                                    else if c.x == targ.x && c.y == targ.y
                                                        then { c | contents = Just { o | y = o.y - 1 }}
                                                    else c)
                                }, Cmd.none)
                Right -> 
                    let 
                        obj : Maybe Object
                        obj = model.cubes
                            |> List.filter (\c -> 
                                case c.contents of
                                    Nothing -> False
                                    Just o -> if o.selected then True else False)
                            |> List.map (\c -> c.contents)
                            |> (\list -> case (List.head list) of
                                Nothing -> Nothing
                                Just o -> o)
                        

                        ctarg = case obj of
                            Nothing -> Nothing
                            Just o -> model.cubes
                                |> List.filter (\c -> c.x == o.x && c.y == o.y + 1) 
                                |> List.head
                    in 
                        case ctarg of
                            Nothing -> (model, Cmd.none)
                            Just targ ->
                                ({ model | cubes =
                                    case obj of
                                        Nothing -> model.cubes
                                        Just o ->
                                            model.cubes
                                                |> List.map (\c -> 
                                                    if c.x == o.x && c.y == o.y
                                                    then { c | contents = Nothing }
                                                    else if c.x == targ.x && c.y == targ.y
                                                        then { c | contents = Just { o | y = o.y + 1}}
                                                    else c)
                                }, Cmd.none)


---- VIEW ----

objView : Model -> Cube -> Maybe Object -> Html Msg
objView m c obj =
    case obj of
        Nothing -> div [] []
        Just o -> div (List.append [onClick (Select2 o)] (objStyle m o c)) []

cubeView : Int -> Cube -> Model -> Html Msg
cubeView size c model =
    div (List.append [(onClick (Select c))] (cubeStyle size c.x c.y) )
        [ objView model c c.contents
        , div (cubeTopStyle c) 
            [ text 
                (if c.selected 
                then (String.fromInt c.x ++ "," ++ String.fromInt c.y ++ "\n H:" ++ String.fromInt c.ht)  
                else ""
                ) 
            ]
        , div (cubeLeftStyle c) []
        , div (cubeRightStyle c) []
        , div (cubeBackRightStyle c) []
        , div (cubeBackLeftStyle c) []
        ]

htUpBtn : Html Msg
htUpBtn =
    button (List.append [ onClick IncHt, attribute "data" "Increase Terrain Height." ] buttonStyle) [ text "+" ]

htDownBtn : Html Msg
htDownBtn =
    button (List.append [ onClick DecHt, attribute "data" "Decrease Terrain Height." ] buttonStyle) [text "-"]

rotLBtn : Html Msg
rotLBtn =
    button (List.append [onClick RotLeft, attribute "data" "Rotate Board Left." ] buttonStyle) [text "<"]

rotRBtn : Html Msg
rotRBtn =
    button (List.append [onClick RotRight, attribute "data" "Rotate Board Right." ] buttonStyle) [text ">"]

sizeUpBtn : Html Msg
sizeUpBtn =
    button (List.append [onClick SizeUp, attribute "data" "Increase Grid Size. \n **(WARNING - this will clear the board!)" ] buttonStyle) [text "G+"]

sizeDownBtn : Html Msg
sizeDownBtn =
    button (List.append [onClick SizeDown, attribute "data" "Decrease Grid Size. \n **(WARNING - this will clear the board!)"] buttonStyle) [text "G-"]

paletteBtn : Terrain -> Html Msg
paletteBtn terr =
    button (List.append [onClick (Swap terr), attribute "palette-data" (getTerrainString terr) ] buttonStyle) [ div (swatchStyle terr) [] ]

toggleBtn : Model -> Html Msg
toggleBtn model =
    input (List.append [ onClick ToggleMSelect, type_ "radio", checked model.mSelect, attribute "radio-data" "multi-select?" ] buttonStyle) [ ]

sidePalette : Model -> Html Msg
sidePalette model = 
    div paletteStyle
        [ paletteBtn Grass
        , paletteBtn Dirt
        , paletteBtn Water
        , paletteBtn Snow  
        , paletteBtn Stone
        , toggleBtn model
        ]


view : Model -> Html Msg
view model =
    div appContainerStyle
        [ div (floorStyle model.cubeSize model.gridSize model.rot) 
            (List.map (\c -> cubeView 3 c model) model.cubes)
        , sidePalette model
        , div footerStyle
            [ rotLBtn
            , sizeUpBtn
            , htUpBtn
            , htDownBtn
            , sizeDownBtn
            , rotRBtn                
            ]
        ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

-- |> SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onKeyUp keyDecoder

keyDecoder : D.Decoder Msg
keyDecoder =
    D.map toKey (D.field "key" D.string)

toKey : String -> Msg
toKey key =
    case key of
        "ArrowUp" ->
            MoveObject Up
        "ArrowDown" ->
            MoveObject Down
        "ArrowLeft" ->
            MoveObject Left
        "ArrowRight" ->
            MoveObject Right
        _ -> 
            Noop



-- |> COMPONENT STYLES

appContainerStyle : List ( Attribute Msg )
appContainerStyle =
    [ style "height" "100vh"
    , style "width" "100vw"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "perspective" "800px"
    , style "z-index" "2"
    , style "transform-style" "preserve-3d"
    ]

swatchStyle : Terrain -> List (Attribute Msg)
swatchStyle terr =
    [ style "height" "80%"
    , style "width" "80%"
    , style "margin" ".3em"
    , terrainTopStyle terr        
    ]

paletteStyle : List (Attribute Msg)
paletteStyle =
    [ style "position" "absolute"
    , style "left" "2.5%"
    , style "display" "flex"
    , style "flex-flow" "column"
    ]

buttonStyle : List (Attribute Msg)
buttonStyle =
    [ style "border-radius" ".2em"
    , style "background-color" "transparent"
    , style "height" "4em"
    , style "width" "4em"
    , style "margin" ".4em"
    ]


floorStyle : Int -> Int -> Int -> List (Attribute Msg)
floorStyle s n rot =
    [ style "transform" ("rotateX(45deg) rotateZ(" ++ String.fromInt rot ++ "deg)")
    , style "background-size" ((String.fromInt s) ++ "em " ++ (String.fromInt s) ++ "em") 
    , style "transition" "transform 1s"
    , style "height" (String.fromInt (s*n) ++ "em")
    , style "width" (String.fromInt (s*n) ++ "em")
    , style "display" "grid"
    , style "grid-template-rows" (getGridTemplate s n)
    , style "grid-template-columns" (getGridTemplate s n)
    , style "transform-style" "preserve-3d"
    , style "z-index" "3"
    ]

cubeLeftStyle : Cube -> List (Attribute Msg)
cubeLeftStyle c =     
    [ terrainLightStyle c.terrain
    , style "position" "absolute"
    , style "height" (String.fromInt (c.size + c.ht) ++ "em")
    , style "width" (String.fromInt (c.size) ++ "em")
    , style "box-shadow" "0 0 .3em .3em #0001 inset"
    , style 
        "transform" ("rotateX(-90deg) translateZ(" ++ (String.fromFloat (0.5 *(toFloat c.size)-0.5 * (toFloat c.ht))) ++ "em) translateY(-" ++ (String.fromFloat (0.5*toFloat c.ht)) ++ "em)")
    ]

cubeRightStyle : Cube -> List (Attribute Msg)
cubeRightStyle c = 
    [ terrainDarkStyle c.terrain
    , style "position" "absolute"
    , style "height" (String.fromInt (c.size) ++ "em")
    , style "width" (String.fromInt (c.size + c.ht) ++ "em")
    , style "box-shadow" "0 0 .3em .3em #0001 inset"
    , style 
        "transform" ("rotateY(90deg) translateZ(" ++ (String.fromFloat (0.5 *(toFloat c.size)-0.5 * (toFloat c.ht))) ++ "em) translateX(-" ++ (String.fromFloat (0.5*toFloat c.ht)) ++ "em)")
    ]

cubeTopStyle : Cube -> List (Attribute Msg)
cubeTopStyle c = 
    [ (terrainTopStyle c.terrain)
    , style "border" (if c.selected then ".2em solid lime" else "")
    , style "box-sizing" "border-box"
    , style "position" "absolute"
    , style "height" (String.fromInt c.size ++ "em")
    , style "width" (String.fromInt c.size ++ "em")
    , style "transform" ("translateZ(" ++ (String.fromFloat (0.5 * (toFloat c.size) + (toFloat c.ht))) ++ "em)")
    , style "box-shadow" "0 0 .3em .3em #0001 inset"
    ]

cubeBackLeftStyle : Cube -> List (Attribute Msg)
cubeBackLeftStyle c = 
    [ terrainLightStyle c.terrain
    , style "position" "absolute"
    , style "height" (String.fromInt (c.size) ++ "em")
    , style "width" (String.fromInt (c.size + c.ht) ++ "em")
    , style "box-shadow" "0 0 .3em .3em #0001 inset"
    , style "transform" ("rotateY(-90deg)" ++ translate "Z" (0.5 * toFloat (c.size + c.ht)) ++ translate "X" (0.5 * toFloat c.ht))
    ]

cubeBackRightStyle : Cube -> List (Attribute Msg)
cubeBackRightStyle c = 
    [ terrainDarkStyle c.terrain
    , style "position" "absolute"
    , style "height" (String.fromInt (c.size + c.ht) ++ "em")
    , style "width" (String.fromInt (c.size) ++ "em")
    , style "box-shadow" "0 0 .3em .3em #0001 inset"
    , style "transform" ("rotateX(90deg)" ++ translate "Z" (0.5 * toFloat (c.size + c.ht)) ++ translate "Y" (0.5 * toFloat c.ht))
    ]

cubeStyle : Int -> Int -> Int -> List (Attribute Msg)
cubeStyle size x y =
    [ style "grid-column" (String.fromInt y)
    , style "grid-row" (String.fromInt x)
    , style "height" (String.fromInt size ++ "em")
    , style "width" (String.fromInt size ++ "em")
    , style "background-color" "transparent"
    , style "box-sizing" "border-box"
    , style "position" "relative"
    , style "transform" ("translateZ(" ++ (String.fromFloat (0.5*toFloat size) ++ "em)"))
    , style "transform-style" "preserve-3d"    
    ]

objStyle : Model -> Object -> Cube -> List (Attribute Msg)
objStyle model obj c =
    if obj.tag == "placeholder" then []
    else
        [ attribute "obj-data" "Use the arrow keys to move me! Click me to select me! I'm red when selected!"
        , style "border-radius" "50%"
        , style "position" "absolute"
        , style "height" ((String.fromFloat ( toFloat (obj.size) + 0.5)) ++ "em")
        , style "width" ((String.fromFloat (toFloat (obj.size) + 0.5)) ++ "em")
        , style "margin" (String.fromFloat ((toFloat obj.size) * 0.5) ++ "em")
        , style "box-shadow" ".3em 0 .25em .25em #0003 inset, -.3em 0 .25em .25em #fff3 inset"
        , style "background-color" (if obj.selected then "red" else "blue")   
        , style "transform" 
            (translate "Z" (toFloat (c.size + c.ht) - 0.5) ++ " rotateX(90deg) rotateY(" ++ (String.fromInt (-model.rot)) ++ "deg)")
        ]

footerStyle : List (Attribute Msg)
footerStyle =
    [ style "position" "fixed"
    , style "bottom" "0%"
    , style "height" "4em"
    , style "width" "100%"
    , style "left" "0%"
    , style "display" "flex"
    , style "justify-content" "center"        
    ]

terrainTopStyle : Terrain -> Attribute Msg
terrainTopStyle terr =
    case terr of
        Grass ->
            style "background-color" green
        Water ->
            style "background-color" water
        Dirt ->
            style "background-color" topBrown
        Snow ->
            style "background-color" "white"
        Stone ->
            style "background-color" "#868696"

terrainDarkStyle : Terrain -> Attribute Msg
terrainDarkStyle terr =
    case terr of
        Grass ->
            style "background-color" darkBrown
        Water ->
            style "background-color" water
        Dirt -> 
            style "background-color" darkBrown
        Snow ->
            style "background-color" darkWhite
        Stone ->
            style "background-color" "#525264"

terrainLightStyle : Terrain -> Attribute Msg
terrainLightStyle terr =
    case terr of
        Grass ->
            style "background-color" lightBrown
        Water ->
            style "background-color" water
        Dirt -> 
            style "background-color" lightBrown
        Snow ->
            style "background-color" lightWhite
        Stone ->
            style "background-color" "#707070"
            
translate : String -> Float -> String
translate s f =
    "translate" ++ s ++ "(" ++ String.fromFloat f ++ "em)"

getGridTemplate : Int -> Int -> String
getGridTemplate s n =
    String.concat (List.repeat n (String.fromInt s ++ "em"))

getTerrainString : Terrain -> String
getTerrainString t =
    case t of
        Grass -> "grass"
        Water -> "water"
        Dirt -> "dirt"
        Snow -> "snow"
        Stone -> "stone"

green : String
green = "#3b5"

lightBrown : String
lightBrown = "#675"

darkBrown : String
darkBrown = "#453"

darkWhite : String
darkWhite = "#bbd"

lightWhite : String
lightWhite = "#ddf"

water : String
water = "#09f9"

topBrown : String
topBrown = "#897"
