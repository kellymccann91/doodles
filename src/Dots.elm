module Dots exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Color
import Html exposing (Html, button, div, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Random
import Session exposing (WithSession)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, viewBox, width, x, y)
import Svg.Events
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , colNumber : Int
        , baseColor : DotColor
        , rgbFunc : RGBFunc
        , rgbDist : RGBDist
        , origin : Coordinate
        }


type alias Coordinate =
    ( Row, Col )


type alias Row =
    Int


type alias Col =
    Int


showCoordinate : Coordinate -> String
showCoordinate coordinate =
    "(" ++ (String.fromInt <| Tuple.first coordinate) ++ "," ++ (String.fromInt <| Tuple.second coordinate) ++ ")"


type PrimaryColor
    = Red
    | Green
    | Blue


type alias DotColor =
    { red : Int
    , green : Int
    , blue : Int
    }


showDotColor : DotColor -> String
showDotColor { red, green, blue } =
    "Red: "
        ++ String.fromInt red
        ++ " Green: "
        ++ String.fromInt green
        ++ " Blue: "
        ++ String.fromInt blue


type alias RGBFunc =
    { redFunc : ColorFunc
    , greenFunc : ColorFunc
    , blueFunc : ColorFunc
    }


type ColorFunc
    = ModColor
    | SinColor


showColorFunc : ColorFunc -> String
showColorFunc colorFunc =
    case colorFunc of
        ModColor ->
            "Modulo"

        SinColor ->
            "Sine Wave"


colorFuncFromString : String -> ColorFunc
colorFuncFromString str =
    case str of
        "Mod" ->
            ModColor

        "Sin" ->
            SinColor

        _ ->
            ModColor


type alias RGBDist =
    { red : DistFunc
    , green : DistFunc
    , blue : DistFunc
    }


type DistFunc
    = Radial
    | Taxi
    | Mult
    | ColOnly
    | RowOnly


type alias Dist =
    Int


getDist : DistFunc -> Row -> Col -> Dist
getDist f rowDist colDist =
    case f of
        Radial ->
            ceiling <| sqrt (toFloat (rowDist * rowDist) + toFloat (colDist * colDist))

        Taxi ->
            rowDist + colDist

        Mult ->
            rowDist * colDist

        ColOnly ->
            colDist

        RowOnly ->
            rowDist


distFuncFromString : String -> DistFunc
distFuncFromString str =
    case str of
        "Radial" ->
            Radial

        "Taxi" ->
            Taxi

        "Mult" ->
            Mult

        "ColOnly" ->
            ColOnly

        "RowOnly" ->
            RowOnly

        _ ->
            Taxi


showDistFunc : DistFunc -> String
showDistFunc distFunc =
    case distFunc of
        Radial ->
            "Radial"

        Taxi ->
            "Taxi"

        Mult ->
            "Multiply"

        ColOnly ->
            "Col Only"

        RowOnly ->
            "Row Only"


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , colNumber = 50
      , baseColor = { red = 12, green = 12, blue = 12 }
      , rgbFunc = { redFunc = SinColor, greenFunc = ModColor, blueFunc = ModColor }
      , rgbDist = { red = Radial, green = Radial, blue = Radial }
      , origin = ( 7, 3 )
      }
    , Task.perform GotViewport getViewport
    )


type Msg
    = GotViewport Viewport
    | IncrementColNumber
    | DecrementColNumber
    | ClickDot DotColor Coordinate
    | RandomizeColor Int
    | RandomizeDotFunc RGBFunc
    | RandomizeDistFunc RGBDist
    | SetRedColorFunction String
    | SetGreenColorFunction String
    | SetBlueColorFunction String
    | SetRedDistFunction String
    | SetGreenDistFunction String
    | SetBlueDistFunction String
    | GetRandomColor


generateBaseColor : Cmd Msg
generateBaseColor =
    Random.generate RandomizeColor <| Random.int 1 12


generateDotColor : Cmd Msg
generateDotColor =
    Random.generate RandomizeDotFunc dotFuncGenerator


dotFuncGenerator : Random.Generator RGBFunc
dotFuncGenerator =
    Random.map3 RGBFunc
        colorFuncGenerator
        colorFuncGenerator
        colorFuncGenerator


colorFuncGenerator : Random.Generator ColorFunc
colorFuncGenerator =
    Random.uniform SinColor [ ModColor ]


generateDotDist : Cmd Msg
generateDotDist =
    Random.generate RandomizeDistFunc dotDistGenerator


dotDistGenerator : Random.Generator RGBDist
dotDistGenerator =
    Random.map3 RGBDist
        distFuncGenerator
        distFuncGenerator
        distFuncGenerator


distFuncGenerator : Random.Generator DistFunc
distFuncGenerator =
    Random.uniform Radial [ Taxi, Mult, RowOnly, ColOnly ]



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        IncrementColNumber ->
            ( { model | colNumber = model.colNumber + 1 }, Cmd.none )

        DecrementColNumber ->
            ( { model | colNumber = model.colNumber - 1 }, Cmd.none )

        ClickDot oldColor origin ->
            let
                nextColor =
                    getNextColor oldColor
            in
            ( { model | baseColor = nextColor, origin = origin }, Cmd.none )

        RandomizeColor color ->
            let
                nextColor =
                    DotColor color 12 12
            in
            ( { model | baseColor = nextColor }, Cmd.none )

        RandomizeDotFunc df ->
            ( { model | rgbFunc = df }, Cmd.none )

        RandomizeDistFunc df ->
            ( { model | rgbDist = df }, Cmd.none )

        GetRandomColor ->
            ( model, Cmd.batch [ generateBaseColor, generateDotColor, generateDotDist ] )

        SetRedColorFunction red ->
            let
                oldRgbFunc =
                    model.rgbFunc

                nextRgbFunc =
                    { oldRgbFunc | redFunc = colorFuncFromString red }
            in
            ( { model | rgbFunc = nextRgbFunc }, Cmd.none )

        SetGreenColorFunction green ->
            let
                oldRgbFunc =
                    model.rgbFunc

                nextRgbFunc =
                    { oldRgbFunc | greenFunc = colorFuncFromString green }
            in
            ( { model | rgbFunc = nextRgbFunc }, Cmd.none )

        SetBlueColorFunction blue ->
            let
                oldRgbFunc =
                    model.rgbFunc

                nextRgbFunc =
                    { oldRgbFunc | blueFunc = colorFuncFromString blue }
            in
            ( { model | rgbFunc = nextRgbFunc }, Cmd.none )

        SetRedDistFunction red ->
            let
                oldRgbDistFunc =
                    model.rgbDist

                nextRgbFunc =
                    { oldRgbDistFunc | red = distFuncFromString red }
            in
            ( { model | rgbDist = nextRgbFunc }, Cmd.none )

        SetGreenDistFunction green ->
            let
                oldRgbDistFunc =
                    model.rgbDist

                nextRgbFunc =
                    { oldRgbDistFunc | green = distFuncFromString green }
            in
            ( { model | rgbDist = nextRgbFunc }, Cmd.none )

        SetBlueDistFunction blue ->
            let
                oldRgbDistFunc =
                    model.rgbDist

                nextRgbFunc =
                    { oldRgbDistFunc | blue = distFuncFromString blue }
            in
            ( { model | rgbDist = nextRgbFunc }, Cmd.none )


getNextColor : DotColor -> DotColor
getNextColor color =
    let
        maxColor =
            if color.red > color.green && color.green > color.blue then
                Red

            else if color.green > color.red && color.red > color.blue then
                Green

            else if color.blue > color.red && color.red > color.green then
                Blue

            else
                Red
    in
    case maxColor of
        Red ->
            { color | red = modBaseColor <| color.red + 10, green = modBaseColor <| color.green - 1, blue = modBaseColor <| color.blue - 1 }

        Green ->
            { color | red = modBaseColor <| color.red - 1, green = modBaseColor <| color.green + 10, blue = modBaseColor <| color.blue - 1 }

        Blue ->
            { color | red = modBaseColor <| color.red - 1, green = modBaseColor <| color.green - 1, blue = modBaseColor <| color.blue + 10 }


modBaseColor : Int -> Int
modBaseColor c =
    modBy 12 c



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        buttonStyle =
            "mr-4 bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-4 border border-blue-500 hover:border-transparent rounded"
    in
    case model.viewport of
        Just viewport ->
            div [ class "p-32" ]
                [ div []
                    [ button [ class buttonStyle, onClick IncrementColNumber ] [ text "+" ]
                    , button [ class buttonStyle, onClick DecrementColNumber ] [ text "-" ]
                    , button [ class buttonStyle, onClick GetRandomColor ] [ text "R" ]
                    , rgbColorFunctionSelector model.rgbFunc
                    , rgbDistFunctionSelector model.rgbDist
                    ]
                , div [] [ text <| showDotColor model.baseColor ]
                , div [] [ text <| showCoordinate model.origin ]
                , art viewport model
                ]

        Nothing ->
            div [] [ text "loading..." ]


rgbColorFunctionSelector : RGBFunc -> Html Msg
rgbColorFunctionSelector rgbFunc =
    div []
        [ colorRedFunctionSelector rgbFunc.redFunc
        , colorGreenFunctionSelector rgbFunc.greenFunc
        , colorBlueFunctionSelector rgbFunc.blueFunc
        ]


colorRedFunctionSelector : ColorFunc -> Html Msg
colorRedFunctionSelector colorFunc =
    div []
        [ select [ onInput SetRedColorFunction ]
            [ option [ value "Mod", selected (colorFunc == ModColor) ] [ text "Mod" ]
            , option [ value "Sin", selected (colorFunc == SinColor) ] [ text "Sin" ]
            ]
        ]


colorGreenFunctionSelector : ColorFunc -> Html Msg
colorGreenFunctionSelector colorFunc =
    div []
        [ select [ onInput SetGreenColorFunction ]
            [ option [ value "Mod", selected (colorFunc == ModColor) ] [ text "Mod" ]
            , option [ value "Sin", selected (colorFunc == SinColor) ] [ text "Sin" ]
            ]
        ]


colorBlueFunctionSelector : ColorFunc -> Html Msg
colorBlueFunctionSelector colorFunc =
    div []
        [ select [ onInput SetBlueColorFunction ]
            [ option [ value "Mod", selected (colorFunc == ModColor) ] [ text "Mod" ]
            , option [ value "Sin", selected (colorFunc == SinColor) ] [ text "Sin" ]
            ]
        ]



-- text <| showColorFunc colorFunc


rgbDistFunctionSelector : RGBDist -> Html Msg
rgbDistFunctionSelector rgbDist =
    div []
        [ redDistFunctionSelector rgbDist.red
        , greenDistFunctionSelector rgbDist.green
        , blueDistFunctionSelector rgbDist.blue
        ]


redDistFunctionSelector : DistFunc -> Html Msg
redDistFunctionSelector distFunc =
    div []
        [ select [ onInput SetRedDistFunction ]
            [ option [ value "Radial", selected (distFunc == Radial) ] [ text "Radial" ]
            , option [ value "Taxi", selected (distFunc == Taxi) ] [ text "Taxi" ]
            , option [ value "Mult", selected (distFunc == Mult) ] [ text "Mult" ]
            , option [ value "RowOnly", selected (distFunc == RowOnly) ] [ text "RowOnly" ]
            , option [ value "ColOnly", selected (distFunc == ColOnly) ] [ text "ColOnly" ]
            ]
        ]


greenDistFunctionSelector : DistFunc -> Html Msg
greenDistFunctionSelector distFunc =
    div []
        [ select [ onInput SetGreenDistFunction ]
            [ option [ value "Radial", selected (distFunc == Radial) ] [ text "Radial" ]
            , option [ value "Taxi", selected (distFunc == Taxi) ] [ text "Taxi" ]
            , option [ value "Mult", selected (distFunc == Mult) ] [ text "Mult" ]
            , option [ value "RowOnly", selected (distFunc == RowOnly) ] [ text "RowOnly" ]
            , option [ value "ColOnly", selected (distFunc == ColOnly) ] [ text "ColOnly" ]
            ]
        ]


blueDistFunctionSelector : DistFunc -> Html Msg
blueDistFunctionSelector distFunc =
    div []
        [ select [ onInput SetBlueDistFunction ]
            [ option [ value "Radial", selected (distFunc == Radial) ] [ text "Radial" ]
            , option [ value "Taxi", selected (distFunc == Taxi) ] [ text "Taxi" ]
            , option [ value "Mult", selected (distFunc == Mult) ] [ text "Mult" ]
            , option [ value "RowOnly", selected (distFunc == RowOnly) ] [ text "RowOnly" ]
            , option [ value "ColOnly", selected (distFunc == ColOnly) ] [ text "ColOnly" ]
            ]
        ]


art : Viewport -> Model -> Html Msg
art viewport { colNumber, baseColor, origin, rgbFunc, rgbDist } =
    let
        { width, height } =
            viewport.viewport

        m =
            min width height

        w =
            ceiling <| (m + 100) / toFloat (colNumber * 2)
    in
    div
        [ class "grid dot-box m-auto border-2" ]
        (listOfDots baseColor rgbFunc rgbDist w colNumber origin)


listOfDots : DotColor -> RGBFunc -> RGBDist -> Int -> Int -> Coordinate -> List (Html Msg)
listOfDots baseColor rgbFunc rgbDist w count origin =
    let
        baseMatrix =
            List.repeat count (List.repeat count 0)
    in
    List.indexedMap (\rowIdx row -> rowToDots baseColor rgbFunc rgbDist w rowIdx row origin) baseMatrix


rowToDots : DotColor -> RGBFunc -> RGBDist -> Int -> Int -> List a -> Coordinate -> Html Msg
rowToDots baseColor rgbFunc rgbDist w rowIdx row origin =
    div [ class "flex flex-row" ]
        (List.indexedMap (\colIdx _ -> dot baseColor rgbFunc rgbDist w rowIdx colIdx origin) row)



---- Color Functions ---


sinColor : Int -> Float
sinColor dist =
    255 * sin (0.5 * toFloat dist)


modColor : Int -> Float
modColor dist =
    modBy 255 (dist * 20) |> toFloat


getRed : ColorFunc -> Int -> Float
getRed colorFunc dist =
    showModColor colorFunc dist


getGreen : ColorFunc -> Int -> Float
getGreen colorFunc dist =
    showModColor colorFunc dist


getBlue : ColorFunc -> Int -> Float
getBlue colorFunc dist =
    showModColor colorFunc dist


showModColor : ColorFunc -> (Int -> Float)
showModColor func =
    case func of
        ModColor ->
            modColor

        SinColor ->
            sinColor


dot : DotColor -> RGBFunc -> RGBDist -> Int -> Int -> Int -> Coordinate -> Html Msg
dot { red, green, blue } { redFunc, greenFunc, blueFunc } rgbDist w rowIdx colIdx origin =
    let
        distanceToRow =
            rowIdx - Tuple.first origin

        distanceToCol =
            colIdx - Tuple.second origin

        redDist =
            getDist rgbDist.red distanceToRow distanceToCol

        greenDist =
            getDist rgbDist.green distanceToRow distanceToCol

        blueDist =
            getDist rgbDist.blue distanceToRow distanceToCol

        nextRed =
            getRed redFunc redDist

        nextGreen =
            getGreen greenFunc greenDist

        nextBlue =
            getBlue blueFunc blueDist

        dotColor =
            DotColor red green blue

        fillColor =
            Color.fromRGB ( nextRed, nextGreen, nextBlue )
                |> Color.toRGBString

        wString =
            String.fromInt w

        rString =
            String.fromInt (w // 2)
    in
    div [ class "w-full h-full flex justify-center items-center" ]
        [ svg [ Svg.Events.onClick <| ClickDot dotColor ( rowIdx, colIdx ), width wString, height wString, viewBox ("0 0 " ++ wString ++ " " ++ wString) ]
            [ circle
                [ cx rString
                , cy rString
                , fill fillColor
                , r rString
                , width <| String.fromInt w
                , height <| String.fromInt w
                ]
                []
            ]
        ]
