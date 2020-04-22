module Doodle.Recaman exposing (Model, Msg, init, update, view)

import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Seq exposing (Seq)
import Session exposing (WithSession)
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, stroke, strokeWidth, viewBox)
import Task


type alias Model =
    WithSession
        { viewport : Maybe Viewport
        , recaman : Recaman
        , scale : Scale
        }


type alias Recaman =
    List Int


type alias Scale =
    Int


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , viewport = Nothing
      , recaman = generateRecaman 0 0 10

      -- [ 0, 10, 15, 30, 20, 25, 7 ]
      , scale = 10
      }
    , Task.perform GotViewport getViewport
    )


sequenceToPairs : List Int -> List ( Int, Int )
sequenceToPairs seq =
    -- [1, 2, 3, 4] -> [(1, 2), (2, 3), (3, 4)]
    let
        h =
            List.head seq

        next =
            List.tail seq
    in
    case ( h, next ) of
        ( Just hd, Just nextSeq ) ->
            let
                nextHead =
                    List.head nextSeq
            in
            case nextHead of
                Just nextH ->
                    ( hd, nextH ) :: sequenceToPairs nextSeq

                Nothing ->
                    []

        ( _, _ ) ->
            []


generateRecaman : Int -> Int -> Int -> List Int
generateRecaman n root count =
    let
        seq =
            Debug.log "seq" (recamanSeq n root Set.empty)

        -- Seq.numbers
        nTerms =
            Debug.log "nTerms" (Seq.take count seq)
    in
    Debug.log "list" <| Seq.toList nTerms



-- [ 1, 2, 4 ]


recamanSeq : Int -> Int -> Set Int -> Seq Int
recamanSeq n lastTerm visited =
    let
        minusTerm =
            Debug.log "minusTerm" <| lastTerm - n

        plusTerm =
            Debug.log "plusTerm" <| lastTerm + n
    in
    if n < 2 then
        if minusTerm >= 0 && not (Set.member minusTerm visited) then
            let
                nextSet =
                    Set.insert minusTerm visited

                -- Set.empty
                nextSeq =
                    recamanSeq (n + 1) minusTerm nextSet
            in
            -- Seq.cons minusTerm nextSeq
            -- (recamanSeq (n + 1) minusTerm (Set.insert minusTerm visited))
            Seq.cons minusTerm Seq.numbers

        else
            let
                nextSet =
                    Set.insert plusTerm visited

                -- Set.empty
                nextSeq =
                    recamanSeq (n + 1) plusTerm nextSet
            in
            -- Seq.cons plusTerm nextSeq
            -- Seq.cons plusTerm Seq.numbers
            Seq.cons plusTerm
                (\a -> recamanSeq (n + 1) plusTerm nextSet)

    else
        Seq.cons minusTerm Seq.numbers



-- (recamanSeq (n + 1) plusTerm (Set.insert plusTerm visited))
-- Seq.cons plusTerm Seq.numbers
--   @JSExport
--   def generateRecaman(n: Int, root: Int, count: Int) = {
--     val list = recaman(n, root, Set()).take(count).toList
--     list.asJson.noSpaces
--   }
--
--   @JSExport
--   def recaman(n: Int, lastTerm: Int, visited: Set[Int]): Stream[Int] = {
--     val minusTerm: Int = lastTerm - n
--     if (minusTerm >= 0 && !visited.contains(minusTerm)) {
--       minusTerm #:: recaman(n + 1, minusTerm, visited.+(minusTerm))
--     } else {
--       val plusTerm: Int = lastTerm + n
--       plusTerm #:: recaman(n + 1, plusTerm, visited + plusTerm)
--     }
--   }
-- }


type Msg
    = GotViewport Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )



---- VIEW ----


view : Model -> Html msg
view model =
    case model.viewport of
        Just viewport ->
            art viewport model

        Nothing ->
            div [] [ text "loading..." ]


art : Viewport -> Model -> Html msg
art viewport { recaman, scale } =
    let
        { width, height } =
            viewport.viewport

        m =
            min width height

        mString =
            String.fromInt <| ceiling m

        pairs =
            Debug.log "pairs" <| sequenceToPairs recaman

        pairsWithScale =
            Debug.log "scaledPairs" <| applyScale scale pairs

        startY =
            200
    in
    div [ class "border bg-gray-100" ]
        [ svg [ viewBox <| "0 0 " ++ mString ++ " " ++ mString ]
            [ path
                [ d <| makeSvgPath startY scale pairsWithScale
                , stroke "black"
                , strokeWidth "1"
                , fill "white"
                ]
                []
            ]
        ]


makeSvgPath : Int -> Int -> List ( Int, Int ) -> String
makeSvgPath startY scale pairs =
    let
        maybeFirstPoint =
            List.head pairs
    in
    case maybeFirstPoint of
        Just ( startX, _ ) ->
            pathDrawing
                (startPoint startX startY
                    :: makeHalfCirlces startY pairs
                )

        Nothing ->
            ""


pathDrawing : List String -> String
pathDrawing parts =
    String.join " " parts


startPoint : Int -> Int -> String
startPoint x y =
    String.join " " [ "M", String.fromInt x, String.fromInt y ]


applyScale : Int -> List ( Int, Int ) -> List ( Int, Int )
applyScale scale seq =
    List.map (\t -> Tuple.mapBoth (\x -> x * scale) (\x -> x * scale) t) seq


makeHalfCirlces : Int -> List ( Int, Int ) -> List String
makeHalfCirlces y points =
    List.indexedMap (\idx ( l, r ) -> halfCircle y l r (sweepDirection l r idx)) points


sweepDirection : Int -> Int -> Int -> Bool
sweepDirection left right idx =
    let
        forward =
            right - left > 0

        odd =
            modBy 2 idx == 1
    in
    forward && odd


halfCircle : Int -> Int -> Int -> Bool -> String
halfCircle startY startX endX orientation =
    let
        radius =
            (endX - startX) // 2

        sweepFlag =
            if orientation then
                1

            else
                0
    in
    arc radius radius 0 sweepFlag endX startY


arc : Int -> Int -> Int -> Int -> Int -> Int -> String
arc rx ry xRot sweepFlag endX endY =
    let
        pieces =
            [ rx, ry, xRot, 0, sweepFlag, endX, endY ]

        strings =
            "A" :: List.map String.fromInt pieces
    in
    String.join " " strings
