module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }

subscriptions : a -> Sub Msg
subscriptions _ =
  onAnimationFrameDelta Delta

type alias Pipe =
  { n : Bool
  , e : Bool
  , s : Bool
  , w : Bool
  }

type alias PipeRow =
  { y : Int
  , pipes : List Pipe
  }

type alias Model =
  { tee : TeeState
  , pipes : List PipeRow
  , theta : Float
  }

type alias TeeState =
  { rows : (List (List Elt))
  , phase : Float
  }

type Elt = T | U | V

omniPipe = Pipe True True True True

omniPipeRow y = PipeRow y [ omniPipe, omniPipe, omniPipe, omniPipe, omniPipe ]

init : () -> ( Model, Cmd msg )
init () =
  ( Model (TeeState [[T, T, T, T, T],
                     [U, U, U, U, U],
                     [V, V, V, V, V]]
                    0)
          [ omniPipeRow 0, omniPipeRow 80, omniPipeRow 160 ]
          0
  , Cmd.none)

type Msg = Delta Float | GotRnd Elt

updateTee : Float -> TeeState -> (TeeState, Cmd Msg)
updateTee delta tee =
  let phase = tee.phase + delta in
  if phase > 1000 then
    ( TeeState tee.rows (phase - 1000), Random.generate GotRnd <| Random.uniform T [ U, V ] )
  else
    ( TeeState tee.rows phase, Cmd.none )

addBox : Elt -> TeeState -> TeeState
addBox r tee =
  TeeState (List.drop 1 tee.rows ++ [[ r, r, r, r, r ]]) tee.phase

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Delta delta ->
      let theta = model.theta + delta/100 in
      let (tee, cmd) = updateTee delta model.tee in
        ( Model tee (updatepipes theta model.pipes) theta
        , cmd)
    GotRnd r ->
      ( Model (addBox r model.tee) model.pipes model.theta
      , Cmd.none)

updatepipes : Float -> List PipeRow -> List PipeRow
updatepipes theta pipes =
  let vispipes = List.filter (\p -> boxpos p.y theta > -50) pipes in
  case List.head <| List.reverse vispipes of
    Nothing -> vispipes
    Just p -> if boxpos p.y theta > -20 then vispipes else vispipes ++ [omniPipeRow <| p.y + 80]

saurImg : String
saurImg = "../asset/saur.png"

greenBox : Svg msg
greenBox =
    rect [ x "100", y "10", width "40", height "40", fill "green"
         , stroke "black" , strokeWidth "2"
         ] []

bagelSpin : String -> Svg msg
bagelSpin rot =
    foreignObject [ x "10", y "100", width "100", height "100", rotation rot ]
                  [ img [src saurImg] [] ]

svgPath : String -> Svg msg
svgPath path = Svg.path [ d path, stroke "blue", fill "none", strokeWidth "0.2" ] []

pathNE = svgPath "M 6 0 L 6 3 A 1 1 0 0 0 7 4 L 10 4"
pathES = svgPath "M 6 10 L 6 7 A 1 1 0 0 1 7 6 L 10 6"
pathSW = svgPath "M 0 6 L 3 6 A 1 1 0 0 1 4 7 L 4 10"
pathWN = svgPath "M 0 4 L 3 4 A 1 1 0 0 0 4 3 L 4 0"
pathNS = svgPath "M 6 0 L 6 10"
pathSN = svgPath "M 4 0 L 4 10"
pathWE = svgPath "M 0 4 L 10 4"
pathEW = svgPath "M 0 6 L 10 6"

pipebox : Int -> Float -> Int -> Int -> Svg msg
pipebox xx yy ww hh =
    svg [ x <| String.fromInt xx, y <| String.fromFloat yy
        , width <| String.fromInt ww, height <| String.fromInt hh
        , viewBox "0 0 10 10"
        ]
        [ pathWN, pathSW, pathNE, pathES ]

boxpos : Int -> Float -> Float
boxpos x theta =
  toFloat x + 10 - theta/3

boxbox : Float -> List PipeRow -> Svg msg
boxbox theta pipes =
    svg [ x "10", y "10", width "80", height "80" ] <|
        List.map (\p -> pipebox 0 (boxpos p.y theta) 80 80) pipes

view : Model -> Html Msg
view { tee, pipes, theta } =
  let rot = String.fromInt (round theta) in
  div [] <| List.map render tee.rows ++ [Html.p [] [text rot], svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    [ greenBox, bagelSpin rot, boxbox theta pipes ]]

rotation : String -> Attribute msg
rotation rot =
    transform ("rotate(" ++ rot ++ ", 60, 150)")

render : List Elt -> Html msg
render elts =
  div [] <| List.map renderElt elts

renderElt : Elt -> Html msg
renderElt e =
  Html.text <| case e of
    T -> "tee"
    U -> "you"
    V -> "vee"
