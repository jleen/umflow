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
  { pipes : List PipeRow
  , theta : Float
  }

type Elt = T | U | V

init : () -> ( Model, Cmd Msg )
init () =
  ( Model [] 0 , generatePipes 0 )

generatePipes y = Random.generate (GotPipes y) pipeGen

type Msg = Delta Float | GotPipes Int (List Pipe)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Delta delta ->
      let theta = model.theta + delta/100 in
      let (pipes, pCmd) = updatepipes theta model.pipes in
        ( Model pipes theta
        , pCmd )
    GotPipes y p ->
      ( Model (model.pipes ++ [PipeRow y p]) model.theta
      , Cmd.none
      )

pipeGen : Random.Generator (List Pipe)
pipeGen =
  let toss = Random.uniform True [False] in
  Random.list 5 <| Random.map4 Pipe toss toss toss toss

updatepipes : Float -> List PipeRow -> (List PipeRow, Cmd Msg)
updatepipes theta pipes =
  let vispipes = List.filter (\p -> boxpos p.y theta > -1) pipes in
  case List.head <| List.reverse vispipes of
    Nothing -> (vispipes, generatePipes 0)
    Just p -> if boxpos p.y theta > 4 then
                (vispipes, Cmd.none)
              else
                (vispipes, generatePipes <| p.y + 1)


---- VIEW ----

saurImg : String
saurImg = "../asset/saur.png"

bagelSpin : String -> Svg msg
bagelSpin rot =
    foreignObject [ x "10", y "100", width "100", height "100", rotation rot ]
                  [ img [src saurImg] [] ]

svgPath : String -> Svg msg
svgPath path = Svg.path [ d path, stroke "blue", fill "none", strokeWidth "0.2" ] []

ne = svgPath "M 6 0 L 6 3 A 1 1 0 0 0 7 4 L 10 4"
es = svgPath "M 6 10 L 6 7 A 1 1 0 0 1 7 6 L 10 6"
sw = svgPath "M 0 6 L 3 6 A 1 1 0 0 1 4 7 L 4 10"
wn = svgPath "M 0 4 L 3 4 A 1 1 0 0 0 4 3 L 4 0"
ns = svgPath "M 6 0 L 6 10"
sn = svgPath "M 4 0 L 4 10"
we = svgPath "M 0 4 L 10 4"
ew = svgPath "M 0 6 L 10 6"
neo = svgPath "M 4 0 L 4 5 A 1 1 0 0 0 5 6 L 10 6"
eso = svgPath "M 4 10 L 4 5 A 1 1 0 0 1 5 4 L 10 4"
swo = svgPath "M 0 4 L 5 4 A 1 1 0 0 1 6 5 L 6 10"
wno = svgPath "M 0 6 L 5 6 A 1 1 0 0 0 6 5 L 6 0"

pathIf path cond = if cond then [ path ] else []

pipePaths : Pipe -> List (Svg msg)
pipePaths { n, e, s, w } =
  List.concat [ pathIf ne <| n && e
              , pathIf es <| e && s
              , pathIf sw <| s && w
              , pathIf wn <| w && n
              , pathIf ns <| n && s && not e
              , pathIf sn <| s && n && not w
              , pathIf we <| w && e && not n
              , pathIf ew <| e && w && not s
              , pathIf neo <| n && e && not s && not w
              , pathIf eso <| e && s && not w && not n
              , pathIf swo <| s && w && not n && not e
              , pathIf wno <| w && n && not e && not s
              ]

pipebox : Int -> Float -> Int -> Int -> Pipe -> Svg msg
pipebox xx yy ww hh pipe =
  svg [ x <| String.fromInt xx, y <| String.fromFloat yy
      , width <| String.fromInt ww, height <| String.fromInt hh
      , viewBox "0 0 10 10"
      ] <| pipePaths pipe

boxpos : Int -> Float -> Float
boxpos x theta = toFloat x + 10 - theta/3

boxrow y theta x p = pipebox x (boxpos y theta) 1 1 p

boxbox : Float -> List PipeRow -> Svg msg
boxbox theta pipeRows =
    svg [ x "10", y "10", width "400", height "240", viewBox "0 0 5 3" ] <|
        List.concat <| List.map
          (\row -> List.map2 (boxrow row.y theta)
            (List.range 0 ((List.length row.pipes) - 1))
            row.pipes
          )
          pipeRows

view : Model -> Html Msg
view model =
  let rot = String.fromInt (round model.theta) in
  div [] [ svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    [ bagelSpin rot, boxbox model.theta model.pipes ]]

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
