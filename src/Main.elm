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

pathNE = svgPath "M 6 0 L 6 3 A 1 1 0 0 0 7 4 L 10 4"
pathES = svgPath "M 6 10 L 6 7 A 1 1 0 0 1 7 6 L 10 6"
pathSW = svgPath "M 0 6 L 3 6 A 1 1 0 0 1 4 7 L 4 10"
pathWN = svgPath "M 0 4 L 3 4 A 1 1 0 0 0 4 3 L 4 0"
pathNS = svgPath "M 6 0 L 6 10"
pathSN = svgPath "M 4 0 L 4 10"
pathWE = svgPath "M 0 4 L 10 4"
pathEW = svgPath "M 0 6 L 10 6"

-- TODO
pipePaths : Pipe -> List (Svg msg)
pipePaths p =
  List.concat [ if p.n && p.e then [ pathNE ] else []
              , if p.e && p.s then [ pathES ] else []
              , if p.s && p.w then [ pathSW ] else []
              , if p.w && p.n then [ pathWN ] else []
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
