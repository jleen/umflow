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

type alias Um =
  { from : Int
  , to : Int
  , endPhase : Int
  }

type alias Model =
  { pipes : List PipeRow
  , theta : Float
  , um : Um
  }

init : () -> ( Model, Cmd Msg )
init () =
  ( Model [] 0 (Um 0 0 1), generatePipes 0 )

generatePipes y = Random.generate (GotPipes y) pipeGen

generateTarget = Random.generate GotTarget (Random.int 0 4)

type Msg = Delta Float | GotPipes Int (List Pipe) | GotTarget Int

-- This is maybe a sort of silly way to do this.
-- We’ve already generated a set of pipes, but
-- they don’t join up properly.
-- So now we transform them to a properly-joined pipe set,
-- just by clobbering N and E pipes with their neighbors.
-- This is a bit inefficient, but it’s not a bottleneck,
-- and it preserves uniform randomness.
appendPipeRow : List PipeRow -> PipeRow -> List PipeRow
appendPipeRow pipeRows pipeRow =
  let row = reconcileV (List.head <| List.reverse pipeRows) pipeRow in
  pipeRows ++ [ { row | pipes = reconcileH row.pipes } ]

reconcileV : Maybe PipeRow -> PipeRow -> PipeRow
reconcileV pp this =
  case pp of
    Nothing -> this
    Just prev -> { this | pipes = List.map2 (\p t -> { t | n = p.s }) prev.pipes this.pipes }

reconcileH : List Pipe -> List Pipe
reconcileH pipes =
  case pipes of
    p1 :: p2 :: ps -> { p1 | e = p2.w } :: (reconcileH <| p2 :: ps)
    ps -> ps

getUpdateUm : Float -> Um -> Cmd Msg
getUpdateUm phase um =
  if phase > (toFloat um.endPhase) then
    generateTarget
  else
    Cmd.none

updateUm : Int -> Float -> Um -> Um
updateUm target phase um =
  Um um.to target (ceiling phase)
  
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Delta delta ->
      let theta = model.theta + delta/500 in
      let (pipes, pCmd) = updatePipes theta model.pipes in
      let umCmd = getUpdateUm (pipePhase theta) model.um in
        ( Model pipes theta model.um
        , Cmd.batch [ pCmd, umCmd ] )
    GotPipes y p ->
      ( { model | pipes = appendPipeRow model.pipes <| PipeRow y p }
      , Cmd.none
      )
    GotTarget target ->
      ( { model | um = updateUm target (pipePhase model.theta) model.um }
      , Cmd.none
      )

pipeGen : Random.Generator (List Pipe)
pipeGen =
  let toss = Random.uniform True [False] in
  Random.list 5 <| Random.map4 Pipe toss toss toss toss

pipePhase : Float -> Float
pipePhase t = t/10

updatePipes : Float -> List PipeRow -> (List PipeRow, Cmd Msg)
updatePipes theta pipes =
  let vispipes = List.filter (\p -> boxpos p.y theta > -1) pipes in
  case List.head <| List.reverse vispipes of
    Nothing -> (vispipes, generatePipes <| round <| pipePhase theta)
    Just p -> if boxpos p.y theta > 4 then
                (vispipes, Cmd.none)
              else
                (vispipes, generatePipes <| p.y + 1)


---- VIEW ----

saurImg : String
saurImg = "../asset/saur.png"

interp : Float -> Float -> Float -> Float -> Float
interp a b t s =
  let tt = Basics.min 1 (t/s) in
  b * tt + a * (1-tt)

spinState : Float -> Um -> Float
spinState phase um =
  let t = 1 + phase - (toFloat um.endPhase) in
  interp (toFloat um.from) (toFloat um.to) t 0.2

bagelSpin : String -> Um -> Float -> Svg msg
bagelSpin rot um shift =
    let pos = 10 + (80 * spinState shift um) in
    foreignObject [ x <| String.fromFloat pos, y "100", width "100", height "100", rotation rot pos ]
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
nx = svgPath "M 4 0 L 4 3 L 6 3 L 6 0"
ex = svgPath "M 10 4 L 7 4 L 7 6 L 10 6"
sx = svgPath "M 6 10 L 6 7 L 4 7 L 4 10"
wx = svgPath "M 0 6 L 3 6 L 3 4 L 0 4"

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
              , pathIf nx <| n && not e && not s && not w
              , pathIf ex <| e && not s && not w && not n
              , pathIf sx <| s && not w && not n && not e
              , pathIf wx <| w && not n && not e && not s
              ]

pipebox : Int -> Float -> Int -> Int -> Pipe -> Svg msg
pipebox xx yy ww hh pipe =
  svg [ x <| String.fromInt xx, y <| String.fromFloat yy
      , width <| String.fromInt ww, height <| String.fromInt hh
      , viewBox "0 0 10 10"
      ] <| pipePaths pipe

boxpos : Int -> Float -> Float
boxpos x theta = toFloat x - pipePhase theta

boxbox : Float -> List PipeRow -> Svg msg
boxbox theta pipeRows =
    svg [ x "10", y "10", width "400", height "240", viewBox "0 0 5 3" ] <|
        List.concat <| List.map
          (\row -> List.map2 (\x p -> pipebox x (boxpos row.y theta) 1 1 p)
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
    [ bagelSpin rot model.um <| pipePhase model.theta , boxbox model.theta model.pipes ]]

rotation : String -> Float -> Attribute msg
rotation rot pos =
    let x = String.fromFloat (pos + 60) in
    let y = String.fromFloat 150 in
    transform ("rotate(" ++ rot ++ ", " ++ x ++ ", " ++ y ++ ")")
