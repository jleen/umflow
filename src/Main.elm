module Main exposing (..)

import Array as A exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import List exposing (all, concat, drop, filter, head, length, map, map2, range, reverse)
import Random
import Svg exposing (Attribute, Svg, foreignObject, svg)
import Svg.Attributes as SA exposing (fill, height, stroke, strokeWidth, transform, viewBox, width)


----=== CONFIGURATION ===----

umImg = "../asset/um.png"
slowness = 250


----=== MODEL DEFINITION ===----

type alias Pipe = { n : Bool , e : Bool , s : Bool , w : Bool }
type alias PipeRow = { y : Int , pipes : List Pipe }
type alias Um = { from : Int , to : Int , endPhase : Int , spin : Bool }
type alias Model = { pipes : List PipeRow , theta : Float , um : Um }


----=== INITIAL CONDITIONS ===----

main : Program () Model Msg
main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }

init : () -> ( Model, Cmd Msg )
init () =
  ( Model [] 0 (Um 0 0 1 False), generatePipes 0 )

subscriptions : a -> Sub Msg
subscriptions _ = onAnimationFrameDelta Delta


---- ENTROPY ----

generatePipes y = Random.generate (GotPipes y) pipeGen

generateTarget = Random.generate GotTarget (Random.int 0 59)

type Msg = Delta Float | GotPipes Int (List Pipe) | GotTarget Int


----=== MODEL EVOLUTION ===----

---- PIPE GRID ----

-- This is maybe a sort of silly way to do this.
-- We’ve already generated a set of pipes, but
-- they don’t join up properly.
-- So now we transform them to a properly-joined pipe set,
-- just by clobbering N and E pipes with their neighbors.
-- This is a bit inefficient, but it’s not a bottleneck,
-- and it preserves uniform randomness.
appendPipeRow : List PipeRow -> PipeRow -> List PipeRow
appendPipeRow pipeRows pipeRow =
  let row = reconcileV (head <| reverse pipeRows) pipeRow in
  pipeRows ++ [ { row | pipes = reconcileH row.pipes } ]

reconcileV : Maybe PipeRow -> PipeRow -> PipeRow
reconcileV pp this =
  case pp of
    Nothing -> this
    Just prev -> { this | pipes = map2 (\p t -> { t | n = p.s }) prev.pipes this.pipes }

reconcileH : List Pipe -> List Pipe
reconcileH pipes =
  case pipes of
    p1 :: p2 :: ps -> { p1 | e = p2.w } :: (reconcileH <| p2 :: ps)
    ps -> ps


---- MON ----

getUpdateUm : Float -> Um -> Cmd Msg
getUpdateUm phase um =
  if phase > (toFloat um.endPhase) then
    generateTarget
  else
    Cmd.none

nth : Int -> List a -> Maybe a
nth n lst = lst |> drop (n-1) |> head

connected : Array Pipe -> Int -> Int -> Bool
connected pipes start end =
  if start == end then True
  else if start < end then all .e (A.toList (A.slice start end pipes))
  else all .w (A.toList (A.slice (end+1) (start+1) pipes))

findCandidates : Int -> Array Pipe -> List Int
findCandidates start pipes =
  filter (connected pipes start) (range 0 4)

seek : List PipeRow -> Int -> Int -> (Int, Bool)
seek rows start target =
  case nth 3 rows of
    Nothing -> (start, True)
    Just row ->
      let blocked = findCandidates start (A.fromList row.pipes) in
      if length blocked == 0 then (start, True)
      else let unblocked = filter (\i -> case (nth (i+1) row.pipes) of
                                                Nothing -> False
                                                Just p -> p.s) blocked in
        let (candidates, spin) = if length unblocked == 0 then (blocked, True) else (unblocked, False) in
        case nth (1 + (modBy (length candidates) target)) candidates of
          Nothing -> (start, True)
          Just i -> (i, spin)

updateUm : List PipeRow -> Int -> Float -> Um -> Um
updateUm pipes target phase um =
  let (dest, spin) = seek pipes um.to target in
  Um um.to dest (ceiling phase) spin
  
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Delta delta ->
      let theta = model.theta + delta / slowness in
      let (pipes, pCmd) = updatePipes theta model.pipes in
      let umCmd = getUpdateUm (pipePhase theta) model.um in
        ( Model pipes theta model.um
        , Cmd.batch [ pCmd, umCmd ] )
    GotPipes y p ->
      ( { model | pipes = appendPipeRow model.pipes <| PipeRow y p }
      , Cmd.none
      )
    GotTarget target ->
      ( { model | um = updateUm model.pipes target (pipePhase model.theta) model.um }
      , Cmd.none
      )

pipeGen : Random.Generator (List Pipe)
pipeGen =
  let toss = Random.uniform True [False] in
  Random.list 5 <| Random.map4 Pipe toss toss toss toss

pipePhase : Float -> Float
pipePhase t = t / 10

updatePipes : Float -> List PipeRow -> (List PipeRow, Cmd Msg)
updatePipes theta pipes =
  let vispipes = filter (\p -> boxpos p.y theta > -2) pipes in
  case head <| reverse vispipes of
    Nothing -> (vispipes, generatePipes <| round <| pipePhase theta)
    Just p -> if boxpos p.y theta > 6 then
                (vispipes, Cmd.none)
              else
                (vispipes, generatePipes <| p.y + 1)


----=== VIEW ===----

view : Model -> Html Msg
view model =
  div [] [ svg
    [ viewBox "0 0 450 400"
    , width "450"
    , height "400"
    ]
    [ umSpin model.um <| pipePhase model.theta , pipeGrid model.theta model.pipes ]]

---- MON ----
interp : Float -> Float -> Float -> Float -> Float
interp a b t s =
  let tt = Basics.min 1 (t/s) in
  b * tt + a * (1-tt)

tc : Float
tc = 0.2

umParam : Float -> Um -> Float
umParam phase um = 1 + phase - (toFloat um.endPhase)

spinState : Float -> Um -> Float
spinState phase um =
  interp (toFloat um.from) (toFloat um.to) (umParam phase um) tc

fallState : Float -> Um -> Float
fallState phase um =
  let t = umParam phase um in
  if t < tc then
    1 - t
  else
    (1 - 2*tc + tc*t) / (1-tc)

umSpin : Um -> Float -> Svg msg
umSpin um phase =
    let x = 10 + (80 * spinState phase um) in
    let y = 70 + 80 * fallState phase um in
    let r = if um.spin then 360 * Basics.max 0 (((1+tc) * umParam phase um) - tc) else 0 in
    foreignObject [ SA.x <| String.fromFloat x, SA.y <| String.fromFloat y,
                    width "100", height "100", rotation (String.fromFloat r) x ]
                  [ img [src umImg, width "80", height "80" ] [] ]

rotation : String -> Float -> Attribute msg
rotation rot pos =
    let x = String.fromFloat (pos + 60) in
    let y = String.fromFloat 150 in
    transform ("rotate(" ++ rot ++ ", " ++ x ++ ", " ++ y ++ ")")

---- PIPES ----

svgPath : String -> Svg msg
svgPath path = Svg.path [ SA.d path, stroke "blue", fill "none", strokeWidth "0.2" ] []

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
  concat [ pathIf ne <| n && e
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
  svg [ SA.x <| String.fromInt xx, SA.y <| String.fromFloat yy
      , width <| String.fromInt ww, height <| String.fromInt hh
      , viewBox "0 0 10 10"
      ] <| pipePaths pipe

boxpos : Int -> Float -> Float
boxpos x theta = toFloat x - pipePhase theta

pipeGrid : Float -> List PipeRow -> Svg msg
pipeGrid theta pipeRows =
    svg [ SA.x "10", SA.y "10", width "450", height "400", viewBox "0 0 5.625 3" ] <|
        concat <| map
          (\row -> map2 (\x p -> pipebox x (boxpos row.y theta) 1 1 p)
            (range 0 ((length row.pipes) - 1))
            row.pipes
          )
          pipeRows
