module Main exposing (..)

import Array as A exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import List exposing (all, concat, drop, filter, head, length, map, map2, range, reverse)
import Random
import String as S
import Svg exposing (Attribute, Svg, foreignObject, svg)
import Svg.Attributes as SA exposing (fill, height, stroke, strokeWidth, transform, viewBox, width)


----=== CONFIGURATION ===----

umImg = "um.png"
slowness = 2500
horizTimeslice = 0.2


----=== MODEL DEFINITION ===----

type alias Pipe = { n : Bool , e : Bool , s : Bool , w : Bool }
type alias PipeRow = { y : Int , pipes : List Pipe }
type alias Um = { from : Int , to : Int , endFrameNum : Int , spin : Bool }
type alias Model = { seed : Maybe Random.Seed , pipes : List PipeRow , frameNum : Float , um : Um }


----=== INITIAL CONDITIONS ===----

main : Program () Model Msg
main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }

init : () -> ( Model, Cmd Msg )
init () =
  ( { seed = Nothing
    , frameNum = 0
    , pipes = []
    , um = { from = 2, to = 2, endFrameNum = 0, spin = False }
    }
  , generateSeed
  )

subscriptions : a -> Sub Msg
subscriptions _ = onAnimationFrameDelta Delta


---- ENTROPY ----

generatePipes : Int -> Random.Seed -> ( List Pipe, Random.Seed )
generatePipes y seed = Random.step pipeGen seed

generateTarget : Random.Seed -> ( Int, Random.Seed )
generateTarget seed = Random.step (Random.int 0 59) seed

generateSeed = Random.generate InitSeed Random.independentSeed

type Msg = InitSeed Random.Seed | Delta Float


----=== MODEL EVOLUTION ===----

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    InitSeed seed ->
      -- Generate initial pipe row which is “behind the clock”
      -- since the frame clock starts at the middle of the field.
      let ( p, seed1 ) = generatePipes -1 seed in
      ( { model | seed = Just seed1, pipes = appendPipeRow model.pipes (PipeRow -1 p) }
      , Cmd.none
      )
    Delta delta ->
      let frameNum = model.frameNum + delta / slowness in
      case model.seed of
        Nothing -> ( model, Cmd.none )  -- Spin our wheels and hope a seed shows up.
        Just seed ->
          let (pipes, pipeUpdate) = updatePipes frameNum model.pipes seed in
          let
            (seed1, pipes1, um0) =
              case pipeUpdate of
                Nothing -> (seed, pipes, model.um)
                Just (y, (p, s)) ->
                  let appendedPipes = appendPipeRow pipes (PipeRow y p) in
                  let ( ss, umm ) = if y == 3 then
                                      let ( target, sss ) = generateTarget s in
                                      ( sss, updateUm appendedPipes target frameNum model.um )
                                    else (s, model.um)
                  in
                  ( ss
                  , appendedPipes
                  , umm
                  )
          in
          let
            (seed2, um) =
              case getUpdateUm frameNum um0 seed1 of
                Nothing -> (seed1, um0)
                Just (target, s) -> (s, updateUm pipes1 target frameNum um0)
          in
          ( Model (Just seed2) pipes1 frameNum um
          , Cmd.none
          )

---- PIPE GRID ----

updatePipes : Float
           -> List PipeRow
           -> Random.Seed
           -> (List PipeRow, Maybe (Int, (List Pipe, Random.Seed)))
updatePipes frameNum pipes seed =
  let vispipes = filter (\p -> boxY p.y frameNum > -2) pipes in
  case head <| reverse vispipes of
    Nothing -> let f = round frameNum in (vispipes, Just (f, generatePipes f seed))
    Just p -> if boxY p.y frameNum > 6 then
                (vispipes, Nothing)
              else
                let yGen = p.y + 1 in
                (vispipes, Just (yGen, generatePipes yGen seed))

pipeGen : Random.Generator (List Pipe)
pipeGen =
  let toss = Random.uniform True [False] in
  Random.list 5 <| Random.map4 Pipe toss toss toss toss

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

getUpdateUm : Float -> Um -> Random.Seed -> Maybe (Int, Random.Seed)
getUpdateUm frameNum um seed =
  if frameNum > (toFloat um.endFrameNum) then
    Just (generateTarget seed)
  else
    Nothing

updateUm : List PipeRow -> Int -> Float -> Um -> Um
updateUm pipes target frameNum um =
  let (dest, spin) = selectGoal pipes um.to target in
  Um um.to dest (ceiling frameNum) spin
  
selectGoal : List PipeRow -> Int -> Int -> (Int, Bool)
selectGoal rows start target =
  case nth 3 rows of
    Nothing -> (start, True)
    Just row ->
      let blocked = findConnected start (A.fromList row.pipes) in
      if length blocked == 0 then (start, True)
      else let unblocked = filter (\i -> case (nth (i+1) row.pipes) of
                                                Nothing -> False
                                                Just p -> p.s) blocked in
        let (candidates, spin) = if length unblocked == 0 then (blocked, True) else (unblocked, False) in
        case nth (1 + (modBy (length candidates) target)) candidates of
          Nothing -> (start, True)
          Just i -> (i, spin)

nth : Int -> List a -> Maybe a
nth n lst = lst |> drop (n - 1) |> head

findConnected : Int -> Array Pipe -> List Int
findConnected start pipes =
  filter (connected pipes start) (range 0 4)

connected : Array Pipe -> Int -> Int -> Bool
connected pipes start end =
  if start == end then True
  else if start < end then all .e (A.toList (A.slice start end pipes))
  else all .w (A.toList (A.slice (end+1) (start+1) pipes))


----=== VIEW ===----

view : Model -> Html Msg
view model =
  div [] [ svg
    [ viewBox "0 0 480 400"
    , width "480"
    , height "400"
    ]
    [ umView model.frameNum model.um , pipeGrid model.frameNum model.pipes ]]

---- MON ----

umView : Float -> Um -> Svg msg
umView frameNum um =
    let x = 40 + (80 * xUm frameNum um) in
    let y = 70 + 80 * yUm frameNum um in
    let r = if um.spin then 360 * max 0 (((1 + horizTimeslice) * umParam frameNum um) - horizTimeslice) else 0 in
    foreignObject [ SA.x <| S.fromFloat x, SA.y <| S.fromFloat y,
                    width "100", height "100", rotation (S.fromFloat r) x ]
                  [ img [src umImg, width "80", height "80" ] [] ]

rotation : String -> Float -> Attribute msg
rotation rot pos =
    let x = S.fromFloat (pos + 45) in
    let y = S.fromFloat 175 in
    transform ("rotate(" ++ rot ++ ", " ++ x ++ ", " ++ y ++ ")")

xUm : Float -> Um -> Float
xUm frameNum um =
  interp (toFloat um.from) (toFloat um.to) (umParam frameNum um) horizTimeslice

yUm : Float -> Um -> Float
yUm frameNum um =
  let t = umParam frameNum um in
  if t < horizTimeslice then
    1 - t
  else
    (1 - 2 * horizTimeslice + horizTimeslice * t) / (1 - horizTimeslice)

umParam : Float -> Um -> Float
umParam frameNum um = 1 + frameNum - (toFloat um.endFrameNum)

interp : Float -> Float -> Float -> Float -> Float
interp a b t s =
  let tt = min 1 (t / s) in
  b * tt + a * (1 - tt)

---- PIPES ----

pipeGrid : Float -> List PipeRow -> Svg msg
pipeGrid frameNum pipeRows =
    svg [ SA.x "0", SA.y "0", width "480", height "400", viewBox "0 0 6 3" ] <|
        concat <| map
          (\row -> map2 (\x p -> pipeCell (0.5 + toFloat x) (boxY row.y frameNum) 1 1 p)
            (range 0 ((length row.pipes) - 1))
            row.pipes
          )
          pipeRows

boxY : Int -> Float -> Float
boxY rowY frameNum = toFloat rowY - frameNum

pipeCell : Float -> Float -> Int -> Int -> Pipe -> Svg msg
pipeCell x y w h pipe =
  svg [ SA.x <| S.fromFloat x, SA.y <| S.fromFloat y
      , width <| S.fromInt w, height <| S.fromInt h
      , viewBox "0 0 10 10"
      ] <| pipePaths pipe

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
