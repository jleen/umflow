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

type Model = Model TeeState (List Int) Float
type TeeState = TeeState (List (List Elt)) Float
type Elt = T | U | V


init : () -> ( Model, Cmd msg )
init () =
  ( Model (TeeState [[T, T, T, T, T],
                     [U, U, U, U, U],
                     [V, V, V, V, V]]
                    0)
          [0, 80, 160]
          0
  , Cmd.none)

type Msg = Delta Float | GotRnd Elt

updateTee : Float -> TeeState -> (TeeState, Cmd Msg)
updateTee delta (TeeState oldTee oldPhase) =
  let phase = oldPhase + delta in
  if phase > 1000 then
    ( TeeState oldTee (phase - 1000), Random.generate GotRnd <| Random.uniform T [ U, V ] )
  else
    ( TeeState oldTee phase, Cmd.none )

addBox : Elt -> TeeState -> TeeState
addBox r (TeeState tee phase) =
  TeeState (List.drop 1 tee ++ [[ r, r, r, r, r ]]) phase

update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model oldTee oldBoxes oldTheta) =
  case msg of
    Delta delta ->
      let theta = oldTheta + delta/100 in
      let (tee, cmd) = updateTee delta oldTee in
        ( Model tee (updateBoxes theta oldBoxes) theta
        , cmd)
    GotRnd r ->
      ( Model (addBox r oldTee) oldBoxes oldTheta
      , Cmd.none)


-- Toss old boxes and add new ones as necessary.
-- TODO: Use an abstract coordinate system
--       so that this isn’t tied to the box size.
-- TODO: Keep separate phases for the animation elements
--       so that theta doesn’t get out of control
updateBoxes : Float -> List Int -> List Int
updateBoxes theta boxes =
  let visBoxes = List.filter (\x -> boxpos x theta > -50) boxes in
  case List.head <| List.reverse visBoxes of
    Nothing -> visBoxes
    Just x -> if boxpos x theta > -20 then visBoxes else visBoxes ++ [x + 80]

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

pipebox : Int -> Float -> Int -> Int -> Svg msg
pipebox xx yy ww hh =
    svg [ x <| String.fromInt xx, y <| String.fromFloat yy
        , width <| String.fromInt ww, height <| String.fromInt hh
        , viewBox "0 0 10 10"
        ]
        [ svgPath "M 0 4 L 3 4 A 1 1 0 0 0 4 3 L 4 0"
        , svgPath "M 0 6 L 3 6 A 1 1 0 0 1 4 7 L 4 10"
        , svgPath "M 6 0 L 6 10"
        ]

boxpos : Int -> Float -> Float
boxpos x theta =
  toFloat x + 10 - theta/3

boxbox : Float -> List Int -> Svg msg
boxbox theta boxes =
    svg [ x "10", y "10", width "80", height "80" ] <|
        List.map (\x -> pipebox 10 (boxpos x theta) 80 80) boxes

view : Model -> Html Msg
view (Model (TeeState tee _) boxes theta) =
  let rot = String.fromInt (round theta) in
  div [] <| List.map render tee ++ [Html.p [] [text rot], svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    [ greenBox, bagelSpin rot, boxbox theta boxes ]]

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
