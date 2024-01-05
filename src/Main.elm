module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


subscriptions _ =
  onAnimationFrameDelta Delta

type Model = Model TeeState (List Int) Float
type TeeState = TeeState (List (List Elt)) Float
type Elt = T | U | V


init () =
  ( Model (TeeState [[T, T, T, T, T],
                     [U, U, U, U, U],
                     [V, V, V, V, V]]
                    0)
          [0, 80, 160]
          0
  , Cmd.none)



type Msg = Delta Float


updateTee delta (TeeState oldTee oldPhase) =
  let phase = oldPhase + delta in
  if phase > 1000 then
    TeeState (List.drop 1 oldTee ++ [newRow]) (phase - 1000)
  else
    TeeState oldTee phase

update msg (Model tee boxes oldTheta) =
  ( case msg of
      Delta delta ->
        let theta = oldTheta + delta/100 in
        Model (updateTee delta tee) (updateBoxes theta boxes) theta
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


newRow = [T, T, T, T, T]

saurImg = "../asset/saur.png"

greenBox =
    rect [ x "100", y "10", width "40", height "40", fill "green"
         , stroke "black" , strokeWidth "2"
         ] []

bagelSpin rot =
    foreignObject [ x "10", y "100", width "100", height "100", rotation rot ]
                  [ img [src saurImg] [] ]

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

rotation rot =
    transform ("rotate(" ++ rot ++ ", 60, 150)")

render elts =
  div [] <| List.map renderElt elts

renderElt e =
  Html.text <| case e of
    T -> "tee"
    U -> "you"
    V -> "vee"
