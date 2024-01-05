module Main exposing (..)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


subscriptions _ =
  Time.every 1000 Tick

type Model = Model (List (List Elt)) Int
type Elt = T | U | V


init () =
  ( Model [[T, T, T, T, T],
          [U, U, U, U, U],
          [V, V, V, V, V]]
          0
  , Cmd.none)



type Msg = Tick Time.Posix


update msg (Model model _) =
  ( case msg of
      Tick time ->
        Model (List.drop 1 model ++ [newRow]) ((Time.toSecond Time.utc time) * 10)
  , Cmd.none)

newRow = [T, T, T, T, T]

saurImg = "https://images.squarespace-cdn.com/content/v1/"
          ++ "5400890ee4b03f524b003725/1415037601583-POTPRXXO72EZZ87SWRTU/favicon.ico?format=100w"

greenBox =
    rect [ x "100", y "10", width "40", height "40", fill "green"
         , stroke "black" , strokeWidth "2"
         ] []

bagelSpin rot =
    foreignObject [ x "10", y "100", width "100", height "100", rotation rot ]
                  [ img [src saurImg] [] ]

svgPath path = Svg.path [ d path, stroke "blue", fill "none", strokeWidth "0.2" ] []

pipebox : Int -> Int -> Int -> Int -> Svg msg
pipebox xx yy ww hh =
    svg [ x <| String.fromInt xx, y <| String.fromInt yy
        , width <| String.fromInt ww, height <| String.fromInt hh
        , viewBox "0 0 10 10"
        ]
        [ svgPath "M 0 4 L 3 4 A 1 1 0 0 0 4 3 L 4 0"
        , svgPath "M 0 6 L 3 6 A 1 1 0 0 1 4 7 L 4 10"
        , svgPath "M 6 0 L 6 10"
        ]

boxbox rot =
    svg [ x "10", y "10", width "80", height "80" ]
        [ pipebox 10 (10-(rot//3)) 80 80 --
        , pipebox 10 (90-(rot//3)) 80 80
        ]

view : Model -> Html Msg
view (Model m theta) =
  let rot = String.fromInt theta in
  div [] <| List.map render m ++ [text rot, svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    [ greenBox, bagelSpin rot, boxbox theta ]]

rotation rot =
    transform ("rotate(" ++ rot ++ ", 60, 150)")

render elts =
  div [] <| List.map renderElt elts

renderElt e =
  Html.text <| case e of
    T -> "tee"
    U -> "you"
    V -> "vee"
