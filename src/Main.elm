module Main exposing (..)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



-- MAIN


main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


subscriptions _ =
  Time.every 1000 Tick

-- MODEL


type Model = Model (List (List Elt)) Int
type Elt = T | U | V


init () =
  (Model [[T, T, T, T, T],
          [U, U, U, U, U],
          [V, V, V, V, V]]
         0
  , Cmd.none)



type Msg = Tick Time.Posix


update msg (Model model theta) =
  (case msg of
     Tick time ->
       Model (List.drop 1 model ++ [newRow]) ((Time.toSecond Time.utc time) * 10)
  , Cmd.none)

newRow =
  [T, T, T, T, T]



-- VIEW


bagelsaurus = "https://images.squarespace-cdn.com/content/v1/5400890ee4b03f524b003725/1415037601583-POTPRXXO72EZZ87SWRTU/favicon.ico?format=100w"
view : Model -> Html Msg
view (Model m theta) =
  let rot = String.fromInt theta in
  div [] <| List.map render m ++ [svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    [ rect
        [ x "100"
        , y "10"
        , width "40"
        , height "40"
        , fill "green"
        , stroke "black"
        , strokeWidth "2"
        ] []
    , foreignObject [ x "10", y "100", width "100", height "100"
                    , transform ("rotate(" ++ rot ++ ", 60, 150)") ]
        [img [src bagelsaurus] []]]]

render elts =
  div [] <| List.map renderElt elts

renderElt e =
  Html.text <| case e of
    T -> "tee"
    U -> "you"
    V -> "vee"
