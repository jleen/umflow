module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



-- MAIN


main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


subscriptions _ =
  Time.every 1000 Tick

-- MODEL


type Model = Model (List (List Elt))
type Elt = T | U | V


init () =
  (Model [[T, T, T, T, T],
          [U, U, U, U, U],
          [V, V, V, V, V]]
  , Cmd.none)



type Msg = Tick Time.Posix


update msg (Model model) =
  (case msg of
     Tick _ ->
       Model <| List.drop 1 model ++ [newRow]
  , Cmd.none)

newRow =
  [T, T, T, T, T]



-- VIEW


view : Model -> Html Msg
view (Model m) =
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
    , foreignObject [ x "10", y "100", width "100", height "100", transform "rotate(-30, 60, 150)" ]
        [Html.img [Html.Attributes.src "https://images.squarespace-cdn.com/content/v1/5400890ee4b03f524b003725/1415037601583-POTPRXXO72EZZ87SWRTU/favicon.ico?format=100w"] []]]]

render elts =
  div [] <| List.map renderElt elts

renderElt e =
  Html.text <| case e of
    T -> "tee"
    U -> "you"
    V -> "vee"
