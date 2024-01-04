module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions =
  Time.every 1000 Tick

-- MODEL


type Model = Model Row Row Row Row Row
type Row = Row Elt Elt Elt Elt Elt
type Elt = T | U | V


init : Model
init =
  Model (Row T T T T T)
        (Row U U U U U)
        (Row V V V V V)
        (Row U U U U U)
        (Row T T T T T)



-- UPDATE


type Msg = Tick


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW


view : Model -> Html Msg
view (Model a b c d e) =
  div []
    [ button [ onClick Decrement ] [ text "+" ]
    , button [ onClick Increment ] [ text "+" ]
    ]
