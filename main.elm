-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html, div, text, br)
import Keyboard exposing (KeyCode)
import Char exposing (fromCode)
import String exposing (concat)
import List exposing (head, tail)
import Maybe

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Zombie =
    { completed : List Char
    , next : Char
    , waiting : List Char
    }

shoot : Zombie -> Char -> Maybe Zombie
shoot zombie char =
    if char == zombie.next then
      case zombie.waiting of
        [] -> Nothing
        c :: cs -> Just (Zombie (char :: zombie.completed) c cs)
    else 
        Just zombie

-- MODEL

type alias Model = 
    { zombies : List Zombie
    }

init : (Model, Cmd Msg)
init =
  ({zombies = {completed = [], next = 'K', waiting = String.toList "illifish"}}, Cmd.none)

-- UPDATE

type Msg
  = Press KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Press code ->
      let char = fromCode code in
        ({ model | zombie = shoot model.zombie char }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.presses Press
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text (String.fromList model.zombie.waiting)
    , br [] []
    , text (String.fromList model.zombie.completed)
    ]

