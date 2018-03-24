import Html exposing (Html, div, text, br)
import Keyboard exposing (KeyCode)
import Char exposing (fromCode)
import String exposing (concat)
import List
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

shoot : Char -> Zombie -> Zombie
shoot char zombie =
  if char == zombie.next then
    case zombie.waiting of
      [] -> zombie
      c :: cs -> Zombie (char :: zombie.completed) c cs
  else 
    zombie

unshoot : Zombie -> Zombie
unshoot zombie =
  case zombie.completed of
    [] -> zombie
    c :: cs -> Zombie cs c (zombie.next :: zombie.waiting)

-- MODEL

type alias Model = 
  { buffer : List Char
  , zombies : List Zombie
  , last : KeyCode
  , count : Int
  }

init : (Model, Cmd Msg)
init =
  ({ buffer = []
   , zombies =
     [ Zombie [] 'K' (String.toList "illifish")
     , Zombie [] 'K' (String.toList "ill the fish")
     ]
   , last = 0
   , count = 0
   }, Cmd.none)

-- UPDATE

type Msg
  = Press KeyCode
  | Down KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Down code ->
      if code == k_backspace || code == k_delete then
        ({ model
        | buffer = case model.buffer of
          [] -> []
          c :: cs -> cs
        , last = code
        , zombies = List.map unshoot model.zombies
        }, Cmd.none)
      else
        ({ model | last = code }, Cmd.none)
    Press code ->
      if isValid code then
        ({ model
        | buffer = Char.fromCode code :: model.buffer
        , last = code
        , zombies = List.map (shoot (Char.fromCode code)) model.zombies
        }, Cmd.none)
      else
        ({ model | last = code }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.presses Press
    , Keyboard.downs Down
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div [] <|
    List.intersperse
      (br [] [])
      (
        [ text (String.fromList (List.reverse model.buffer))
        , text (toString model.last)
        , text (toString model.count)
        ]
        ++
        (List.map
          (\zombie -> text (String.fromList (zombie.next :: zombie.waiting)))
          model.zombies
        )
      )

-- keycode constants
k_backspace = 8
k_delete = 46
k_space = Char.toCode ' '
k_period = Char.toCode '.'
k_comma = Char.toCode ','
k_bang = Char.toCode '!'
k_question = Char.toCode '?'
k_zero = Char.toCode '0'
k_nine = Char.toCode '9'
k_a = Char.toCode 'a'
k_z = Char.toCode 'z'
k_A = Char.toCode 'A'
k_Z = Char.toCode 'Z'

isValid : KeyCode -> Bool
isValid code
  = k_a <= code && code <= k_z
  || k_A <= code && code <= k_Z
  || k_zero <= code && code <= k_nine
  || code == k_space
  || code == k_bang
  || code == k_question
  || code == k_comma
  || code == k_period

