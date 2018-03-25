module Main exposing (..)

import Json.Decode
import String
import List
import Maybe
import Css exposing (..)
import Css.Colors exposing (black)
import Keyboard
import Char exposing (KeyCode)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (on, onInput, onFocus, onBlur, keyCode)
import Html.Styled.Attributes exposing (css, placeholder, value)


main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Target =
    { chars : List Char
    , done : List Bool
    }


target : String -> Target
target x =
    { chars = String.toList x, done = List.repeat (String.length x) False }


shoot : Target -> String -> Target
shoot target text =
    { target
        | done = matching target.chars (String.toList text)
    }


matching : List Char -> List Char -> List Bool
matching expected actual =
    if List.length actual > List.length expected then
        List.repeat (List.length expected) False
    else
        case expected of
            [] ->
                []

            expectedFirst :: expectedRest ->
                case actual of
                    [] ->
                        False :: matching expectedRest []

                    actualFirst :: actualRest ->
                        (actualFirst == expectedFirst)
                            :: matching expectedRest actualRest


type alias Model =
    { field : String
    , placeholder : String
    , targets : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { field = ""
      , placeholder = "Click here to begin!"
      , targets =
            [ "hello"
            , "foo"
            , "world!"
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | Confirm
    | Focus
    | Blur


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Blur ->
            ( { model
                | placeholder = "Click to resume"
              }
            , Cmd.none
            )

        Focus ->
            ( { model
                | placeholder = ""
              }
            , Cmd.none
            )

        Confirm ->
            ( { model
                | field = ""
                , targets = List.filter (\t -> model.field /= t) model.targets
              }
            , Cmd.none
            )

        Change text ->
            ( { model
                | field = text
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


untyped : Style
untyped =
    Css.batch
        [ color black
        ]


typed : Style
typed =
    Css.batch
        [ color (hex "aaa")
        ]


viewStringTarget : String -> String -> List (Html Msg)
viewStringTarget expected actual =
    viewTarget (shoot (target expected) actual)


viewTarget : Target -> List (Html Msg)
viewTarget target =
    List.map2
        (\c b ->
            span
                [ css
                    [ if b then
                        typed
                      else
                        untyped
                    ]
                ]
                [ text <| String.fromChar c ]
        )
        target.chars
        target.done



-- onEnter : https://github.com/evancz/elm-todomvc/blob/166e5f2afc704629ee6d03de00deac892dfaeed0/Todo.elm#L237-L246


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        on "keydown" (Json.Decode.andThen isEnter keyCode)



-- Html.Styled.Events.on : String -> Json.Decode.Decoder msg -> Html.Styled.Attribute msg
-- Json.Decode.andThen : (a -> Json.Decode.Decoder b) -> Json.Decode.Decoder a -> Json.Decode.Decoder b


view : Model -> Html Msg
view model =
    div []
        [ input
            [ onInput Change
            , onEnter Confirm
            , onFocus Focus
            , onBlur Blur
            , placeholder model.placeholder
            , value model.field
            ]
            []
        , ul []
            (List.map
                (\t -> li [] (viewStringTarget t model.field))
                model.targets
            )
        ]
