module Main exposing (..)

import Json.Decode
import String
import List
import Css exposing (..)
import Css.Colors exposing (black)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (on, onInput, onFocus, onBlur, keyCode)
import Html.Styled.Attributes exposing (css, placeholder, value)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


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
            , "world!"
            , "foo bar"
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
                | placeholder = "Click here to resume"
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


view : Model -> Html Msg
view model =
    div [ css [ fontFamily monospace ] ]
        [ p [] [ text "Type a word and press <enter> to eliminate it." ]
        , input
            [ onInput Change
            , onEnter Confirm
            , onFocus Focus
            , onBlur Blur
            , placeholder model.placeholder

            {- Setting the input value field to a model member introduces the
               "cursor jump" bug: https://github.com/elm-lang/html/issues/105
            -}
            , value model.field
            ]
            []
        , ul
            [ css [ listStyle none ] ]
            (List.map
                (\target ->
                    let
                        targetChars =
                            String.toList target

                        fieldChars =
                            String.toList model.field
                    in
                        li [] (viewTarget targetChars (matching targetChars fieldChars))
                )
                model.targets
            )
        ]



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


matching : List Char -> List Char -> List Bool
matching expected actual =
    if List.length expected < List.length actual then
        List.map (\_ -> False) expected
    else
        case expected of
            [] ->
                []

            expectedFirst :: expectedRest ->
                case actual of
                    [] ->
                        False :: matching expectedRest []

                    actualFirst :: actualRest ->
                        (actualFirst == expectedFirst) :: matching expectedRest actualRest


viewTarget : List Char -> List Bool -> List (Html Msg)
viewTarget chars mask =
    List.map2
        (\char bit ->
            if bit then
                span [ css [ typed ] ] [ text <| String.fromChar char ]
            else
                span [ css [ untyped ] ] [ text <| String.fromChar char ]
        )
        chars
        mask


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
