module Main exposing (Model, Msg(..), Todo, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)


type alias Model =
    { todos : List Todo }


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


init : Model
init =
    { todos =
        [ { id = 1
          , title = "Buy oranges"
          , completed = False
          }
        ]
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


todoView : Todo -> Html Msg
todoView { title } =
    li []
        [ input [ type_ "checkbox" ] []
        , text title
        ]


view : Model -> Html Msg
view { todos } =
    div []
        [ div [] [ text "Grocery list:" ]
        , ul [] (List.map todoView todos)
        ]



-- INIT


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
