module Main exposing (Model, Msg(..), Todo, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, span, text, ul)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { todos : List Todo
    , transient : Transient
    , nextId : Int
    }


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


type alias Transient =
    { text : String
    }


init : Model
init =
    { todos =
        [ { id = 1
          , title = "Buy oranges"
          , completed = False
          }
        ]
    , transient = initialTransient
    , nextId = 2
    }


initialTransient : Transient
initialTransient =
    { text = "" }



-- UPDATE


type Msg
    = UpdateText String
    | CreateTodo
    | RemoveTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateText newText ->
            let
                transient =
                    model.transient
            in
            { model | transient = { transient | text = newText } }

        CreateTodo ->
            if String.isEmpty (String.trim model.transient.text) then
                model

            else
                { model
                    | todos = Todo model.nextId model.transient.text False :: model.todos
                    , transient = initialTransient
                    , nextId = model.nextId + 1
                }

        RemoveTodo todoId ->
            { model
                | todos = List.filter (\todo -> todo.id /= todoId) model.todos
            }



-- VIEW


todoView : Todo -> Html Msg
todoView { id, completed, title } =
    li []
        [ input [ type_ "checkbox" ] []
        , span [] [ text title ]
        , button [ onClick (RemoveTodo id) ] [ text "Remove" ]
        ]


view : Model -> Html Msg
view { todos, transient } =
    div []
        [ div []
            [ text "Add item"
            , input [ placeholder "Banana", onInput UpdateText, value transient.text ] []
            , button [ onClick CreateTodo ] [ text "Add" ]
            ]
        , div [] [ text "Grocery list:" ]
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
