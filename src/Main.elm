module Main exposing (..)

import Browser
import Html exposing (Html, button, div, fieldset, h1, img, input, label, legend, li, ol, p, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)



---- MODEL ----
{--
How are we going to model our State?
Lets create a custom type called Todo
    this Todo Type will be a record holding
    text that is a type of String
    completed that is a type of Bool
--}


type alias Todo =
    { text : String
    , completed : Bool
    }



{--
We want to see a List of Todo's
and we want a input field to add those todos.
and we want to track that input
--}


type alias Model =
    { todos : List Todo
    , inputText : String
    , filter : Filter
    }


type alias RadioWithLabelProps =
    { filter : Filter
    , label : String
    , name : String
    , checked : Bool
    }



{--
            type vs type alias
            type IS defining and naming a new type
            type alias IS NOT creating a distint type
                IT IS giving a name to an existing TYPE
                it is to save keystrokes nothing more
            --}
{--
This is a list of possiable actions that can happen in the todo app
Each one has to have (EVENT?? ) that happens in the UPDATE section
thase are actions from UPDATE that accept an argument (except AddTodo)
--}


type Msg
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | ChangeInput String
    | ChangeFilter Filter


type Filter
    = All
    | Completed
    | Remaining



-- init : ( Model, Cmd Msg )


init =
    { todos = []
    , inputText = ""
    , filter = All
    }



---- UPDATE ----
{--
You can this of the UPDATE as the "reducer" of a Redux app.
It is called whenever you trigger an action in your (MODEL??)
it takes in your old state model and expects you to return the updated state model
--}
-- type Msg
--     = NoOp
{--
MESSAGE is a ACTION
this takes the current state and a message
    then it expects you to return the new updated state
--}
-- update : Action taken -> current state -> return updated state


update : Msg -> Model -> Model
update msg model =
    case msg of
        {--
        this { model | something } copies the current state
        then overrides anything to the right of the pipe
        In this example we change the whole model so it is not needed
        but it helps if we make changes in the future to keep the syntax so we can extend our app
        --}
        AddTodo ->
            { model
                | todos = addToList model.inputText model.todos
                , inputText = ""
            }

        RemoveTodo index ->
            { model | todos = removeFromList index model.todos }

        ToggleTodo index ->
            { model | todos = toggleAtIndex index model.todos }

        ChangeInput input ->
            { model | inputText = input }

        ChangeFilter filter ->
            { model | filter = filter }



{--
This grabs a String and- > a List of type Todo -> then retunrs a updated List of types of Todo
--}


addToList : String -> List Todo -> List Todo



{--
This grabs our Todo's that are nested inside of a list.
Then updates it by adding another Todo to our List of Todos
This function addToList(input, todos){
    returns [...todo, {text: input, completed: false}]
}
--}


addToList input todos =
    todos ++ [ { text = input, completed = False } ]


removeFromList : Int -> List Todo -> List Todo
removeFromList index list =
    List.take index list ++ List.drop (index + 1) list


toggleAtIndex : Int -> List Todo -> List Todo



{--
This takes the list and use a method that maps thru the index of each element using an anonymous function (denoted by the \ lambda symbol)
if that index matches the index we are trying to toggle then
update the todo... the pipe says only update the completed part of the todo
not says to change it to whatever it currently is not
const toggleAtIndex = indexToToggle => todos => 
  todos.map((todo, currentIndex) => 
    indexToToggle === currentIndex 
      ? { ...todo, completed: !todo.completed } 
      : todo
  )
--}


toggleAtIndex indexToToggle list =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == indexToToggle then
                { todo | completed = not todo.completed }

            else
                todo
        )
        list


viewRadioWithLabel : RadioWithLabelProps -> Html Msg
viewRadioWithLabel config =
    label []
        [ input
            [ type_ "radio"
            , name config.name
            , checked config.checked
            , onClick (ChangeFilter config.filter)
            ]
            []
        , text config.label
        ]


viewSelectedFilter : Filter -> Html Msg
viewSelectedFilter filter =
    fieldset []
        [ legend [] [ text "Current filter" ]
        , viewRadioWithLabel
            { filter = All
            , name = "filter"
            , checked = filter == All
            , label = "All Items"
            }
        , viewRadioWithLabel
            { filter = Completed
            , name = "filter"
            , checked = filter == Completed
            , label = "Completed items"
            }
        , viewRadioWithLabel
            { filter = Remaining
            , name = "filter"
            , checked = filter == Remaining
            , label = "Remaining items"
            }
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.form
        [ onSubmit AddTodo
        , class "todoContainer"
        ]
        [ h1
            []
            [ text "Brent's Todo's" ]
        , input
            [ value model.inputText
            , onInput ChangeInput
            , placeholder "Add a new item ..."
            ]
            []
        , viewSelectedFilter model.filter
        , if List.isEmpty model.todos then
            p [] [ text "You should add some items todo!" ]

          else
            ol
                [ class "todoList" ]
                (showTodoItems model.filter model.todos)
        ]


showTodoItems : Filter -> List Todo -> List (Html Msg)
showTodoItems filter todos =
    let
        filteredTodos =
            List.filter (filterTodo filter) todos

        filteredIncomplete =
            List.filter (\t -> not t.completed) filteredTodos

        filteredComplete =
            List.filter (\t -> t.completed) filteredTodos
    in
    List.indexedMap viewTodo (filteredIncomplete ++ filteredComplete)


filterTodo : Filter -> Todo -> Bool
filterTodo filter todo =
    case filter of
        All ->
            True

        Completed ->
            todo.completed

        Remaining ->
            not todo.completed



{--
--Tries that failed above...

--}
{--
Below is made for each List.indexedMap from above.
--}


viewTodo : Int -> Todo -> Html Msg
viewTodo index todo =
    li
        [ style "text-decoration"
            (if todo.completed then
                "line-through"

             else
                "none"
            )
        ]
        [ div [ class "listItem" ]
            [ button [ type_ "button", onClick (ToggleTodo index), class "button toggle" ] [ text "Complete?" ]
            , text todo.text
            , button [ type_ "button", onClick (RemoveTodo index), class "button delete" ] [ text "Delete" ]
            ]
        ]



{---- PROGRAM ----}
--main : Program () Model Msg


main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
--}
