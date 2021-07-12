module Main exposing (main)

import Browser
import Html exposing (Html, button, fieldset, h1, input, label, legend, li, ol, p, text)
import Html.Attributes exposing (checked, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)

main = Browser.sandbox { init = init, view = view, update = update }

type alias Todo =
    {   text : String
    ,   completed : Bool
    }

type alias Model =
    {   todos : List Todo
    ,   inputText : String
    ,   filter : Filter
    }

type alias RadioWithLabelProps =
    {   filter : Filter
    ,   label : String
    ,   name : String
    ,   checked : Bool
    }

init : Model
init =
    {   todos = []
    ,   inputText = ""
    ,   filter = All
    }

type Message
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | ChangeInput String
    | ChangeFilter Filter

type Filter
    = All
    | Completed
    | Remaining

update : Message -> Model -> Model
update message model =
    case message of
        AddTodo ->
            {
                model
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
            { model | filter = filter}

addToList : String -> List Todo -> List Todo
addToList input todos =
    todos ++ [{ text = input, completed = False }]

removeFromList : Int -> List Todo -> List Todo
removeFromList index list =
    List.take index list ++ List.drop (index + 1) list

toggleAtIndex : Int -> List Todo -> List Todo
toggleAtIndex indexToToggle list =
    List.indexedMap (\currentIndex todo ->
        if currentIndex == indexToToggle then
            { todo | completed = not todo.completed }
        else
            todo
    ) list

view : Model -> Html Message
view model =
    Html.form [ onSubmit AddTodo ]
        [ h1 [] [ text "Todos in Elm" ]
        , input [ value model.inputText, onInput ChangeInput, placeholder "What do you want to do?" ] []
        , viewSelectFilter model.filter
        , if List.isEmpty model.todos then
            p [] [ text "The list is clean 🧘‍♀️" ]
          else
            ol [] (model.todos
                |> List.filter (applyFilter model.filter)
                |> List.indexedMap viewTodo)
        ]

applyFilter : Filter -> Todo -> Bool
applyFilter filter todo =
    case filter of
        All -> True
        Completed -> todo.completed
        Remaining -> not todo.completed

viewTodo : Int -> Todo -> Html Message
viewTodo index todo =
    li
        [ style "text-decoration"
            (if todo.completed then
                "line-through"
             else
                "none"
            )
        ]
        [ text todo.text
        , button [ type_ "button", onClick (ToggleTodo index) ] [ text "Toggle" ]
        , button [ type_ "button", onClick (RemoveTodo index) ] [ text "Delete" ]
        ]

viewRadioWithLabel : RadioWithLabelProps -> Html Message
viewRadioWithLabel config =
    label []
        [ input
            [ type_ "radio"
            , name config.name
            , checked config.checked
            , onClick (ChangeFilter config.filter)
            ] []
        , text config.label
        ]

viewSelectFilter: Filter -> Html Message
viewSelectFilter filter =
    fieldset []
        [ legend [] [ text "Current Filter" ]
        , viewRadioWithLabel
            { filter = All
            , name = "filter"
            , checked = filter == All
            , label = "All items"
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