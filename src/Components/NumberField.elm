module Components.NumberField exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (Cmd)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options exposing (css)
import String
import Html.Events exposing (..)


-- APP


main : Program Never
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { value : Float
    , origValue : String
    , hasError : Bool
    , errorMessage : String
    , mdl :
        Material.Model
        -- Boilerplate: mdl is the Model store for any and all MDL components you need.
    }


init : ( Model, Cmd msg )
init =
    ( { value = 0
      , origValue = ""
      , hasError = False
      , errorMessage = ""
      , mdl =
            Material.model
            -- Boilerplate: Always use this initial MDL model store.
      }
    , Cmd.none
    )



-- ACTION, UPDATE


type Msg
    = InputNumber String Float
    | Error String String
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputNumber origValue n ->
            ( { model | value = n, origValue = origValue, hasError = False, errorMessage = "" }, Cmd.none )

        Error origValue err ->
            ( { model | origValue = origValue, hasError = True, errorMessage = err }, Cmd.none )

        {- Boilerplate: MDL action handler. It should always look like this. -}
        MDL subMsg ->
            Material.update MDL subMsg model



-- VIEW


inputNumber : Int -> Model -> String -> Html Msg
inputNumber n model lab =
    let
        onInputHandler =
            \str ->
                case String.toFloat str of
                    Ok n ->
                        if n > 0 then
                            InputNumber str n
                        else
                            Error str "Number should be > 0"

                    Err err ->
                        Error str err
    in
        div [ style [ ( "padding", "2em" ) ] ]
            ([ Textfield.render MDL
                 [ n ]
                 model.mdl
                 [ Textfield.label lab
                 , Textfield.floatingLabel
                 , Textfield.value model.origValue
                 , Textfield.onInput onInputHandler
                 ]
             ]
                ++ if model.hasError then
                    [ div
                        [ class "alert alert-danger"
                        , attribute "role" "alert"
                        ]
                        [ span
                            [ class "glyphicon glyphicon-exclamation-sign"
                            , class "aria-hidden"
                            ]
                            []
                        , span [] [ text model.errorMessage ]
                        ]
                    ]
                   else
                    []
            )


view : Model -> Html Msg
view model =
    inputNumber 0 model "sample"
        |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
