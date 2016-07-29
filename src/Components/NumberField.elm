module Components.NumberField exposing (update, view, init, Model, Msg(..), render)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (Cmd)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Options exposing (css)
import String
import Html.Events exposing (..)


-- APP


main : Program Never
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { value : Maybe Float
    , stringValue : String
    , errorMessage : String
    , mdl :
        Material.Model
        -- Boilerplate: mdl is the Model store for any and all MDL components you need.
    }


init : ( Model, Cmd msg )
init =
    ( { value = Nothing
      , stringValue = ""
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
        InputNumber str n ->
            ( { model | value = Just n, stringValue = str, errorMessage = "" }, Cmd.none )

        Error str err ->
            ( { model | value = Nothing, stringValue = str, errorMessage = err }, Cmd.none )

        {- Boilerplate: MDL action handler. It should always look like this. -}
        MDL subMsg ->
            Material.update MDL subMsg model



-- VIEW


render : Int -> Model -> String -> Html Msg
render n model lab =
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
            [ Textfield.render MDL
                [ n ]
                model.mdl
                [ Textfield.label lab
                , Textfield.floatingLabel
                , Textfield.value model.stringValue
                , Textfield.onInput onInputHandler
                , case model.value of
                    Nothing ->
                        Textfield.error <| model.errorMessage

                    Just _ ->
                        Options.nop
                ]
            ]


view : Model -> Html Msg
view model =
    render 0 model "sample"
        |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
