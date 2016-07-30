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
    = ValueChanged String
    | Validate (Result String Float)
    | MDL Material.Msg


convertAndValidate : String -> Result String Float
convertAndValidate str =
    (String.toFloat str)
        `Result.andThen` \num ->
                            if (num >= 0) then
                                Ok num
                            else
                                Err "Number should be > 0"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValueChanged str ->
            update (Validate <| convertAndValidate str) { model | stringValue = str }

        Validate result ->
            let
                newModel =
                    case result of
                        Ok num ->
                            { model | value = Just num }

                        Err err ->
                            { model | value = Nothing, errorMessage = err }
            in
                ( newModel, Cmd.none )

        {- Boilerplate: MDL action handler. It should always look like this. -}
        MDL subMsg ->
            Material.update MDL subMsg model



-- VIEW


render : Int -> Model -> String -> Html Msg
render n model lab =
    div [ style [ ( "padding", "2em" ) ] ]
        [ Textfield.render MDL
            [ n ]
            model.mdl
            [ Textfield.label lab
            , Textfield.floatingLabel
            , Textfield.value model.stringValue
            , Textfield.onInput <| ValueChanged
            , case model.value of
                Nothing ->
                    if model.stringValue == "" then
                        Options.nop
                    else
                        Textfield.error <| model.errorMessage

                Just _ ->
                    Options.nop
            ]
        ]


view : Model -> Html Msg
view model =
    render 0 model "Sample"
        |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
