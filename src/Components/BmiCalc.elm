module Components.BmiCalc exposing (Msg(InputDataChanged), Model, init, update, view)

import Html exposing (..)
import Html.App as App
import Html.Attributes
import Material
import Material.Grid as Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Color exposing (Hue(..))
import Material.Scheme as Scheme
import Material.Options as Options exposing (cs, css, Style)
import Components.NumberField as NumberField exposing (..)
import Cmd.Extra

-- MODEL


type alias Model =
    { heightComponentModel : NumberField.Model
    , weightComponentModel : NumberField.Model
    , bmiValue : Maybe Float
    , mdl : Material.Model
    }


init : Model
init =
    { heightComponentModel = fst NumberField.init
    , weightComponentModel = fst NumberField.init
    , bmiValue = Nothing
    , mdl = Material.model
    }



-- ACTION, UPDATE


calcBMI h w =
    w / (h / 100) ^ 2


round_1 x =
    (toFloat <| round (x * 10)) / 10


type Msg
    = HeightChanged NumberField.Msg
    | WeightChanged NumberField.Msg
    | ReCalc
    | InputDataChanged (Maybe Float)
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeightChanged subMsg ->
            update ReCalc
                { model | heightComponentModel = fst <| NumberField.update subMsg model.heightComponentModel }

        WeightChanged subMsg ->
            update ReCalc
                { model | weightComponentModel = fst <| NumberField.update subMsg model.weightComponentModel }

        ReCalc ->
            let
                bmiValue =
                    Maybe.map round_1
                        <| Maybe.map2 calcBMI model.heightComponentModel.value model.weightComponentModel.value

                newModel =
                    { model | bmiValue = bmiValue }

                cmd =
                    Cmd.Extra.message (InputDataChanged bmiValue)
            in
                ( newModel, cmd )

        InputDataChanged bmiValue ->
            ( model, Cmd.none )

        MDL msg' ->
            Material.update MDL msg' model



-- VIEW


view model =
    Options.div []
        [ grid []
            [ cell
                [ Grid.size All 6
                , Grid.size Phone 12
                ]
                [ App.map HeightChanged <| NumberField.render 0 model.heightComponentModel "身長を入力してください" ]
            , cell
                [ Grid.size All 6
                , Grid.size Phone 12
                ]
                [ App.map WeightChanged <| NumberField.render 1 model.weightComponentModel "体重を入力してください"
                ]
            , cell [ Grid.size All 12 ]
                [ h4 [ Html.Attributes.style [ ( "padding-left", "1em" ) ] ]
                    [ (case model.bmiValue of
                        Just n ->
                            text <| "あなたのBMIは" ++ (toString n) ++ "です"

                        Nothing ->
                            text "身長と体重を入力してください"
                      )
                    ]
                ]
            ]
        ]


main : Program Never
main =
    App.program
        { init = ( init, Layout.sub0 MDL )
        , view = view >> Scheme.topWithScheme Blue Green
        , subscriptions = .mdl >> Layout.subs MDL
        , update = update
        }
