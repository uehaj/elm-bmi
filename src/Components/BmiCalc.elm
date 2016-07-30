module Components.BmiCalc exposing (Msg(InputDataChanged), Model, init, update, view)

import Material.Options as Options exposing (cs, css, Style)
import Html exposing (..)
import Html.App as App
import Html.Attributes
import Material
import Material.Grid as Grid
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Color exposing (Hue(..))
import Material.Scheme as Scheme
import Components.NumberField as NumberField
import Components.NumberField exposing (..)
import Msgs exposing (..)


-- MODEL


type alias Model =
    { heightComponentModel : NumberField.Model
    , weightComponentModel : NumberField.Model
    , bmiValue : Maybe Float
    , mdl :
        Material.Model
        -- Boilerplate: mdl is the Model store for any and all MDL components you need.
    }


init : Model
init =
    let
        ( m1, _ ) =
            NumberField.init

        ( m2, _ ) =
            NumberField.init
    in
        { heightComponentModel = m1
        , weightComponentModel = m2
        , bmiValue = Nothing
        , mdl =
            Material.model
            -- Boilerplate: Always use this initial MDL model store.
        }



-- ACTION, UPDATE


calcBMI h w =
    w / (h / 100) ^ 2


round_1 x =
    (toFloat <| round (x * 10)) / 10


type Msg
    = HeightChanged NumberField.Msg
    | WeightChanged NumberField.Msg
    | InputDataChanged
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeightChanged subMsg ->
            update InputDataChanged
                { model | heightComponentModel = fst <| NumberField.update subMsg model.heightComponentModel }

        WeightChanged subMsg ->
            update InputDataChanged
                { model | weightComponentModel = fst <| NumberField.update subMsg model.weightComponentModel }

        InputDataChanged ->
            ( { model
                | bmiValue =
                    Maybe.map round_1
                        <| Maybe.map2 calcBMI model.heightComponentModel.value model.weightComponentModel.value
              }
            , Cmd.none
            )

        {- Boilerplate: MDL action handler. It should always look like this. -}
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
                [ h3 [ Html.Attributes.style [ ( "padding-left", "1em" ) ] ]
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
