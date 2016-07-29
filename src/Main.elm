module Main exposing (..)

import Maybe exposing (Maybe(Just, Nothing))
import Platform.Cmd exposing (Cmd)
import Html exposing (..)
import Html.App as App
import Html.Attributes
import Material
import Material.Color as Color
import Material.Color exposing (Hue(..))
import Material.Grid as Grid
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Layout as Layout
import Material.Layout exposing (..)
import Material.Options as Options exposing (cs, css, Style)
import Material.Scheme
import Material.Scheme as Scheme
import Material.Textfield as Textfield
import Components.NumberField as NumberField
import Components.NumberField exposing (..)
import Components.Ribbon as Ribbon
import Components.Ribbon exposing (..)


-- MODEL


type alias Model =
    { heightComponent : NumberField.Model
    , weightComponent : NumberField.Model
    , bmiValue : Maybe Float
    , mdl :
        Material.Model
        -- Boilerplate: mdl is the Model store for any and all MDL components you need.
    }


model0 : Model
model0 =
    let
        ( m1, _ ) =
            NumberField.init

        ( m2, _ ) =
            NumberField.init
    in
        { heightComponent = m1
        , weightComponent = m2
        , bmiValue = Nothing
        , mdl =
            Material.model
            -- Boilerplate: Always use this initial MDL model store.
        }



-- ACTION, UPDATE


type Msg
    = HeightChanged NumberField.Msg
    | WeightChanged NumberField.Msg
    | ReCalc
      {--Boilerplate: Msg for MDL actions (ripple animations etc.). --}
    | MDL Material.Msg


bmi h w =
    w / (h / 100) ^ 2


round_1 x =
    (toFloat <| round (x * 10)) / 10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeightChanged subMsg ->
            let
                ( m1, _ ) =
                    NumberField.update subMsg model.heightComponent
            in
                update ReCalc
                    { model | heightComponent = m1 }

        WeightChanged subMsg ->
            let
                ( m2, _ ) =
                    NumberField.update subMsg model.weightComponent
            in
                update ReCalc
                    { model | weightComponent = m2 }

        ReCalc ->
            ( { model
                | bmiValue =
                    Maybe.map round_1
                        <| Maybe.map2 bmi model.heightComponent.value model.weightComponent.value
              }
            , Cmd.none
            )

        {- Boilerplate: MDL action handler. It should always look like this. -}
        MDL msg' ->
            Material.update MDL msg' model



-- VIEW


type alias Mdl =
    Material.Model


header =
    [ Layout.row []
        [ Layout.title [] [ text "BMIを計算(Elm)" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link [ Layout.href "https://elm-lang.org/" ]
                [ text "Elm" ]
            ]
        ]
    ]


body model =
    [ Options.div []
        [ grid []
            [ cell
                [ Grid.size All 6
                , Grid.size Phone 12
                ]
                [ App.map HeightChanged <| NumberField.render 0 model.heightComponent "身長を入力してください" ]
            , cell
                [ Grid.size All 6
                , Grid.size Phone 12
                ]
                [ App.map WeightChanged <| NumberField.render 1 model.weightComponent "体重を入力してください"
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
    ]


view : Model -> Html Msg
view model =
    div []
        [ Ribbon.view
        , Html.header [ Html.Attributes.title "BMIの計算(Elm)" ] []
        , Layout.render MDL
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedTabs
            , Layout.waterfall True
            ]
            { header = header
            , drawer = [ text "hge" ]
            , tabs = ( [], [] )
            , main = body model
            }
        ]
        |> Scheme.topWithScheme Blue Green


main : Program Never
main =
    App.program
        { init = ( model0, Layout.sub0 MDL )
        , view = view
        , subscriptions = .mdl >> Layout.subs MDL
        , update = update
        }
