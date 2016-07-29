module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.App as App
import Material
import Material.Options as Options exposing (cs, css, Style)
import Material.Color as Color
import Material.Color exposing (Hue(..))
import Material.Layout as Layout
import Material.Layout as Layout
import Material.Layout exposing (..)
import Material.Scheme
import Material.Scheme as Scheme
import Material.Textfield as Textfield
import Platform.Cmd exposing (Cmd)
import Material
import Material.Scheme
import Material.Grid as Grid
import Material.Grid exposing (grid, cell, size, Device(..))
import Components.NumberField exposing (..)
import Components.Ribbon exposing (..)


-- MODEL


type alias Model =
    { heightComponent : Components.NumberField.Model
    , weightComponent : Components.NumberField.Model
    , bmiValue : Float
    , mdl :
        Material.Model
        -- Boilerplate: mdl is the Model store for any and all MDL components you need.
    }


model : Model
model =
    let
        ( m1, cmd1 ) =
            Components.NumberField.init

        ( m2, cmd2 ) =
            Components.NumberField.init
    in
        { heightComponent = m1
        , weightComponent = m2
        , bmiValue = 0
        , mdl =
            Material.model
            -- Boilerplate: Always use this initial MDL model store.
        }



-- ACTION, UPDATE


type Msg
    = HeightChanged Components.NumberField.Msg
    | WeightChanged Components.NumberField.Msg
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
                ( m1, cmd1 ) =
                    Components.NumberField.update subMsg model.heightComponent
            in
                update ReCalc
                    { model | heightComponent = m1 }

        WeightChanged subMsg ->
            let
                ( m2, cmd2 ) =
                    Components.NumberField.update subMsg model.weightComponent
            in
                update ReCalc
                    { model | weightComponent = m2 }

        ReCalc ->
            ( { model | bmiValue = round_1 <| bmi model.heightComponent.value model.weightComponent.value }, Cmd.none )

        {- Boilerplate: MDL action handler. It should always look like this. -}
        MDL msg' ->
            Material.update MDL msg' model


header =
    [ Layout.row []
        [ Layout.title [] [ text "BMIを計算(Elm)" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link [ Layout.href "https://www.github.io/uehaj/elm-bmi/" ]
                [ text "Source" ]
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
                [ App.map HeightChanged <| inputNumber 0 model.heightComponent "身長を入力してください" ]
            , cell
                [ Grid.size All 6
                , Grid.size Phone 12
                ]
                [ App.map WeightChanged <| inputNumber 1 model.weightComponent "体重を入力してください"
                ]
            , cell [ Grid.size All 12 ]
                [ h3 [ Html.Attributes.style [ ( "padding-left", "1em" ) ] ] [ text "身長と体重を入力してください" ] ]
            ]
        ]
    ]


view : Model -> Html Msg
view model =
    div []
        [ Components.Ribbon.view
        , Layout.render MDL
            model.mdl
            [ Layout.fixedHeader
            , Layout.fixedTabs
            , Layout.waterfall False
            ]
            { header = header
            , drawer = []
            , tabs = ( [], [] )
            , main = body model
            }
        ]
        |> Scheme.topWithScheme Red Green


main : Program Never
main =
    App.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
