module Main exposing (..)

import Maybe exposing (Maybe(Just, Nothing))
import Platform.Cmd exposing (Cmd)
import Html exposing (..)
import Html.App as App
import Html.Attributes
import Material
import Material.Color as Color
import Material.Color exposing (Hue(..))
import Material.Layout as Layout
import Material.Layout exposing (row, render)
import Material.Scheme
import Material.Scheme as Scheme
import Material.Textfield as Textfield
import Components.Ribbon as Ribbon
import Components.Ribbon exposing (..)
import Components.BmiCalc as BmiCalc
import Components.BmiCalc exposing (..)


-- MODEL


type alias Model =
    { bmiCalcComponentModel : BmiCalc.Model
    , mdl : Material.Model
    }


init : Model
init =
    { bmiCalcComponentModel = BmiCalc.init
    , mdl = Material.model
    }



-- ACTION, UPDATE


type Msg
    = BMI BmiCalc.Msg
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BMI msg' ->
            ( { model | bmiCalcComponentModel = fst <| BmiCalc.update msg' model.bmiCalcComponentModel }, Cmd.none )

        MDL msg' ->
            Material.update MDL msg' model



-- VIEW


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


view : Model -> Html Msg
view model =
    div []
        [ Ribbon.view "https://github.com/uehaj/elm-bmi"
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
            , main = [ App.map BMI <| BmiCalc.view model.bmiCalcComponentModel ]
            }
        ]
        |> Scheme.topWithScheme Blue Green


main : Program Never
main =
    App.program
        { init = ( init, Layout.sub0 MDL )
        , view = view
        , subscriptions = .mdl >> Layout.subs MDL
        , update = update
        }
