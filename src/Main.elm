module Main exposing (..)

import Maybe exposing (Maybe(Just, Nothing))
import Platform.Cmd exposing (Cmd)
import Html exposing (..)
import Html.App as App
import Html.Attributes
import Material
import Material.Color exposing (Hue(Green))
import Material.Layout as Layout
import Material.Layout exposing (row, render)
import Material.Scheme as Scheme
import Material.Textfield as Textfield
import Material.Options as Options exposing (cs, css, Style)
import Components.Ribbon as Ribbon
import Components.Ribbon exposing (..)
import Components.BmiCalc as BmiCalc
import Components.BmiCalc exposing (..)
import Cmd.Extra


-- MODEL


type alias Model =
    { bmiCalcComponentModel : BmiCalc.Model
    , level : String
    , mdl : Material.Model
    }


init : Model
init =
    { bmiCalcComponentModel = BmiCalc.init
    , level = ""
    , mdl = Material.model
    }



-- ACTION, UPDATE


ovesityLevel bmiValue =
    case bmiValue of
        Just x ->
            if 25 <= x && x < 30 then
                "肥満"
            else if 30 <= x && x < 35 then
                "肥満2度"
            else if 35 <= x && x < 40 then
                "肥満3度"
            else if 40 <= x then
                "肥満4度"
            else if 18.5 <= x && x < 25 then
                "普通体重"
            else
                "低体重"

        Nothing ->
            ""


type Msg
    = BMI BmiCalc.Msg
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BMI (BmiCalc.InputDataChanged bmiValue) ->
            ( { model | level = ovesityLevel bmiValue }, Cmd.none )

        BMI msg' ->
            let
                ( newModel, newCmd ) =
                    BmiCalc.update msg' model.bmiCalcComponentModel
            in
                ( { model | bmiCalcComponentModel = newModel }, Cmd.map BMI newCmd )

        MDL msg' ->
            Material.update MDL msg' model



-- VIEW


header : Html Msg
header =
    Layout.row []
        [ Layout.title [] [ text "BMIを計算(Elm)" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link [ Layout.href "https://elm-lang.org/" ]
                [ text "elm-lang.org" ]
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
            { header = [ header ]
            , drawer = [ text "" ]
            , tabs = ( [], [] )
            , main =
                [ App.map BMI <| BmiCalc.view model.bmiCalcComponentModel
                , Options.div [ css "padding-left" "5em" ] [ text model.level ]
                ]
            }
        ]
        |> Scheme.topWithScheme Material.Color.Blue Material.Color.Green


main : Program Never
main =
    App.program
        { init = ( init, Layout.sub0 MDL )
        , view = view
        , subscriptions = .mdl >> Layout.subs MDL
        , update = update
        }
