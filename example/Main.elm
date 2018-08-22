module Main exposing (..)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Translate exposing (Translations, getCurrentLang, init, loadTranslations, translate)


type Model
    = Loading Translations
    | Loaded Translations


type Msg
    = UpdateTranslations Translate.Msg
    | ChangeTranslations String


type alias Flags =
    { language : Value }


init : Flags -> ( Model, Cmd Msg )
init { language } =
    let
        ln =
            Decode.decodeValue Decode.string language
                |> Result.withDefault ""

        url =
            [ "/assets/i18n/" -- prefix
            , "en"
            , ".json" -- suffix
            ]
                |> String.join ""
    in
    Loading (Translate.init ln) ! [ Cmd.map UpdateTranslations (Translate.loadTranslations url) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTranslations subMsg ->
            updateTranslations subMsg model

        ChangeTranslations ln ->
            let
                url =
                    [ "/assets/i18n/" -- prefix
                    , "en"
                    , ".json" -- suffix
                    ]
                        |> String.join ""
            in
            model ! [ Cmd.map UpdateTranslations (Translate.loadTranslations url) ]


updateTranslations : Translate.Msg -> Model -> ( Model, Cmd Msg )
updateTranslations msg model =
    case model of
        Loading t ->
            Loaded (Translate.update msg t) ! [ Cmd.none ]

        Loaded t ->
            Loaded (Translate.update msg t) ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    case model of
        Loading translations ->
            let
                ln =
                    translations
                        |> Translate.getCurrentLang
            in
            translate "Home.nowloading" [] translations
                |> text

        Loaded translations ->
            let
                ln =
                    translations
                        |> Translate.getCurrentLang
            in
            translate "Home.helloworld" [ ( "name", "Chihaya" ) ] translations
                |> text


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
