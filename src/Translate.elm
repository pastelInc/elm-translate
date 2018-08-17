module Translate exposing (Translations, get, getBrowserLang, load, setDefaultLang, use)

import Dict exposing (Dict)
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Regex exposing (HowMany(All), Regex, escape, regex, replace)


type Translations
    = Translations
        { currentLang : Maybe Lang
        , translations : Dict String String
        }


type Lang
    = Ja
    | En


type Msg
    = Loaded (Result Http.Error Translations)


update : Msg -> Translations -> Translations
update msg tr =
    case msg of
        Loaded res ->
            setResult res tr


setResult : Result Http.Error Translations -> Translations
setResult res tr =
    case res of
        Ok tr_ ->
            merge tr_ tr


loadingTranslations : String -> ( Translations, Cmd Msg )
loadingTranslations lang =
    let
        url =
            [ "/assets/i18n/" -- prefix
            , lang
            , ".json" -- suffix
            ]
                |> String.join ""
    in
    ( defaultTranslations lang, fetchTranslations Loaded url )


getLnFromCode : String -> Maybe Lang
getLnFromCode code =
    case code of
        "en" ->
            Just En

        "ja" ->
            Just Ja

        _ ->
            Noting


defaultTranslations : String -> Translations
defaultTranslations lang =
    Translations
        { currentLang = getLnFromCode lang
        , translations = Dict.empty
        }


get : String -> List ( String, String ) -> Translations -> String
get key interpolateParams translation =
    "translated text"


setDefaultLang : String -> Translations -> Translations
setDefaultLang lang translation =
    case translation of
        Translations s ->
            Translations { s | defaultLang = Just Japanese }


getBrowserLang : String
getBrowserLang =
    "ja"


use : String -> Translations -> Translations
use lang translation =
    case translation of
        Translations s ->
            Translations { s | currentLang = Just Japanese }



-- INNER FUNCTION


type Tree
    = Branch (Dict String Tree)
    | Leaf String



-- Loader


fetchTranslations : (Result Http.Error Translations -> msg) -> String -> Cmd msg
fetchTranslations msg url =
    Http.send msg (request url)


request : String -> Request Translations
request url =
    Http.get url decode


decode : Decoder Translations
decode =
    Decode.map mapTreeToDict treeDecoder


mapTreeToDict : Tree -> Translations
mapTreeToDict tree =
    case tree of
        Branch dict ->
            foldTree Dict.empty dict ""
                |> Translations

        _ ->
            loaded


treeDecoder : Decoder Tree
treeDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.map Leaf
        , Decode.lazy
            (\_ -> Decode.dict treeDecoder |> Decode.map Branch)
        ]


foldTree : Dict String String -> Dict String Tree -> String -> Dict String String
foldTree initialValue dict namespace =
    Dict.foldl
        (\key val acc ->
            let
                newNamespace key =
                    if String.isEmpty namespace then
                        key
                    else
                        namespace ++ "." ++ key
            in
            case val of
                Leaf str ->
                    Dict.insert (newNamespace key) str acc

                Branch dict ->
                    foldTree acc dict (newNamespace key)
        )
        initialValue
        dict
