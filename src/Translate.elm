module Translate exposing (Msg, Translations, getCurrentLang, init, loadTranslations, translate, update)

import Dict exposing (Dict)
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Regex exposing (HowMany(All), Regex, escape, regex, replace)


type Translations
    = Translations
        { currentLang : String
        , doneFirstFetch : Bool
        , translations : Dict String String
        }


type Msg
    = Loaded (Result Http.Error (Dict String String))


type Tree
    = Branch (Dict String Tree)
    | Leaf String


update : Msg -> Translations -> Translations
update msg translations =
    case msg of
        Loaded res ->
            setTranslations res translations


setTranslations : Result Http.Error (Dict String String) -> Translations -> Translations
setTranslations res (Translations model) =
    case res of
        Ok translations ->
            Translations
                { model | translations = translations, doneFirstFetch = True }

        Err _ ->
            Translations
                { model | doneFirstFetch = True }


loadTranslations : String -> Cmd Msg
loadTranslations url =
    fetchTranslations Loaded url


init : String -> Translations
init lang =
    Translations
        { currentLang = lang
        , doneFirstFetch = False
        , translations = Dict.empty
        }


translate : String -> List ( String, String ) -> Translations -> String
translate key interpolateParams ((Translations model) as translations) =
    if not model.doneFirstFetch then
        ""
    else if Dict.isEmpty model.translations then
        ""
            |> Debug.log "WARINIG: Please load translations at first. Click here to read more."
    else
        Dict.get key model.translations
            |> Maybe.withDefault key


getCurrentLang : Translations -> String
getCurrentLang (Translations { currentLang }) =
    currentLang



-- INNER FUNCTION


fetchTranslations : (Result Http.Error (Dict String String) -> msg) -> String -> Cmd msg
fetchTranslations msg url =
    Http.send msg (request url)


request : String -> Request (Dict String String)
request url =
    Http.get url decode


decode : Decoder (Dict String String)
decode =
    Decode.map mapTreeToDict treeDecoder


mapTreeToDict : Tree -> Dict String String
mapTreeToDict tree =
    case tree of
        Branch dict ->
            foldTree Dict.empty dict ""

        _ ->
            Dict.empty


treeDecoder : Decoder Tree
treeDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.map Leaf
        , Decode.lazy
            (\_ -> Decode.dict treeDecoder |> Decode.map Branch)
        ]


foldTree : Dict String String -> Dict String Tree -> String -> Dict String String
foldTree accumulator dict namespace =
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
        accumulator
        dict
