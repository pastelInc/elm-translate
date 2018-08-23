module Translate exposing (Msg, Translations, getCurrentLang, init, loadTranslations, tr, update)

import Dict exposing (Dict)
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Regex exposing (HowMany(All), Regex, escape, regex, replace)


type Translations
    = Translations
        { currentLang : String
        , doneFirstFetch : Bool
        , translation : Translation
        }


type Msg
    = Loaded (Result Http.Error Translation)


type Tree
    = Branch (Dict String Tree)
    | Leaf String


type Delims
    = Curly


type alias Translation =
    Dict String String


type alias InterpolateParams =
    List ( String, String )


update : Msg -> Translations -> Translations
update msg translations =
    case msg of
        Loaded res ->
            setTranslation res translations


setTranslation : Result Http.Error Translation -> Translations -> Translations
setTranslation res (Translations properties) =
    case res of
        Ok translation ->
            Translations
                { properties | translation = translation, doneFirstFetch = True }

        Err _ ->
            Translations
                { properties | doneFirstFetch = True }


loadTranslations : String -> Cmd Msg
loadTranslations url =
    fetchTranslations Loaded url


init : String -> Translations
init lang =
    Translations
        { currentLang = lang
        , doneFirstFetch = False
        , translation = Dict.empty
        }


tr : String -> List ( String, String ) -> Translations -> String
tr key interpolateParams ((Translations { doneFirstFetch, translation }) as translations) =
    if not doneFirstFetch then
        ""
    else if Dict.isEmpty translation then
        ""
            |> Debug.log "WARINIG: Please load translations at first. Click here to read more."
    else
        Dict.get key translation
            |> Maybe.map
                (replace All
                    (placeholderRegex Curly)
                    (replaceMatch interpolateParams)
                )
            |> Maybe.withDefault key


getCurrentLang : Translations -> String
getCurrentLang (Translations { currentLang }) =
    currentLang



-- INNER FUNCTION


fetchTranslations : (Result Http.Error Translation -> msg) -> String -> Cmd msg
fetchTranslations msg url =
    Http.send msg (request url)


request : String -> Request Translation
request url =
    Http.get url decode


decode : Decoder Translation
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


delimsToTuple : Delims -> ( String, String )
delimsToTuple delims =
    case delims of
        Curly ->
            ( "{{", "}}" )


placeholderRegex : Delims -> Regex
placeholderRegex delims =
    let
        ( startDelim, endDelim ) =
            delimsToTuple delims
    in
    regex (escape startDelim ++ "(.*?)" ++ escape endDelim)


replaceMatch : InterpolateParams -> Regex.Match -> String
replaceMatch interpolateParams { match, submatches } =
    case submatches of
        maybeName :: _ ->
            maybeName
                |> Maybe.andThen
                    (\name ->
                        Dict.fromList interpolateParams |> Dict.get name
                    )
                |> Maybe.withDefault match

        [] ->
            match
