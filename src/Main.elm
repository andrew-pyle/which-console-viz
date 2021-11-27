module Main exposing (..)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html, button, div, form, h2, hr, img, input, li, p, pre, section, span, text, ul)
import Html.Attributes exposing (class, src, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { userTitles : List TitleData
    , userPlatforms : List PlatformData
    , inFlightHttpRequests : Int
    , searchResults : List Entity

    -- Id Lookups
    , platformLookup : Dict Int PlatformData
    , coverLookup : Dict Int CoverData
    , platformLogoLookup : PlatformLogoLookup

    -- Status
    , appStatus : String

    -- Search
    , titleSearchText : String
    , titleSearchResults : List TitleData
    , platformSearchText : String
    , platformSearchResults : List PlatformData
    }


type IgdbEntity
    = IgdbGame
    | IgdbPlatform


type Entity
    = Title TitleData
    | Platform PlatformData



-- Title


type alias TitleData =
    { name : String
    , igdbId : Int
    , coverId : Maybe Int
    , platforms : List Int
    }



-- Platform


type alias PlatformData =
    { igdbId : Int
    , name : String
    , logoId : Maybe Int
    }


type alias PlatformLookup =
    Dict Int PlatformData



-- Cover


type alias CoverData =
    { igdbId : Int
    , url : String
    }


type alias CoverLookup =
    Dict Int CoverData



-- Platform Logo


type alias PlatformLogoData =
    { igdbId : Int
    , url : String
    }


type alias PlatformLogoLookup =
    Dict Int PlatformLogoData


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { userTitles = []
      , userPlatforms = []
      , inFlightHttpRequests = 0
      , searchResults = []
      , platformLookup = Dict.empty
      , coverLookup = Dict.empty
      , platformLogoLookup = Dict.empty
      , appStatus = ""
      , titleSearchText = ""
      , platformSearchResults = []
      , titleSearchResults = []
      , platformSearchText = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = -- User Analysis
      AddUserTitle TitleData
    | AddUserPlatform PlatformData
      -- UI
    | UpdateTitleSearchText String
    | UpdatePlatformSearchText String
      -- IGDB API Search
    | ReqIgdbTitleSearch String
    | ResIgdbTitleSearch (Result Http.Error (List TitleData))
    | ReqIgdbPlatformSearch String
    | ResIgdbPlatformSearch (Result Http.Error (List PlatformData))
      -- IGDB API Query
    | ResIgdbPlatform (Result Http.Error (List PlatformData))
    | ResIgdbCover (Result Http.Error (List CoverData))
    | ResIgdbPlatformLogo (Result Http.Error (List PlatformLogoData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddUserTitle newTitle ->
            ( { model | userTitles = newTitle :: model.userTitles }
            , Cmd.none
            )

        -- UI
        UpdateTitleSearchText text ->
            ( { model | titleSearchText = text }, Cmd.none )

        UpdatePlatformSearchText text ->
            ( { model | platformSearchText = text }, Cmd.none )

        AddUserPlatform newPlatform ->
            ( { model | userPlatforms = newPlatform :: model.userPlatforms }, Cmd.none )

        ReqIgdbTitleSearch titleText ->
            ( { model
                | inFlightHttpRequests = model.inFlightHttpRequests + 1
              }
            , fetchTitleSearch titleText
            )

        -- Platform
        ReqIgdbPlatformSearch platformText ->
            ( { model
                | inFlightHttpRequests = model.inFlightHttpRequests + 1
              }
            , fetchPlatformSearch platformText
            )

        ResIgdbPlatformSearch response ->
            case response of
                Ok platformList ->
                    ( { model
                        | platformSearchResults = platformList
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , fetchPlatformLogoData model.platformLogoLookup (List.filterMap (\t -> t.logoId) platformList)
                    )

                Err _ ->
                    ( { model
                        | appStatus = "😱 Http Error."
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.none
                    )

        ResIgdbPlatform response ->
            case response of
                Ok platformList ->
                    let
                        newPlatforms =
                            Dict.fromList <| List.map (\p -> ( p.igdbId, p )) platformList
                    in
                    ( { model
                        | platformLookup = Dict.union newPlatforms model.platformLookup
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | appStatus = "😱 Http Error."
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.none
                    )

        ResIgdbCover response ->
            case response of
                Ok coverList ->
                    let
                        newCovers =
                            Dict.fromList <| List.map (\p -> ( p.igdbId, p )) coverList
                    in
                    ( { model
                        | coverLookup = Dict.union newCovers model.coverLookup
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | appStatus = "😱 Http Error."
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.none
                    )

        ResIgdbTitleSearch response ->
            case response of
                Ok titleList ->
                    ( { model
                        | titleSearchResults = titleList
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.batch
                        [ fetchPlatformData (List.foldl (\t acc -> List.append acc t.platforms) [] titleList)
                        , fetchCoverData
                            model.coverLookup
                            (List.filterMap (\t -> t.coverId) titleList)
                        ]
                    )

                Err _ ->
                    ( { model
                        | appStatus = "😱 Http Error."
                        , inFlightHttpRequests = model.inFlightHttpRequests - 1
                      }
                    , Cmd.none
                    )

        -- Platform Logo
        ResIgdbPlatformLogo res ->
            case res of
                Ok apiLogos ->
                    let
                        apiLogoDict =
                            Dict.fromList <| List.map (\p -> ( p.igdbId, p )) apiLogos
                    in
                    ( { model | platformLogoLookup = Dict.union apiLogoDict model.platformLogoLookup }, Cmd.none )

                Err err ->
                    ( { model | appStatus = "😱 Http Error.", inFlightHttpRequests = model.inFlightHttpRequests - 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


htmlDocumentTitle : String
htmlDocumentTitle =
    "Game Viz"


view : Model -> Document Msg
view model =
    { title = htmlDocumentTitle
    , body =
        [ section [ class "dashboard" ]
            [ h2 [] [ text "Dashboard" ]
            , p [ class "status" ] [ text <| model.appStatus ]
            , p [] [ text <| "In-Flight HTTP: " ++ String.fromInt model.inFlightHttpRequests ]
            ]
        , hr [] []
        , section []
            [ h2 [] [ text "Games to Analyze" ]
            , ul [ class "titles" ]
                (List.map (\title -> elementTitle model.platformLookup model.coverLookup title) model.userTitles)
            ]
        , section []
            [ h2 [] [ text "Platforms to Analyze" ]
            , ul [ class "platforms" ]
                (List.map (elementPlatformSearchResult model.platformLogoLookup) model.userPlatforms)
            ]
        , hr [] []
        , section [ class "games" ]
            [ h2 [] [ text "Add some Games" ]
            , form
                [ onSubmit (ReqIgdbTitleSearch model.titleSearchText) ]
                [ input [ type_ "text", onInput UpdateTitleSearchText ] []
                , button [ type_ "submit" ] [ text "Search IGDB for this Game" ]
                ]
            , ul [ class "title-search-results" ]
                (List.map
                    (\title ->
                        li []
                            [ button [ onClick (AddUserTitle title) ] [ text "Add Title to User List" ]
                            , elementTitle model.platformLookup model.coverLookup title
                            ]
                    )
                    model.titleSearchResults
                )
            ]
        , section [ class "platforms" ]
            [ form [ onSubmit (ReqIgdbPlatformSearch model.platformSearchText) ]
                [ input [ type_ "text", onInput UpdatePlatformSearchText ] []
                , button
                    [ type_ "submit" ]
                    [ text "Search IGDB for this Platform" ]
                ]
            , ul [ class "platform-search-results" ]
                (List.map (elementPlatformSearchResult model.platformLogoLookup) model.platformSearchResults)
            ]
        ]
    }


elementTitle : PlatformLookup -> CoverLookup -> TitleData -> Html Msg
elementTitle platformLookup coverLookup title =
    let
        platforms =
            List.map (\id -> getPlatformById platformLookup id) title.platforms

        coverArtUrl : String
        coverArtUrl =
            case title.coverId of
                Just id ->
                    case Dict.get id coverLookup of
                        Just cover ->
                            cover.url

                        Nothing ->
                            unknownCoverUrl

                Nothing ->
                    unknownCoverUrl
    in
    div [ class "title" ]
        [ span [] [ img [ src coverArtUrl ] [] ]
        , span [] [ text title.name ]
        , ul [] (List.map elementPlatform platforms)
        ]


elementPlatform : PlatformData -> Html Msg
elementPlatform platform =
    li []
        [ --  img [ src platform.imageLink ] []
          span [] [ text platform.name ]
        ]


elementPlatformSearchResult : PlatformLogoLookup -> PlatformData -> Html Msg
elementPlatformSearchResult lookup platform =
    let
        logoUrl : String
        logoUrl =
            case platform.logoId of
                Just id ->
                    case Dict.get id lookup of
                        Just logo ->
                            logo.url

                        Nothing ->
                            unknownCoverUrl

                Nothing ->
                    unknownCoverUrl
    in
    li []
        [ img [ src logoUrl ] []
        , span [] [ text platform.name ]
        , button [ onClick (AddUserPlatform platform) ] [ text "Add Platform to User List" ]
        ]



-- HTTP
-- https://thoughtbot.com/blog/decoding-json-structures-with-elm


igdbApiUrl : String
igdbApiUrl =
    "http://localhost:8080/v4"



-- Title


fetchTitleSearch : String -> Cmd Msg
fetchTitleSearch searchText =
    Http.post
        { url = igdbApiUrl ++ "/games" --"https://api.igdb.com/v4/platforms"
        , body = Http.stringBody "text/plain" ("search \"" ++ searchText ++ "\"; fields id,name,cover,summary,platforms; limit 500;")
        , expect = Http.expectJson ResIgdbTitleSearch titleListDecoder
        }


titleListDecoder : Decoder (List TitleData)
titleListDecoder =
    Decode.list titleDataDecoder


titleDataDecoder : Decoder TitleData
titleDataDecoder =
    Decode.map4 TitleData
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.maybe (Decode.field "cover" Decode.int))
        (Decode.maybe (Decode.field "platforms" (Decode.list Decode.int))
            |> Decode.andThen decodePlatformList
        )


decodePlatformList : Maybe (List Int) -> Decoder (List Int)
decodePlatformList lst =
    Decode.succeed (Maybe.withDefault [] lst)



-- Platform


fetchPlatformSearch : String -> Cmd Msg
fetchPlatformSearch searchText =
    Http.post
        { url = igdbApiUrl ++ "/platforms" --"https://api.igdb.com/v4/platforms"
        , body = Http.stringBody "text/plain" ("search \"" ++ searchText ++ "\"; fields id,name,platform_logo; limit 500;")
        , expect = Http.expectJson ResIgdbPlatformSearch platformListDecoder
        }


platformListDecoder : Decoder (List PlatformData)
platformListDecoder =
    Decode.list platformDataDecoder


platformDataDecoder : Decoder PlatformData
platformDataDecoder =
    Decode.map3 PlatformData
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "platform_logo" Decode.int))


fetchPlatformData : List Int -> Cmd Msg
fetchPlatformData ids =
    let
        idOrList =
            "(" ++ String.join "," (List.map String.fromInt ids) ++ ")"
    in
    -- Cmd.batch allPlatformRequests
    Http.post
        { url = igdbApiUrl ++ "/platforms" --"https://api.igdb.com/v4/platforms"
        , body = Http.stringBody "text/plain" ("where id =" ++ idOrList ++ "; fields id,name; limit 500;")
        , expect = Http.expectJson ResIgdbPlatform platformListDecoder
        }


unknownPlatform : PlatformData
unknownPlatform =
    PlatformData 0 "Unknown" Nothing


unknownCoverUrl : String
unknownCoverUrl =
    "https://httpbin.org/image/webp"



-- Cover


fetchCoverData : CoverLookup -> List Int -> Cmd Msg
fetchCoverData lookup ids =
    let
        newIds : List String
        newIds =
            List.filterMap
                (\id ->
                    case Dict.get id lookup of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just (String.fromInt id)
                )
                ids

        newIdsStringOrFormat =
            String.join "," newIds
    in
    Http.post
        { url = igdbApiUrl ++ "/covers" --"https://api.igdb.com/v4/covers("
        , body = Http.stringBody "text/plain" ("where id = (" ++ newIdsStringOrFormat ++ "); fields id,url; limit 500;")
        , expect = Http.expectJson ResIgdbCover coverListDecoder
        }


coverListDecoder : Decoder (List CoverData)
coverListDecoder =
    Decode.list coverDataDecoder


coverDataDecoder : Decoder CoverData
coverDataDecoder =
    Decode.map2 CoverData
        (Decode.field "id" Decode.int)
        (Decode.field "url" Decode.string)



-- Platform Logo


fetchPlatformLogoData : PlatformLogoLookup -> List Int -> Cmd Msg
fetchPlatformLogoData lookup ids =
    let
        newIds : List String
        newIds =
            List.filterMap
                (\id ->
                    case Dict.get id lookup of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just (String.fromInt id)
                )
                ids

        newIdsStringOrFormat =
            String.join "," newIds
    in
    Http.post
        { url = igdbApiUrl ++ "/platform_logos" --"https://api.igdb.com/v4/platform_logos"
        , body = Http.stringBody "text/plain" ("where id = (" ++ newIdsStringOrFormat ++ "); fields id,url; limit 500;")
        , expect = Http.expectJson ResIgdbPlatformLogo platformLogoListDecoder
        }


platformLogoListDecoder : Decoder (List PlatformLogoData)
platformLogoListDecoder =
    Decode.list coverDataDecoder


platformLogoDataDecoder : Decoder PlatformLogoData
platformLogoDataDecoder =
    Decode.map2 CoverData
        (Decode.field "id" Decode.int)
        (Decode.field "url" Decode.string)



-- Business Logic


getPlatformById : PlatformLookup -> Int -> PlatformData
getPlatformById lookup id =
    case Dict.get id lookup of
        Just data ->
            data

        Nothing ->
            unknownPlatform


staticGameData : TitleData
staticGameData =
    { name = "Dragon Quest VIII: Journey of the Cursed King"
    , igdbId = 1819

    -- , coverArtUrl = "https://images.igdb.com/igdb/image/upload/t_thumb/co1x9z.jpg"
    , coverId = Just 2
    , platforms =
        [ 8
        , 34
        , 37
        , 39
        ]
    }



-- https://api.igdb.com/v4/games
-- search "Dragon Quest 8";
-- fields id,name,cover,slug,url,summary,platforms;
-- [
--     {
--         "id": 145528,
--         "cover": 136512,
--         "name": "Dragon Quest VIII: Journey of the Cursed King",
--         "platforms": [
--             37
--         ],
--         "slug": "dragon-quest-viii-journey-of-the-cursed-king--1",
--         "summary": "This is a portable version of the classic RPG, with added content and other 3DS enhancements.",
--         "url": "https://www.igdb.com/games/dragon-quest-viii-journey-of-the-cursed-king--1"
--     },
--     {
--         "id": 1819,
--         "cover": 89783,
--         "name": "Dragon Quest VIII: Journey of the Cursed King",
--         "platforms": [
--             8,
--             34,
--             39
--         ],
--         "slug": "dragon-quest-viii-journey-of-the-cursed-king",
--         "summary": "Dragon Quest VIII: Journey of the Cursed King is the eighth installment in the Dragon Quest series, developed by Level-5 and published by Square Enix for the PlayStation 2 video game console. It was also later released for the Nintendo 3DS in 2015.\n\nIt is a traditional Japanese-style role-playing game with random enemy encounters, simple turn-based combat mechanics, and management of a party consisting of four characters, each belonging to a clearly defined class. Unlike its predecessors in the series and most Japanese RPGs in general, the game features a continuous world with fairly vast landscapes and integrated towns and dungeons, as opposed to world map traveling and locations represented by icons. The game features full camera rotation and optional first-person view.\n\nLike in the previous Dragon Quest games, many objects can be interacted with; for example, barrels can be physically lifted, carried, and broken, to reveal items hidden within.",
--         "url": "https://www.igdb.com/games/dragon-quest-viii-journey-of-the-cursed-king"
--     }
-- ]
