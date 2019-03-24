module Main exposing (Model, Msg(..), What(..), Who(..), init, listOfWhats, listOfWhos, main, update, view)

--import Element.Attributes as Attrs

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Region as Element
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Random
import Random.List as Random
import Task



---- MODEL ----


type Who
    = Migrants
    | Refugees
    | Muslims
    | Americans
    | NatoSoldiers
    | Sunshines
    | Havloids
    | EuBureaucrats
    | PragueCafe
    | Welcomers
    | Ngos
    | Activists


type What
    = Rape
    | SupportMigration
    | ToKillUsAll
    | BanPork
    | ConvertUsToIslam
    | DestroyTraditionalCulture
    | Sharia
    | WarAgainstRussia
    | TakesMoneyFromSoros
    | SupportEuropeDecay


type Suffix
    = NoSuffix
    | DontLike
    | NoMoreSuffering
    | SomeoneDoSomething
    | NotInCzechTv
    | ExtremistsWhistleblowers
    | LeaveEuNato
    | ShareUntilRemoved
    | Exclamations


listOfWhats : List What
listOfWhats =
    [ Rape
    , SupportMigration
    , ToKillUsAll
    , BanPork
    , ConvertUsToIslam
    , DestroyTraditionalCulture
    , Sharia
    , WarAgainstRussia
    , TakesMoneyFromSoros
    , SupportEuropeDecay
    ]


listOfWhos : List Who
listOfWhos =
    [ Migrants
    , Refugees
    , Muslims
    , Americans
    , NatoSoldiers
    , Sunshines
    , Havloids
    , EuBureaucrats
    , PragueCafe
    , Welcomers
    , Ngos
    , Activists
    ]


listOfSuffixes : List Suffix
listOfSuffixes =
    [ NoSuffix
    , DontLike
    , NoMoreSuffering
    , SomeoneDoSomething
    , NotInCzechTv
    , ExtremistsWhistleblowers
    , LeaveEuNato
    , ShareUntilRemoved
    , Exclamations
    ]


whoToString : Who -> String
whoToString who =
    case who of
        Migrants ->
            "Migranti"

        Refugees ->
            "Uprchlíci"

        Muslims ->
            "Muslimové"

        Americans ->
            "Američani"

        NatoSoldiers ->
            "Vojáci NATO"

        Sunshines ->
            "Sluníčkáři"

        Havloids ->
            "Havloidi"

        EuBureaucrats ->
            "Úředníci EU"

        PragueCafe ->
            "Povaleči z pražských kaváren"

        Welcomers ->
            "Vítači"

        Ngos ->
            "Flákači z neziskovek"

        Activists ->
            "Aktivisté"


whatToString : What -> String
whatToString what =
    case what of
        Rape ->
            "chtějí znásilnit naše ženy a děti"

        SupportMigration ->
            "podporují nekontrolovatelnou migraci"

        ToKillUsAll ->
            "nás chtějí všechny podřezat"

        BanPork ->
            "nám chtějí zakázat vepřové"

        ConvertUsToIslam ->
            "nám chtějí vnutit Islám"

        DestroyTraditionalCulture ->
            "chtějí rozvrátit naše tradiční hodnoty"

        Sharia ->
            "chtějí u nás zavést právo šaría"

        WarAgainstRussia ->
            "chtějí rozpoutat válku proti Rusku"

        TakesMoneyFromSoros ->
            "berou peníze od Sorose"

        SupportEuropeDecay ->
            "podporují rozpad Evropy"


suffixToString : Suffix -> String
suffixToString suffix =
    case suffix of
        NoSuffix ->
            ""

        DontLike ->
            "To si necháme líbit?"

        NoMoreSuffering ->
            "Do kdy to budeme ještě trpět?"

        SomeoneDoSomething ->
            "Kdy s tím konečně někdo něco udělá?"

        NotInCzechTv ->
            "Tohle nám v ČT neukážou!"

        ExtremistsWhistleblowers ->
            "Kdo na to upozorní je hned extrémista!"

        LeaveEuNato ->
            "Vystoupit z EU a NATO!"

        ShareUntilRemoved ->
            "Sdílejte, než to smažou!"

        Exclamations ->
            "!!!!!!!!!!!!!!!!!!"


type Sentence
    = NoSentence
    | Sentence Who What Suffix


type alias Model =
    { sentence : Sentence }


init : ( Model, Cmd Msg )
init =
    ( { sentence = NoSentence }, (Task.perform identity << Task.succeed) GenerateRandomCombination )



---- UPDATE ----


type Msg
    = NoOp
    | GenerateRandomCombination
    | SetSentence Sentence


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomCombination ->
            let
                whoGenerator =
                    Random.choose listOfWhos |> Random.map (Maybe.withDefault Migrants << Tuple.first)

                whatGenerator =
                    Random.choose listOfWhats |> Random.map (Maybe.withDefault BanPork << Tuple.first)

                suffixGenerator =
                    Random.choose listOfSuffixes |> Random.map (Maybe.withDefault NoSuffix << Tuple.first)

                sentenceGenerator =
                    Random.map3 Sentence whoGenerator whatGenerator suffixGenerator
            in
            ( model, Random.generate SetSentence sentenceGenerator )

        SetSentence sentence ->
            ( { model | sentence = sentence }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


sentenceToString : Sentence -> String
sentenceToString sentence =
    let
        ending suffix =
            case suffix of
                NoSuffix ->
                    "."

                Exclamations ->
                    suffixToString Exclamations

                _ ->
                    ". " ++ suffixToString suffix
    in
    case sentence of
        NoSentence ->
            "Generuji...."

        Sentence who what suffix ->
            whoToString who ++ " " ++ whatToString what ++ ending suffix


primaryColor =
    Element.rgb255 180 1 0


sentenceFontStyle =
    [ Font.color primaryColor
    , Font.size 34
    , Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]
    , Element.padding 20
    ]


titleStyle =
    [ Font.color <| Element.rgb 0 0 0
    , Font.size 20
    , Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]
    ]


buttonStyle =
    [ Background.color primaryColor
    , Element.padding 20
    , Font.color <| Element.rgb 1 1 1
    , Font.size 16
    , Font.heavy
    , Font.family
        [ Font.typeface "Roboto"
        , Font.sansSerif
        ]
    ]


footerStyle =
    [ Font.size 16
    , Font.family
        [ Font.typeface "Roboto"
        , Font.sansSerif
        ]
    , Element.padding 20
    ]


linkStyle =
    [ Font.color primaryColor, Font.underline ]


view : Model -> Browser.Document Msg
view model =
    { title = "Hoax Generátor"
    , body =
        [ Element.layout [] <|
            Element.column [ Element.spacing 10, Element.width Element.fill, Element.height Element.fill ]
                [ Element.row [ Element.padding 20, Element.centerX ]
                    [ Element.image [ Element.width (Element.px 40), Element.height (Element.px 40) ] { src = "recycle_bin_full.png", description = "Hoax generator" }
                    , Element.el (titleStyle ++ [ Element.heading 1, Element.centerX ]) <| Element.text "Genrátor hoaxů"
                    ]
                , Element.el [ Element.heading 2, Element.centerX, Element.centerY ] <| Element.paragraph sentenceFontStyle [ Element.text <| sentenceToString model.sentence ]
                , Input.button (buttonStyle ++ [ Element.alignBottom, Element.centerX ]) { onPress = Just GenerateRandomCombination, label = Element.text "VYGENEROVAT NOVÝ HOAX" }
                , Element.paragraph (footerStyle ++ [ Element.alignBottom, Element.centerX ])
                    [ Element.text "Inpirováno "
                    , Element.link linkStyle
                        { url = "https://twitter.com/Posledniskaut/status/1109755950729179136"
                        , label = Element.text "tweetem od @PosledniSkaut"
                        }
                    , Element.text ". Ikonka od "
                    , Element.link linkStyle
                        { url = "https://www.deviantart.com/rokey"
                        , label = Element.text "Rokeyho"
                        }
                    , Element.text ". Zdrojový kód naleznete na "
                    , Element.link linkStyle
                        { url = "https://github.com/kraklin/hoax-generator/"
                        , label = Element.text "GitHubu"
                        }
                    ]
                ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
