module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text, img)
import Html.Events exposing (onClick)
import Browser.Dom exposing (Viewport)
import Html.Attributes exposing (class, src, style)

main =
        Browser.sandbox
                { init =
                        { pressed = False
                        , list = cardList
                        }
                , update = update
                , view = view
                }


type alias Model =
    { pressed :  Bool
    , list : List Card
    }

type alias Card =
        { name : String
        , imageSource : String
        }
type Msg
        = Press
        | Maybe 


cardList : List Card
cardList =
        [       
                  { name = "Outage"
                  , imageSource = "https://imgs.xkcd.com/comics/service_outage.png"
                  }
                , { name = "Aurora"
                  , imageSource = "https://imgs.xkcd.com/comics/aurora_meaning.png"
                  }
                , { name = "Arcane"
                  , imageSource = "https://imgs.xkcd.com/comics/arcane_bullshit.png"
                  }
        ]
update : Msg -> Model -> Model
update msg model =
        model
view : Model -> Html Msg
view model =
        div
                []
                [
                button
                        [ onClick Press]
                        [ text "Test" ]
                , div
                        []
                        (readCards model.list)
                ]

len : List a -> Int
len list =
        case list of
                h :: t ->                        
                        1 + len t 
                [] -> 
                        0

len2 : List a -> Int
len2 list =
        let
                myLen : List a -> Int -> Int
                myLen xs acc =
                        case xs of
                                h :: t ->
                                        myLen t (acc + 1)
                                [] ->
                                        acc
        in
                myLen list 0

readCards : List Card -> List (Html Msg)
readCards list =
                case list of
                        h :: t ->
                                viewCard (20*len list) h :: readCards t                                 
                        [] ->
                                []
        

        

viewCard : Int -> Card -> Html Msg
viewCard offset card =
                div [ class "deck" ] [
                        div
                                [ class "border"
                                , style "left" (String.fromInt offset ++ "px")
                                , style "top" (String.fromInt offset ++ "px") ]
                                [ div
                                [ class "title" ]
                                [ text card.name ]
                                , img
                                [ class "image", src   card.imageSource]
                                []
                                ]
                        ]
