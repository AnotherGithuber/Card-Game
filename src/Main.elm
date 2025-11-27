module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text, img)
import Html.Events exposing (onClick)
import Browser.Dom exposing (Viewport)
import Html.Attributes exposing (class, src, style)
import Platform.Cmd exposing (none)
import Random exposing (..)
import Html.Attributes exposing (title)

main =
        Browser.element
                { init = init
                , update = update
                , subscriptions = \_ -> Sub.none
                , view = view
                }

randomValue : Model -> Seed -> ((Int, Seed), Model)
randomValue model s =
        (step (Random.int 0 10) s, {model | rng_seed = Tuple.second (step (Random.int 0 10) s) })


init : Int  -> (Model, Cmd Msg)
init v =
        ( {pressed = False, deck = deckList, list = cardList, discard = discardList, rng_seed = Random.initialSeed v }, Cmd.none)
type alias Model =
    { pressed :  Bool
    , deck : List Card
    , list : List Card
    , discard : List Card
    , rng_seed : Random.Seed
    }

type alias Card =
        { name : String
        , imageSource : String
        }
type Msg
        = Shuffle
        | Discard
        | Draw

shuffle : List Int -> List (Card, Int) -> List Card -> Seed -> Model -> List (Card, Int)
shuffle rList tList list seed model=
        let
                finalList = tList
                randomList =
                        if List.length rList > 0 then
                                makeRandomList seed model (List.length list) []
                        else
                                rList 
        in
                 if List.length list > 0 then
                        case list of
                                h :: t ->
                                        case randomList of
                                                head :: tail ->
                                                        shuffle tail ((Tuple.pair h head) :: finalList) list seed model
                                                [] ->
                                                        finalList
                                [] ->
                                        finalList
                else
                        []

zip : List a -> List b -> List (a, b)
zip ax bx =
        case (ax, bx) of
                ( [], _ ) -> []
                ( _, [] ) -> []
                ( h0::r0 , h1::r1 ) -> (h0, h1) :: zip r0 r1

shuffle2 : Model -> Model
shuffle2 model =
        let
                (model_, randomList) =
                        makeRandomList2 model (List.length model.list)
                newCardList =
                        zip model.list randomList
                        |> List.sortBy Tuple.second
                        |> List.map Tuple.first
        in
                { model_ | list =newCardList }

makeRandomList2 : Model -> Int -> (Model, List Float)
makeRandomList2 model length =
        let
                stepper : Seed -> Int -> List Float -> (Seed, List Float)
                stepper seed countdown acc =
                        let
                                (f, newSeed) = Random.step (Random.float 0 1) seed
                        in
                                case countdown of
                                        0 -> (newSeed, acc)
                                        _ -> stepper newSeed (countdown - 1) (f :: acc)
                (finalSeed, randList) =
                        stepper model.rng_seed length []
        in
                ( { model | rng_seed = finalSeed }, randList)

makeRandomList : Seed -> Model -> Int -> List Int -> List Int
makeRandomList seed model length currentList =
        if length > 0 then
                makeRandomList seed model (length  - 1) currentList
        else
                Tuple.first(Tuple.first(randomValue model seed)) :: currentList
discard : Model -> Model
discard model =
        case model.list of
            [] -> model
            (h :: t) -> {model | list = t, discard = h :: model.discard }
draw : Model -> Model
draw model =
        case model.deck of
            [] -> { model | deck = model.discard }
            (h :: t) -> { model | deck = t, list = h :: model.list }
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
deckList : List Card
deckList = []
discardList : List Card
discardList = []
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        case msg of 
                Shuffle ->
                        (shuffle2 model, Cmd.none)
                Draw ->
                        (draw model, Cmd.none)
                Discard ->
                        (discard model, Cmd.none)
view : Model -> Html Msg
view model =
        div
                []
                [
                button
                        [ onClick Shuffle ]
                        [ text "Shuffle" ]
                , button
                        [ onClick Discard ]
                        [ text "Discrard" ]
                , button
                        [ onClick Draw ]
                        [ text "Draw" ] 
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
                                , style "top" (String.fromInt offset ++ "px")
                                , title card.name
                                ]
                                [ div
                                [ class "title"]
                                [ text card.name ]
                                , img
                                [ class "image", src   card.imageSource]
                                []
                                ]
                        ]
