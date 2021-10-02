module Main exposing (main)

-- This is a conversion of
-- https://github.com/lucamug/elm-donut/blob/master/docs/a1k0n-originals/donut.js

import Array exposing (Array)
import Browser
import Browser.Events
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias I_loopData =
    { ct : Float
    , st : Float
    }


type alias J_loopData =
    { cA : Float
    , cB : Float
    , sA : Float
    , sB : Float
    }


type alias Donut =
    { b : Array String
    , z : Array Float
    }


loop :
    { counter : number
    , max : number
    , increment : number
    , data : data
    , donut : donut
    , helper :
        { counter : number
        , data : data
        , donut : donut
        }
        -> donut
    }
    -> donut
loop args =
    if args.counter >= args.max then
        -- We reached the limit, we simply return
        -- the current donut
        args.donut

    else
        -- Otherwise we call `loop` recursively...
        loop
            -- ..after incrementing the counter...
            { counter = args.counter + args.increment

            -- ..and calculating a new donut...
            , donut =
                args.helper
                    { counter = args.counter
                    , data = args.data
                    , donut = args.donut
                    }

            -- ...while the rest of the data
            --    remain as it is.
            , max = args.max
            , increment = args.increment
            , data = args.data
            , helper = args.helper
            }


i_loop :
    { data : ( I_loopData, J_loopData )
    , donut : Donut
    }
    -> Donut
i_loop { data, donut } =
    --
    -- JavaScript:
    --
    -- for (var i = 0; i < 6.28; i += 0.02) {...}
    --
    loop
        { counter = 0
        , max = 6.28
        , increment = 0.02
        , data = data
        , donut = donut
        , helper = i_loopHelper
        }


j_loop : J_loopData -> Donut
j_loop data =
    --
    -- JavaScript:
    --
    -- for (var j = 0; j < 6.28; j += 0.07) {...}
    --
    loop
        { counter = 0
        , max = 6.28
        , increment = 0.07
        , data = data
        , donut = blankDonut
        , helper = j_loopHelper
        }


k_loop :
    { counter : Int
    , donut : Donut
    }
    -> Donut
k_loop { counter, donut } =
    --
    -- JavaScript:
    --
    -- for (var k = 0; k < 1760; k++) {...}
    --
    loop
        { counter = counter
        , max = 1760
        , increment = 1
        , data = ()
        , donut = donut
        , helper = k_loopHelper
        }


i_loopHelper :
    { counter : Float
    , data : ( I_loopData, J_loopData )
    , donut : Donut
    }
    -> Donut
i_loopHelper { counter, data, donut } =
    --
    -- JavaScript:
    --
    -- var sp = Math.sin(i),
    --     cp = Math.cos(i),
    --     h = ct + 2, // R1 + R2*cos(theta)
    --     D = 1 / (sp * h * sA + st * cA + 5), // this is 1/z
    --     t = sp * h * cA - st * sA; // this is a clever factoring of some of the terms in x' and y'
    --
    -- var x = 0 | (40 + 30 * D * (cp * h * cB - t * sB)),
    --     y = 0 | (12 + 15 * D * (cp * h * sB + t * cB)),
    --     o = x + 80 * y,
    --     N = 0 | (8 * ((st * sA - sp * ct * cA) * cB - sp * ct * sA - st * cA - cp * ct * sB));
    -- if (y < 22 && y >= 0 && x >= 0 && x < 79 && D > z[o]) {
    --     z[o] = D;
    --     b[o] = ".,-~:;=!*#$@" [N > 0 ? N : 0];
    -- }
    --
    let
        ( { ct, st }, { sA, cA, cB, sB } ) =
            data

        sp : Float
        sp =
            sin counter

        cp : Float
        cp =
            cos counter

        h : Float
        h =
            ct + 2

        d : Float
        d =
            1 / (sp * h * sA + st * cA + 5)

        t : Float
        t =
            sp * h * cA - st * sA

        x : Int
        x =
            round (40 + 30 * d * (cp * h * cB - t * sB))

        y : Int
        y =
            round (12 + 15 * d * (cp * h * sB + t * cB))

        o : Int
        o =
            x + 80 * y

        n : Int
        n =
            round (8 * ((st * sA - sp * ct * cA) * cB - sp * ct * sA - st * cA - cp * ct * sB))
    in
    case Array.get o donut.z of
        Just zValue ->
            if y < 22 && y >= 0 && x >= 0 && x < 79 && d > zValue then
                { z = Array.set o d donut.z
                , b = Array.set o (filler n) donut.b
                }

            else
                donut

        Nothing ->
            donut


j_loopHelper :
    { counter : Float
    , data : J_loopData
    , donut : Donut
    }
    -> Donut
j_loopHelper { counter, data, donut } =
    --
    -- JavaScript:
    --
    -- var ct = Math.cos(j),
    --     st = Math.sin(j);
    --
    i_loop
        { data = ( { ct = cos counter, st = sin counter }, data )
        , donut = donut
        }


k_loopHelper :
    { counter : Int
    , data : ()
    , donut : Donut
    }
    -> Donut
k_loopHelper { counter, data, donut } =
    --
    -- JavaScript:
    --
    -- b[k] = k % 80 == 79 ? "\n" : " ";
    -- z[k] = 0;
    --
    { b =
        Array.push
            (if remainderBy 80 counter == 79 then
                "\n"

             else
                " "
            )
            donut.b
    , z = Array.push 0 donut.z
    }


fillers : Array String
fillers =
    --
    -- JavaScript:
    --
    --  ".,-~:;=!*#$@"[N > 0 ? N : 0];
    --
    Array.fromList (String.split "" " ,-~:;!*$▚")


filler : Int -> String
filler n =
    --
    -- JavaScript:
    --
    --  ".,-~:;=!*#$@"[N > 0 ? N : 0];
    --
    if n <= 0 then
        " "

    else
        Maybe.withDefault "▓" (Array.get n fillers)


emptyDonut : Donut
emptyDonut =
    { b = Array.fromList []
    , z = Array.fromList []
    }


blankDonut : Donut
blankDonut =
    k_loop
        { counter = 0
        , donut = emptyDonut
        }


donutBuilder : J_loopData -> String
donutBuilder data =
    j_loop data
        |> .b
        |> Array.toList
        |> String.join ""


type alias Model =
    { animation : Bool
    , aCounter : Int
    , aSteps : Int
    , aStepsAsString : String
    , bCounter : Int
    , bSteps : Int
    , bStepsAsString : String
    , cache : Dict.Dict String String
    , cached : Bool
    , cacheFound : Bool
    , donatAsString : String
    , frame : Int
    , fps : Int
    }


performanceTest : Float -> ()
performanceTest counter =
    --
    -- This function generate 1000 donuts in rapid succession to test the
    -- performance of the script.
    --
    let
        a =
            1 + counter * 0.07

        b =
            1 + counter + 0.03
    in
    if counter >= 1000 then
        ()

    else
        let
            _ =
                donutBuilder
                    { cA = cos a
                    , cB = cos b
                    , sA = sin a
                    , sB = sin b
                    }
        in
        performanceTest (counter + 1)


init : () -> ( Model, Cmd msg )
init _ =
    let
        performanceTestActive : Bool
        performanceTestActive =
            False

        a : Int
        a =
            100

        b : Int
        b =
            200

        _ =
            if performanceTestActive then
                performanceTest 0

            else
                ()
    in
    ( { animation = not performanceTestActive
      , aCounter = 0
      , aSteps = a
      , aStepsAsString = String.fromInt a
      , bCounter = 0
      , bSteps = b
      , bStepsAsString = String.fromInt b
      , cache = Dict.empty
      , cached = True
      , cacheFound = False
      , donatAsString = ""
      , frame = 0
      , fps = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.animation then
        Browser.Events.onAnimationFrameDelta Animation

    else
        Sub.none


type Msg
    = ToggleAnimation
    | ToggleCached
    | Animation Float
    | ChangeASteps String
    | ChangeBSteps String
    | ChangeACounter String
    | ChangeBCounter String
    | ChangeFps String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnimation ->
            ( { model | animation = not model.animation }, Cmd.none )

        ToggleCached ->
            ( { model | cached = not model.cached }, Cmd.none )

        Animation delta ->
            if model.frame < model.fps then
                ( { model | frame = model.frame + 1 }, Cmd.none )

            else
                ( updateDonut
                    { model
                        | frame = 0
                        , aCounter =
                            if model.aCounter >= model.aSteps then
                                1

                            else
                                model.aCounter + 1
                        , bCounter =
                            if model.bCounter >= model.bSteps then
                                1

                            else
                                model.bCounter + 1
                    }
                , Cmd.none
                )

        ChangeASteps string ->
            ( updateDonut
                { model
                    | aStepsAsString = string
                    , aSteps = changeValue model.aSteps string
                }
            , Cmd.none
            )

        ChangeFps string ->
            ( updateDonut
                { model
                    | fps = Maybe.withDefault 0 (String.toInt string)
                }
            , Cmd.none
            )

        ChangeBSteps string ->
            ( updateDonut
                { model
                    | bStepsAsString = string
                    , bSteps = changeValue model.bSteps string
                }
            , Cmd.none
            )

        ChangeACounter string ->
            ( updateDonut
                { model
                    | aCounter = Maybe.withDefault model.aCounter (String.toInt string)
                }
            , Cmd.none
            )

        ChangeBCounter string ->
            ( updateDonut
                { model
                    | bCounter = Maybe.withDefault model.bCounter (String.toInt string)
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ style "background" "#111"
        , style "color" "#bbb"
        , style "display" "inline-block"
        ]
        [ pre
            [ style "cursor" "pointer"
            , onClick ToggleAnimation
            ]
            [ text model.donatAsString ]
        , pre [ style "font-family" "\"IBM Plex Sans\", sans-serif" ]
            [ span
                [ title "Start/Stop the animation. You can also click on the donut for the same effect."
                , onClick ToggleAnimation
                , style "cursor" "pointer"
                ]
                [ text "  "
                , text <|
                    if model.animation then
                        "▢"

                    else
                        "▷"
                , text "     "
                ]
            , span
                [ title "When cache is active, every step is saved in memory and re-used when available, insted of being recalculated."
                , onClick ToggleCached
                , style "cursor" "pointer"
                ]
                [ text <|
                    if model.cached then
                        if model.cacheFound then
                            "🟢"

                        else
                            "🟡"

                    else
                        "🔴"
                , text "Cache"
                ]
            , text "  "
            , input
                (sliderStyle
                    { title = "Slide to change the frames per seconds"
                    , msg = ChangeFps
                    , min = 0
                    , max = 10
                    , value = model.fps
                    }
                )
                []
            , text "FPS"
            , text "  "
            , input (inputStyle ChangeASteps model.aStepsAsString) []
            , input (inputStyle ChangeBSteps model.bStepsAsString) []
            , text " "
            , input
                (sliderStyle
                    { title = "Counter (Slide to change the position of the donut)"
                    , msg = ChangeACounter
                    , min = 0
                    , max = model.aSteps
                    , value = model.aCounter
                    }
                )
                []
            , input
                (sliderStyle
                    { title = "Counter (Slide to change the position of the donut)"
                    , msg = ChangeBCounter
                    , min = 0
                    , max = model.bSteps
                    , value = model.bCounter
                    }
                )
                []
            , text "  "
            , a
                [ href "https://lucamug.github.io/elm-donut/"
                , style "color" "#ccc"
                ]
                [ text "built with Elm" ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- HELPERS


changeValue : Int -> String -> Int
changeValue defaultValue newValueAsString =
    case String.toInt newValueAsString of
        Nothing ->
            defaultValue

        Just int ->
            if 0 < int && int <= 1000 then
                int

            else
                defaultValue


j_LoopDataBuilder : { a : Float, b : Float } -> J_loopData
j_LoopDataBuilder { a, b } =
    --
    -- Javascript:
    --
    --     var cA=Math.cos(A), sA=Math.sin(A),
    --         cB=Math.cos(B), sB=Math.sin(B);
    --
    { cA = cos a
    , cB = cos b
    , sA = sin a
    , sB = sin b
    }


seedBuilder : Model -> { a : Float, b : Float }
seedBuilder model =
    { a = (degrees 360 / toFloat model.aSteps) * toFloat model.aCounter
    , b = (degrees 360 / toFloat model.bSteps) * toFloat model.bCounter
    }


updateDonut : Model -> Model
updateDonut model =
    let
        j_LoopData : J_loopData
        j_LoopData =
            j_LoopDataBuilder (seedBuilder model)
    in
    if model.cached then
        let
            key : String
            key =
                String.fromInt model.aCounter
                    ++ "/"
                    ++ String.fromInt model.aSteps
                    ++ " "
                    ++ String.fromInt model.bCounter
                    ++ "/"
                    ++ String.fromInt model.bSteps
        in
        case Dict.get key model.cache of
            Just cached ->
                { model
                    | donatAsString = cached
                    , cacheFound = True
                }

            Nothing ->
                let
                    donatAsString : String
                    donatAsString =
                        donutBuilder j_LoopData
                in
                { model
                    | cache = Dict.insert key donatAsString model.cache
                    , donatAsString = donatAsString
                    , cacheFound = False
                }

    else
        { model | donatAsString = donutBuilder j_LoopData }


inputStyle : (String -> msg) -> String -> List (Html.Attribute msg)
inputStyle msg v =
    [ title "Steps (This effect the speed of rotation. Lower the number, higher the speed. The range is 1 ~ 1000)"
    , style "background" "#555"
    , style "color" "#fff"
    , style "width" "35px"
    , style "border" "0px"
    , style "font-family" "monospace"
    , style "margin" "0 10px"
    , onInput msg
    , value v
    ]


sliderStyle : { title : String, max : Int, min : Int, msg : String -> msg, value : Int } -> List (Attribute msg)
sliderStyle args =
    [ title args.title
    , type_ "range"
    , Html.Attributes.min <| String.fromInt args.min
    , Html.Attributes.max <| String.fromInt args.max
    , value <| String.fromInt args.value
    , style "width" "50px"
    , onInput args.msg
    , style "margin" "0 10px"
    , style "transform" "translateY(5px)"
    ]
