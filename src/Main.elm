module Main exposing (main)

import Browser
import VirtualDom
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onInput, onClick)
import Chart as C
import Chart.Attributes as CA
import Css exposing (..)
import Time
import List exposing (length, tail)


main : Program () Model Msg
main =
    Browser.element
        {   init = init, 
            view = view, 
            update = update, 
            subscriptions = subscriptions
        }


type alias Model =
    {
        position: Float,
        velocity: Float,
        systematic: Float,  -- Systematic error
        output : Float,     -- Acceleration or angle of board
        integration: Float, -- Current integration
        last_error: Float,
        setpoint: Float,
        proportional: Float,
        integral: Float,    -- Weighting of integral term
        derivative: Float,
        history: List Float,
        setpoint_history: List Float,
        scenario: Scenario
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model 
        10      -- Position
        0       -- Velocity
        0       -- Systematic
        0       -- Output
        0       -- Integration
        0       -- Last error
        20      -- Setpoint
        0       -- P
        0       -- I
        0       -- D
        []      -- History
        []      -- Setpoint History
        Normal   -- Scenario
    , Cmd.none )


type Msg
    = Tick      -- Every 10 ms
    | ChangeP Float
    | ChangeI Float
    | ChangeD Float
    | Changesetpoint Float
    | ChangeSystematic Float
    | SwitchViz -- Switch between normal and rolling ball
    | Disturb

type Scenario 
    = Normal
    | RollingBall

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        Tick ->
            case model.scenario of
                Normal      -> run_tick model
                RollingBall -> run_tick_ball model

        ChangeP val -> 
            {model | proportional = val}

        ChangeI val -> 
            {model | integral = val}

        ChangeD val -> 
            {model | derivative = val}

        Changesetpoint val -> 
            {model | setpoint = val}

        ChangeSystematic val ->
            {model | systematic = val}

        SwitchViz -> 
            {model | scenario = 
                case model.scenario of 
                    Normal -> RollingBall 
                    RollingBall -> Normal
            }
        Disturb -> 
            {model | velocity = model.velocity + 1.5}

    , Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 (\_ -> Tick)


view : Model -> VirtualDom.Node Msg
view model =
    toUnstyled (
        div [css [marginLeft (px 10)]]
        [ 
            div [] 
                (case model.scenario of
                    Normal      -> normal_view model
                    RollingBall -> rolling_ball model
                ),
            slider model.setpoint Changesetpoint 100 "Setpoint",
            slider model.proportional ChangeP 1000 "Proportional",
            slider model.integral ChangeI 1000 "Integral",
            slider model.derivative ChangeD 1000 "Derivative",
            slider model.systematic ChangeSystematic 5000 "Systematic Error",
            chart model,
            button [onClick SwitchViz] [text (
                case model.scenario of
                    Normal -> "switch to ball"
                    RollingBall -> "switch to normal"
                )],
            button [onClick Disturb] [text "disturb"]
        ]
    )

normal_view : Model -> List (Html Msg)
normal_view model = 
    [
        moving_box model,
        setpoint_line model
    ]

moving_box : Model -> Html Msg
moving_box model = 
            div [css [
                position absolute,
                bottom (vh (model.position - 0.5)),
                left (vw 50),
                height (vh 1),
                width (vh 1),
                border3 (px 2) solid (rgb 120 120 120)
            ]] []

setpoint_line : Model -> Html Msg
setpoint_line model = 
            div [css [
                position absolute,
                bottom (vh (model.setpoint)),
                left (vw 50),
                width (vh 1),
                border3 (px 2) solid (rgb 120 10 10)
            ]] []

rolling_ball : Model -> List (Html Msg)
rolling_ball model =
    [
        div [css [
            transform (rotate (deg (model.output * 20))),
            position absolute,
            left (vw 50),
            top (vh 50)
        ]] [
            div [css [
                width (vw 20),
                height (px 2),
                border3 (px 2) solid (rgb 0 0 0)
            ]] [],
            div [css [
                width (px 20),
                height (px 20),
                backgroundColor (rgb 120 120 120),
                transforms [
                    translateX (vw (-0.7 + model.position / 5)),
                    translateY (px -26)
                ],
                borderRadius (pct 100)
            ]] []
        ]
    ]

slider : Float -> (Float -> Msg) -> Int -> String -> Html Msg
slider model msg max label =
    div [] [
        text label,
        input
        [ 
            type_ "range", 
            Html.Styled.Attributes.min "0", 
            Html.Styled.Attributes.max (String.fromInt max), 
            value <| String.fromFloat model, 
            onInput (\string -> String.toFloat string |> Maybe.withDefault 0 |> msg)
        ]
        []
    ]


combine_data : List Float -> List Float -> List { x : Float, y : Float, z : Float }
combine_data pos_hist setpoint_hist  =
    List.map3 (\x y z -> {x = x, y = y - z, z = z}) (List.map toFloat (List.range 1 history_length)) pos_hist setpoint_hist


chart : Model -> Html Msg
chart model = 
    div [css [
        width (vh 50),
        height (vh 50),
        margin (px 20),
        backgroundColor (rgb 250 244 250)
    ]] [
        fromUnstyled (C.chart [
            CA.domain
                [ 
                    CA.lowest 0 CA.exactly, 
                    CA.highest 100 CA.exactly
                ],
            CA.range
                [ 
                    CA.lowest 0 CA.exactly, 
                    CA.highest (toFloat history_length) CA.exactly
                ]
        ]
        [
            C.series .x
            [ C.stacked
                [ 
                    C.interpolated .y
                        [ CA.opacity 0.2, CA.color CA.purple ]
                        [], 
                    C.interpolated .z
                        [ CA.monotone, CA.opacity 0, CA.color CA.red ]
                        []
                ]
            ]
            (combine_data model.history model.setpoint_history)
        ])
    ]


dt : Float
dt = 0.01

history_length : Int
history_length = 500

run_tick : Model -> Model
run_tick model = 
    let
        error = model.setpoint - model.position

        derivative = (model.last_error - error) / dt

        p = model.proportional
        i = model.integral
        d = model.derivative

        acceleration = (p * error + i * model.integration - d * derivative + model.systematic) / 100000

        velocity = model.velocity + acceleration

        position = model.position + velocity
    in
    {model | 
        position = position,
        velocity = velocity,
        integration = model.integration + error * dt,
        last_error = error,
        history = update_history model.history position,
        setpoint_history = update_history model.setpoint_history model.setpoint,
        output = acceleration
    }

run_tick_ball : Model -> Model
run_tick_ball model =
    let 
        error = 50 - model.position

        derivative = (model.last_error - error) / dt

        p = model.proportional
        i = model.integral
        d = model.derivative

        angle = (p * error + i * model.integration - d * derivative + model.systematic) / 10000

        velocity = model.velocity + (sin angle) / 20

        position = model.position + velocity
    in
    {model | 
        position = position,
        velocity = velocity,
        integration = model.integration + error * dt,
        last_error = error,
        history = update_history model.history position,
        setpoint_history = update_history model.setpoint_history model.setpoint,
        output = angle
    }


update_history : List Float -> Float -> List Float
update_history history new =
    (
        if length history >= history_length then
            tail history |> Maybe.withDefault []
        else 
            history
    ) ++ [new]