module Rfc3339 exposing
    ( DateTime(..)
    , parse
    , Error(..)
    , toString
    )

{-| Parses a String into an [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339) date time format.

@docs DateTime
@docs parse
@docs Error

@docs toString

-}

import Parser.Advanced exposing ((|.), (|=))
import Time


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = ParsingDate
    | ParsingTime
    | ParsingOffset


type Problem
    = PrbExpectedDateSeparator
    | PrbExpectedDateTimeSeparator
    | PrbExpectedTimeSeparator
    | PrbExpectedOffsetSeparator
    | PrbInvalidMonth
    | PrbDayTooLarge Int
    | PrbExpectedZuluOffset
    | PrbExpectedOffsetSign
    | PrbExpectedFractionalSecondSeparator
    | PrbExpectedDigit
    | PrbExpectedAFloat
    | PrbExpectedAnInt
    | PrbInvalidNegativeDigits
    | PrbInvalidHour
    | PrbInvalidMinute
    | PrbInvalidSecond
    | PrbInvalidDay


{-| Represents one of:

  - **local time**: e.g. 09:15:22
  - **local date**: e.g. 1970-11-21
  - **local date time**: e.g. 1970-11-21T09:15:22
  - **date time with offset**: e.g. 1970-11-21T09:15:22+01:00

-}
type DateTime
    = DateTimeOffset
        { year : Int
        , month : Time.Month
        , day : Int
        , hour : Int
        , minute : Int
        , second : Float
        , offset : { hour : Int, minute : Int }
        }
    | DateTimeLocal
        { year : Int
        , month : Time.Month
        , day : Int
        , hour : Int
        , minute : Int
        , second : Float
        }
    | DateLocal
        { year : Int
        , month : Time.Month
        , day : Int
        }
    | TimeLocal
        { hour : Int
        , minute : Int
        , second : Float
        }


{-| All of the ways a parsing can fail.
-}
type Error
    = ExpectedDateSeparator
    | ExpectedDateTimeSeparator
    | ExpectedTimeSeparator
    | ExpectedOffsetSeparator
    | InvalidMonth
    | DayTooLarge Int
    | ExpectedZuluOffset
    | ExpectedOffsetSign
    | ExpectedFractionalSecondSeparator
    | ExpectedDigit
    | ExpectedAFloat
    | ExpectedAnInt
    | InvalidNegativeDigits
    | InvalidHour
    | InvalidMinute
    | InvalidSecond
    | InvalidDay


{-| Attempts to convert a `String` to a `DateTime`
-}
parse : String -> Result (List Error) DateTime
parse input =
    let
        useDateParser : Bool
        useDateParser =
            input
                |> String.left 3
                |> String.dropLeft 2
                |> String.uncons
                |> Maybe.map (\( char, _ ) -> Char.isDigit char)
                |> Maybe.withDefault True
    in
    Result.mapError (List.map toError) <|
        if useDateParser then
            Parser.Advanced.run dateTimeParser input

        else
            Parser.Advanced.run (Parser.Advanced.map TimeLocal timeLocalParser) input


toError : Parser.Advanced.DeadEnd Context Problem -> Error
toError deadEnd =
    case deadEnd.problem of
        PrbExpectedDateSeparator ->
            ExpectedDateSeparator

        PrbExpectedDateTimeSeparator ->
            ExpectedDateTimeSeparator

        PrbExpectedTimeSeparator ->
            ExpectedTimeSeparator

        PrbExpectedOffsetSeparator ->
            ExpectedOffsetSeparator

        PrbInvalidMonth ->
            InvalidMonth

        PrbDayTooLarge amt ->
            DayTooLarge amt

        PrbExpectedZuluOffset ->
            ExpectedZuluOffset

        PrbExpectedOffsetSign ->
            ExpectedOffsetSign

        PrbExpectedFractionalSecondSeparator ->
            ExpectedFractionalSecondSeparator

        PrbExpectedDigit ->
            ExpectedDigit

        PrbExpectedAFloat ->
            ExpectedAFloat

        PrbExpectedAnInt ->
            ExpectedAnInt

        PrbInvalidNegativeDigits ->
            InvalidNegativeDigits

        PrbInvalidHour ->
            InvalidHour

        PrbInvalidMinute ->
            InvalidMinute

        PrbInvalidSecond ->
            InvalidSecond

        PrbInvalidDay ->
            InvalidDay


dateTimeParser : Parser DateTime
dateTimeParser =
    Parser.Advanced.succeed
        (\date maybeTimeOffset ->
            case maybeTimeOffset of
                Nothing ->
                    DateLocal date

                Just ( time, maybeOffset ) ->
                    case maybeOffset of
                        Nothing ->
                            DateTimeLocal
                                { year = date.year
                                , month = date.month
                                , day = date.day
                                , hour = time.hour
                                , minute = time.minute
                                , second = time.second
                                }

                        Just offset ->
                            DateTimeOffset
                                { year = date.year
                                , month = date.month
                                , day = date.day
                                , hour = time.hour
                                , minute = time.minute
                                , second = time.second
                                , offset = offset
                                }
        )
        |= dateParser
        |= Parser.Advanced.oneOf
            [ Parser.Advanced.succeed (\time maybeOffset -> Just ( time, maybeOffset ))
                |. Parser.Advanced.oneOf
                    [ Parser.Advanced.token (Parser.Advanced.Token "T" PrbExpectedDateTimeSeparator)
                    , Parser.Advanced.token (Parser.Advanced.Token "t" PrbExpectedDateTimeSeparator)
                    , Parser.Advanced.token (Parser.Advanced.Token " " PrbExpectedDateTimeSeparator)
                        |> Parser.Advanced.backtrackable
                    ]
                |= timeLocalParser
                |= Parser.Advanced.oneOf
                    [ Parser.Advanced.map Just offsetParser
                    , Parser.Advanced.succeed Nothing
                    ]
            , Parser.Advanced.succeed Nothing
            ]
        |> Parser.Advanced.andThen
            (\dateTime ->
                case dateTime of
                    TimeLocal _ ->
                        Parser.Advanced.succeed dateTime

                    DateLocal date ->
                        let
                            maxDays : Int
                            maxDays =
                                daysInMonth date
                        in
                        if date.day > maxDays then
                            Parser.Advanced.problem (PrbDayTooLarge maxDays)

                        else
                            Parser.Advanced.succeed dateTime

                    DateTimeLocal date ->
                        let
                            maxDays : Int
                            maxDays =
                                daysInMonth date
                        in
                        if date.day > maxDays then
                            Parser.Advanced.problem (PrbDayTooLarge maxDays)

                        else
                            Parser.Advanced.succeed dateTime

                    DateTimeOffset date ->
                        let
                            maxDays : Int
                            maxDays =
                                daysInMonth date
                        in
                        if date.day > maxDays then
                            Parser.Advanced.problem (PrbDayTooLarge maxDays)

                        else
                            Parser.Advanced.succeed dateTime
            )


dateParser : Parser { year : Int, month : Time.Month, day : Int }
dateParser =
    Parser.Advanced.succeed
        (\year month day ->
            { year = year
            , month = month
            , day = day
            }
        )
        |= parseDigits 4
        |. Parser.Advanced.token (Parser.Advanced.Token "-" PrbExpectedDateSeparator)
        |= (parseDigits 2
                |> Parser.Advanced.andThen
                    (\int ->
                        case intToMonth int of
                            Nothing ->
                                Parser.Advanced.problem PrbInvalidMonth

                            Just month ->
                                Parser.Advanced.succeed month
                    )
           )
        |. Parser.Advanced.token (Parser.Advanced.Token "-" PrbExpectedDateSeparator)
        |= parseDigitsInRange 2 { min = 1, max = 31 } PrbInvalidDay
        |> Parser.Advanced.inContext ParsingDate


offsetParser : Parser { hour : Int, minute : Int }
offsetParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed { hour = 0, minute = 0 }
            |. Parser.Advanced.token (Parser.Advanced.Token "Z" PrbExpectedZuluOffset)
        , Parser.Advanced.succeed (\sign hour minute -> { hour = sign hour, minute = minute })
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.succeed identity
                    |. Parser.Advanced.token (Parser.Advanced.Token "+" PrbExpectedOffsetSign)
                , Parser.Advanced.succeed negate
                    |. Parser.Advanced.token (Parser.Advanced.Token "-" PrbExpectedOffsetSign)
                ]
            |= hourParser
            |. Parser.Advanced.token (Parser.Advanced.Token ":" PrbExpectedOffsetSeparator)
            |= minuteParser
        ]
        |> Parser.Advanced.inContext ParsingOffset


timeLocalParser :
    Parser
        { hour : Int
        , minute : Int
        , second : Float
        }
timeLocalParser =
    Parser.Advanced.succeed
        (\hour minute second ->
            { hour = hour
            , minute = minute
            , second = second
            }
        )
        |= hourParser
        |. Parser.Advanced.token (Parser.Advanced.Token ":" PrbExpectedTimeSeparator)
        |= minuteParser
        |. Parser.Advanced.token (Parser.Advanced.Token ":" PrbExpectedTimeSeparator)
        |= (Parser.Advanced.succeed Tuple.pair
                |= parseDigitsInRange 2 { min = 0, max = 59 } PrbInvalidSecond
                |= Parser.Advanced.oneOf
                    [ Parser.Advanced.succeed Just
                        |. Parser.Advanced.token (Parser.Advanced.Token "." PrbExpectedFractionalSecondSeparator)
                        |= (Parser.Advanced.succeed ()
                                |. Parser.Advanced.chompIf Char.isDigit PrbExpectedDigit
                                |. Parser.Advanced.chompWhile Char.isDigit
                                |> Parser.Advanced.getChompedString
                           )
                    , Parser.Advanced.succeed Nothing
                    ]
                |> Parser.Advanced.andThen
                    (\( second, fracSeconds ) ->
                        case fracSeconds of
                            Nothing ->
                                Parser.Advanced.succeed (toFloat second)

                            Just frac ->
                                case String.toFloat (String.fromInt second ++ "." ++ frac) of
                                    Nothing ->
                                        Parser.Advanced.problem PrbExpectedAFloat

                                    Just f ->
                                        Parser.Advanced.succeed f
                    )
           )
        |> Parser.Advanced.inContext ParsingTime


parseDigits : Int -> Parser Int
parseDigits size =
    Parser.Advanced.loop size parseDigitsHelper
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\digits ->
                case String.toInt digits of
                    Nothing ->
                        Parser.Advanced.problem PrbExpectedAnInt

                    Just i ->
                        Parser.Advanced.succeed i
            )


parseDigitsInRange : Int -> { min : Int, max : Int } -> Problem -> Parser Int
parseDigitsInRange size limits limitProblem =
    Parser.Advanced.loop size parseDigitsHelper
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\digits ->
                case String.toInt digits of
                    Nothing ->
                        Parser.Advanced.problem PrbExpectedAnInt

                    Just i ->
                        if i < limits.min then
                            Parser.Advanced.problem limitProblem

                        else if i > limits.max then
                            Parser.Advanced.problem limitProblem

                        else
                            Parser.Advanced.succeed i
            )


parseDigitsHelper : Int -> Parser (Parser.Advanced.Step Int ())
parseDigitsHelper leftToChomp =
    if leftToChomp < 0 then
        Parser.Advanced.problem PrbInvalidNegativeDigits

    else if leftToChomp > 0 then
        Parser.Advanced.succeed (Parser.Advanced.Loop (leftToChomp - 1))
            |. Parser.Advanced.chompIf Char.isDigit PrbExpectedDigit

    else
        Parser.Advanced.succeed (Parser.Advanced.Done ())


intToMonth : Int -> Maybe Time.Month
intToMonth i =
    case i of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


hourParser : Parser Int
hourParser =
    parseDigitsInRange 2 { min = 0, max = 23 } PrbInvalidHour


minuteParser : Parser Int
minuteParser =
    parseDigitsInRange 2 { min = 0, max = 59 } PrbInvalidMinute


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0)


daysInMonth : { a | year : Int, month : Time.Month } -> Int
daysInMonth date =
    case date.month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear date.year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


{-| Prints an RFC3339 formatted String, using `T` as the date time separator and `Z` for an offset of 0 hours and 0 minutes.
-}
toString : DateTime -> String
toString dateTime =
    case dateTime of
        TimeLocal time ->
            timeToString time

        DateLocal date ->
            dateToString date

        DateTimeLocal dateT ->
            dateToString dateT ++ "T" ++ timeToString dateT

        DateTimeOffset dateT ->
            dateToString dateT ++ "T" ++ timeToString dateT ++ offsetToString dateT.offset


offsetToString : { hour : Int, minute : Int } -> String
offsetToString offset =
    if offset.hour == 0 && offset.minute == 0 then
        "Z"

    else
        (if offset.hour >= 0 then
            padInt2 offset.hour

         else
            "-" ++ padInt2 (abs offset.hour)
        )
            ++ ":"
            ++ padInt2 offset.minute


dateToString : { a | year : Int, month : Time.Month, day : Int } -> String
dateToString date =
    String.join "-"
        [ String.padLeft 4 '0' (String.fromInt date.year)
        , date.month
            |> monthToInt
            |> padInt2
        , padInt2 date.day
        ]


timeToString : { a | hour : Int, minute : Int, second : Float } -> String
timeToString time =
    String.join ":"
        [ padInt2 time.hour
        , padInt2 time.minute
        , time.second
            |> String.fromFloat
            |> String.split "."
            |> (\seconds ->
                    case seconds of
                        [ whole ] ->
                            [ String.padLeft 2 '0' whole ]

                        [ whole, part ] ->
                            [ String.padLeft 2 '0' whole, part ]

                        _ ->
                            seconds
               )
            |> String.join "."
        ]


padInt2 : Int -> String
padInt2 i =
    String.padLeft 2 '0' (String.fromInt i)
