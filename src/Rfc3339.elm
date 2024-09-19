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

import Date
import Parser.Advanced exposing ((|.), (|=))
import Time
import Time.Extra


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
        { instant : Time.Posix
        , offset : { hour : Int, minute : Int }
        }
    | DateTimeLocal Time.Extra.Parts
    | DateLocal Date.Date
    | TimeLocal
        { hour : Int
        , minute : Int
        , second : Int
        , millisecond : Int
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
            case
                input
                    |> String.slice 2 3
                    |> String.uncons
            of
                Nothing ->
                    True

                Just ( char, _ ) ->
                    Char.isDigit char
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
                    let
                        parts : Time.Extra.Parts
                        parts =
                            { year = Date.year date
                            , month = Date.month date
                            , day = Date.day date
                            , hour = time.hour
                            , minute = time.minute
                            , second = time.second
                            , millisecond = time.millisecond
                            }
                    in
                    case maybeOffset of
                        Nothing ->
                            DateTimeLocal parts

                        Just offset ->
                            DateTimeOffset
                                { instant = Time.Extra.partsToPosix (fakeZone offset) parts
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


fakeZone : { hour : Int, minute : Int } -> Time.Zone
fakeZone offset =
    Time.customZone
        (if offset.hour >= 0 then
            offset.hour * 60 + offset.minute

         else
            offset.hour * 60 - offset.minute
        )
        []


checkDay : { a | year : Int, month : Time.Month, day : Int } -> Parser Date.Date
checkDay date =
    let
        maxDays : Int
        maxDays =
            daysInMonth date
    in
    if date.day > maxDays then
        Parser.Advanced.problem (PrbDayTooLarge maxDays)

    else
        Parser.Advanced.succeed (Date.fromCalendarDate date.year date.month date.day)


dateParser : Parser Date.Date
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
                        if int < 1 || int > 12 then
                            Parser.Advanced.problem PrbInvalidMonth

                        else
                            Parser.Advanced.succeed (Date.numberToMonth int)
                    )
           )
        |. Parser.Advanced.token (Parser.Advanced.Token "-" PrbExpectedDateSeparator)
        |= parseDigitsInRange 2 { min = 1, max = 31 } PrbInvalidDay
        |> Parser.Advanced.andThen checkDay
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
        , second : Int
        , millisecond : Int
        }
timeLocalParser =
    Parser.Advanced.succeed
        (\hour minute ( second, millisecond ) ->
            { hour = hour
            , minute = minute
            , second = second
            , millisecond = millisecond
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
                                Parser.Advanced.succeed ( second, 0 )

                            Just frac ->
                                case String.toInt (String.left 3 (frac ++ "000")) of
                                    Nothing ->
                                        Parser.Advanced.problem PrbExpectedAnInt

                                    Just f ->
                                        Parser.Advanced.succeed ( second, f )
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
            Date.toIsoString date

        DateTimeLocal dateT ->
            dateToString dateT ++ "T" ++ timeToString dateT

        DateTimeOffset dateT ->
            let
                parts : Time.Extra.Parts
                parts =
                    Time.Extra.posixToParts (fakeZone dateT.offset) dateT.instant
            in
            dateToString parts ++ "T" ++ timeToString parts ++ offsetToString dateT.offset


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
            |> Date.monthToNumber
            |> padInt2
        , padInt2 date.day
        ]


timeToString : { a | hour : Int, minute : Int, second : Int, millisecond : Int } -> String
timeToString time =
    String.join ":"
        [ padInt2 time.hour
        , padInt2 time.minute
        , if time.millisecond == 0 then
            padInt2 time.second

          else
            padInt2 time.second ++ "." ++ String.padLeft 3 '0' (String.fromInt time.millisecond)
        ]


padInt2 : Int -> String
padInt2 i =
    String.padLeft 2 '0' (String.fromInt i)
