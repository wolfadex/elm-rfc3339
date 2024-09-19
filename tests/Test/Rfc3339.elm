module Test.Rfc3339 exposing (rounding, suite)

import Date
import Expect
import Rfc3339
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "dates and times"
        [ localTimes
        , localDates
        , localDateTimes
        , offsetDateTimes
        , printing
        ]


localTimes : Test
localTimes =
    describe "local times"
        [ test "midnight" <|
            \() ->
                "00:00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.TimeLocal
                                { hour = 0
                                , minute = 0
                                , second = 0
                                , millisecond = 0
                                }
                            )
                        )
        , test "right after midnight" <|
            \() ->
                "00:00:00.001"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.TimeLocal
                                { hour = 0
                                , minute = 0
                                , second = 0
                                , millisecond = 1
                                }
                            )
                        )
        , test "hour is much too large" <|
            \() ->
                "100:00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.ExpectedDigit
                            ]
                        )
        , test "hour is too large" <|
            \() ->
                "24:00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.InvalidHour
                            ]
                        )
        , test "minute is too large" <|
            \() ->
                "00:60:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.InvalidMinute
                            ]
                        )
        , test "second is too large" <|
            \() ->
                "00:00:60"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.InvalidSecond
                            ]
                        )
        ]


localDates : Test
localDates =
    describe "local dates"
        [ test "the beginning of 'time'" <|
            \() ->
                "1970-01-01"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateLocal (Date.fromCalendarDate 1970 Time.Jan 1))
                        )
        , test "leap  day" <|
            \() ->
                "1988-02-29"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateLocal (Date.fromCalendarDate 1988 Time.Feb 29))
                        )
        , test "bad leap day" <|
            \() ->
                "1989-02-29"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.DayTooLarge 28
                            ]
                        )
        , test "bad month" <|
            \() ->
                "1989-13-29"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.InvalidMonth
                            ]
                        )
        , test "bad day" <|
            \() ->
                "1989-04-50"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.InvalidDay
                            ]
                        )
        ]


localDateTimes : Test
localDateTimes =
    describe "local date times"
        [ test "the beginning of 'time'" <|
            \() ->
                "1970-01-01T00:00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateTimeLocal
                                { year = 1970
                                , month = Time.Jan
                                , day = 1
                                , hour = 0
                                , minute = 0
                                , second = 0
                                , millisecond = 0
                                }
                            )
                        )
        , test "lower 't' seprator" <|
            \() ->
                "1970-01-01t00:00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateTimeLocal
                                { year = 1970
                                , month = Time.Jan
                                , day = 1
                                , hour = 0
                                , minute = 0
                                , second = 0
                                , millisecond = 0
                                }
                            )
                        )
        , test "space separator" <|
            \() ->
                "1970-01-01 00:00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateTimeLocal
                                { year = 1970
                                , month = Time.Jan
                                , day = 1
                                , hour = 0
                                , minute = 0
                                , second = 0
                                , millisecond = 0
                                }
                            )
                        )
        ]


offsetDateTimes : Test
offsetDateTimes =
    describe "offset date times"
        [ test "the beginning of 'time'" <|
            \() ->
                "1970-01-01T00:00:00+00:00"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateTimeOffset
                                { instant = Time.millisToPosix 0
                                , offset = { hour = 0, minute = 0 }
                                }
                            )
                        )
        , test "also the beginning" <|
            \() ->
                "1970-01-01T00:00:00Z"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateTimeOffset
                                { instant = Time.millisToPosix 0
                                , offset = { hour = 0, minute = 0 }
                                }
                            )
                        )
        , test "negative offset" <|
            \() ->
                "1970-01-01T00:00:00-12:34"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Ok
                            (Rfc3339.DateTimeOffset
                                { instant = Time.millisToPosix ((12 * 60 + 34) * 60 * 1000)
                                , offset = { hour = -12, minute = 34 }
                                }
                            )
                        )
        , test "bad offset" <|
            \() ->
                "1970-01-01T00:00:00-24:34"
                    |> Rfc3339.parse
                    |> Expect.equal
                        (Err
                            [ Rfc3339.InvalidHour
                            ]
                        )
        ]


printing : Test
printing =
    describe "can print a valid date time"
        [ test "local time zero" <|
            \() ->
                "00:00:00"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "00:00:00")
        , test "local time right after midnight" <|
            \() ->
                "00:00:00.001"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "00:00:00.001")
        , test "local date" <|
            \() ->
                "1970-01-01"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "1970-01-01")
        , test "local date time" <|
            \() ->
                "1970-01-01 00:00:00"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "1970-01-01T00:00:00")
        , test "the beginning of 'time'" <|
            \() ->
                "1970-01-01T00:00:00+00:00"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "1970-01-01T00:00:00Z")
        , test "date, time, and offset" <|
            \() ->
                "1970-01-01T00:00:00-01:23"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "1970-01-01T00:00:00-01:23")
        ]


rounding : Test
rounding =
    describe "can round correctly"
        [ test "right after midnight" <|
            \() ->
                "00:00:00.00001"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "00:00:00")
        , test "right before midnight" <|
            \() ->
                "23:59:59.9996"
                    |> Rfc3339.parse
                    |> Result.map Rfc3339.toString
                    |> Expect.equal (Ok "23:59:59.999")
        ]
