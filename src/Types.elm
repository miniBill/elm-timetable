module Types exposing (Station, Timetable)

import Time


type alias Timetable =
    List
        { from : Station
        , to : Station
        , links : List { from : Time.Posix, to : Time.Posix }
        }


type alias Station =
    String
