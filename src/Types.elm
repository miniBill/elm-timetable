module Types exposing (Event(..), Model, Msg(..), Station, Timetable, ViewMode(..))

import Time


type alias Model =
    { timetable : Timetable
    , mode : ViewMode
    }


type ViewMode
    = ViewSimple


type alias Timetable =
    List
        { from : Station
        , to : Station
        , links : List { from : Time.Posix, to : Time.Posix }
        }


type alias Station =
    String


type Event
    = Arrival
    | Departure


type Msg
    = ViewMode ViewMode
