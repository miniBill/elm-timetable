module Data exposing (feeds, fromStringUnsafe, munchenToZoetermeer, villachToUdine)

import Iso8601
import Time
import Types exposing (Timetable)


feeds : List String
feeds =
    [ "de"
    ]


villachToUdine : Timetable
villachToUdine =
    [ { from = "Villach Hbf"
      , to = "Udine"
      , links =
            [ { from = fromStringUnsafe "16:49"
              , to = fromStringUnsafe "18:16"
              }
            , { from = fromStringUnsafe "19:29"
              , to = fromStringUnsafe "21:13"
              }
            ]
      }
    , { from = "München Hbf"
      , to = "Villach Hbf"
      , links =
            [ { from = fromStringUnsafe "12:17"
              , to = fromStringUnsafe "16:44"
              }
            , { from = fromStringUnsafe "14:17"
              , to = fromStringUnsafe "18:44"
              }
            ]
      }
    ]


munchenToZoetermeer : Timetable
munchenToZoetermeer =
    [ { from = "München Hbf"
      , to = "Hannover Hbf"
      , links =
            [ { from = fromStringUnsafe "03:56"
              , to = fromStringUnsafe "08:32"
              }
            , { from = fromStringUnsafe "05:09"
              , to = fromStringUnsafe "09:33"
              }
            , { from = fromStringUnsafe "05:55"
              , to = fromStringUnsafe "10:32"
              }
            , { from = fromStringUnsafe "07:09"
              , to = fromStringUnsafe "11:33"
              }
            , { from = fromStringUnsafe "07:52"
              , to = fromStringUnsafe "12:32"
              }
            , { from = fromStringUnsafe "09:09"
              , to = fromStringUnsafe "13:33"
              }
            , { from = fromStringUnsafe "09:52"
              , to = fromStringUnsafe "14:32"
              }
            , { from = fromStringUnsafe "11:09"
              , to = fromStringUnsafe "15:33"
              }
            , { from = fromStringUnsafe "11:52"
              , to = fromStringUnsafe "16:33"
              }
            , { from = fromStringUnsafe "13:09"
              , to = fromStringUnsafe "17:33"
              }
            , { from = fromStringUnsafe "13:52"
              , to = fromStringUnsafe "18:33"
              }
            , { from = fromStringUnsafe "15:12"
              , to = fromStringUnsafe "19:33"
              }
            , { from = fromStringUnsafe "17:09"
              , to = fromStringUnsafe "21:33"
              }
            , { from = fromStringUnsafe "17:52"
              , to = fromStringUnsafe "22:31"
              }
            ]
      }
    , { from = "München Hbf"
      , to = "Halle(Saale)Hbf"
      , links =
            [ { from = fromStringUnsafe "05:48"
              , to = fromStringUnsafe "08:38"
              }
            , { from = fromStringUnsafe "06:52"
              , to = fromStringUnsafe "09:45"
              }
            , { from = fromStringUnsafe "07:13"
              , to = fromStringUnsafe "11:04"
              }
            , { from = fromStringUnsafe "08:52"
              , to = fromStringUnsafe "11:45"
              }
            , { from = fromStringUnsafe "09:55"
              , to = fromStringUnsafe "13:04"
              }
            , { from = fromStringUnsafe "10:52"
              , to = fromStringUnsafe "13:45"
              }
            , { from = fromStringUnsafe "11:14"
              , to = fromStringUnsafe "15:04"
              }
            , { from = fromStringUnsafe "12:52"
              , to = fromStringUnsafe "15:45"
              }
            , { from = fromStringUnsafe "13:54"
              , to = fromStringUnsafe "17:04"
              }
            , { from = fromStringUnsafe "14:52"
              , to = fromStringUnsafe "17:45"
              }
            , { from = fromStringUnsafe "15:14"
              , to = fromStringUnsafe "19:04"
              }
            , { from = fromStringUnsafe "16:50"
              , to = fromStringUnsafe "19:45"
              }
            , { from = fromStringUnsafe "17:35"
              , to = fromStringUnsafe "20:38"
              }
            , { from = fromStringUnsafe "18:52"
              , to = fromStringUnsafe "21:45"
              }
            , { from = fromStringUnsafe "19:55"
              , to = fromStringUnsafe "22:38"
              }
            ]
      }
    , { from = "Halle(Saale)Hbf"
      , to = "Hannover Hbf"
      , links =
            []
      }
    , { from = "München Hbf"
      , to = "Nürnberg Hbf"
      , links =
            []
      }
    , { from = "Nürnberg Hbf"
      , to = "Halle(Saale)Hbf"
      , links =
            []
      }
    ]


fromStringUnsafe : String -> Time.Posix
fromStringUnsafe hourAndMinutes =
    case Iso8601.toTime <| "2024-07-09T" ++ hourAndMinutes ++ ":00" of
        Ok t ->
            t

        Err _ ->
            Debug.todo <| "Failed to parse " ++ hourAndMinutes
