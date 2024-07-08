module Data exposing
    ( feeds
    , fromStringUnsafe
      -- , villachToUdine
    )

import Iso8601
import Time
import Types exposing (Timetable)


feeds : List String
feeds =
    [ --  "de" ,
      "oebb-2024"
    ]



-- villachToUdine : Timetable
-- villachToUdine =
--     [ { from = "Villach Hbf"
--       , to = "Udine"
--       , links =
--             [ { from = fromStringUnsafe "16:49"
--               , to = fromStringUnsafe "18:16"
--               }
--             , { from = fromStringUnsafe "19:29"
--               , to = fromStringUnsafe "21:13"
--               }
--             ]
--       }
--     , { from = "München Hbf"
--       , to = "Villach Hbf"
--       , links =
--             [ { from = fromStringUnsafe "12:17"
--               , to = fromStringUnsafe "16:44"
--               }
--             , { from = fromStringUnsafe "14:17"
--               , to = fromStringUnsafe "18:44"
--               }
--             ]
--       }
--     ]


fromStringUnsafe : String -> Time.Posix
fromStringUnsafe hourAndMinutes =
    case Iso8601.toTime <| "2024-07-09T" ++ hourAndMinutes ++ ":00" of
        Ok t ->
            t

        Err _ ->
            Debug.todo <| "Failed to parse " ++ hourAndMinutes
