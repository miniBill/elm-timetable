module Id exposing
    ( BlockId
    , Id
    , LevelId
    , LocationGroupId
    , LocationId
    , PathwayId
    , RouteId
    , ServiceId
    , ShapeId
    , StopId
    , TripId
    , ZoneId
    , compare
    , fromString
    , toString
    )


type ZoneId
    = ZoneId


type LevelId
    = LevelId


type PathwayId
    = PathwayId


type LocationId
    = LocationId


type LocationGroupId
    = LocationGroupId


type TripId
    = TripId


type StopId
    = StopId


type BlockId
    = BlockId


type ShapeId
    = ShapeId


type RouteId
    = RouteId


type ServiceId
    = ServiceId


type Id kind
    = Id String


toString : Id kind -> String
toString (Id id) =
    id


fromString : String -> Id kind
fromString id =
    Id id


compare : Id kind -> Id kind -> Order
compare (Id l) (Id r) =
    Basics.compare l r
