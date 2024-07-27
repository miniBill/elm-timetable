module Id exposing
    ( AgencyId
    , BlockId
    , FeedId
    , Id
    , LevelId
    , LocationGroupId
    , LocationId
    , NetworkId
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


type AgencyId
    = AgencyId


type NetworkId
    = NetworkId


type ServiceId
    = ServiceId


type FeedId
    = FeedId


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
