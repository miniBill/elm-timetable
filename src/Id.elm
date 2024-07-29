module Id exposing
    ( AgencyId
    , AreaId
    , BlockId
    , BookingRuleId
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


type AgencyId
    = AgencyId


type AreaId
    = AreaId


type BookingRuleId
    = BookingRuleId


type BlockId
    = BlockId


type FeedId
    = FeedId


type LevelId
    = LevelId


type LocationGroupId
    = LocationGroupId


type LocationId
    = LocationId


type NetworkId
    = NetworkId


type PathwayId
    = PathwayId


type RouteId
    = RouteId


type ServiceId
    = ServiceId


type ShapeId
    = ShapeId


type StopId
    = StopId


type TripId
    = TripId


type ZoneId
    = ZoneId


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
