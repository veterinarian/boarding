module Boarding where

import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Date exposing (Date, fromString, day)
import Time
import Date.Duration exposing (Duration(Day))
import Maybe exposing (Maybe(Just, Nothing))
import Date.Compare exposing (is, Compare2(After), is3, Compare3(BetweenOpen))
import List.Extra exposing (unfoldr)

main =
  start
    { model = model
    , update = update
    , view = view
    }

type Action = NoAction
            | AddBooking Booking
            | DeleteBooking Booking

update _ model = model

type alias Booking = { name: String, fromDate: Date, toDate: Date }

-- number of runs followed by a list of dates and their occupancy
type alias Model = { runs: Int
                   , bookings: List Booking
                   , from: Maybe Date
                   , to: Maybe Date
                   , cal: List Date
                   }

testBookings : List Booking
testBookings =
  [ {name = "A", fromDate = (dateFromString "10/11/2016"),
     toDate = (dateFromString "10/14/2016")}
  , {name = "B", fromDate = (dateFromString "10/04/2016"),
     toDate = (dateFromString "10/13/2016")}
  , {name = "C", fromDate = (dateFromString "10/12/2016"),
     toDate = (dateFromString "10/19/2016")}
  ]

model : Model
model = { runs = 5
        , bookings = testBookings
        , from = Nothing
        , to = Nothing
        , cal = makeCalFromStrings "10/10/2016" "10/16/2016"
        }

dateFromString : String -> Date
dateFromString date =
  case Date.fromString date of
    Ok d -> d
    Err _ -> Date.fromTime 0

incr : Date -> Date
incr d = Date.fromTime (Date.toTime d + (24 * Time.hour))

makeCalFromStrings : String -> String -> List Date
makeCalFromStrings fs ts =
  makeCalFromDates (dateFromString fs) (dateFromString ts)


makeCalFromDates : Date -> Date -> List Date
makeCalFromDates dateFrom dateTo =
  unfoldr (\d -> if (is After d dateTo) then Nothing else Just (d, incr d))
          dateFrom

dayString date = toString (Date.day date)

avail : Int -> (Date,Int) -> Html
avail n (_,numOccupied)  =
  td [ if n > numOccupied then class "available" else class "occupied" ] []

rooms : Int -> List (Date,Int) -> List Html
rooms numRooms occupancy =
  let r n occupancy =
        case n of
          0 -> []
          _ -> (tr [ class "row" ] (List.map (\occ -> avail n occ) occupancy))
               ::(r (n-1) occupancy)
  in List.reverse (r numRooms occupancy)

occupancyOf model =
  let occupancyOn date =
    ( date
    , List.length (List.filter (\bk -> is3 BetweenOpen date bk.fromDate bk.toDate)
                     model.bookings)
    )
  in List.map occupancyOn model.cal

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ table [ class "cal" ]
        ((tr [ class "hdr" ]
               (List.map
                      (\date ->
                         td [] [ text (dayString date) ])
                      model.cal))
        :: rooms model.runs (occupancyOf model))
    ]

