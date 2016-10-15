module Boarding where

import StartApp exposing (start)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Date exposing (Date, fromString, day)
import Time
import Date.Duration exposing (Duration(Day))
import Maybe exposing (Maybe(Just, Nothing))
import Date.Compare exposing (is, Compare2(After), is3, Compare3(BetweenOpen))
import List.Extra exposing (unfoldr)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import TaskTutorial exposing (getCurrentTime)

main = app.html
       
app =
  start
    { init = (model, resetDate)
    , update = update
    , view = view
    , inputs = []
    }

resetDate = Effects.task
            <| Task.map (\d -> SetCal (Date.fromTime d))
            <| getCurrentTime
            
port tasks : Signal (Task Never ())
port tasks = app.tasks

type Action = NoAction
            | AddBooking Booking
            | DeleteBooking Booking
            | SetCal Date
            | ResetDate
              
update action model =
  case action of
    SetCal date ->
      ( { model |
            cal = makeCalFromDates
              (Date.Duration.add Day -3 date)
              (Date.Duration.add Day 27 date)
        , curDate = date
        }
      , Effects.none
      )
    ResetDate -> (model, resetDate)
    _ -> (model, Effects.none)

type alias Booking = { name: String, fromDate: Date, toDate: Date }

-- number of runs followed by a list of dates and their occupancy
type alias Model = { runs: Int
                   , bookings: List Booking
                   , from: Maybe Date
                   , to: Maybe Date
                   , cal: List Date
                   , curDate: Date
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
        , cal = []
        , curDate = Date.fromTime 0
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
    , div []
        [ button [ onClick address <| SetCal (Date.Duration.add Day -20 model.curDate) ] [ text "<" ]
        , button [ onClick address ResetDate ] [ text "Reset to Current Date" ]
        , button [ onClick address <| SetCal (Date.Duration.add Day 20 model.curDate) ] [ text ">" ]
        ]
    ]

