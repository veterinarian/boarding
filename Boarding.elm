module Boarding where

import Html exposing (..)
import Html.Attributes exposing (class, type', placeholder, value)
import Html.Events exposing (onClick)
import Date exposing (Date, fromString, day)
import Date.Duration exposing (Duration(Day))
import Date.Compare exposing (is, Compare2(SameOrAfter, SameOrBefore))
import Date.Format

type alias Pet = { name: String, client: String }

type Action = NoBookingAction | Add Booking | Cancel Booking | CheckIn Booking | CheckOut Booking
              | Entry { pet: Pet, from: String, to: String }

update: Action -> Model -> Model
update action model = 
  case action of 
    NoBookingAction -> model
    Add booking -> 
      if (isOverlapped booking model.bookings)
      then model
      else { model | bookings = booking::model.bookings }
    Entry e -> { model | entry = e }
    otherwise -> model

isOverlapped booking bookings =
  let ov bk1 bk2 =
    bk1.pet == bk2.pet
    && (is SameOrAfter bk1.to bk2.from || is SameOrBefore bk1.from bk2.to )
  in not (List.isEmpty (List.filter (ov booking) bookings))
  
type Status = Reserved | CheckedIn Date | CheckedOut Date | Cancelled Date

type alias Booking = 
  { pet: Pet
  , from: Date
  , to: Date
  , status: Status
  }

-- number of runs followed by a list of bookings and their occupancy
type alias Model = { runs: Int
                   , bookings: List Booking
                   , boundsFrom: Maybe Date
                   , boundsTo: Maybe Date
                   , entry: { pet: Pet, from: String, to: String }
                   }

model : Model
model = { runs = 5
        , bookings = [ ]
        , boundsFrom = Nothing
        , boundsTo = Nothing
        , entry = { pet = { name = "", client = "" }, from = "", to = "" }
        }

dayString resdate =
  case resdate of
    Ok date -> toString (Date.day date)
    Err date -> "-"

-- avail x n = td [ class (if x.filled >= n then "occupied" else "available") ] []

--rooms : Int -> List Booking -> List Html 
--rooms n dates =
--  let r n dates = 
--        case n of
--          0 -> []
--          _ -> (tr [ class "row" ] (List.map (\x -> avail x n) dates)) :: (r (n-1) dates)
--  in List.reverse (r n dates)

printBookings : List Booking -> List Html
printBookings = 
  List.map 
    (\bk -> div [] 
      [ text (bk.pet.client 
          ++ ", " ++ bk.pet.name ++ ":"
          ++ Date.Format.isoString bk.from 
          ++ " -> " ++ Date.Format.isoString bk.to) 
      ])
    
view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    (List.append (printBookings model.bookings) 
      [ form []
        [ input [ placeholder "Pet name", value model.entry.pet.name ] []
        , input [ placeholder "Client name", value model.entry.pet.client ] [] 
        , input [ placeholder "From", value model.entry.from ] [] 
        , input [ placeholder "To", value model.entry.from ] []
        , input [ type' "button" ] []
        ]
      ])
