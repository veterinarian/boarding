module Boarding where

import Html exposing (..)
import Html.Attributes exposing (class, type', placeholder, value, disabled)
import Html.Events exposing (onClick, on, targetValue)
import Date exposing (Date, fromString, day)
import Date.Duration exposing (Duration(Day))
import Date.Compare exposing (is3, Compare3(Between, BetweenOpenStart
                                           , BetweenOpenEnd, BetweenOpen))
import Date.Format
import Result.Extra exposing (isOk, isErr)

type alias Pet = { name: String, client: String }

type Field = PetName | ClientName | From | To

type Action = NoBookingAction | Add Booking | Cancel Booking | CheckIn Booking | CheckOut Booking
              | Entry Field String

update : Action -> Model -> Model
update action model = 
  case action of 
    NoBookingAction -> model
    Add booking -> 
      if (isOverlapped booking model.bookings)
      then model
      else { model | bookings = booking::model.bookings }
    Entry fld str ->
      let 
        e = model.entry
        p = model.entry.pet
      in case fld of
        PetName -> { model | entry = { e | pet = { p | name = str } } }
        ClientName -> { model | entry = { e | pet = { p | client = str } } }
        From -> { model | entry = { e | from = str } }
        To -> { model | entry = { e | to = str } }
    Cancel booking ->
      let repl b =
        if booking == b then { booking | status = Cancelled } else b
      in { model | bookings = (List.map repl model.bookings) }
    otherwise -> model

isCancelled booking =
  case booking.status of
    Cancelled -> True
    otherwise -> False

hasOverlappedDates bk1 bk2 =
  is3 BetweenOpen bk1.to bk2.to bk2.from 
  || is3 BetweenOpen bk1.from bk2.to bk1.from 

isOverlapped booking bookings =
  let ov bk1 bk2 =
    not (isCancelled bk1) 
    && not (isCancelled bk2) -- if one is cancelled, they do not overlap.
    && bk1.pet == bk2.pet 
    && hasOverlappedDates bk1 bk2
  in not (List.isEmpty (List.filter (ov booking) bookings))
  
type Status = Reserved | CheckedIn Date | CheckedOut Date | Cancelled --Date

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

printBookings : Signal.Address Action -> List Booking -> List Html
printBookings address =
  List.map 
    (\bk -> div [] 
      [ text (bk.pet.client 
          ++ ", " ++ bk.pet.name ++ ":"
          ++ Date.Format.isoString bk.from 
          ++ " -> " ++ Date.Format.isoString bk.to)
      , button 
          [ onClick address (Cancel bk)
          ]
          [ text "Cancel" ]
      ])

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

inputField : Signal.Address a -> String -> String -> (String -> a) -> Html 
inputField address p v contentToValue =
  input [ placeholder p, value v, onInput address contentToValue ] []

resMsg r =
  case r of
    Ok _ -> ""
    Err msg -> msg

view : Signal.Address Action -> Model -> Html
view address model = 
  let 
    petOk = model.entry.pet.name /= ""
    clientOk = model.entry.pet.name /= ""
    fromRes = Date.fromString model.entry.from
    toRes = Date.fromString model.entry.to
    isValid = (petOk && clientOk && isOk fromRes && isOk toRes)
    booking = 
      if not isValid 
      then Nothing 
      else 
        case fromRes of
          Err _ -> Nothing
          Ok from -> case toRes of
            Err _ -> Nothing
            Ok to -> 
              Just { pet = { name = model.entry.pet.name
                           , client = model.entry.pet.client }
                   , status = Reserved
                   , from = from
                   , to = to
                   }
  in
    div []
      (List.append (printBookings address model.bookings) 
        [ form []
          [ inputField address "Pet name" model.entry.pet.name (Entry PetName)
          , br [] []
          , inputField address "Client" model.entry.pet.client (Entry ClientName)
          , br [] []
          , inputField address "From" model.entry.from (Entry From)
          , text (resMsg fromRes)
          , br [] []
          , inputField address "To" model.entry.to (Entry To)
          , text (resMsg toRes)
          , br [] []
          , input [ type' "button"
                  , onClick address 
                     (case booking of 
                        Nothing -> NoBookingAction
                        Just b -> Add b)
                  , disabled (not isValid) 
                  , value "Add"
                  ] [text "Add"]
          ]
        , text model.entry.pet.name
        ])
