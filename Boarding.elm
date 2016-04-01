module Boarding where

import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Date exposing (Date, fromString, day)
import Date.Duration exposing (Duration(Day))

main =
  start
    { model = model
    , update = update
    , view = view
    }

type Action = Inc | Dec
update _ model = model

type alias Booking = { date: Result String Date, filled: Int }

-- number of runs followed by a list of dates and their occupancy
type alias Model = { runs: Int
                   , dates: List Booking
                   }

model : Model
model = { runs = 5
        , dates = [ { date = Date.fromString("2016-03-31"), filled = 2 }
                  , { date = Date.fromString("2016-04-01"), filled = 3 }
                  , { date = Date.fromString("2016-04-02"), filled = 3 }
                  , { date = Date.fromString("2016-04-03"), filled = 2 }
                  , { date = Date.fromString("2016-04-04"), filled = 0 }
                  , { date = Date.fromString("2016-04-05"), filled = 1 }
                  ]
        }

dayString resdate =
  case resdate of
    Ok date -> toString (Date.day date)
    Err date -> "-"

avail x n = td [ class (if x.filled >= n then "occupied" else "available") ] []

rooms : Int -> List Booking -> List Html 
rooms n dates =
  let r n dates = 
        case n of
          0 -> []
          _ -> (tr [ class "row" ] (List.map (\x -> avail x n) dates)) :: (r (n-1) dates)
  in List.reverse (r n dates)

view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    [ table [ class "cal" ]
        ((tr [ class "hdr" ]
               (List.map
                      (\booking ->
                         td [] [ text (dayString booking.date) ])
                      model.dates))
        :: rooms model.runs model.dates)
    --button [ onClick address Dec ] [ text "-" ]      
    ]
  
