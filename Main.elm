module Main where

import Boarding exposing (Action)
import StartApp.Simple exposing (start)

import Html exposing (Html, div, h1, text)

main =
  start
    { model = model
    , update = update
    , view = view
    }

type Action = NoAction | Board Boarding.Action

type alias Model = 
  { boarding : Boarding.Model
  }
  
model = { boarding = Boarding.model }

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [ h1 [] [ text "Boarding" ]
         , Boarding.view (Signal.forwardTo address Board) model.boarding
         ]

update : Action -> Model -> Model
update action model = 
  case action of
    NoAction -> model
    Board act -> { model | boarding = Boarding.update act model.boarding}
