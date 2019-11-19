module Engine exposing (..)

import Entity

type alias State =
  { businesses = List Entity
  , households = List Entity
  }