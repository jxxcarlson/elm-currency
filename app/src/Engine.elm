module Engine exposing (State)

import Entity exposing(Entity)

type alias State =
  { businesses : List Entity
  , households : List Entity
  }