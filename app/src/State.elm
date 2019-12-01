module State exposing (State)

import Entity exposing(Entity)
import Random

type alias State =
  { businesses : List Entity
  , households : List Entity
  , seed : Random.Seed
  , randInt : Int
  }

