module Utility exposing (applyToList, iterate)

{-|

    > applyToList (\a b -> a + b) [1,2,3] 0
    --> Just 6
-}
applyToList : (a -> b -> b) -> List a -> b -> Maybe b
applyToList f list b0 =
   let
      step : (List a, Maybe b) -> (List a, Maybe b)
      step  (list_, b_) =
         let
            b__ = Maybe.map2 f (List.head list_) b_
         in
            (List.drop 1 list_, b__)
   in
      iterate (List.length list) step (list, (Just b0))
        |> Tuple.second


{-|
    > iterate 3 (\x -> 2*x) 1
    8 : number
-}
iterate : Int -> (a -> a) -> a -> a
iterate n f x0 =
    if n == 0 then
      x0
    else
       iterate (n - 1) f  (f x0)