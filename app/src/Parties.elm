module Parties exposing (..)

import Entity exposing(Entity, Item)
import Money exposing(Money, BankTime)
import Account
import Value


type Parties = Parties Entity Entity
type TransactionStatus = TSSuccess | TSFailure String


buyer : Parties -> Entity
buyer (Parties buyer_ _) =
    buyer_

seller : Parties -> Entity
seller (Parties _ seller_) =
    seller_

--sufficientFunds : Money -> Entity -> Bool
--sufficientFunds money entity =
--    case

--
--
--buy : BankTime -> Money -> Item -> Parties -> (TransactionStatus, Parties)
--buy bt money item parties =
--   let
--     b = buyer parties
--     s = seller parties
--     maybeBuyerAccount = Entity.selectAccount money b
--     maybeSellerAccount = Entity.selectAccount money s
--   in
--   case (maybeBuyerAccount, maybeSellerAccount) of
--       (Just buyerAccount, Just sellerAccount ) ->
--           let
--               conditions =
--                   [Value.gte (Account.value bt buyerAccount) (Money.value bt money)
--                   , Money.value bt money == item.price ]
--           in
--
--


