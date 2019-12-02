module Parties exposing (..)

import Entity exposing(Entity, Item)
import Money exposing(Money, BankTime)
import Account
import Value
import Internal.Utility as Utility


type Parties = Parties Entity Entity
type TransactionStatus = TSuccess | TSFailure String


buyer : Parties -> Entity
buyer (Parties buyer_ _) =
    buyer_

seller : Parties -> Entity
seller (Parties _ seller_) =
    seller_

--
--buy : BankTime -> Money -> Item -> Parties -> (TransactionStatus, Parties)
--buy bt money item parties =
--   let
--     b = buyer parties
--     s = seller parties
--     maybeBuyerAccount = Entity.selectAccount money b
--     maybeSellerAccount = Entity.selectAccount money s
--     buyerInventory = Entity.inventory b
--     sellerInventory = Entity.inventory s
--   in
--   case (maybeBuyerAccount, maybeSellerAccount) of
--       (Just buyerAccount, Just sellerAccount ) ->
--           let
--               conditions =
--                   [Value.gte (Account.value bt buyerAccount) (Money.value bt money)
--                   , Money.value bt money == Value.mul item.quantity item.price
--                   , ]
--           in
--               case Utility.andOfList conditions of
--                   False -> (TSFailure "Insufficient funds or something else")
--                   True ->
--                     let
--                       updatedSellerAccount = Account.credit bt money sellerAccount
--                       updatedBuyerAccount = Account.debit bt money buyerAccount
--
--                     in
--


