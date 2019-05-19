module Action
  ( Action(..)
  , execute
  ) where

import Prelude

import Action.Dice as ActionDice
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import HTTPure (ResponseM)

data Action
  = Dice (Maybe String) String

derive instance eqAction :: Eq Action
derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow

execute :: Action -> ResponseM
execute =
  case _ of
    Dice responseUrl notation ->
      ActionDice.execute responseUrl notation
