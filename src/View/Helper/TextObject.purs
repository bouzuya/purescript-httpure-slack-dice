module View.Helper.TextObject
  ( TextObject
  , plainText
  ) where

import Data.Maybe (Maybe)
import Data.Maybe as Maybe

-- https://api.slack.com/reference/messaging/composition-objects#text
type TextObject =
  { type :: String -- "plain_text" or "mrkdwn"
  , text :: String -- Object is not supported
  , emoji :: Maybe Boolean -- plain_text only
  , verbatim :: Maybe Boolean -- mrkdwn only
  }

plainText :: { text :: String } -> TextObject
plainText { text } =
  { type: "plain_text"
  , text
  , emoji: Maybe.Nothing -- TODO: not supported
  , verbatim: Maybe.Nothing -- TODO: not supported
  }
