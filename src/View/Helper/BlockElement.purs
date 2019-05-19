-- https://api.slack.com/reference/messaging/block-elements
module View.Helper.BlockElement
  ( BlockElement
  , actionButton
  , linkButton
  ) where

import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Simple.JSON (class WriteForeign)
import Simple.JSON as SimpleJSON
import View.Helper.TextObject (TextObject)
import View.Helper.TextObject as TextObject

data BlockElement
  -- TODO: = Image
  = Button ButtonJSON
  -- TODO: | Select Menus
  -- TODO: | Overflow Menu
  -- TODO: | Date Picker

instance writeForeignBlockElement :: WriteForeign BlockElement where
  writeImpl = case _ of
    (Button r) -> SimpleJSON.writeImpl r

-- https://api.slack.com/reference/messaging/block-elements#button
type ButtonJSON =
  { type :: String -- "button"
  , text :: TextObject -- Text Object -- plain_text && max length 75
  , action_id :: String -- max length 255
  , url :: Maybe String -- max length 3000
  , value :: Maybe String -- payload max length 2000
  , style :: Maybe String -- "primary" or "danger"
  -- , confirm: Maybe Object -- Not supported
  }

actionButton ::
  { text :: String
  , action_id :: String
  , value :: Maybe String
  , style :: Maybe String
  }
  -> BlockElement
actionButton { text, action_id, value, style } = Button
  -- TODO: CodeUnits.length text <= 75
  -- TODO: CodeUnits.length action_id <= 255
  -- TODO: CodeUnits.length value <= 2000
  -- TODO: style == Just "primary" or Just "danger" or Nothing
  { type: "button"
  , text: TextObject.plainText { text }
  , action_id
  , url: Maybe.Nothing
  , value
  , style
  }

linkButton ::
  { text :: String
  , action_id :: String
  , url :: String
  , style :: Maybe String
  }
  -> BlockElement
linkButton { text, action_id, url, style } = Button
  -- TODO: CodeUnits.length text <= 75
  -- TODO: CodeUnits.length action_id <= 255
  -- TODO: CodeUnits.length value <= 2000
  -- TODO: style == Just "primary" or Just "danger" or Nothing
  { type: "button"
  , text: TextObject.plainText { text }
  , action_id
  , url: Maybe.Just url
  , value: Maybe.Nothing
  , style
  }
