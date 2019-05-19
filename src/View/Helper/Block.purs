-- https://api.slack.com/reference/messaging/blocks
module View.Helper.Block
  ( Block
  , actionsBlock
  , sectionBlock
  ) where

import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Simple.JSON (class WriteForeign)
import Simple.JSON as SimpleJSON
import View.Helper.BlockElement (BlockElement)
import View.Helper.TextObject (TextObject)

-- https://api.slack.com/reference/messaging/blocks#section
type SectionBlockJSON =
  { type :: String
  , text :: TextObject
  , block_id :: Maybe String
  , fields :: Maybe (Array TextObject)
  , accessory :: Maybe BlockElement
  }

-- https://api.slack.com/reference/messaging/blocks#actions
type ActionsBlockJSON =
  { type :: String
  , elements :: Array BlockElement
  , block_id :: Maybe String
  }

data Block
  = SectionBlock SectionBlockJSON
  -- TODO: | Divider
  -- TODO: | Image
  | ActionsBlock ActionsBlockJSON
  -- TODO: | Context

instance writeForeignBlock :: WriteForeign Block where
  writeImpl = case _ of
    (SectionBlock r) -> SimpleJSON.writeImpl r
    (ActionsBlock r) -> SimpleJSON.writeImpl r

actionsBlock :: { elements :: Array BlockElement } -> Block
actionsBlock { elements } = ActionsBlock
  -- TODO: length elements <= 5
  -- TODO: length block_id <= 255
  { type: "actions"
  , elements
  , block_id: Maybe.Nothing -- TODO: not supported
  }

sectionBlock :: { text :: TextObject } -> Block
sectionBlock { text } = SectionBlock
  -- TODO: length text <= 3000
  -- TODO: length block_id <= 255
  -- TODO: length fields <= 10
  -- TODO: length fields[i].text <= 2000
  { type: "section"
  , text
  , block_id: Maybe.Nothing -- TODO: not supported
  , fields: Maybe.Nothing -- TODO: not supported
  , accessory: Maybe.Nothing -- TODO: not supported
  }
