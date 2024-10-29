-- |
-- Docs on the format:
-- https://cloud.yandex.ru/en/docs/serverless-containers/concepts/logs
module YcLogger.Models.Domain where

import BasePrelude hiding (bool, null)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

data Record = Record
  { streamName :: Text,
    level :: Level,
    message :: Text,
    payload :: [(Text, Aeson.Value)]
  }
  deriving (Show, Read, Eq, Ord)

data Level
  = TraceLevel
  | DebugLevel
  | InfoLevel
  | WarnLevel
  | ErrorLevel
  | FatalLevel
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance IsLabel "trace" Level where fromLabel = TraceLevel

instance IsLabel "debug" Level where fromLabel = DebugLevel

instance IsLabel "info" Level where fromLabel = InfoLevel

instance IsLabel "warn" Level where fromLabel = WarnLevel

instance IsLabel "error" Level where fromLabel = ErrorLevel

instance IsLabel "fatal" Level where fromLabel = FatalLevel
