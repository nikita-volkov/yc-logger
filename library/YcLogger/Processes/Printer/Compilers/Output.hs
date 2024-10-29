-- |
-- Docs on the format:
-- https://cloud.yandex.ru/en/docs/serverless-containers/concepts/logs
module YcLogger.Processes.Printer.Compilers.Output where

import BasePrelude hiding (bool, null)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Jsonifier
import PtrPoker.Write qualified as Write
import YcLogger.Models.Domain

compile :: NonEmpty Record -> ByteString
compile =
  Write.writeToByteString . outputWrite
  where
    outputWrite :: NonEmpty Record -> Write.Write
    outputWrite = foldMap recordLineWrite

    recordLineWrite :: Record -> Write.Write
    recordLineWrite = flip mappend newlineWrite . toWrite . record

    newlineWrite :: Write.Write
    newlineWrite = Write.word8 (fromIntegral (ord '\n'))

record :: Record -> Json
record x =
  x.payload
    & (fmap . second) aesonValue
    & mappend
      [ ("message/msg", textString x.message),
        ("stream_name", textString x.streamName),
        ("level", level x.level)
      ]
    & object

level :: Level -> Json
level =
  textString . \case
    TraceLevel -> "trace"
    DebugLevel -> "debug"
    InfoLevel -> "info"
    WarnLevel -> "warn"
    ErrorLevel -> "error"
    FatalLevel -> "fatal"

aesonValue :: Aeson.Value -> Json
aesonValue = \case
  Aeson.Null -> null
  Aeson.Bool x -> bool x
  Aeson.String x -> textString x
  Aeson.Number x -> scientificNumber x
  Aeson.Array x ->
    x
      & fmap aesonValue
      & array
  Aeson.Object x ->
    x
      & Aeson.KeyMap.toList
      & fmap (bimap Aeson.Key.toText aesonValue)
      & object
