module YcLogger
  ( Service,

    -- * Management
    start,
    stop,

    -- * Usage
    log,

    -- * Modification
    setMinLevel,
    raiseMinLevel,
    nestStreamName,

    -- * Domain
    Level (..),
  )
where

import BasePrelude hiding (log)
import Control.Concurrent.STM
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import YcLogger.Models.Domain
import YcLogger.Processes.Printer qualified as Printer

data Service = Service
  { onlineVar :: TVar Bool,
    queue :: TBQueue Record,
    minLevel :: Level,
    streamName :: Text
  }

setMinLevel :: Level -> Service -> Service
setMinLevel level service =
  service {minLevel = level}

raiseMinLevel :: Service -> Service
raiseMinLevel service =
  if service.minLevel == maxBound
    then service
    else service {minLevel = succ service.minLevel}

-- |
-- Prepend a segment to a @/@-separated path that forms the stream name.
--
-- Keep in mind that the stream name should not exceed 63 symbols
nestStreamName :: Text -> Service -> Service
nestStreamName namespace Service {..} =
  Service
    { streamName = mconcat [namespace, "/", streamName],
      ..
    }

start :: IO Service
start = do
  onlineVar <- newTVarIO True
  queue <- newTBQueueIO 1000
  forkIO do
    Printer.run
      Printer.Deps
        { fetchRecords = atomically do
            list <- flushTBQueue queue
            case list of
              [] ->
                readTVar onlineVar >>= \case
                  True -> retry
                  False -> pure Nothing
              h : t ->
                pure (Just (h :| t))
        }
  pure
    Service
      { minLevel = TraceLevel,
        streamName = "/",
        ..
      }

stop :: Service -> IO ()
stop service =
  atomically do
    writeTVar service.onlineVar False

log ::
  Service ->
  -- | Level.
  Level ->
  -- | Message.
  Text ->
  -- | JSON payload.
  [(Text, Aeson.Value)] ->
  IO ()
log service level message payload =
  unless (level < service.minLevel) do
    atomically do
      online <- readTVar service.onlineVar
      when online do
        writeTBQueue service.queue record
  where
    record =
      Record
        { streamName = service.streamName,
          level,
          message,
          payload
        }
