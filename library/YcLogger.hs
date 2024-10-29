module YcLogger
  ( Service,

    -- * Management
    start,
    stop,

    -- * Usage
    log,

    -- * Modification
    setLevel,
    raiseLevel,

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
    minLevel :: Level
  }

setLevel :: Level -> Service -> Service
setLevel level service =
  service {minLevel = level}

raiseLevel :: Service -> Service
raiseLevel service =
  if service.minLevel == maxBound
    then service
    else service {minLevel = succ service.minLevel}

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
        ..
      }

stop :: Service -> IO ()
stop service =
  atomically do
    writeTVar service.onlineVar False

log ::
  Service ->
  -- | Stream name. 1-63 symbols.
  Text ->
  -- | Level.
  Level ->
  -- | Message.
  Text ->
  -- | JSON payload.
  [(Text, Aeson.Value)] ->
  IO ()
log service streamName level message payload =
  unless (level < service.minLevel) do
    atomically do
      online <- readTVar service.onlineVar
      when online do
        writeTBQueue service.queue record
  where
    record = Record {streamName, level, message, payload}
