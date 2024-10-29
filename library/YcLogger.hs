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
    Printer.Level (..),
  )
where

import BasePrelude hiding (log)
import Control.Concurrent.STM
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import YcLogger.Processes.Printer qualified as Printer

data Service = Service
  { onlineVar :: TVar Bool,
    queue :: TBQueue Printer.Record,
    minLevel :: Printer.Level
  }

setLevel :: Printer.Level -> Service -> Service
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
      { minLevel = Printer.TraceLevel,
        ..
      }

stop :: Service -> IO ()
stop service =
  atomically do
    writeTVar service.onlineVar False

log ::
  Service ->
  -- | Level.
  Printer.Level ->
  -- | Message.
  Text ->
  -- | JSON payload.
  Aeson.Value ->
  IO ()
log service level message payload =
  unless (level < service.minLevel) do
    atomically do
      online <- readTVar service.onlineVar
      when online do
        writeTBQueue service.queue record
  where
    record = Printer.Record {level, message, payload}
