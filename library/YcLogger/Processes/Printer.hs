module YcLogger.Processes.Printer
  ( Deps (..),
    run,
  )
where

import BasePrelude
import Data.ByteString qualified as ByteString
import YcLogger.Models.Domain
import YcLogger.Processes.Printer.Compilers.Output qualified as OutputCompiler

data Deps = Deps
  { fetchRecords :: IO (Maybe (NonEmpty Record))
  }

run :: Deps -> IO ()
run Deps {..} =
  go
  where
    go =
      fetchRecords >>= \case
        Nothing -> pure ()
        Just records -> do
          ByteString.hPutStr stderr (OutputCompiler.compile records)
          hFlush stderr
          go
