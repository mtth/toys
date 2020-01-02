{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Logging utilities.
module Bananagrams.Log (
  Message, Loggable, Severity(..), Shown(..),
  log0, log1, logs,
  LoggableIO, liftST,
  loggingToStderr
) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Log (Handler, LoggingT, MonadLog, Severity(..), WithSeverity(..), defaultBatchingOptions, logMessage, msgSeverity, renderWithSeverity, renderWithTimestamp, runLoggingT, timestamp, withFDHandler)
import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.Text.Buildable (Buildable)
import Data.Text.Format (Format, Only(..), Shown(..), format)
import Data.Text.Format.Params (Params)
import Data.Text.Prettyprint.Doc (Doc, pretty)
import qualified Data.Text.Lazy as Text'
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import System.IO (stderr)

type Message = WithSeverity Text'.Text
type Loggable = MonadLog Message
type LoggableIO m = (MonadIO m, Loggable m)

log0 :: Loggable m => Severity -> Text'.Text -> m ()
log0 sev txt = logMessage $ WithSeverity sev txt

log1 :: (Buildable a, Loggable m) => Severity -> Format -> a -> m ()
log1 sev fmt arg = log0 sev $ format fmt (Only arg)

logs :: (Params p, Loggable m) => Severity -> Format -> p -> m ()
logs sev fmt params = log0 sev $ format fmt params

liftST :: LoggableIO m => ST RealWorld a -> m a
liftST = liftIO . stToIO

renderMessage :: MonadIO m => Message -> m (Doc a)
renderMessage msg =
  let renderTime = formatTime defaultTimeLocale $ iso8601DateFormat $ Just "%H:%M:%S"
  in renderWithTimestamp renderTime (renderWithSeverity pretty) <$> timestamp msg

loggingToStderr :: Severity -> LoggingT Message IO a -> IO a
loggingToStderr sev axn = withFDHandler defaultBatchingOptions stderr 0.4 80 $ \handler ->
  runLoggingT axn $ \msg -> if msgSeverity msg <= sev
    then renderMessage msg >>= handler
    else pure ()
