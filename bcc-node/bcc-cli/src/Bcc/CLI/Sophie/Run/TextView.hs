module Bcc.CLI.Sophie.Run.TextView
  ( SophieTextViewFileError(..)
  , renderSophieTextViewFileError
  , runTextViewCmd
  ) where

import           Bcc.Prelude

import qualified Data.Text as Text

import           Bcc.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Bcc.CLI.Sophie.Parsers

import           Bcc.Api

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Data.ByteString.Lazy.Char8 as LBS

data SophieTextViewFileError
  = TextViewReadFileError (FileError TextEnvelopeError)
  | TextViewCBORPrettyPrintError !HelpersError
  deriving Show

renderSophieTextViewFileError :: SophieTextViewFileError -> Text
renderSophieTextViewFileError err =
  case err of
    TextViewReadFileError fileErr -> Text.pack (displayError fileErr)
    TextViewCBORPrettyPrintError hlprsErr ->
      "Error pretty printing CBOR: " <> renderHelpersError hlprsErr


runTextViewCmd :: TextViewCmd -> ExceptT SophieTextViewFileError IO ()
runTextViewCmd cmd =
  case cmd of
    TextViewInfo fpath mOutfile -> runTextViewInfo fpath mOutfile

runTextViewInfo :: FilePath -> Maybe OutputFile -> ExceptT SophieTextViewFileError IO ()
runTextViewInfo fpath mOutFile = do
  tv <- firstExceptT TextViewReadFileError $ newExceptT (readTextEnvelopeFromFile fpath)
  let lbCBOR = LBS.fromStrict (textEnvelopeRawCBOR tv)
  case mOutFile of
    Just (OutputFile oFpath) -> liftIO $ LBS.writeFile oFpath lbCBOR
    Nothing -> firstExceptT TextViewCBORPrettyPrintError $ pPrintCBOR lbCBOR
