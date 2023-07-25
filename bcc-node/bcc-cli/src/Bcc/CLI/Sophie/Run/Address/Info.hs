{-# LANGUAGE GADTs #-}
module Bcc.CLI.Sophie.Run.Address.Info
  ( runAddressInfo
  , SophieAddressInfoError(..)
  ) where

import           Bcc.Api
import           Bcc.CLI.Sophie.Parsers (OutputFile (..))
import           Bcc.Prelude
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as LBS

newtype SophieAddressInfoError = SophieAddressInvalid Text
  deriving Show

instance Error SophieAddressInfoError where
  displayError (SophieAddressInvalid addrTxt) =
    "Invalid address: " <> show addrTxt

data AddressInfo = AddressInfo
  { aiType :: !Text
  , aiEra :: !Text
  , aiEncoding :: !Text
  , aiAddress :: !Text
  , aiBase16 :: !Text
  }

instance ToJSON AddressInfo where
  toJSON addrInfo =
    object
      [ "type" .= aiType addrInfo
      , "era" .= aiEra addrInfo
      , "encoding" .= aiEncoding addrInfo
      , "address" .= aiAddress addrInfo
      , "base16" .= aiBase16 addrInfo
      ]

runAddressInfo :: Text -> Maybe OutputFile -> ExceptT SophieAddressInfoError IO ()
runAddressInfo addrTxt mOutputFp = do
    addrInfo <- case (Left  <$> deserialiseAddress AsAddressAny addrTxt)
                 <|> (Right <$> deserialiseAddress AsStakeAddress addrTxt) of

      Nothing ->
        left $ SophieAddressInvalid addrTxt

      Just (Left (AddressCole payaddr)) ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "cole"
              , aiEncoding = "base58"
              , aiAddress = addrTxt
              , aiBase16 = serialiseToRawBytesHexText payaddr
              }

      Just (Left (AddressSophie payaddr)) ->
            pure $ AddressInfo
              { aiType = "payment"
              , aiEra = "sophie"
              , aiEncoding = "bech32"
              , aiAddress = addrTxt
              , aiBase16 = serialiseToRawBytesHexText payaddr
              }

      Just (Right addr) ->
        pure $ AddressInfo
          { aiType = "stake"
          , aiEra = "sophie"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiBase16 = serialiseToRawBytesHexText addr
          }

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty addrInfo
      Nothing -> liftIO $ LBS.putStrLn $ encodePretty addrInfo

