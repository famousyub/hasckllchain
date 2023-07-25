{-# LANGUAGE DefaultSignatures #-}

-- | CBOR serialisation
--
module Bcc.Api.SerialiseCBOR
  ( SerialiseAsCBOR(..)
  , FromCBOR(..)
  , ToCBOR(..)
  ) where

import           Prelude

import           Data.ByteString (ByteString)

import           Bcc.Binary (FromCBOR, ToCBOR)
import qualified Bcc.Binary as CBOR

import           Bcc.Api.HasTypeProxy


class HasTypeProxy a => SerialiseAsCBOR a where
    serialiseToCBOR :: a -> ByteString
    deserialiseFromCBOR :: AsType a -> ByteString -> Either CBOR.DecoderError a

    default serialiseToCBOR :: ToCBOR a => a -> ByteString
    serialiseToCBOR = CBOR.serialize'

    default deserialiseFromCBOR :: FromCBOR a
                                => AsType a
                                -> ByteString
                                -> Either CBOR.DecoderError a
    deserialiseFromCBOR _proxy = CBOR.decodeFull'

