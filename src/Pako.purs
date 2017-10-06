module Pako where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION, Error, try)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Char (fromCharCode)
import Data.Either (Either)
import Data.String (fromCharArray)
import Data.Traversable (traverse)

foreign import data PAKO :: Effect

foreign import deflateImpl :: forall e. Uint8Array -> Eff (exception :: EXCEPTION, pako :: PAKO | e) Uint8Array

foreign import inflateImpl :: forall e. Uint8Array -> Eff (exception :: EXCEPTION, pako :: PAKO | e) Uint8Array


deflate :: forall e. Uint8Array -> Eff (pako :: PAKO | e) (Either Error Uint8Array)
deflate = try <<< deflateImpl

inflate :: forall e. Uint8Array -> Eff (pako :: PAKO | e) (Either Error Uint8Array)
inflate = try <<< inflateImpl

byteSize :: Uint8Array -> Int
byteSize = DV.byteLength <<< TA.dataView

asBytes :: forall e. String -> Eff (arrayBuffer :: AB.ARRAY_BUFFER | e) Uint8Array
asBytes s = do
  ab <- AB.fromString s
  pure $ TA.asUint8Array $ DV.whole ab

asString :: forall e. Uint8Array ->  Eff (arrayBuffer :: AB.ARRAY_BUFFER | e) String
asString b = do
  is <- TA.toIntArray $ TA.asInt16Array $ TA.dataView b
  let cs = fromCharCode <$> is
  pure $ fromCharArray cs

deflateText :: forall e. String -> Eff (pako :: PAKO, arrayBuffer :: AB.ARRAY_BUFFER | e) (Either Error Uint8Array)
deflateText = deflate <=< asBytes

inflateText :: forall e. Uint8Array -> Eff (pako :: PAKO, arrayBuffer :: AB.ARRAY_BUFFER | e) (Either Error String)
inflateText = traverse asString <=< inflate
