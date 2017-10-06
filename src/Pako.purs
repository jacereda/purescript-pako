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
import Data.Enum (class Enum)
import Data.Foreign (Foreign)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericFromEnum, genericPred, genericSucc)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Simple.JSON (class WriteForeign, write, writeImpl)

foreign import data PAKO :: Effect

data Level = Level0
           | Level1
           | Level2
           | Level3
           | Level4
           | Level5
           | Level6
           | Level7
           | Level8
           | Level9

derive instance genericLevel :: Generic Level _
derive instance eqLevel :: Eq Level
instance showLevel :: Show Level where show = genericShow
instance ordLevel :: Ord Level where compare = genericCompare
instance boundedLevel :: Bounded Level where
  top = genericTop
  bottom = genericBottom
instance enumLevel :: Enum Level where 
  pred = genericPred
  succ = genericSucc
instance writeForeignlevel :: WriteForeign Level where
  writeImpl = writeImpl <<< genericFromEnum

data WindowBits = WindowBits8
                | WindowBits9
                | WindowBits10
                | WindowBits11
                | WindowBits12
                | WindowBits13
                | WindowBits14
                | WindowBits15

derive instance genericWindowBits :: Generic WindowBits _
derive instance eqWindowBits :: Eq WindowBits
instance showWindowBits :: Show WindowBits where show = genericShow
instance ordWindowBits :: Ord WindowBits where compare = genericCompare
instance boundedWindowBits :: Bounded WindowBits where
  top = genericTop
  bottom = genericBottom
instance enumWindowBits :: Enum WindowBits where 
  pred = genericPred
  succ = genericSucc
instance writeForeignWindowBits :: WriteForeign WindowBits where
  writeImpl = writeImpl <<< (_ + 8) <<< genericFromEnum

data MemLevel = MemLevel1
              | MemLevel2
              | MemLevel3
              | MemLevel4
              | MemLevel5
              | MemLevel6
              | MemLevel7
              | MemLevel8
              | MemLevel9

derive instance genericMemLevel :: Generic MemLevel _
derive instance eqMemLevel :: Eq MemLevel
instance showMemLevel :: Show MemLevel where show = genericShow
instance ordMemLevel :: Ord MemLevel where compare = genericCompare
instance boundedMemLevel :: Bounded MemLevel where
  top = genericTop
  bottom = genericBottom
instance enumMemLevel :: Enum MemLevel where 
  pred = genericPred
  succ = genericSucc
instance writeForeignMemLevel :: WriteForeign MemLevel where
  writeImpl = writeImpl <<< (_ + 1) <<< genericFromEnum


data Strategy = StrategyDefault
              | StrategyFiltered
              | StrategyHuffmanOnly
              | StrategyRLE
              | StrategyFixed

derive instance genericStrategy :: Generic Strategy _
derive instance eqStrategy :: Eq Strategy
instance showStrategy :: Show Strategy where show = genericShow
instance ordStrategy :: Ord Strategy where compare = genericCompare
instance boundedStrategy :: Bounded Strategy where
  top = genericTop
  bottom = genericBottom
instance enumStrategy :: Enum Strategy where 
  pred = genericPred
  succ = genericSucc
instance writeForeignStrategy :: WriteForeign Strategy where
  writeImpl = writeImpl <<< genericFromEnum


type Options = { level :: Level
               , windowBits :: WindowBits
               , memLevel :: MemLevel
               , strategy :: Strategy
               }

foreign import deflateImpl :: forall e. Foreign -> Uint8Array -> Eff (exception :: EXCEPTION, pako :: PAKO | e) Uint8Array

foreign import inflateImpl :: forall e. Uint8Array -> Eff (exception :: EXCEPTION, pako :: PAKO | e) Uint8Array


defaultOptions :: Options
defaultOptions = { level: Level6
                 , windowBits: WindowBits15
                 , memLevel: MemLevel8
                 , strategy: StrategyDefault
                 }


deflateWithOptions :: forall e. Options -> Uint8Array -> Eff (pako :: PAKO | e) (Either Error Uint8Array)
deflateWithOptions options bytes = try $ deflateImpl (write options) bytes

deflate :: forall e. Uint8Array -> Eff (pako :: PAKO | e) (Either Error Uint8Array)
deflate = deflateWithOptions defaultOptions

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

deflateTextWithOptions :: forall e. Options -> String -> Eff (pako :: PAKO, arrayBuffer :: AB.ARRAY_BUFFER | e) (Either Error Uint8Array)
deflateTextWithOptions options = deflateWithOptions options <=< asBytes

deflateText :: forall e. String -> Eff (pako :: PAKO, arrayBuffer :: AB.ARRAY_BUFFER | e) (Either Error Uint8Array)
deflateText = deflateTextWithOptions defaultOptions

inflateText :: forall e. Uint8Array -> Eff (pako :: PAKO, arrayBuffer :: AB.ARRAY_BUFFER | e) (Either Error String)
inflateText = traverse asString <=< inflate
