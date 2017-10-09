module Pako where

import Prelude

import Control.Monad.Eff (Eff, runPure, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION, Error, try)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
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
import Simple.JSON (class WriteForeign, write, writeImpl)

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

foreign import deflateImpl :: forall e. Foreign -> ArrayBuffer -> Eff (exception :: EXCEPTION | e) ArrayBuffer

foreign import inflateImpl :: forall e. ArrayBuffer -> Eff (exception :: EXCEPTION | e) ArrayBuffer


defaultOptions :: Options
defaultOptions = { level: Level6
                 , windowBits: WindowBits15
                 , memLevel: MemLevel8
                 , strategy: StrategyDefault
                 }


deflateWithOptions :: Options -> ArrayBuffer -> Either Error ArrayBuffer
deflateWithOptions options = runPure <<< try <<< deflateImpl (write options)

deflate :: ArrayBuffer -> Either Error ArrayBuffer
deflate = deflateWithOptions defaultOptions

inflate :: ArrayBuffer -> Either Error ArrayBuffer
inflate = runPure <<< try <<< inflateImpl

byteSize :: ArrayBuffer -> Int
byteSize = AB.byteLength

asBytes :: String -> ArrayBuffer
asBytes = AB.fromString

asString :: ArrayBuffer -> String
asString = fromCharArray <<< map fromCharCode <<< TA.toIntArray <<< TA.asInt16Array <<< DV.whole

deflateTextWithOptions :: Options -> String -> Either Error ArrayBuffer
deflateTextWithOptions options = deflateWithOptions options <<< asBytes

deflateText :: String -> Either Error ArrayBuffer
deflateText = deflateTextWithOptions defaultOptions

inflateText :: ArrayBuffer -> Either Error String
inflateText = rmap asString <<< inflate
