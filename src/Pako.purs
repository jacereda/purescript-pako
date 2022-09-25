module Pako where

import Prelude

import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.Char (fromCharCode)
import Data.Either (Either)
import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericFromEnum, genericPred, genericSucc)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
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

foreign import deflateImpl :: Foreign -> ArrayBuffer -> Effect ArrayBuffer

foreign import inflateImpl :: ArrayBuffer -> Effect ArrayBuffer

foreign import fromString :: String -> ArrayBuffer
foreign import toString :: ArrayBuffer -> String

defaultOptions :: Options
defaultOptions = { level: Level6
                 , windowBits: WindowBits15
                 , memLevel: MemLevel8
                 , strategy: StrategyDefault
                 }


deflateWithOptions :: Options -> ArrayBuffer -> Either Error ArrayBuffer
deflateWithOptions options = unsafePerformEffect <<< try <<< deflateImpl (write options)

deflate :: ArrayBuffer -> Either Error ArrayBuffer
deflate = deflateWithOptions defaultOptions

inflate :: ArrayBuffer -> Either Error ArrayBuffer
inflate = unsafePerformEffect <<< try <<< inflateImpl

byteSize :: ArrayBuffer -> Int
byteSize = AB.byteLength


deflateTextWithOptions :: Options -> String -> Either Error ArrayBuffer
deflateTextWithOptions options = deflateWithOptions options <<< fromString

deflateText :: String -> Either Error ArrayBuffer
deflateText = deflateTextWithOptions defaultOptions

inflateText :: ArrayBuffer -> Either Error String
inflateText = rmap toString <<< inflate
