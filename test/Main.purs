module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error)
import Control.Monad.Eff.Random (RANDOM)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.Either (Either(..))
import Data.Traversable (for_, traverse)
import Pako (PAKO, asBytes, deflateText, inflateText)
import Test.QuickCheck (QC, arbitrary, quickCheck', (<?>))
import Test.QuickCheck.Gen (randomSample')

newtype Res = Res (Either Error String)

derive newtype instance showRes :: Show Res
instance eqRes :: Eq Res where
  eq (Res (Right a)) (Res (Right b)) = a == b
  eq (Res (Left a)) (Res (Left b)) = true -- (eq `on` message) a b
  eq _ _ = false


assertEffEquals :: forall a e. Eq a => Show a => a -> QC e a -> QC e Unit
assertEffEquals expectedValue computation = do
  actualValue <- computation
  let msg = show expectedValue <> " /= " <> show actualValue
  quickCheck' 1 $ actualValue == expectedValue <?> msg
  

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION, pako :: PAKO, arrayBuffer :: ARRAY_BUFFER | e) Unit
main = do
  xs <- randomSample' 100 arbitrary
  for_ xs \x -> do
    assertEffEquals (Res (Right x)) do
      c <- deflateText x
      d <- traverse inflateText c
      pure $ Res $ join d
    assertEffEquals (Res (Left (error "incorrect header check"))) do
      invalid <- asBytes x
      res <- inflateText invalid
      pure $ Res res
