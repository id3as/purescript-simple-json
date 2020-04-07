module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromLeft, isRight)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Erl.Data.Map (Map)
import Foreign (Foreign, ForeignError(..), MultipleErrors)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, parseJSON, readJSON, writeJSON)
import Test.Assert (assertEqual)
import Test.EnumSumGeneric as Test.EnumSumGeneric
import Test.Generic as Test.Generic
import Test.Inferred as Test.Inferred
import Test.Quickstart as Test.Quickstart
import Type.Proxy (Proxy(..))

type E a = Either MultipleErrors a


type MyTestNoArray =
  { a :: Int
  , b :: String
  , c :: Boolean
  }

type MyTest =
  { a :: Int
  , b :: String
  , c :: Boolean
  , d :: Array String
  }

type MyTestNumber =
    { a :: Number
    , b :: Int
    }

type MyTestNull =
  { a :: Int
  , b :: String
  , c :: Boolean
  , d :: Array String
  , e :: Maybe (Array String)
  }

type MyTestStrMap =
  { a :: Int
  , b :: Map String Int
  }

newtype AlsoAString = AlsoAString String
derive instance alsoAStringNewtype :: Newtype AlsoAString _

type MyTestStrMapNewtype =
  { a :: Int
  , b :: Map AlsoAString Int
  }

type MyTestMaybe =
  { a :: Maybe String
  }

type MyTestManyMaybe =
  { a         :: Maybe String
  , aNull     :: Maybe String
  , b         :: Maybe Int
  , bNull     :: Maybe Int
  , c         :: Maybe Boolean
  , cNull     :: Maybe Boolean
  , d         :: Maybe Number
  , dNull     :: Maybe Number
  , e         :: Maybe (Array (Maybe String))
  , eNull     :: Maybe (Array (Maybe String))
  }

type MyTestNullable =
  { a :: Nullable String
  , b :: Nullable String
  }

type MyTestVariant = Variant
  ( a :: String
  , b :: Int
  )


roundtrips :: forall a. ReadForeign a => WriteForeign a => Proxy a -> String -> Effect Unit
roundtrips _ enc0 = do
  let parseJSON' = lmap show <<< runExcept <<< parseJSON
      dec0 :: E a
      dec0 = readJSON enc0
      enc1 = either (const "bad1") writeJSON dec0
  log $ either show writeJSON dec0
  let json0 :: Either String Foreign
      json0 = parseJSON' enc0
      json1 :: Either String Foreign
      json1 = parseJSON' enc1
      dec1 :: E a
      dec1 = readJSON enc1
      enc2 = either (const "bad2") writeJSON dec1
  when (enc1 /= enc2) $ throw $ enc0 <> " ||| " <> enc1 <> " ||| " <> enc2

shouldEqual :: forall a . Eq a => Show a => a -> a -> Effect Unit
shouldEqual a b =
  assertEqual { actual: a, expected: b}


main :: Effect Unit
main = do
  shouldEqual 1 1
  
  -- "fails with invalid JSON"
  let r1 :: E MyTest
      r1 = readJSON """{ "c": 1, "d": 2}"""
  (unsafePartial $ fromLeft r1) `shouldEqual`
     (NonEmptyList (NonEmpty (ErrorAtProperty "a" (TypeMismatch "integer" "atom")) ((ErrorAtProperty "b" (TypeMismatch "binary" "atom")) : (ErrorAtProperty "c" (TypeMismatch "boolean" "integer")) : (ErrorAtProperty "d" (TypeMismatch "list" "integer")) : Nil)))
  isRight (r1 :: E MyTest) `shouldEqual` false

  -- "works with missing Maybe fields by setting them to Nothing"
  let r2 = readJSON "{}"
  (writeJSON <$> (r2 :: E MyTestMaybe)) `shouldEqual` (Right """{}""")

  -- "fails with undefined for null with correct error message"
  let r3 = readJSON """
    { "a": "asdf" }
  """
  (unsafePartial $ fromLeft r3) `shouldEqual`
    (NonEmptyList (NonEmpty (ErrorAtProperty "b" (TypeMismatch "Nullable binary" "atom")) Nil))
  (isRight (r3 :: E MyTestNullable)) `shouldEqual` false

  roundtrips (Proxy :: Proxy MyTestNoArray) """
    { "a": 1, "b": "asdf", "c": true }
  """

  -- roundtrips
  -- "works with proper JSON"
  roundtrips (Proxy :: Proxy MyTest) """
    { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
  """

  -- "works with JSON lacking Maybe field"
  roundtrips (Proxy :: Proxy MyTestNull) """
    { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
  """

  -- "works with JSON containing Maybe field"
  roundtrips (Proxy :: Proxy MyTestNull) """
    { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"], "e": ["C", "D"]}
  """

-- "works with JSON containing floats "
  roundtrips (Proxy :: Proxy MyTestNumber) """
    { "a": 1.0, "b": 1 }
  """

  -- "works with JSON containing floats which happen to be ints"
  roundtrips (Proxy :: Proxy MyTestNumber) """
    { "a": 1, "b": 1 }
  """
    

  -- "works with JSON containing Map field"
  roundtrips (Proxy :: Proxy MyTestStrMap) """
    { "a": 1, "b": {"asdf": 1, "c": 2} }
  """

  -- "works with JSON containing Map field with newtyped keys"
  roundtrips (Proxy :: Proxy MyTestStrMapNewtype) """
    { "a": 1, "b": {"asdf": 1, "c": 2} }
  """


  -- "works with Maybe field and existing value"
  roundtrips (Proxy :: Proxy MyTestMaybe) """
    { "a": "foo" }
  """

  -- "works with Nullable"
  roundtrips (Proxy :: Proxy MyTestNullable) """
    { "a": null, "b": "a" }
  """

  -- "works with Variant"
  roundtrips (Proxy :: Proxy MyTestVariant) """
    { "type": "b", "value": 123  }
  """

  log "Generic"


  -- run examples
  Test.Generic.main
  log "EnumSumGeneric"

  Test.EnumSumGeneric.main
  log "Inferred"

  Test.Inferred.main
  log "Quickstart"

  Test.Quickstart.main
  log "done"