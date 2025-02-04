module Simple.JSON
  ( E
  , readJSON
  , readJSON'
  , readJSON_
  , writeJSON
  , write
  , read
  , read'
  , read_
  , readAsForeign
  , parseJSON
  , undefined
  , class ReadForeign
  , readImpl
  , class ReadForeignFields
  , getFields
  , class ReadForeignVariant
  , readVariantImpl
  , class ReadForeignKey
  , readKeyImpl
  , class WriteForeign
  , writeImpl
  , class WriteForeignKey
  , writeKeyImpl
  , class WriteForeignFields
  , writeImplFields
  , class WriteForeignVariant
  , writeVariantImpl
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExcept)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Bifunctor (bimap, lmap)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..), hush, note)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty (NonEmptyString, fromString, toString)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, on)
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.List.NonEmpty as NEL
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple as Tuple
import Erl.Kernel.Inet (Ip4Address(..), Ip6Address(..), IpAddress(..), Port(..), parseIp4Address, parseIp6Address, parseIpAddress)
import Erl.Types (Hextet(..), MonotonicTime(..), Octet(..), Ref, hextet, octet, refToString, stringToRef)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, isNull, isUndefined, readBoolean, readChar, readInt, readNull, readNumber, readString, tagOf, unsafeFromForeign, unsafeReadTagged, unsafeToForeign)
import Foreign.Index (readProp)
import Partial.Unsafe (unsafeCrashWith)
import Pathy (Abs, AbsDir, AbsFile, Dir, File, Rel, RelDir, RelFile, SandboxedPath, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, posixParser, posixPrinter, printPath)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record (get)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (RLProxy(..))

-- | An alias for the Either result of decoding
type E a = Either MultipleErrors a

-- | Read a JSON string to a type `a` while returning a `MultipleErrors` if the
-- | parsing failed.
readJSON
  :: forall a
   . ReadForeign a
  => String
  -> E a
readJSON = runExcept <<< (readImpl <=< parseJSON)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON'
  :: forall a
   . ReadForeign a
  => String
  -> F a
readJSON' = readImpl <=< parseJSON

-- | Read a JSON string to a type `a` while returning `Nothing` if the parsing
-- | failed.
readJSON_
  :: forall a
   . ReadForeign a
  => String
  -> Maybe a
readJSON_ = hush <<< readJSON

foreign import stringifyJSON :: Foreign -> String

-- | Write a JSON string from a type `a`.
writeJSON
  :: forall a
   . WriteForeign a
  => a
  -> String
writeJSON = stringifyJSON <<< writeImpl

write
  :: forall a
   . WriteForeign a
  => a
  -> Foreign
write = writeImpl

-- | Read a Foreign value to a type
read
  :: forall a
   . ReadForeign a
  => Foreign
  -> E a
read = runExcept <<< readImpl

-- | Read a value of any type as Foreign to a type
readAsForeign
  :: forall a b
   . ReadForeign a
  => b
  -> E a
readAsForeign = read <<< unsafeToForeign

read'
  :: forall a
   . ReadForeign a
  => Foreign
  -> F a
read' = readImpl

-- | Read a Foreign value to a type, as a Maybe of type
read_
  :: forall a
   . ReadForeign a
  => Foreign
  -> Maybe a
read_ = hush <<< read

foreign import _parseJSON :: EU.EffectFn1 String Foreign

parseJSON :: String -> F Foreign
parseJSON =
  ExceptT
    <<< Identity
    <<< lmap (pure <<< ForeignError <<< message)
    <<< runPure
    <<< try
    <<< EU.runEffectFn1 _parseJSON
  where
  -- Nate Faubion: "It uses unsafePerformEffect because that’s the only way to catch exceptions and still use the builtin json decoder"
  runPure = unsafePerformEffect

foreign import _undefined :: Foreign

undefined :: Foreign
undefined = _undefined

-- | A class for reading foreign values to a type
class ReadForeign a where
  readImpl :: Foreign -> F a

instance readForeign :: ReadForeign Foreign where
  readImpl = pure

instance readChar :: ReadForeign Char where
  readImpl = readChar

instance readNumber :: ReadForeign Number where
  readImpl n = readNumber n <|> (toNumber <$> readInt n)

instance readInt :: ReadForeign Int where
  readImpl = readInt

instance readString :: ReadForeign String where
  readImpl = readString

instance ReadForeign NonEmptyString where
  readImpl f = (\s -> except $ note (singleton $ ForeignError "NonEmptyString is empty") $ fromString s) =<< readString f

instance ReadForeign RelFile where
  readImpl f = (\s -> except $ note (singleton $ ForeignError "Invalid RelFile") $ parseRelFile posixParser s) =<< readString f

instance ReadForeign AbsFile where
  readImpl f = (\s -> except $ note (singleton $ ForeignError "Invalid AbsFile") $ parseAbsFile posixParser s) =<< readString f

instance ReadForeign RelDir where
  readImpl f = (\s -> except $ note (singleton $ ForeignError "Invalid RelDir") $ parseRelDir posixParser s) =<< readString f

instance ReadForeign AbsDir where
  readImpl f = (\s -> except $ note (singleton $ ForeignError "Invalid AbsDir") $ parseAbsDir posixParser s) =<< readString f

instance readBinary :: ReadForeign Binary where
  readImpl b = except $ note (singleton $ ForeignError "Invalid base64") $ base64Decode b

instance readBoolean :: ReadForeign Boolean where
  readImpl = readBoolean

instance readArray :: ReadForeign a => ReadForeign (Array a) where
  readImpl = traverseWithIndex readAtIdx <=< (pure <<< Array.fromFoldable) <=< readListF
    where
    readAtIdx i f = withExcept (map (ErrorAtIndex i)) (readImpl f)

instance readList :: ReadForeign a => ReadForeign (List a) where
  readImpl = traverse readImpl <=< readListF

readListF :: Foreign -> F (List Foreign)
readListF = unsafeReadTagged "list"

instance readNonEmptyList :: ReadForeign a => ReadForeign (NEL.NonEmptyList a) where
  readImpl = (except <<< note (singleton $ ForeignError "Empty List") <<< NEL.fromList) <=< traverse readImpl <=< readListF

instance readMaybe :: ReadForeign a => ReadForeign (Maybe a) where
  readImpl = readNullOrUndefined readImpl
    where
    readNullOrUndefined _ value | isNull value || isUndefined value = pure Nothing
    readNullOrUndefined f value = Just <$> f value

instance ReadForeign Hextet where
  readImpl = readInt >=> mapToHextet
    where
    mapToHextet i = except $ note (singleton $ ForeignError "Invalid hextet") $ hextet i

instance ReadForeign Octet where
  readImpl = readInt >=> mapToOctet
    where
    mapToOctet i = except $ note (singleton $ ForeignError "Invalid octet") $ octet i

instance ReadForeign Port where
  readImpl p = Port <$> readInt p

instance ReadForeign MonotonicTime where
  readImpl p = MonotonicTime <$> readInt p

instance ReadForeign Ref where
  readImpl = readString >=> mapToRef
    where
    mapToRef s = except $ note (singleton $ ForeignError "Invalid ref") $ stringToRef s

instance ReadForeign Milliseconds where
  readImpl p = Milliseconds <$> readNumber p

instance ReadForeign Instant where
  readImpl = readNumber >=> mapToInstant
    where
    mapToInstant i = except $ note (singleton $ ForeignError "Invalid instance") $ instant $ Milliseconds i

instance ReadForeign IpAddress where
  readImpl = readString >=> mapToIp
    where
    mapToIp i = except $ note (singleton $ ForeignError "Invalid ip address") $ parseIpAddress i

instance ReadForeign Ip4Address where
  readImpl = readString >=> mapToIp
    where
    mapToIp i = except $ note (singleton $ ForeignError "Invalid ip4 address") $ parseIp4Address i

instance ReadForeign Ip6Address where
  readImpl = readString >=> mapToIp
    where
    mapToIp i = except $ note (singleton $ ForeignError "Invalid ip6 address") $ parseIp6Address i

instance readNullable :: ReadForeign a => ReadForeign (Nullable a) where
  readImpl o =
    withExcept (map reformat)
      $
        map toNullable
          <$> traverse readImpl
          =<< readNull o
    where
    reformat error = case error of
      TypeMismatch inner other -> TypeMismatch ("Nullable " <> inner) other
      _ -> error

instance (Ord a, ReadForeign a) => ReadForeign (Set a) where
  readImpl xs = Set.fromFoldable <$> (readImpl xs :: F (List a))

instance readMap :: (ReadForeignKey k, ReadForeign a) => ReadForeign (Map k a) where
  readImpl f =
    Map.fromFoldable <$> ((sequence <<< map readEntry) =<< (readObject' f))
    where
    readObject' :: Foreign -> F (List (Tuple Foreign Foreign))
    readObject' value
      | tagOf value == "map" = pure $ Map.toUnfoldable $ unsafeFromForeign value
      | otherwise = fail $ TypeMismatch "Object" (tagOf value)

    readEntry :: Tuple Foreign Foreign -> F (Tuple k a)
    readEntry (Tuple key value) = do
      (\k -> Tuple k <$> readImpl value) =<< readKeyImpl key

instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList () fields
  ) =>
  ReadForeign (Record fields) where
  readImpl o = flip Builder.build {} <$> getFields fieldListP o
    where
    fieldListP = RLProxy :: RLProxy fieldList

-- | A class for reading foreign values from properties
class ReadForeignFields (xs :: RowList Type) (from :: Row Type) (to :: Row Type) | xs -> from to where
  getFields
    :: RLProxy xs
    -> Foreign
    -> F (Builder (Record from) (Record to))

instance readFieldsCons ::
  ( IsSymbol name
  , ReadForeign ty
  , ReadForeignFields tail from from'
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) =>
  ReadForeignFields (Cons name ty tail) from to where
  getFields _ obj = (compose <$> first) `exceptTApply` rest
    where
    first = do
      value <- withExcept' (readImpl =<< readProp name obj)
      pure $ Builder.insert nameP value
    rest = getFields tailP obj
    nameP = SProxy :: SProxy name
    tailP = RLProxy :: RLProxy tail
    name = reflectSymbol nameP
    withExcept' = withExcept <<< map $ ErrorAtProperty name

exceptTApply :: forall a b e m. Semigroup e => Applicative m => ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
exceptTApply fun a =
  ExceptT
    $ applyEither
        <$> runExceptT fun
        <*> runExceptT a

applyEither :: forall e a b. Semigroup e => Either e (a -> b) -> Either e a -> Either e b
applyEither (Left e) (Right _) = Left e
applyEither (Left e1) (Left e2) = Left (e1 <> e2)
applyEither (Right _) (Left e) = Left e
applyEither (Right fun) (Right a) = Right (fun a)

instance readFieldsNil ::
  ReadForeignFields Nil () () where
  getFields _ _ =
    pure identity

instance readForeignVariant ::
  ( RowToList variants rl
  , ReadForeignVariant rl variants
  ) =>
  ReadForeign (Variant variants) where
  readImpl o = readVariantImpl (RLProxy :: RLProxy rl) o

class ReadForeignVariant (xs :: RowList Type) (row :: Row Type) | xs -> row where
  readVariantImpl
    :: RLProxy xs
    -> Foreign
    -> F (Variant row)

instance readVariantNil ::
  ReadForeignVariant Nil trash where
  readVariantImpl _ _ = fail $ ForeignError "Unable to match any variant member."

instance readVariantCons ::
  ( IsSymbol name
  , ReadForeign ty
  , Row.Cons name ty trash row
  , ReadForeignVariant tail row
  ) =>
  ReadForeignVariant (Cons name ty tail) row where
  readVariantImpl _ o =
    do
      obj :: { type :: String, value :: Foreign } <- readImpl o
      if obj.type == name then do
        value :: ty <- readImpl obj.value
        pure $ inj namep value
      else
        (fail <<< ForeignError $ "Did not match variant tag " <> name)
      <|> readVariantImpl (RLProxy :: RLProxy tail) o
    where
    namep = SProxy :: SProxy name
    name = reflectSymbol namep

-- -- | A class for writing a value into JSON
-- -- | need to do this intelligently using Foreign probably, because of null and undefined whatever
class WriteForeign a where
  writeImpl :: a -> Foreign

instance writeForeignForeign :: WriteForeign Foreign where
  writeImpl = identity

instance writeForeignString :: WriteForeign String where
  writeImpl = unsafeToForeign

instance WriteForeign NonEmptyString where
  writeImpl = unsafeToForeign <<< toString

instance writeForeignInt :: WriteForeign Int where
  writeImpl = unsafeToForeign

instance writeForeignChar :: WriteForeign Char where
  writeImpl = unsafeToForeign

instance writeForeignNumber :: WriteForeign Number where
  writeImpl = unsafeToForeign

instance writeForeignBoolean :: WriteForeign Boolean where
  writeImpl = unsafeToForeign

instance WriteForeign (SandboxedPath Rel File) where
  writeImpl = unsafeToForeign <<< printPath posixPrinter

instance WriteForeign (SandboxedPath Abs File) where
  writeImpl = unsafeToForeign <<< printPath posixPrinter

instance WriteForeign (SandboxedPath Rel Dir) where
  writeImpl = unsafeToForeign <<< printPath posixPrinter

instance WriteForeign (SandboxedPath Abs Dir) where
  writeImpl = unsafeToForeign <<< printPath posixPrinter

instance writeForeignMilliseconds :: WriteForeign Milliseconds where
  writeImpl (Milliseconds m) = unsafeToForeign m

instance writeForeignInstant :: WriteForeign Instant where
  writeImpl = writeImpl <<< unInstant

instance writeForeignPort :: WriteForeign Port where
  writeImpl (Port p) = writeImpl p

instance WriteForeign IpAddress where
  writeImpl = case _ of
    Ip4 (Ip4Address a) -> (flip Tuple.uncurry4) a \d1 d2 d3 d4 ->
      writeImpl $ Array.intercalate "." $ map (show <<< un Octet) [ d1, d2, d3, d4 ]
    Ip6 (Ip6Address a) -> (flip Tuple.uncurry8) a \d1 d2 d3 d4 d5 d6 d7 d8 ->
      writeImpl $ Array.intercalate "." $ map (show <<< un Hextet) [ d1, d2, d3, d4, d5, d6, d7, d8 ]

instance writeForeignMonotonicTime :: WriteForeign MonotonicTime where
  writeImpl (MonotonicTime m) = unsafeToForeign m

instance writeForeignRef :: WriteForeign Ref where
  writeImpl ref = unsafeToForeign $ refToString ref

instance writeForeignArray :: WriteForeign a => WriteForeign (Array a) where
  writeImpl xs = writeImpl $ List.fromFoldable xs

instance WriteForeign a => WriteForeign (Set a) where
  writeImpl xs = writeImpl $ List.fromFoldable xs

instance writeForeignList :: WriteForeign a => WriteForeign (List a) where
  writeImpl xs = unsafeToForeign $ writeImpl <$> xs

instance writeNonEmptyList :: WriteForeign a => WriteForeign (NEL.NonEmptyList a) where
  writeImpl xs = writeImpl $ NEL.toList xs

instance writeForeignMaybe :: WriteForeign a => WriteForeign (Maybe a) where
  writeImpl = maybe undefined writeImpl

instance writeForeignBinary :: WriteForeign Binary where
  writeImpl = base64Encode

instance writeForeignNullable :: WriteForeign a => WriteForeign (Nullable a) where
  writeImpl = maybe (unsafeToForeign $ toNullable Nothing) writeImpl <<< toMaybe

instance (WriteForeignKey b, WriteForeign a) => WriteForeign (Map b a) where
  writeImpl = unsafeToForeign
    <<< (Map.fromFoldable :: List (Tuple Foreign Foreign) -> Map Foreign Foreign)
    <<< map (bimap writeKeyImpl writeImpl)
    <<< Map.toUnfoldable

instance recordWriteForeign ::
  ( RowToList row rl
  , WriteForeignFields rl row
  ) =>
  WriteForeign (Record row) where
  writeImpl rec = unsafeToForeign $ writeImplFields rlp rec
    where
    rlp = RLProxy :: RLProxy rl

class WriteForeignFields (rl :: RowList Type) row | rl -> row where
  writeImplFields :: forall g. g rl -> Record row -> Map String Foreign

instance consWriteForeignFields ::
  ( IsSymbol name
  , WriteForeign ty
  , WriteForeignFields tail row
  , Row.Cons name ty whatever row
  ) =>
  WriteForeignFields (Cons name ty tail) row where
  writeImplFields _ rec = result
    where
    namep = SProxy :: SProxy name
    value = writeImpl $ get namep rec
    tailp = RLProxy :: RLProxy tail
    rest = writeImplFields tailp rec
    result = Map.insert (reflectSymbol namep) value rest

instance nilWriteForeignFields ::
  WriteForeignFields Nil row where
  writeImplFields _ _ = Map.empty

instance writeForeignVariant ::
  ( RowToList row rl
  , WriteForeignVariant rl row
  ) =>
  WriteForeign (Variant row) where
  writeImpl variant = writeVariantImpl (RLProxy :: RLProxy rl) variant

class WriteForeignVariant (rl :: RowList Type) (row :: Row Type) | rl -> row where
  writeVariantImpl :: forall g. g rl -> Variant row -> Foreign

instance nilWriteForeignVariant ::
  WriteForeignVariant Nil () where
  writeVariantImpl _ _ =
    -- a PureScript-defined variant cannot reach this path, but a JavaScript FFI one could.
    unsafeCrashWith "Variant was not able to be writen row WriteForeign."

instance consWriteForeignVariant ::
  ( IsSymbol name
  , WriteForeign ty
  , Row.Cons name ty subRow row
  , WriteForeignVariant tail subRow
  ) =>
  WriteForeignVariant (Cons name ty tail) row where
  writeVariantImpl _ variant =
    on
      namep
      writeVariant
      (writeVariantImpl (RLProxy :: RLProxy tail))
      variant
    where
    namep = SProxy :: SProxy name
    writeVariant value =
      unsafeToForeign
        { type: reflectSymbol namep
        , value: writeImpl value
        }

instance readForeignNEArray :: ReadForeign a => ReadForeign (NonEmptyArray a) where
  readImpl f = do
    raw :: Array a <- readImpl f
    except $ note (singleton $ ForeignError "Nonempty array expected, got empty array") $ fromArray raw

instance writeForeignNEArray :: WriteForeign a => WriteForeign (NonEmptyArray a) where
  writeImpl a = writeImpl <<< toArray $ a

-- | Classes for writing a value into a JSON Object Key
class ReadForeignKey a where
  readKeyImpl :: Foreign -> F a

instance ReadForeignKey String where
  readKeyImpl = readImpl

instance ReadForeignKey NonEmptyString where
  readKeyImpl = readImpl

class WriteForeignKey a where
  writeKeyImpl :: a -> Foreign

instance WriteForeignKey String where
  writeKeyImpl = writeImpl

instance WriteForeignKey NonEmptyString where
  writeKeyImpl = unsafeToForeign <<< toString

foreign import base64Encode :: Binary -> Foreign
foreign import base64Decode :: Foreign -> Maybe Binary
