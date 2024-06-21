module Common where

import Control.Monad (forM_)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as Bytestring
import Data.Char (chr, ord)
import qualified Data.Double.Conversion.Text
import qualified Data.Fixed
import qualified Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (lookup, sort, sortBy)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TLIO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import Deque.Lazy (Deque)
import qualified Deque.Lazy as Deque
import qualified GHC.IsList
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, joinPath)

type UVector = UVector.Vector

type Positional = [Value]

type Named = [(String, Value)]

type Arguments = (Positional, Named)

type Asserts = [Fields {- self -} -> Fields {- super -} -> Value]

data Fields
  = GeneralFields
      ( HashMap
          String
          (Int, Value, Fields {- self -} -> Fields {- super -} -> Value)
      )

emptyObjectFields :: Fields
emptyObjectFields = GeneralFields HashMap.empty

data Value
  = Null
  | Bool Bool
  | String TL.Text (UVector Char)
  | Number Double
  | Array (Deque (Vector Value)) (Vector Value)
  | Function Int (Arguments -> Value)
  | Object Asserts Fields

makeString :: String -> Value
makeString s = String (TL.pack s) (UVector.fromList s)

makeArray :: Vector Value -> Value
makeArray v = Array (GHC.IsList.fromList [v]) v

makeArrayFromList :: [Value] -> Value
makeArrayFromList a = makeArray $ Vector.fromList a

stdCmp :: Value -> Value -> Ordering
stdCmp (Number n1) (Number n2) = compare n1 n2
stdCmp (String s1 _) (String s2 _) = compare s1 s2
stdCmp (Array _ a1) (Array _ a2) = Vector.cmpBy stdCmp a1 a2
stdCmp _ _ = error "stdCmp: invalid arguments"

valueToLazyText :: Value -> TL.Text
valueToLazyText Null = TL.pack "null"
valueToLazyText (Bool True) = TL.pack "true"
valueToLazyText (Bool False) = TL.pack "false"
valueToLazyText (Number n) = TL.fromStrict $ Data.Double.Conversion.Text.toShortest n
valueToLazyText x =
  case x of
    Array _ _ -> manifestation False x
    Object _ _ -> manifestation False x
    _ -> error "valueToLazyText: not expected type"

binaryAdd :: Value -> Value -> Value
binaryAdd (Number n1) (Number n2) = Number (n1 + n2)
binaryAdd (Array a1 _) (Array a2 _) =
  let a = a1 <> a2
   in Array a $ Vector.concat $ GHC.IsList.toList a
binaryAdd (String s1 v1) (String s2 v2) = String (TL.append s1 s2) (v1 UVector.++ v2)
binaryAdd (String s1 v1) rhs =
  let t = TL.append s1 (valueToLazyText rhs)
   in String t (UVector.fromList $ TL.unpack t)
binaryAdd lhs (String s2 v2) =
  let t = TL.append (valueToLazyText lhs) s2
   in String t (UVector.fromList $ TL.unpack t)
binaryAdd (Object asserts1 fields1@(GeneralFields m1)) (Object asserts2 (GeneralFields m2)) =
  Object (asserts1 ++ map (\v self _ -> v self fields1) asserts2) $
    fillObjectCache $
      GeneralFields
        ( HashMap.unionWith
            (\(h1, _, _) (h2, _, v2) -> (if h2 == 1 then h1 else h2, Null, v2))
            m1
            (HashMap.map (\(h, _, v) -> (h, Null, \self _ -> v self fields1)) m2)
        )
binaryAdd _ _ = error "binaryAdd: invalid values"

binarySub :: Value -> Value -> Value
binarySub v1 v2 = Number $ getNumber v1 - getNumber v2

binaryMult :: Value -> Value -> Value
binaryMult v1 v2 = Number $ getNumber v1 * getNumber v2

binaryDiv :: Value -> Value -> Value
binaryDiv v1 v2 = Number $ getNumber v1 / getNumber v2

binaryAnd :: Value -> Value -> Value
binaryAnd v1 v2 = Bool $ getBool v1 && getBool v2

binaryOr :: Value -> Value -> Value
binaryOr v1 v2 = Bool $ getBool v1 || getBool v2

binaryLand :: Value -> Value -> Value
binaryLand v1 v2 =
  let v = truncate (getNumber v1) .&. truncate (getNumber v2) :: Int
   in Number $ fromIntegral v

binaryLor :: Value -> Value -> Value
binaryLor v1 v2 =
  let v = truncate (getNumber v1) .|. truncate (getNumber v2) :: Int
   in Number $ fromIntegral v

binaryXor :: Value -> Value -> Value
binaryXor v1 v2 =
  let v = truncate (getNumber v1) `xor` truncate (getNumber v2) :: Int
   in Number $ fromIntegral v

binaryLsl :: Value -> Value -> Value
binaryLsl v1 v2 =
  let v = truncate (getNumber v1) `shiftL` truncate (getNumber v2) :: Int
   in Number $ fromIntegral v

binaryLsr :: Value -> Value -> Value
binaryLsr v1 v2 =
  let v = truncate (getNumber v1) `shiftR` truncate (getNumber v2) :: Int
   in Number $ fromIntegral v

binaryLt :: Value -> Value -> Value
binaryLt v1 v2 = Bool $ stdCmp v1 v2 == LT

binaryLe :: Value -> Value -> Value
binaryLe v1 v2 = Bool $ stdCmp v1 v2 == LT || stdCmp v1 v2 == EQ

binaryGt :: Value -> Value -> Value
binaryGt v1 v2 = Bool $ stdCmp v1 v2 == GT

binaryGe :: Value -> Value -> Value
binaryGe v1 v2 = Bool $ stdCmp v1 v2 == GT || stdCmp v1 v2 == EQ

unaryNot :: Value -> Value
unaryNot v = Bool $ not $ getBool v

unaryLnot :: Value -> Value
unaryLnot v =
  let v' = complement $ truncate $ getNumber v :: Int
   in Number $ fromIntegral v'

unaryNeg :: Value -> Value
unaryNeg v = Number $ -(getNumber v)

unaryPos :: Value -> Value
unaryPos v = Number $ (getNumber v)

if_ :: Value -> Value -> Value -> Value
if_ v1 v2 v3 = if getBool v1 then v2 else v3

getBool :: Value -> Bool
getBool (Bool b) = b
getBool _ = error "not bool"

getString :: Value -> TL.Text
getString (String s _) = s
getString _ = error "not string"

getNumber :: Value -> Double
getNumber (Number n) = n
getNumber _ = error "not number"

getArray :: Value -> Vector Value
getArray (Array _ v) = v
getArray _ = error "not array"

getFunction :: Value -> Arguments -> Value
getFunction (Function _ f) = f
getFunction _ = error "not function"

getObject :: Value -> (Asserts, Fields)
getObject (Object asserts fields) = (asserts, fields)
getObject _ = error "not object"

functionParam :: Arguments -> Int -> String -> Maybe Value -> Value
functionParam (positional, named) i id v =
  if i < length positional
    then positional !! i
    else case lookup id named of
      Just x -> x
      Nothing -> case v of Just v -> v; Nothing -> error "parameter not bound"

inSuper :: Fields -> Value -> Value
inSuper (GeneralFields super) key =
  Bool $ HashMap.member (TL.unpack $ getString key) super

superIndex :: Fields -> Value -> Value
superIndex super@(GeneralFields superFields) key =
  let key' = TL.unpack $ getString key
   in case HashMap.lookup key' superFields of
        Nothing -> error ("field does not exist: " ++ key')
        Just (_, v, _) -> v

arrayIndex :: Value -> Value -> Value
arrayIndex (Array _ a) v2 = a Vector.! truncate (getNumber v2)
arrayIndex (String _ s) v2 =
  let c = s UVector.! truncate (getNumber v2)
   in String (TL.singleton c) (UVector.singleton c)
arrayIndex (Object _ self@(GeneralFields fields)) v2 =
  case HashMap.lookup (TL.unpack $ getString v2) fields of
    Nothing -> error ("object field not found: " ++ TL.unpack (getString v2))
    Just (_, v, _) -> v

objectField :: Int -> Value -> (Fields -> Fields -> Value) -> Fields -> Fields
objectField h k v fields@(GeneralFields m) =
  case k of
    Null -> fields
    String k _ -> GeneralFields $ HashMap.insert (TL.unpack k) (h, Null, v) m

fillObjectCache :: Fields -> Fields
fillObjectCache (GeneralFields m) =
  let self = GeneralFields $ HashMap.map (\(h, _, f) -> (h, f self emptyObjectFields, f)) m
   in self

objectFor :: (Fields -> Value -> Fields) -> Value -> Fields
objectFor f e3 = fillObjectCache $ Vector.foldl f emptyObjectFields $ getArray e3

error' :: Value -> a
error' v =
  error $ TL.unpack $ manifestation True v

objectFieldPlusValue :: Fields -> Value -> Value -> Value
objectFieldPlusValue super e1 e2 =
  if getBool $ inSuper super e1
    then binaryAdd (superIndex super e1) e2
    else e2

extractVisibleFields :: Fields -> [(String, Value)]
extractVisibleFields fields@(GeneralFields m) =
  map (\(k, (_, v, _)) -> (k, v)) $
    sortBy (\(k1, _) (k2, _) -> compare k1 k2) $
      filter (\(_, (h, _, _)) -> h /= 2) $
        HashMap.toList m

quoteString :: String -> TB.Builder
quoteString xs =
  let escape x = case x of
        '"' -> TB.fromString "\\\""
        '\\' -> TB.fromString "\\\\"
        '\b' -> TB.fromString "\\b"
        '\f' -> TB.fromString "\\f"
        '\n' -> TB.fromString "\\n"
        '\r' -> TB.fromString "\\r"
        '\t' -> TB.fromString "\\t"
        '\0' -> TB.fromString "\\u0000"
        x -> TB.singleton x
   in TB.singleton '"'
        <> foldr
          (\x a -> escape x <> a)
          (TB.singleton '"')
          xs

manifestation' :: Bool -> Int -> Bool -> Value -> TB.Builder
manifestation' hasInitInd ind multiLine v =
  let indent = TB.fromString $ if multiLine then take ind (repeat ' ') else ""
      newline = TB.fromString $ if multiLine then "\n" else ""
      initInd = if hasInitInd then indent else TB.fromString ""
   in case v of
        Null -> initInd <> TB.fromString "null"
        (Bool True) -> initInd <> TB.fromString "true"
        (Bool False) -> initInd <> TB.fromString "false"
        (String s _) -> initInd <> quoteString (TL.unpack s)
        (Number n) -> initInd <> TB.fromText (Data.Double.Conversion.Text.toShortest n)
        (Array _ xs) ->
          initInd
            <> if Vector.null xs
              then TB.fromString "[ ]"
              else
                TB.fromString "["
                  <> newline
                  <> Vector.ifoldr
                    ( \i x b ->
                        manifestation' True (ind + 3) multiLine x
                          <> TB.fromString
                            (if i == Vector.length xs - 1 then "" else (if multiLine then "," else ", "))
                          <> newline
                          <> b
                    )
                    (indent <> TB.fromString "]")
                    xs
        (Object _ fields) ->
          let encode (k, v) =
                TB.fromString (if multiLine then take (ind + 3) (repeat ' ') else "")
                  <> quoteString k
                  <> TB.fromString ": "
                  <> manifestation' False (ind + 3) multiLine v
           in initInd <> case reverse $ extractVisibleFields fields of
                [] -> TB.fromString "{ }"
                hd : tl ->
                  TB.fromString "{"
                    <> newline
                    <> foldl
                      (\b x -> encode x <> TB.fromString (if multiLine then "," else ", ") <> newline <> b)
                      (encode hd <> newline <> indent <> TB.fromString "}")
                      tl

manifestation :: Bool -> Value -> TL.Text
manifestation multi x =
  TB.toLazyText $ manifestation' True 0 multi x

mainNormal :: Value -> IO ()
mainNormal v = TLIO.putStrLn $ manifestation True v

mainString :: Value -> IO ()
mainString v@(String s _) = TLIO.putStr s
mainString _ = error "stringManifestation: not string"

mainMulti :: String -> Bool -> Value -> IO ()
mainMulti targetDir string v@(Object _ fields) =
  forM_ (extractVisibleFields fields) $ \(k, v) ->
    let filePath = joinPath [targetDir, k]
     in case (v, string) of
          (String s _, True) -> do
            createDirectoryIfMissing True $ dropFileName filePath
            TLIO.writeFile filePath s
          (_, True) -> error "multiManifestation: expect string values in objects"
          (_, False) -> do
            createDirectoryIfMissing True $ dropFileName filePath
            TLIO.writeFile filePath $ manifestation True v

stdMakeArray :: Arguments -> Value
stdMakeArray args =
  let sz = getNumber $ functionParam args 0 "sz" Nothing
      func = getFunction $ functionParam args 1 "func" Nothing
   in makeArray $
        Vector.generate
          (truncate sz)
          (\i -> func ([Number (fromIntegral i)], []))

stdPrimitiveEquals :: Arguments -> Value
stdPrimitiveEquals args =
  case ((functionParam args 0 "x" Nothing), (functionParam args 1 "y" Nothing)) of
    (Null, Null) -> Bool True
    (Bool lhs, Bool rhs) -> Bool (lhs == rhs)
    (String lhs _, String rhs _) -> Bool (lhs == rhs)
    (Number lhs, Number rhs) -> Bool (lhs == rhs)
    _ -> Bool False

stdLength :: Arguments -> Value
stdLength args =
  Number $
    fromIntegral $
      case functionParam args 0 "x" Nothing of
        Array _ xs -> Vector.length xs
        String _ xs -> UVector.length xs
        Object _ (GeneralFields fields) -> HashMap.size $ HashMap.filter (\(h, _, _) -> h /= 2) fields
        Function n _ -> n
        _ -> error "std.length: invalid type argument"

stdType :: Arguments -> Value
stdType args =
  makeString $
    case functionParam args 0 "x" Nothing of
      Null -> "null"
      Bool True -> "boolean"
      Bool False -> "boolean"
      String _ _ -> "string"
      Function _ _ -> "function"
      Number _ -> "number"
      Array _ _ -> "array"
      Object _ _ -> "object"

stdFilter :: Arguments -> Value
stdFilter args =
  let func = getFunction $ functionParam args 0 "func" Nothing
      arr = getArray $ functionParam args 1 "arr" Nothing
   in makeArray $ Vector.filter (\x -> getBool $ func ([x], [])) arr

stdObjectHasEx :: Arguments -> Value
stdObjectHasEx args =
  let (_, (GeneralFields fields)) = getObject $ functionParam args 0 "obj" Nothing
      f = getString $ functionParam args 1 "f" Nothing
      b' = getBool $ functionParam args 2 "b'" Nothing
   in case HashMap.lookup (TL.unpack f) fields of
        Nothing -> Bool False
        Just (h, _, _) -> Bool (h /= 2 || b')

stdObjectFieldsEx :: Arguments -> Value
stdObjectFieldsEx args =
  let (_, (GeneralFields fields)) = getObject $ functionParam args 0 "obj" Nothing
      b' = getBool $ functionParam args 1 "b'" Nothing
   in makeArrayFromList $
        map makeString $
          sort $
            map (\(k, _) -> k) $
              filter (\(_, (h, _, _)) -> h /= 2 || b') $
                HashMap.toList fields

stdModulo :: Arguments -> Value
stdModulo args =
  let a = getNumber $ functionParam args 0 "a" Nothing
      b = getNumber $ functionParam args 1 "b" Nothing
   in Number $ Data.Fixed.mod' a b

stdCodepoint :: Arguments -> Value
stdCodepoint args =
  let str = getString $ functionParam args 0 "str" Nothing
   in Number $ fromIntegral $ ord $ TL.index str 0

stdChar :: Arguments -> Value
stdChar args =
  let c = chr $ truncate $ getNumber $ functionParam args 0 "n" Nothing
   in String (TL.singleton c) (UVector.singleton c)

stdFloor :: Arguments -> Value
stdFloor args =
  let x = getNumber $ functionParam args 0 "x" Nothing
   in Number $ fromInteger $ floor x

stdPow :: Arguments -> Value
stdPow args =
  let x = getNumber $ functionParam args 0 "x" Nothing
      n = getNumber $ functionParam args 1 "n" Nothing
   in Number $ x ** n

stdLog :: Arguments -> Value
stdLog args =
  let x = getNumber $ functionParam args 0 "x" Nothing
   in Number $ log x

insertStd :: Fields -> Fields
insertStd (GeneralFields fields) =
  GeneralFields $
    foldl
      (\a (k, v) -> HashMap.insert k v a)
      fields
      [ ("primitiveEquals", (2, Null, \_ _ -> Function 2 stdPrimitiveEquals))
      , ("length", (2, Null, \_ _ -> Function 1 stdLength))
      , ("makeArray", (2, Null, \_ _ -> Function 2 stdMakeArray))
      , ("type", (2, Null, \_ _ -> Function 1 stdType))
      , ("filter", (2, Null, \_ _ -> Function 2 stdFilter))
      , ("objectHasEx", (2, Null, \_ _ -> Function 3 stdObjectHasEx))
      , ("objectFieldsEx", (2, Null, \_ _ -> Function 2 stdObjectFieldsEx))
      , ("modulo", (2, Null, \_ _ -> Function 2 stdModulo))
      , ("codepoint", (2, Null, \_ _ -> Function 1 stdCodepoint))
      , ("char", (2, Null, \_ _ -> Function 1 stdChar))
      , ("floor", (2, Null, \_ _ -> Function 1 stdFloor))
      , ("log", (2, Null, \_ _ -> Function 1 stdLog))
      , ("pow", (2, Null, \_ _ -> Function 2 stdPow))
      ]

readBin :: String -> IO Value
readBin path = do
  body <- Bytestring.readFile path
  pure $
    makeArrayFromList $
      reverse $
        Bytestring.foldl (\acc x -> (Number $ fromIntegral x) : acc) [] body

readStr :: String -> IO Value
readStr path = do
  body <- readFile path
  pure $ makeString body
