module Common where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString qualified as Bytestring
import Data.Char (chr, ord)
import Data.Double.Conversion.Text qualified
import Data.Fixed qualified
import Data.Foldable qualified
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.List (lookup, sort, sortBy)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.IO qualified as TLIO
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Deque.Lazy (Deque)
import Deque.Lazy qualified as Deque
import GHC.IsList qualified
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath (dropFileName, joinPath)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

type UVector = UVector.Vector

type Positional = [Value]

type Named = HashMap String Value

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
  | String String TB.Builder (UVector Char)
  | Number Double
  | Array (Deque (Vector Value)) (Vector Value)
  | Function Int (Arguments -> Value)
  | Object Asserts Fields

throwError :: String -> a
throwError = error

makeString :: String -> Value
makeString s = String s (TB.fromString s) (UVector.fromList s)

makeArray :: Vector Value -> Value
makeArray v = Array (GHC.IsList.fromList [v]) v

makeArrayFromList :: [Value] -> Value
makeArrayFromList a = makeArray $ Vector.fromList a

getBool :: Value -> Bool
getBool (Bool b) = b
getBool _ = throwError "not bool"

getString :: Value -> String
getString (String s _ _) = s
getString _ = throwError "not string"

getNumber :: Value -> Double
getNumber (Number n) = n
getNumber _ = throwError "not number"

getArray :: Value -> Vector Value
getArray (Array _ v) = v
getArray _ = throwError "not array"

getFunction :: Value -> (Arguments -> Value)
getFunction (Function _ f) = f
getFunction _ = throwError "not function"

getObject :: Value -> (Asserts, Fields)
getObject (Object asserts fields) = (asserts, fields)
getObject _ = throwError "not object"

evalAsserts :: Asserts -> Fields -> ()
evalAsserts asserts fields =
  unsafePerformIO $
    forM_ asserts $ \f ->
      evaluate $ f fields emptyObjectFields

stdCmp :: Value -> Value -> Ordering
stdCmp (Number n1) (Number n2) = compare n1 n2
stdCmp (String s1 _ _) (String s2 _ _) = compare s1 s2
stdCmp (Array _ a1) (Array _ a2) = Vector.cmpBy stdCmp a1 a2
stdCmp _ _ = throwError "stdCmp: invalid arguments"

valueToTextBuilder :: Value -> TB.Builder
valueToTextBuilder Null = TB.fromString "null"
valueToTextBuilder (Bool True) = TB.fromString "true"
valueToTextBuilder (Bool False) = TB.fromString "false"
valueToTextBuilder (Number n) = TB.fromText $ Data.Double.Conversion.Text.toShortest n
valueToTextBuilder x@(Array _ _) = TB.fromLazyText $ manifestation False x
valueToTextBuilder x@(Object _ _) = TB.fromLazyText $ manifestation False x
valueToTextBuilder _ = throwError "valueToLazyText: not expected type"

binaryAdd :: Value -> Value -> Value
binaryAdd (Number n1) (Number n2) = Number (n1 + n2)
binaryAdd (Array a1 _) (Array a2 _) =
  let a = a1 <> a2
   in Array a $ Vector.concat $ GHC.IsList.toList a
binaryAdd (String s1 b1 v1) (String s2 b2 v2) =
  let b = b1 <> b2
   in String (TL.unpack $ TB.toLazyText b) b (UVector.fromList $ TL.unpack $ TB.toLazyText b)
binaryAdd (String s1 b1 v1) rhs =
  let b = b1 <> valueToTextBuilder rhs
      s = TL.unpack $ TB.toLazyText b
   in String s b (UVector.fromList s)
binaryAdd lhs (String s2 b2 v2) = do
  let b = valueToTextBuilder lhs <> b2
      s = TL.unpack $ TB.toLazyText b
   in String s b (UVector.fromList s)
binaryAdd (Object asserts1 fields1@(GeneralFields m1)) (Object asserts2 (GeneralFields m2)) =
  Object (asserts1 ++ map (\v self _ -> v self fields1) asserts2) $
    fillObjectCache $
      GeneralFields
        ( HashMap.unionWith
            (\(h1, _, _) (h2, _, v2) -> (if h2 == 1 then h1 else h2, Null, v2))
            m1
            (HashMap.map (\(h, _, v) -> (h, Null, \self _ -> v self fields1)) m2)
        )
binaryAdd _ _ = throwError "binaryAdd: invalid values"

binarySub :: Value -> Value -> Value
binarySub v1 v2 =
  Number $ getNumber v1 - getNumber v2

binaryMult :: Value -> Value -> Value
binaryMult v1 v2 =
  Number $ getNumber v1 * getNumber v2

binaryDiv :: Value -> Value -> Value
binaryDiv v1 v2 = do
  Number $ getNumber v1 / getNumber v2

binaryAnd :: Value -> Value -> Value
binaryAnd (Bool False) _ = Bool False
binaryAnd _ (Bool b) = Bool b
binaryAnd _ _ = error "binaryAnd: not bool"

binaryOr :: Value -> Value -> Value
binaryOr (Bool True) _ = Bool True
binaryOr _ (Bool b) = Bool b
binaryOr _ _ = error "binaryOr: not bool"

binaryLand :: Value -> Value -> Value
binaryLand v1 v2 =
  let l = truncate $ getNumber v1
      r = truncate $ getNumber v2
   in Number $ fromIntegral (l .&. r :: Int)

binaryLor :: Value -> Value -> Value
binaryLor v1 v2 =
  let l = truncate $ getNumber v1
      r = truncate $ getNumber v2
   in Number $ fromIntegral (l .|. r :: Int)

binaryXor :: Value -> Value -> Value
binaryXor v1 v2 =
  let l = truncate $ getNumber v1
      r = truncate $ getNumber v2
   in Number $ fromIntegral (l `xor` r :: Int)

binaryLsl :: Value -> Value -> Value
binaryLsl v1 v2 =
  let l = truncate $ getNumber v1
      r = truncate $ getNumber v2
   in Number $ fromIntegral (l `shiftL` r :: Int)

binaryLsr :: Value -> Value -> Value
binaryLsr v1 v2 =
  let l = truncate $ getNumber v1
      r = truncate $ getNumber v2
   in Number $ fromIntegral (l `shiftR` r :: Int)

binaryLt :: Value -> Value -> Value
binaryLt v1 v2 = Bool $ stdCmp v1 v2 == LT

binaryLe :: Value -> Value -> Value
binaryLe v1 v2 =
  let x = stdCmp v1 v2
   in Bool $ x == LT || x == EQ

binaryGt :: Value -> Value -> Value
binaryGt v1 v2 = Bool $ stdCmp v1 v2 == GT

binaryGe :: Value -> Value -> Value
binaryGe v1 v2 =
  let x = stdCmp v1 v2
   in Bool $ x == GT || x == EQ

unaryNot :: Value -> Value
unaryNot v =
  Bool $ not $ getBool v

unaryLnot :: Value -> Value
unaryLnot v =
  Number $ fromIntegral $ complement (truncate $ getNumber v :: Int)

unaryNeg :: Value -> Value
unaryNeg v = Number $ -(getNumber v)

unaryPos :: Value -> Value
unaryPos v = Number $ getNumber v

if_ :: Value -> Value -> Value -> Value
if_ v1 v2 v3 = if getBool v1 then v2 else v3

functionParam :: Arguments -> Int -> String -> Maybe Value -> Value
functionParam (positional, named) i id v =
  if i < length positional
    then positional !! i
    else case HashMap.lookup id named of
      Just x -> x
      Nothing -> case v of Just v -> v; Nothing -> throwError "parameter not bound"

inSuper :: Fields -> Value -> Value
inSuper (GeneralFields super) key =
  Bool $ HashMap.member (getString key) super

superIndex :: Fields -> Value -> Value
superIndex super@(GeneralFields superFields) key =
  let key' = getString key
   in case HashMap.lookup key' superFields of
        Nothing -> throwError ("field does not exist: " ++ key')
        Just (_, v, _) -> v

arrayIndex :: Value -> Value -> Value
arrayIndex (Array _ a) v2 = a Vector.! truncate (getNumber v2)
arrayIndex (String _ _ s) v2 =
  let c = s UVector.! truncate (getNumber v2)
   in String [c] (TB.fromString [c]) (UVector.singleton c)
arrayIndex (Object asserts self@(GeneralFields fields)) v2 =
  evalAsserts asserts self `seq`
    let x = getString v2
     in case HashMap.lookup x fields of
          Just (_, v, _) -> v
          Nothing ->
            let x = getString v2
             in throwError ("object field not found: " ++ x)
arrayIndex _ _ = throwError "arrayIndex: invalid value"

objectField :: Int -> Value -> (Fields -> Fields -> Value) -> Fields -> Fields
objectField h k v fields =
  case k of
    Null -> fields
    String k _ _ ->
      case fields of
        GeneralFields m ->
          GeneralFields $ HashMap.insert k (h, Null, v) m
    _ -> throwError "objectField: not valid key type"

fillObjectCache :: Fields -> Fields
fillObjectCache (GeneralFields m) =
  let self = GeneralFields $ HashMap.map (\(h, _, f) -> (h, f self emptyObjectFields, f)) m
   in self

objectFor :: (Fields -> Value -> Fields) -> Value -> Fields
objectFor f e3 =
  fillObjectCache $ Vector.foldl f emptyObjectFields $ getArray e3

error' :: Value -> a
error' v = throwError $ TL.unpack $ manifestation True v

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
  let indent = TB.fromString $ if multiLine then replicate ind ' ' else ""
      newline = TB.fromString $ if multiLine then "\n" else ""
      initInd = if hasInitInd then indent else TB.fromString ""
   in case v of
        Null -> initInd <> TB.fromString "null"
        (Bool True) -> initInd <> TB.fromString "true"
        (Bool False) -> initInd <> TB.fromString "false"
        (String s _ _) -> initInd <> quoteString s
        (Number n) -> initInd <> TB.fromText (Data.Double.Conversion.Text.toShortest n)
        (Array _ xs) ->
          if Vector.null xs
            then initInd <> TB.fromString "[ ]"
            else
              let bs = Vector.map (manifestation' True (ind + 3) multiLine) xs
               in initInd
                    <> TB.fromString "["
                    <> newline
                    <> Vector.ifoldr
                      ( \i b acc ->
                          b
                            <> TB.fromString
                              ( if i == Vector.length xs - 1
                                  then ""
                                  else (if multiLine then "," else ", ")
                              )
                            <> newline
                            <> acc
                      )
                      (indent <> TB.fromString "]")
                      bs
        (Object asserts fields) ->
          evalAsserts asserts fields `seq`
            case reverse $ extractVisibleFields fields of
              [] -> initInd <> TB.fromString "{ }"
              xs ->
                let bs =
                      map
                        ( \(k, v) ->
                            let v' = manifestation' False (ind + 3) multiLine v
                             in (k, v')
                        )
                        xs
                    encode (k, b) =
                      TB.fromString (if multiLine then replicate (ind + 3) ' ' else "")
                        <> quoteString k
                        <> TB.fromString ": "
                        <> b
                 in case bs of
                      [] -> error "unreachable"
                      hd : tl ->
                        initInd
                          <> TB.fromString "{"
                          <> newline
                          <> foldl
                            ( \b x ->
                                encode x
                                  <> TB.fromString (if multiLine then "," else ", ")
                                  <> newline
                                  <> b
                            )
                            (encode hd <> newline <> indent <> TB.fromString "}")
                            tl

manifestation :: Bool -> Value -> TL.Text
manifestation multi x = TB.toLazyText $ manifestation' True 0 multi x

mainNormal :: Value -> IO ()
mainNormal v = TLIO.putStrLn $ manifestation True v

mainString :: Value -> IO ()
mainString (String s _ _) = putStr s
mainString _ = error "stringManifestation: not string"

mainMulti :: String -> Bool -> Value -> IO ()
mainMulti targetDir string (Object _ fields) =
  forM_ (extractVisibleFields fields) $ \(k, v) ->
    let filePath = joinPath [targetDir, k]
     in case (v, string) of
          (String s _ _, True) -> do
            createDirectoryIfMissing True $ dropFileName filePath
            writeFile filePath s
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
          (\i -> func ([Number (fromIntegral i)], HashMap.empty))

stdPrimitiveEquals :: Arguments -> Value
stdPrimitiveEquals args =
  let x = functionParam args 0 "x" Nothing
      y = functionParam args 1 "y" Nothing
   in case (x, y) of
        (Null, Null) -> Bool True
        (Bool lhs, Bool rhs) -> Bool $ lhs == rhs
        (String lhs _ _, String rhs _ _) -> Bool $ lhs == rhs
        (Number lhs, Number rhs) -> Bool $ lhs == rhs
        _ -> Bool False

stdLength :: Arguments -> Value
stdLength args =
  let x = functionParam args 0 "x" Nothing
   in (Number . fromIntegral) $
        case x of
          Array _ xs -> Vector.length xs
          String _ _ xs -> UVector.length xs
          Object _ (GeneralFields fields) -> HashMap.size $ HashMap.filter (\(h, _, _) -> h /= 2) fields
          Function n _ -> n
          _ -> throwError "std.length: invalid type argument"

stdType :: Arguments -> Value
stdType args =
  let x = functionParam args 0 "x" Nothing
   in makeString $
        case x of
          Null -> "null"
          Bool True -> "boolean"
          Bool False -> "boolean"
          String{} -> "string"
          Function _ _ -> "function"
          Number _ -> "number"
          Array _ _ -> "array"
          Object _ _ -> "object"

stdFilter :: Arguments -> Value
stdFilter args =
  let func = getFunction $ functionParam args 0 "func" Nothing
      arr = getArray $ functionParam args 1 "arr" Nothing
   in makeArray $ Vector.filter (\x -> getBool $ func ([x], HashMap.empty)) arr

stdObjectHasEx :: Arguments -> Value
stdObjectHasEx args =
  let (_, GeneralFields fields) = getObject $ functionParam args 0 "obj" Nothing
      f = getString $ functionParam args 1 "f" Nothing
      b' = getBool $ functionParam args 2 "b'" Nothing
   in case HashMap.lookup f fields of
        Nothing -> Bool False
        Just (h, _, _) -> Bool (h /= 2 || b')

stdObjectFieldsEx :: Arguments -> Value
stdObjectFieldsEx args =
  let (_, GeneralFields fields) = getObject $ functionParam args 0 "obj" Nothing
      b' = getBool $ functionParam args 1 "b'" Nothing
   in makeArrayFromList $
        map makeString $
          sort $
            map fst $
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
   in Number $ fromIntegral $ ord $ head str

stdChar :: Arguments -> Value
stdChar args =
  let x = getNumber $ functionParam args 0 "n" Nothing
      c = chr $ truncate x
   in String [c] (TB.fromString [c]) (UVector.singleton c)

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
        Bytestring.foldl (\acc x -> Number (fromIntegral x) : acc) [] body

readStr :: String -> IO Value
readStr path = do
  body <- readFile path
  pure $ makeString body
