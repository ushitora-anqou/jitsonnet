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
import System.Exit
import System.FilePath (dropFileName, joinPath)
import System.IO

type UVector = UVector.Vector

type Positional = [Result Value]

type Named = [(String, Result Value)]

type Arguments = (Positional, Named)

type Asserts = [Fields {- self -} -> Fields {- super -} -> Result Value]

data Fields
  = GeneralFields
      ( HashMap
          String
          (Int, Result Value, Fields {- self -} -> Fields {- super -} -> Result Value)
      )

emptyObjectFields :: Fields
emptyObjectFields = GeneralFields HashMap.empty

data Value
  = Null
  | Bool Bool
  | String String TB.Builder (UVector Char)
  | Number Double
  | Array (Deque (Vector (Result Value))) (Vector (Result Value))
  | Function Int (Arguments -> Result Value)
  | Object Asserts Fields

data Result a = Error String | Ok a

instance Functor Result where
  fmap f (Ok x) = Ok (f x)
  fmap f (Error e) = Error e

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)

instance Monad Result where
  (Ok x) >>= f = f x
  (Error e) >>= _ = Error e

throwError :: String -> Result a
throwError msg = Error msg

makeString :: String -> Result Value
makeString s = return $ String s (TB.fromString s) (UVector.fromList s)

makeArray :: Vector (Result Value) -> Result Value
makeArray v = return $ Array (GHC.IsList.fromList [v]) v

makeArrayFromList :: [Result Value] -> Result Value
makeArrayFromList a = makeArray $ Vector.fromList a

getBool :: Result Value -> Result Bool
getBool x = do
  x' <- x
  case x' of
    Bool b -> return b
    _ -> throwError "not bool"

getString :: Result Value -> Result String
getString x = do
  x' <- x
  case x' of
    String s _ _ -> return s
    _ -> throwError "not string"

getNumber :: Result Value -> Result Double
getNumber x = do
  x' <- x
  case x' of
    Number n -> return n
    _ -> throwError "not number"

getArray :: Result Value -> Result (Vector (Result Value))
getArray x = do
  x' <- x
  case x' of
    Array _ v -> return v
    _ -> throwError "not array"

getFunction :: Result Value -> Result (Arguments -> Result Value)
getFunction x = do
  x' <- x
  case x' of
    Function _ f -> return f
    _ -> throwError "not function"

getObject :: Result Value -> Result (Asserts, Fields)
getObject x = do
  x' <- x
  case x' of
    Object asserts fields -> return (asserts, fields)
    _ -> throwError "not object"

evalAsserts :: Asserts -> Fields -> Result ()
evalAsserts asserts fields =
  forM_ asserts (\f -> f fields emptyObjectFields)

stdCmp :: Result Value -> Result Value -> Result Ordering
stdCmp lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  aux lhs' rhs'
 where
  aux (Number n1) (Number n2) = return $ compare n1 n2
  aux (String s1 _ _) (String s2 _ _) = return $ compare s1 s2
  aux (Array _ a1) (Array _ a2) = do
    x <-
      Vector.foldM
        ( \a (x, y) ->
            case a of
              EQ -> stdCmp x y
              a -> return a
        )
        EQ
        $ Vector.zip a1 a2
    case x of
      LT -> return LT
      GT -> return GT
      EQ -> return $ compare (Vector.length a1) (Vector.length a2)
  aux _ _ = throwError "stdCmp: invalid arguments"

valueToTextBuilder :: Value -> Result TB.Builder
valueToTextBuilder Null = return $ TB.fromString "null"
valueToTextBuilder (Bool True) = return $ TB.fromString "true"
valueToTextBuilder (Bool False) = return $ TB.fromString "false"
valueToTextBuilder (Number n) = return $ TB.fromText $ Data.Double.Conversion.Text.toShortest n
valueToTextBuilder x@(Array _ _) = do
  x' <- manifestation False (return x)
  return $ TB.fromLazyText x'
valueToTextBuilder x@(Object _ _) = do
  x' <- manifestation False (return x)
  return $ TB.fromLazyText x'
valueToTextBuilder _ = throwError "valueToLazyText: not expected type"

binaryAdd :: Result Value -> Result Value -> Result Value
binaryAdd lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  aux lhs' rhs'
 where
  aux (Number n1) (Number n2) = return $ Number (n1 + n2)
  aux (Array a1 _) (Array a2 _) =
    let a = a1 <> a2
     in return $ Array a $ Vector.concat $ GHC.IsList.toList a
  aux (String s1 b1 v1) (String s2 b2 v2) =
    let b = b1 <> b2
     in return $
          String (TL.unpack $ TB.toLazyText b) b (UVector.fromList $ TL.unpack $ TB.toLazyText b)
  aux (String s1 b1 v1) rhs = do
    rhs' <- valueToTextBuilder rhs
    let b = b1 <> rhs'
        s = TL.unpack $ TB.toLazyText b
    return $ String s b (UVector.fromList s)
  aux lhs (String s2 b2 v2) = do
    lhs' <- valueToTextBuilder lhs
    let b = lhs' <> b2
        s = TL.unpack $ TB.toLazyText b
     in return $ String s b (UVector.fromList s)
  aux (Object asserts1 fields1@(GeneralFields m1)) (Object asserts2 (GeneralFields m2)) =
    return $
      Object (asserts1 ++ map (\v self _ -> v self fields1) asserts2) $
        fillObjectCache $
          GeneralFields
            ( HashMap.unionWith
                (\(h1, _, _) (h2, _, v2) -> (if h2 == 1 then h1 else h2, Ok Null, v2))
                m1
                (HashMap.map (\(h, _, v) -> (h, Ok Null, \self _ -> v self fields1)) m2)
            )
  aux _ _ = throwError "binaryAdd: invalid values"

binarySub :: Result Value -> Result Value -> Result Value
binarySub v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  return $ Number $ l - r

binaryMult :: Result Value -> Result Value -> Result Value
binaryMult v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  return $ Number $ l * r

binaryDiv :: Result Value -> Result Value -> Result Value
binaryDiv v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  return $ Number $ l / r

binaryAnd :: Result Value -> Result Value -> Result Value
binaryAnd v1 v2 = do
  l <- getBool v1
  if not l
    then return $ Bool False -- short
    else do
      r <- getBool v2
      return $ Bool $ l && r

binaryOr :: Result Value -> Result Value -> Result Value
binaryOr v1 v2 = do
  l <- getBool v1
  if l
    then return $ Bool True
    else do
      r <- getBool v2
      return $ Bool $ l || r

binaryLand :: Result Value -> Result Value -> Result Value
binaryLand v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  let v = truncate l .&. truncate r :: Int
  return $ Number $ fromIntegral v

binaryLor :: Result Value -> Result Value -> Result Value
binaryLor v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  let v = truncate l .|. truncate r :: Int
   in return $ Number $ fromIntegral v

binaryXor :: Result Value -> Result Value -> Result Value
binaryXor v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  let v = truncate l `xor` truncate r :: Int
   in return $ Number $ fromIntegral v

binaryLsl :: Result Value -> Result Value -> Result Value
binaryLsl v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  let v = truncate l `shiftL` truncate r :: Int
   in return $ Number $ fromIntegral v

binaryLsr :: Result Value -> Result Value -> Result Value
binaryLsr v1 v2 = do
  l <- getNumber v1
  r <- getNumber v2
  let v = truncate l `shiftR` truncate r :: Int
   in return $ Number $ fromIntegral v

binaryLt :: Result Value -> Result Value -> Result Value
binaryLt v1 v2 = do
  x <- stdCmp v1 v2
  return $ Bool (x == LT)

binaryLe :: Result Value -> Result Value -> Result Value
binaryLe v1 v2 = do
  x <- stdCmp v1 v2
  return $ Bool $ x == LT || x == EQ

binaryGt :: Result Value -> Result Value -> Result Value
binaryGt v1 v2 = do
  x <- stdCmp v1 v2
  return $ Bool $ x == GT

binaryGe :: Result Value -> Result Value -> Result Value
binaryGe v1 v2 = do
  x <- stdCmp v1 v2
  return $ Bool $ x == GT || x == EQ

unaryNot :: Result Value -> Result Value
unaryNot v = do
  x <- getBool v
  return $ Bool $ not x

unaryLnot :: Result Value -> Result Value
unaryLnot v = do
  x <- getNumber v
  let v' = complement $ truncate x :: Int
  return $ Number $ fromIntegral v'

unaryNeg :: Result Value -> Result Value
unaryNeg v = do
  x <- getNumber v
  return $ Number $ -x

unaryPos :: Result Value -> Result Value
unaryPos v = do
  x <- getNumber v
  return $ Number x

if_ :: Result Value -> Result Value -> Result Value -> Result Value
if_ v1 v2 v3 = do
  x <- getBool v1
  if x then v2 else v3

functionParam :: Arguments -> Int -> String -> Maybe (Result Value) -> Result Value
functionParam (positional, named) i id v =
  if i < length positional
    then positional !! i
    else case lookup id named of
      Just x -> x
      Nothing -> case v of Just v -> v; Nothing -> throwError "parameter not bound"

inSuper :: Fields -> Result Value -> Result Value
inSuper (GeneralFields super) key = do
  x <- getString key
  return $ Bool $ HashMap.member x super

superIndex :: Fields -> Result Value -> Result Value
superIndex super@(GeneralFields superFields) key = do
  key' <- getString key
  case HashMap.lookup key' superFields of
    Nothing -> throwError ("field does not exist: " ++ key')
    Just (_, v, _) -> v

arrayIndex :: Result Value -> Result Value -> Result Value
arrayIndex lhs rhs = do
  lhs' <- lhs
  aux lhs' rhs
 where
  aux (Array _ a) v2 = do
    x <- getNumber v2
    a Vector.! truncate x
  aux (String _ _ s) v2 = do
    x <- getNumber v2
    let c = s UVector.! truncate x
    return $ String [c] (TB.fromString [c]) (UVector.singleton c)
  aux (Object asserts self@(GeneralFields fields)) v2 = do
    evalAsserts asserts self
    x <- getString v2
    case HashMap.lookup x fields of
      Just (_, v, _) -> v
      Nothing -> do
        x <- getString v2
        throwError ("object field not found: " ++ x)
  aux _ _ = throwError "arrayIndex: invalid value"

objectField ::
  Int ->
  Result Value ->
  (Fields -> Fields -> Result Value) ->
  Result Fields ->
  Result Fields
objectField h k v fields = do
  k' <- k
  fields' <- fields
  return $
    case k' of
      Null -> fields'
      String k _ _ ->
        case fields' of
          GeneralFields m ->
            GeneralFields $ HashMap.insert k (h, Ok Null, v) m

fillObjectCache :: Fields -> Fields
fillObjectCache (GeneralFields m) =
  let self = GeneralFields $ HashMap.map (\(h, _, f) -> (h, f self emptyObjectFields, f)) m
   in self

objectFor :: (Fields -> Result Value -> Result Fields) -> Result Value -> Result Fields
objectFor f e3 = do
  x <- getArray e3
  y <- Vector.foldM f emptyObjectFields x
  return $ fillObjectCache y

error' :: Result Value -> Result a
error' v = do
  v' <- manifestation True v
  throwError $ TL.unpack v'

objectFieldPlusValue :: Fields -> Result Value -> Result Value -> Result Value
objectFieldPlusValue super e1 e2 = do
  x <- getBool $ inSuper super e1
  if x
    then binaryAdd (superIndex super e1) e2
    else e2

extractVisibleFields :: Fields -> [(String, Result Value)]
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

manifestation' :: Bool -> Int -> Bool -> Result Value -> Result TB.Builder
manifestation' hasInitInd ind multiLine v = do
  let indent = TB.fromString $ if multiLine then take ind (repeat ' ') else ""
  let newline = TB.fromString $ if multiLine then "\n" else ""
  let initInd = if hasInitInd then indent else TB.fromString ""
  v' <- v
  case v' of
    Null -> return $ initInd <> TB.fromString "null"
    (Bool True) -> return $ initInd <> TB.fromString "true"
    (Bool False) -> return $ initInd <> TB.fromString "false"
    (String s _ _) -> return $ initInd <> quoteString s
    (Number n) -> return $ initInd <> TB.fromText (Data.Double.Conversion.Text.toShortest n)
    (Array _ xs) ->
      if Vector.null xs
        then return $ initInd <> TB.fromString "[ ]"
        else do
          bs <- Vector.mapM (manifestation' True (ind + 3) multiLine) xs
          return $
            initInd
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
    (Object asserts fields) -> do
      evalAsserts asserts fields
      case reverse $ extractVisibleFields fields of
        [] -> return $ initInd <> TB.fromString "{ }"
        xs -> do
          bs <-
            mapM
              ( \(k, v) -> do
                  v' <- manifestation' False (ind + 3) multiLine v
                  return (k, v')
              )
              xs
          let encode (k, b) =
                TB.fromString (if multiLine then take (ind + 3) (repeat ' ') else "")
                  <> quoteString k
                  <> TB.fromString ": "
                  <> b
          case bs of
            [] -> error "unreachable"
            hd : tl ->
              return $
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

manifestation :: Bool -> Result Value -> Result TL.Text
manifestation multi x =
  fmap TB.toLazyText $ manifestation' True 0 multi x

handleError :: String -> IO ()
handleError msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
  exitWith $ ExitFailure 1

mainNormal :: Result Value -> IO ()
mainNormal v =
  case manifestation True v of
    Ok s -> TLIO.putStrLn s
    Error msg -> handleError msg

mainString :: Result Value -> IO ()
mainString (Ok (String s _ _)) = putStr s
mainString (Error msg) = handleError msg
mainString _ = error "stringManifestation: not string"

mainMulti :: String -> Bool -> Result Value -> IO ()
mainMulti _ _ (Error msg) = handleError msg
mainMulti targetDir string (Ok (Object _ fields)) =
  forM_ (extractVisibleFields fields) $ \(k, v) ->
    let filePath = joinPath [targetDir, k]
     in case (v, string) of
          (Ok (String s _ _), True) -> do
            createDirectoryIfMissing True $ dropFileName filePath
            writeFile filePath s
          (_, True) -> error "multiManifestation: expect string values in objects"
          (_, False) -> do
            createDirectoryIfMissing True $ dropFileName filePath
            case manifestation True v of
              Error msg -> handleError msg
              Ok v -> TLIO.writeFile filePath v

stdMakeArray :: Arguments -> Result Value
stdMakeArray args = do
  sz <- getNumber $ functionParam args 0 "sz" Nothing
  func <- getFunction $ functionParam args 1 "func" Nothing
  makeArray $
    Vector.generate
      (truncate sz)
      (\i -> func ([return $ Number (fromIntegral i)], []))

stdPrimitiveEquals :: Arguments -> Result Value
stdPrimitiveEquals args = do
  x <- functionParam args 0 "x" Nothing
  y <- functionParam args 1 "y" Nothing
  case (x, y) of
    (Null, Null) -> return $ Bool True
    (Bool lhs, Bool rhs) -> return $ Bool $ lhs == rhs
    (String lhs _ _, String rhs _ _) -> return $ Bool $ lhs == rhs
    (Number lhs, Number rhs) -> return $ Bool $ lhs == rhs
    _ -> return $ Bool False

stdLength :: Arguments -> Result Value
stdLength args = do
  x <- functionParam args 0 "x" Nothing
  fmap (Number . fromIntegral) $
    case x of
      Array _ xs -> return $ Vector.length xs
      String _ _ xs -> return $ UVector.length xs
      Object _ (GeneralFields fields) -> return $ HashMap.size $ HashMap.filter (\(h, _, _) -> h /= 2) fields
      Function n _ -> return n
      _ -> throwError "std.length: invalid type argument"

stdType :: Arguments -> Result Value
stdType args = do
  x <- functionParam args 0 "x" Nothing
  makeString $
    case x of
      Null -> "null"
      Bool True -> "boolean"
      Bool False -> "boolean"
      String _ _ _ -> "string"
      Function _ _ -> "function"
      Number _ -> "number"
      Array _ _ -> "array"
      Object _ _ -> "object"

stdFilter :: Arguments -> Result Value
stdFilter args = do
  func <- getFunction $ functionParam args 0 "func" Nothing
  arr <- getArray $ functionParam args 1 "arr" Nothing
  x <- Vector.filterM (\x -> getBool $ func ([x], [])) arr
  makeArray x

stdObjectHasEx :: Arguments -> Result Value
stdObjectHasEx args = do
  (_, (GeneralFields fields)) <- getObject $ functionParam args 0 "obj" Nothing
  f <- getString $ functionParam args 1 "f" Nothing
  b' <- getBool $ functionParam args 2 "b'" Nothing
  return $
    case HashMap.lookup f fields of
      Nothing -> Bool False
      Just (h, _, _) -> Bool (h /= 2 || b')

stdObjectFieldsEx :: Arguments -> Result Value
stdObjectFieldsEx args = do
  (_, (GeneralFields fields)) <- getObject $ functionParam args 0 "obj" Nothing
  b' <- getBool $ functionParam args 1 "b'" Nothing
  makeArrayFromList $
    map makeString $
      sort $
        map (\(k, _) -> k) $
          filter (\(_, (h, _, _)) -> h /= 2 || b') $
            HashMap.toList fields

stdModulo :: Arguments -> Result Value
stdModulo args = do
  a <- getNumber $ functionParam args 0 "a" Nothing
  b <- getNumber $ functionParam args 1 "b" Nothing
  return $ Number $ Data.Fixed.mod' a b

stdCodepoint :: Arguments -> Result Value
stdCodepoint args = do
  str <- getString $ functionParam args 0 "str" Nothing
  return $ Number $ fromIntegral $ ord $ head str

stdChar :: Arguments -> Result Value
stdChar args = do
  x <- getNumber $ functionParam args 0 "n" Nothing
  let c = chr $ truncate x
  return $ String [c] (TB.fromString [c]) (UVector.singleton c)

stdFloor :: Arguments -> Result Value
stdFloor args = do
  x <- getNumber $ functionParam args 0 "x" Nothing
  return $ Number $ fromInteger $ floor x

stdPow :: Arguments -> Result Value
stdPow args = do
  x <- getNumber $ functionParam args 0 "x" Nothing
  n <- getNumber $ functionParam args 1 "n" Nothing
  return $ Number $ x ** n

stdLog :: Arguments -> Result Value
stdLog args = do
  x <- getNumber $ functionParam args 0 "x" Nothing
  return $ Number $ log x

insertStd :: Fields -> Fields
insertStd (GeneralFields fields) =
  GeneralFields $
    foldl
      (\a (k, v) -> HashMap.insert k v a)
      fields
      [ ("primitiveEquals", (2, return Null, \_ _ -> return $ Function 2 stdPrimitiveEquals))
      , ("length", (2, return Null, \_ _ -> return $ Function 1 stdLength))
      , ("makeArray", (2, return Null, \_ _ -> return $ Function 2 stdMakeArray))
      , ("type", (2, return Null, \_ _ -> return $ Function 1 stdType))
      , ("filter", (2, return Null, \_ _ -> return $ Function 2 stdFilter))
      , ("objectHasEx", (2, return Null, \_ _ -> return $ Function 3 stdObjectHasEx))
      , ("objectFieldsEx", (2, return Null, \_ _ -> return $ Function 2 stdObjectFieldsEx))
      , ("modulo", (2, return Null, \_ _ -> return $ Function 2 stdModulo))
      , ("codepoint", (2, return Null, \_ _ -> return $ Function 1 stdCodepoint))
      , ("char", (2, return Null, \_ _ -> return $ Function 1 stdChar))
      , ("floor", (2, return Null, \_ _ -> return $ Function 1 stdFloor))
      , ("log", (2, return Null, \_ _ -> return $ Function 1 stdLog))
      , ("pow", (2, return Null, \_ _ -> return $ Function 2 stdPow))
      ]

readBin :: String -> IO (Result Value)
readBin path = do
  body <- Bytestring.readFile path
  pure $
    makeArrayFromList $
      reverse $
        Bytestring.foldl (\acc x -> (return $ Number $ fromIntegral x) : acc) [] body

readStr :: String -> IO (Result Value)
readStr path = do
  body <- readFile path
  pure $ makeString body
