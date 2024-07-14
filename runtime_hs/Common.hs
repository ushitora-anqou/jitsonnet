{-# LANGUAGE LambdaCase #-}

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
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (intercalate, lookup, sort, sortBy)
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
import Text.Printf (hPrintf, printf)

type UVector = UVector.Vector

type Positional = [Value]

type Named = HashMap String Value

type Arguments = (Positional, Named)

type VisitedAssertIDs = HashSet Int

type Asserts =
  [ ( Int -- id
    , Fields {- self -} -> Fields {- super -} -> VisitedAssertIDs {- visited ids -} -> Value
    )
  ]

data Fields
  = GeneralFields
      ( HashMap
          String
          (Int, Value, Fields {- self -} -> Fields {- super -} -> Value)
      )

emptyObjectFields :: Fields
emptyObjectFields = GeneralFields HashMap.empty

data CallFrame
  = CFLocation
      { filePath :: String
      , startLoc :: (Int, Int)
      , endLoc :: (Int, Int)
      }
  | CFName String
  deriving (Show)

type CallStack = [CallFrame]

withName cs name f = f $ CFName name : cs

withLoc cs Nothing f = f cs
withLoc cs (Just (filePath, startLine, startColumn, endLine, endColumn)) f =
  f $
    CFLocation
      { filePath = filePath
      , startLoc = (startLine, startColumn)
      , endLoc = (endLine, endColumn)
      }
      : cs

data Value
  = Null
  | Bool Bool
  | String String TB.Builder (UVector Char)
  | Number Double
  | Array (Deque (Vector Value)) (Vector Value)
  | Function Int (CallStack -> Arguments -> Value)
  | Object Asserts Fields

{-# OPAQUE throwError #-}
throwError :: CallStack -> String -> a
throwError cs msg =
  unsafePerformIO $ do
    hPrintf stderr "RUNTIME ERROR: %s\n%s" msg $
      unlines $
        snd $
          foldr
            ( \fr (name, lines) ->
                case fr of
                  CFName name -> (name, lines)
                  CFLocation{filePath, startLoc = (line0, col0), endLoc = (line1, col1)} ->
                    let lineLoc =
                          ( if line0 == line1
                              then
                                printf
                                  "%s:%d:%d-%d"
                                  filePath
                                  line0
                                  (col0 + 1)
                                  (col1 + 1)
                              else
                                printf
                                  "%s:(%d,%d)-(%d,%d)"
                                  filePath
                                  line0
                                  (col0 + 1)
                                  line1
                                  (col1 + 1)
                          )
                        lineName = if name == "" then "" else printf "\tfunction <%s>" name
                        line = "\t" ++ lineLoc ++ lineName
                     in (name, line : lines)
            )
            ("", [])
            cs
    hFlush stdout
    exitFailure

makeString :: String -> Value
makeString s = String s (TB.fromString s) (UVector.fromList s)

makeArray :: Vector Value -> Value
makeArray v = Array (GHC.IsList.fromList [v]) v

makeArrayFromList :: [Value] -> Value
makeArrayFromList a = makeArray $ Vector.fromList a

getBool :: CallStack -> Value -> Bool
getBool _ (Bool b) = b
getBool cs _ = throwError cs "not bool"

getString :: CallStack -> Value -> String
getString _ (String s _ _) = s
getString cs _ = throwError cs "not string"

getNumber :: CallStack -> Value -> Double
getNumber _ (Number n) = n
getNumber cs _ = throwError cs "not number"

getArray :: CallStack -> Value -> Vector Value
getArray _ (Array _ v) = v
getArray cs _ = throwError cs "not array"

getFunction :: CallStack -> Value -> (CallStack -> Arguments -> Value)
getFunction _ (Function _ f) = f
getFunction cs _ = throwError cs "not function"

getObject :: CallStack -> Value -> (Asserts, Fields)
getObject _ (Object asserts fields) = (asserts, fields)
getObject cs _ = throwError cs "not object"

{-# OPAQUE evalAsserts #-}
evalAsserts :: Asserts -> Fields -> VisitedAssertIDs -> ()
evalAsserts asserts fields visited =
  let visited' =
        foldl
          ( \visited (x, _) ->
              case visited of
                Nothing -> Nothing
                Just visited ->
                  if HashSet.member x visited
                    then Nothing
                    else Just $ HashSet.insert x visited
          )
          (Just visited)
          asserts
   in case visited' of
        Nothing -> ()
        Just visited ->
          unsafePerformIO $
            forM_ asserts $ \(_, f) ->
              evaluate $ f fields emptyObjectFields visited

stdCmp :: CallStack -> Value -> Value -> Ordering
stdCmp _ (Number n1) (Number n2) = compare n1 n2
stdCmp _ (String s1 _ _) (String s2 _ _) = compare s1 s2
stdCmp cs (Array _ a1) (Array _ a2) = Vector.cmpBy (stdCmp cs) a1 a2
stdCmp cs _ _ = throwError cs "stdCmp: invalid arguments"

valueToTextBuilder :: CallStack -> Value -> TB.Builder
valueToTextBuilder _ Null = TB.fromString "null"
valueToTextBuilder _ (Bool True) = TB.fromString "true"
valueToTextBuilder _ (Bool False) = TB.fromString "false"
valueToTextBuilder _ (Number n) = TB.fromText $ Data.Double.Conversion.Text.toShortest n
valueToTextBuilder _ x@(Array _ _) = TB.fromLazyText $ manifestation False x
valueToTextBuilder _ x@(Object _ _) = TB.fromLazyText $ manifestation False x
valueToTextBuilder cs _ = throwError cs "valueToLazyText: not expected type"

binaryAdd :: CallStack -> Value -> Value -> Value
binaryAdd _ (Number n1) (Number n2) = Number (n1 + n2)
binaryAdd _ (Array a1 _) (Array a2 _) =
  let a = a1 <> a2
   in Array a $ Vector.concat $ GHC.IsList.toList a
binaryAdd _ (String s1 b1 v1) (String s2 b2 v2) =
  let b = b1 <> b2
   in String (TL.unpack $ TB.toLazyText b) b (UVector.fromList $ TL.unpack $ TB.toLazyText b)
binaryAdd cs (String s1 b1 v1) rhs =
  let b = b1 <> valueToTextBuilder cs rhs
      s = TL.unpack $ TB.toLazyText b
   in String s b (UVector.fromList s)
binaryAdd cs lhs (String s2 b2 v2) = do
  let b = valueToTextBuilder cs lhs <> b2
      s = TL.unpack $ TB.toLazyText b
   in String s b (UVector.fromList s)
binaryAdd _ (Object asserts1 fields1) (Object asserts2 fields2) =
  Object (asserts1 ++ map (\(i, v) -> (i, \self _ -> v self fields1)) asserts2) $
    fillObjectCache $
      GeneralFields $
        addObjectFields fields1 fields2
binaryAdd cs _ _ = throwError cs "binaryAdd: invalid values"

addObjectFields ::
  Fields ->
  Fields ->
  HashMap String (Int, Value, Fields -> Fields -> Value)
addObjectFields fields1@(GeneralFields m1) fields2@(GeneralFields m2) =
  HashMap.unionWith
    (\(h1, _, _) (h2, _, v2) -> (if h2 == 1 then h1 else h2, Null, v2))
    m1
    $ HashMap.map
      ( \(h, _, v) ->
          ( h
          , Null
          , \self super ->
              v
                self
                $ GeneralFields
                $ HashMap.map
                  ( \(h, _, f) -> (h, f self emptyObjectFields, f)
                  )
                {- We can't use (super+fields1) directly here and need to
                   craft 'super' this way. Consider the following
                   example:
                     { y: self.z, z: 0, a: super.xxx } { x: super.y, z: 1 } -}
                $ addObjectFields super fields1
          )
      )
      m2

binarySub :: CallStack -> Value -> Value -> Value
binarySub cs v1 v2 =
  Number $ getNumber cs v1 - getNumber cs v2

binaryMult :: CallStack -> Value -> Value -> Value
binaryMult cs v1 v2 =
  Number $ getNumber cs v1 * getNumber cs v2

binaryDiv :: CallStack -> Value -> Value -> Value
binaryDiv cs v1 v2 = do
  Number $ getNumber cs v1 / getNumber cs v2

binaryAnd :: CallStack -> Value -> Value -> Value
binaryAnd _ (Bool False) _ = Bool False
binaryAnd _ _ (Bool b) = Bool b
binaryAnd cs _ _ = throwError cs "binaryAnd: not bool"

binaryOr :: CallStack -> Value -> Value -> Value
binaryOr _ (Bool True) _ = Bool True
binaryOr _ _ (Bool b) = Bool b
binaryOr cs _ _ = throwError cs "binaryOr: not bool"

binaryLand :: CallStack -> Value -> Value -> Value
binaryLand cs v1 v2 =
  let l = truncate $ getNumber cs v1
      r = truncate $ getNumber cs v2
   in Number $ fromIntegral (l .&. r :: Int)

binaryLor :: CallStack -> Value -> Value -> Value
binaryLor cs v1 v2 =
  let l = truncate $ getNumber cs v1
      r = truncate $ getNumber cs v2
   in Number $ fromIntegral (l .|. r :: Int)

binaryXor :: CallStack -> Value -> Value -> Value
binaryXor cs v1 v2 =
  let l = truncate $ getNumber cs v1
      r = truncate $ getNumber cs v2
   in Number $ fromIntegral (l `xor` r :: Int)

binaryLsl :: CallStack -> Value -> Value -> Value
binaryLsl cs v1 v2 =
  let l = truncate $ getNumber cs v1
      r = truncate $ getNumber cs v2
   in Number $ fromIntegral (l `shiftL` (r `mod` 64) :: Int)

binaryLsr :: CallStack -> Value -> Value -> Value
binaryLsr cs v1 v2 =
  let l = truncate $ getNumber cs v1
      r = truncate $ getNumber cs v2
   in Number $ fromIntegral (l `shiftR` (r `mod` 64) :: Int)

binaryLt :: CallStack -> Value -> Value -> Value
binaryLt cs v1 v2 = Bool $ stdCmp cs v1 v2 == LT

binaryLe :: CallStack -> Value -> Value -> Value
binaryLe cs v1 v2 =
  let x = stdCmp cs v1 v2
   in Bool $ x == LT || x == EQ

binaryGt :: CallStack -> Value -> Value -> Value
binaryGt cs v1 v2 = Bool $ stdCmp cs v1 v2 == GT

binaryGe :: CallStack -> Value -> Value -> Value
binaryGe cs v1 v2 =
  let x = stdCmp cs v1 v2
   in Bool $ x == GT || x == EQ

unaryNot :: CallStack -> Value -> Value
unaryNot cs v =
  Bool $ not $ getBool cs v

unaryLnot :: CallStack -> Value -> Value
unaryLnot cs v =
  Number $ fromIntegral $ complement (truncate $ getNumber cs v :: Int)

unaryNeg :: CallStack -> Value -> Value
unaryNeg cs v = Number $ -(getNumber cs v)

unaryPos :: CallStack -> Value -> Value
unaryPos cs v = Number $ getNumber cs v

if_ :: CallStack -> Value -> Value -> Value -> Value
if_ cs v1 v2 v3 = if getBool cs v1 then v2 else v3

functionParam :: CallStack -> Arguments -> Int -> String -> Maybe Value -> Value
functionParam cs (positional, named) i id v =
  if i < length positional
    then positional !! i
    else case HashMap.lookup id named of
      Just x -> x
      Nothing ->
        case v of
          Just v -> v
          Nothing -> throwError cs "parameter not bound"

inSuper :: CallStack -> Fields -> Value -> Value
inSuper cs (GeneralFields super) key =
  Bool $ HashMap.member (getString cs key) super

superIndex :: CallStack -> Fields -> Value -> Value
superIndex cs super@(GeneralFields superFields) key =
  let key' = getString cs key
   in case HashMap.lookup key' superFields of
        Nothing -> throwError cs ("field does not exist: " ++ key')
        Just (_, v, _) -> v

arrayIndex :: CallStack -> VisitedAssertIDs -> Value -> Value -> Value
arrayIndex cs _ (Array _ a) v2 = a Vector.! truncate (getNumber cs v2)
arrayIndex cs _ (String _ _ s) v2 =
  let c = s UVector.! truncate (getNumber cs v2)
   in String [c] (TB.fromString [c]) (UVector.singleton c)
arrayIndex cs visited (Object asserts self@(GeneralFields fields)) v2 =
  evalAsserts asserts self visited `seq`
    let x = getString cs v2
     in case HashMap.lookup x fields of
          Just (_, v, _) -> v
          Nothing ->
            let x = getString cs v2
             in throwError cs ("object field not found: " ++ x)
arrayIndex cs _ _ _ = throwError cs "arrayIndex: invalid value"

objectField ::
  CallStack -> Int -> Value -> (Fields -> Fields -> Value) -> Fields -> Fields
objectField cs h k v fields =
  case k of
    Null -> fields
    String k _ _ ->
      case fields of
        GeneralFields m ->
          GeneralFields $ HashMap.insert k (h, Null, v) m
    _ -> throwError cs "objectField: not valid key type"

fillObjectCache :: Fields -> Fields
fillObjectCache (GeneralFields m) =
  let self = GeneralFields $ HashMap.map (\(h, _, f) -> (h, f self emptyObjectFields, f)) m
   in self

objectFor :: CallStack -> (Fields -> Value -> Fields) -> Value -> Fields
objectFor cs f e3 =
  fillObjectCache $ Vector.foldl f emptyObjectFields $ getArray cs e3

error' :: CallStack -> Value -> a
error' cs v =
  case v of
    String s _ _ -> throwError cs s
    _ -> throwError cs $ TL.unpack $ manifestation True v

objectFieldPlusValue :: CallStack -> Fields -> Value -> Value -> Value
objectFieldPlusValue cs super e1 e2 =
  if getBool cs $ inSuper cs super e1
    then binaryAdd cs (superIndex cs super e1) e2
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
        Function 0 f -> manifestation' hasInitInd ind multiLine $ f [] ([], HashMap.empty)
        Function _ _ -> throwError [] "manifestation: missing argument"
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
          evalAsserts asserts fields HashSet.empty `seq`
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

mainNormal :: (CallStack -> VisitedAssertIDs -> Value) -> IO ()
mainNormal f = TLIO.putStrLn $ manifestation True $ f [] HashSet.empty

mainString :: (CallStack -> VisitedAssertIDs -> Value) -> IO ()
mainString f =
  case f [] HashSet.empty of
    String s _ _ -> putStr s
    _ -> error "stringManifestation: not string"

mainMulti :: String -> Bool -> (CallStack -> VisitedAssertIDs -> Value) -> IO ()
mainMulti targetDir string f =
  case f [] HashSet.empty of
    (Object _ fields) ->
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
    _ -> error "mainMulti: not object"

stdMakeArray :: CallStack -> Arguments -> Value
stdMakeArray cs args =
  let sz = getNumber cs $ functionParam cs args 0 "sz" Nothing
      func = getFunction cs $ functionParam cs args 1 "func" Nothing
   in makeArray $
        Vector.generate
          (truncate sz)
          (\i -> func cs ([Number (fromIntegral i)], HashMap.empty))

stdPrimitiveEquals :: CallStack -> Arguments -> Value
stdPrimitiveEquals cs args =
  let x = functionParam cs args 0 "x" Nothing
      y = functionParam cs args 1 "y" Nothing
   in case (x, y) of
        (Null, Null) -> Bool True
        (Bool lhs, Bool rhs) -> Bool $ lhs == rhs
        (String lhs _ _, String rhs _ _) -> Bool $ lhs == rhs
        (Number lhs, Number rhs) -> Bool $ lhs == rhs
        _ -> Bool False

stdLength :: CallStack -> Arguments -> Value
stdLength cs args =
  let x = functionParam cs args 0 "x" Nothing
   in (Number . fromIntegral) $
        case x of
          Array _ xs -> Vector.length xs
          String _ _ xs -> UVector.length xs
          Object _ (GeneralFields fields) -> HashMap.size $ HashMap.filter (\(h, _, _) -> h /= 2) fields
          Function n _ -> n
          _ -> throwError cs "std.length: invalid type argument"

stdType :: CallStack -> Arguments -> Value
stdType cs args =
  let x = functionParam cs args 0 "x" Nothing
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

stdFilter :: CallStack -> Arguments -> Value
stdFilter cs args =
  let func = getFunction cs $ functionParam cs args 0 "func" Nothing
      arr = getArray cs $ functionParam cs args 1 "arr" Nothing
   in makeArray $ Vector.filter (\x -> getBool cs $ func cs ([x], HashMap.empty)) arr

stdObjectHasEx :: CallStack -> Arguments -> Value
stdObjectHasEx cs args =
  let (_, GeneralFields fields) = getObject cs $ functionParam cs args 0 "obj" Nothing
      f = getString cs $ functionParam cs args 1 "f" Nothing
      b' = getBool cs $ functionParam cs args 2 "b'" Nothing
   in case HashMap.lookup f fields of
        Nothing -> Bool False
        Just (h, _, _) -> Bool (h /= 2 || b')

stdObjectFieldsEx :: CallStack -> Arguments -> Value
stdObjectFieldsEx cs args =
  let (_, GeneralFields fields) = getObject cs $ functionParam cs args 0 "obj" Nothing
      b' = getBool cs $ functionParam cs args 1 "b'" Nothing
   in makeArrayFromList $
        map makeString $
          sort $
            map fst $
              filter (\(_, (h, _, _)) -> h /= 2 || b') $
                HashMap.toList fields

stdModulo :: CallStack -> Arguments -> Value
stdModulo cs args =
  let a = getNumber cs $ functionParam cs args 0 "a" Nothing
      b = getNumber cs $ functionParam cs args 1 "b" Nothing
   in Number $ Data.Fixed.mod' a b

stdCodepoint :: CallStack -> Arguments -> Value
stdCodepoint cs args =
  let str = getString cs $ functionParam cs args 0 "str" Nothing
   in Number $ fromIntegral $ ord $ head str

stdChar :: CallStack -> Arguments -> Value
stdChar cs args =
  let x = getNumber cs $ functionParam cs args 0 "n" Nothing
      c = chr $ truncate x
   in String [c] (TB.fromString [c]) (UVector.singleton c)

stdFloor :: CallStack -> Arguments -> Value
stdFloor cs args =
  let x = getNumber cs $ functionParam cs args 0 "x" Nothing
   in Number $ fromInteger $ floor x

stdPow :: CallStack -> Arguments -> Value
stdPow cs args =
  let x = getNumber cs $ functionParam cs args 0 "x" Nothing
      n = getNumber cs $ functionParam cs args 1 "n" Nothing
   in Number $ x ** n

stdLog :: CallStack -> Arguments -> Value
stdLog cs args =
  let x = getNumber cs $ functionParam cs args 0 "x" Nothing
   in Number $ log x

insertStd :: String -> Fields -> Fields
insertStd thisFile (GeneralFields fields) =
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
      , ("thisFile", (2, Null, \_ _ -> makeString thisFile))
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
