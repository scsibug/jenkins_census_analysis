import Data.Aeson
import Data.Aeson.Parser
import Data.Attoparsec as A (Result(..), parse)
import qualified Data.ByteString as B
import Data.Map as M (elems, keys, lookup)
import Data.Vector as V (toList, filter, (!?))
import Data.Maybe (fromMaybe)
import System.Environment

data Record = Record { install :: String,
                       timestamp :: String,
                       masterJvmVersion :: String,
                       masterJvmVendor :: String
} deriving (Read, Show)

-- Each sample (pair under the top level object) contains an array which contains 1 or more 

main = do argv <- getArgs
          let filename : xs = argv
          f <- B.readFile filename
          let r = A.parse json f
          mapM putStrLn $ (map (recordToCSV . sampleToRecord) (samples (getDone r)))
--          print ("# of samples: "++(show (length (samples (getDone r)))))

-- Take a sample, and produce a record for output and later analysis
sampleToRecord :: Value -> Record
sampleToRecord sample = Record { install = (fromMaybe "unknown" (getInstall sample)), timestamp = (fromMaybe "unknown" (getTimestamp sample)), masterJvmVersion = (fromMaybe "Nothing" (getMasterJvmVersion sample)), masterJvmVendor = (fromMaybe "Nothing" (getMasterJvmVendor sample))  }

recordToCSV :: Record -> String
recordToCSV r = (install r) ++ "," ++ (timestamp r) ++ "," ++ (masterJvmVersion r) ++ "," ++ (masterJvmVendor r)

getTimestamp :: Value -> Maybe String
getTimestamp sample =
    case sample of
      Object m -> M.lookup (mkText "timestamp") m >>= (\x -> Just (mkRealString x))

getInstall :: Value -> Maybe String
getInstall sample =
    case sample of
      Object m -> M.lookup (mkText "install") m >>= (\x -> Just (mkRealString x))

-- Take a sample, and determine the version string for the master executor
getMasterJvmVersion :: Value -> Maybe String
getMasterJvmVersion s =
    case s of
      Object m -> (M.lookup (mkText "nodes") m) >>= findMaster >>= jvmVersion >>= (\x -> Just (mkRealString x)) where
           jvmVersion r = case r of
                            Object rm -> M.lookup (mkText "jvm-version") rm
      otherwise -> Nothing

-- Take a sample, and determine the JVM vendor for the master executor
getMasterJvmVendor :: Value -> Maybe String
getMasterJvmVendor s =
    case s of
      Object m -> (M.lookup (mkText "nodes") m) >>= findMaster >>= jvmVendor >>= (\x -> Just (mkRealString x)) where
           jvmVendor r = case r of
                            Object rm -> M.lookup (mkText "jvm-vendor") rm
      otherwise -> Nothing

-- in a list of nodes, return the first master
findMaster nodesArr =
    case nodesArr of
      Array nodes -> ($ V.filter isMaster nodes) (!?) 0 where
                  isMaster n =
                      case n of
                        Object nm -> case (M.lookup (mkText "master") nm) of
                                      Nothing -> False
                                      Just (Bool mb) -> mb
                                      otherwise -> False

--mkText :: String -> Text
mkText s = case (toJSON s) of
             String t -> t

mkRealString s = case s of
                   String t -> show t

samples :: Value -> [Value]
samples v = concat $ map expandContainers (sampleArrays v) where
    expandContainers c =
        case c of
          Array v -> V.toList v
          otherwise -> [Null]

-- Return a list of census sample containers from the top-level-object
sampleArrays :: Value -> [Value]
sampleArrays v =
    case v of
      Object m  -> (M.elems m)
      otherwise -> []

-- Turn a result into a Value
getDone :: A.Result Value -> Value
getDone r =
    case r of
      Done remaining v -> v
      otherwise -> Null

