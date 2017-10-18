{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module SignatureGenerator(makeManifest) where
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Directory
import Data.Aeson
import Data.Time.Clock
import qualified Data.Text as T
import Control.Applicative (empty)
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.SHA
import Data.List(intercalate)

data FileInfo = FileInfo { path :: String,
                           time :: UTCTime,
                           size :: Integer,
                           hash :: String } deriving Show
                           
instance ToJSON FileInfo where
  toJSON (FileInfo pV tV sV hV) = object [ "path" .= pV,
                                        "time" .= tV,
                                        "size" .= sV,
                                        "hash" .= hV ] 
  toEncoding FileInfo{..} = pairs $
    "path" .= path <>
    "time" .= time <>
    "size" .= size <>
    "hash" .= hash
    
instance FromJSON FileInfo where
  parseJSON (Object v) = FileInfo <$>
                         v .: "path" <*>
                         v .: "time" <*>
                         v .: "size" <*>
                         v .: "hash"
  parseJSON _          = empty

writeFileInfo :: Integer -> StateT [FilePath] IO Integer
writeFileInfo len
  | len == 0 = StateT $ \s -> do
      appendFile mPath " ]"
      appendFile mPath "}"
      return (0, s)
  | len == 1 = do
      makeFileInfo
      s <- get
      writeFileInfo $ toInteger (length s)
  | otherwise = do
      result <- makeFileInfo
      if result then lift $ appendFile mPath ","
      else return ()
      s <- get
      writeFileInfo $ toInteger (length s)
  where mPath = "./manifest.json"


makeFileInfo :: StateT [FilePath] IO Bool
makeFileInfo = StateT $ \s -> do
  (x:xs) <- return s
  exist <- doesFileExist x
  if exist then do
    time <- getModificationTime x
    size <- getFileSize x
    content <- BL.readFile x
    let info = FileInfo x time size (showDigest $ sha1 content)
    BL.appendFile mPath (encode info)
    return (True, xs)
  else do
    subFiles <- listDirectory x
    return (False, xs ++ (appended x subFiles))
  where mPath = "./manifest.json"
        appended par subs = map (\a -> intercalate "/" [par, a]) subs


 
makeManifest :: FilePath -> IO ()
makeManifest path = do
  writeFile mPath "{"
  appendFile mPath " \"files\":[ "
  files <- listDirectory "."
  runStateT (writeFileInfo (toInteger $ length (filt files))) $ filt files
  return ()
    where mPath = path ++ "/manifest.json"
          filt = filter (/="manifest.json")
    
    
   
    