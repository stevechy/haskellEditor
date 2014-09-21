module HaskellEditor.Configuration 
where

import HaskellEditor.Configuration.Types
import qualified HaskellEditor.Files
import qualified Data.ByteString 
import qualified Data.ByteString.Lazy
import System.IO
import Data.Yaml

loadConfigFile :: FilePath -> IO (Maybe Configuration)
loadConfigFile path = do
    maybeConfiguration <- parseConfig path    
    return maybeConfiguration

parseConfig :: FilePath ->  IO (Maybe HaskellEditor.Configuration.Types.Configuration)
parseConfig filePath = do
    lazyContents <- withFile filePath ReadMode $ HaskellEditor.Files.getFile
    let strictContents = Data.ByteString.concat $ Data.ByteString.Lazy.toChunks lazyContents
    case (Data.Yaml.decodeEither' strictContents) of
        Left parseException -> do
            putStrLn $ show parseException
            return Nothing
        Right configuration -> return $ Just configuration
    
getCanonicalRootPath :: Configuration -> FilePath -> IO FilePath
getCanonicalRootPath config filepath = do
  let configRootFolder = rootFolder config
  HaskellEditor.Files.getCanonicalRootPathFromPath configRootFolder filepath
  


