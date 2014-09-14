module HaskellEditor.Files
where

import Control.Concurrent.STM
import Graphics.UI.Gtk
import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import HaskellEditor.Types
import qualified System.FilePath
import Pipes
import qualified Pipes.ByteString as P
import qualified Data.ByteString.Lazy
import Data.IORef
import qualified Data.ByteString 
import System.IO
import Data.Tree
import qualified Shelly
import qualified Data.Ord
import Data.String
import qualified System.Directory
import qualified Data.List


getFile :: Handle ->  IO (Data.ByteString.Lazy.ByteString)
getFile handle = do         
        byteStringVar <- newIORef Data.ByteString.Lazy.empty
        runEffect $ (P.fromHandle handle) >-> (consumeFile byteStringVar)
        readIORef byteStringVar 

consumeFile :: IORef (Data.ByteString.Lazy.ByteString) -> Consumer Data.ByteString.ByteString IO ()
consumeFile byteStringVar = do
    byteStringChunk <- await
    lift $ modifyIORef byteStringVar (\ current -> Data.ByteString.Lazy.append current (Data.ByteString.Lazy.fromChunks [byteStringChunk]))
    consumeFile byteStringVar

saveFiles :: EditorWindow -> IO ()
saveFiles editor = do    
  buffers <- atomically $ readTVar $ sourceBuffers editor
  rootPathMaybe <- atomically $ readTVar $ _rootPath editor
  case rootPathMaybe of       
    Just rootPath -> do
      _ <- saveBuffers rootPath buffers
      return ()
    Nothing -> return ()
  return ()

saveBuffers :: TextBufferClass self =>
                     FilePath -> IntMap.IntMap (FilePath, self) -> IO [()]
saveBuffers rootPath buffers = do
  forM 
    (IntMap.elems buffers) 
    (\ (fileName, buffer) -> do 
        startIter <- textBufferGetStartIter buffer
        endIter <- textBufferGetEndIter buffer
        text <- textBufferGetText buffer startIter endIter True       
        writeFile (System.FilePath.combine rootPath fileName) text
        return ()
    )


orderDirectoryNodes :: Tree DirectoryEntry -> Tree DirectoryEntry -> Ordering  
orderDirectoryNodes nodeA nodeB =
   case (nodeA, nodeB) of 
     (Node ( Directory _ ) _ , Node (PlainFile _ _) _) ->  LT
     (Node (PlainFile _ _) _, Node (Directory _) _) -> GT
     (Node (Directory a) _ , Node (Directory b) _ ) -> Data.Ord.compare a b
     (Node (PlainFile _ a) _, Node (PlainFile _ b) _) -> Data.Ord.compare a b
     
getFileNode :: FilePath -> FilePath -> FilePath -> IO (Tree DirectoryEntry)
getFileNode rootPath relPath filePath = do  
  isDirectory <- Shelly.shelly $ Shelly.test_d $ fromString $ System.FilePath.combine rootPath $ System.FilePath.combine relPath filePath
  
  if isDirectory 
    then do
         subDirectories <- getDirContentsAsTreeWithRelpath rootPath (System.FilePath.combine relPath filePath)
         return Node { rootLabel = Directory filePath, subForest = subDirectories } 
    else return (Node { rootLabel = PlainFile filePath (System.FilePath.combine relPath filePath), subForest = [] })




getDirContentsAsTree :: FilePath -> IO [Tree DirectoryEntry]  
getDirContentsAsTree rootPath = getDirContentsAsTreeWithRelpath rootPath "."

getDirContentsAsTreeWithRelpath :: FilePath -> FilePath -> IO [Tree DirectoryEntry]  
getDirContentsAsTreeWithRelpath rootPath relPath = do
  dirContents <- System.Directory.getDirectoryContents (System.FilePath.combine rootPath relPath)
  
  forest <- mapM (getFileNode rootPath relPath) $ filter (\x -> x /= "." && x /= "..") $  dirContents
  return $ Data.List.sortBy orderDirectoryNodes forest      
  
