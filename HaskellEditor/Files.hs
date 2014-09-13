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

