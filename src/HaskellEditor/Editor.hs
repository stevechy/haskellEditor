module HaskellEditor.Editor

where

import Control.Concurrent.STM
import Data.IORef
import HaskellEditor.Types
import HaskellEditor.Configuration.Types
import HaskellEditor.Files
import HaskellEditor.Gui.Util
import HaskellEditor.Gui.FileTree
import qualified HaskellEditor.Configuration
import qualified Data.IntMap.Strict as IntMap
import HaskQuery
import qualified System.FilePath
import Graphics.UI.Gtk

makeEditorWindow ::  IO EditorWindow
makeEditorWindow = do
    filePath <- atomically $ newTVar Nothing
    buffers <- atomically $ newTVar IntMap.empty
    propertyRelation <- atomically $ newTVar HaskQuery.empty
    widgetTVar <- atomically $ newTVar emptyWidgets
   
    guiId <- newIORef 0
    
    let editorWindow =  EditorWindow {   
                          _editorWidgets = widgetTVar,   
                          _rootPath = filePath, 
                          nextGuiId = guiId,
                          sourceBuffers = buffers,
                          _properties = propertyRelation
                        } 
    return editorWindow
    

loadFile :: EditorWindow -> FilePath -> IO ()
loadFile editor path = do
  configuration <- if (System.FilePath.takeExtension path) == ".cabal"
                        then return $ Just $ Configuration { rootFolder =".", cabalFile = System.FilePath.takeFileName path, commands = Nothing }
                        else HaskellEditor.Configuration.loadConfigFile path
  case configuration of
    Just config -> do
      putStrLn $ show $ config
      loadConfiguration editor config path
      return ()
    Nothing -> do
      messageDialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsOk "The selected file was not a propery Configuration.yaml file"
      _ <- dialogRun messageDialog
      widgetDestroy messageDialog
      return ()

loadConfiguration :: EditorWindow -> Configuration -> FilePath -> IO ()
loadConfiguration editor config filepath = do
  
  canonicalRootPath <- HaskellEditor.Configuration.getCanonicalRootPath config filepath
  
  atomically $ do 
      writeTVar (_rootPath editor) $ Just canonicalRootPath  
      modifyTVar (_properties editor) $ (\properties -> HaskQuery.insert properties (ConfigurationProperty { _name = "rootPath", _value = canonicalRootPath}))
  refreshFolders editor

refreshFolders :: EditorWindow -> IO ()
refreshFolders editor = do
  canonicalRootPathMaybe <- atomically $ readTVar (_rootPath editor) 
  case canonicalRootPathMaybe of 
        Just canonicalRootPath -> do
            forest <- getDirContentsAsTree canonicalRootPath
            _ <- HaskQuery.runQueryM $ do
                 widgets <- getWidgets (_editorWidgets editor)
                 fileTreeStore <- selectWidgetRef widgets fileTreeStoreRef 
                 HaskQuery.executeM $ do
                     treeStoreClear fileTreeStore  
                     treeStoreInsertForest fileTreeStore [] 0 forest
            return ()
        Nothing -> return ()

