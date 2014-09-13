module HaskellEditor.Types

where

import Control.Concurrent.STM
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceBuffer
import qualified Data.IntMap.Strict as IntMap
import Data.IORef

data DirectoryEntry = Directory String | PlainFile String String

data EditorWindow = EditorWindow { mainPane:: VPaned, 
                                   _fileTreeStore :: TreeStore DirectoryEntry, 
                                   _fileTreeView:: TreeView, 
                                   notebook :: Notebook, 
                                   _rootPath :: TVar (Maybe FilePath),                                    
                                   nextGuiId :: IORef (Int), 
                                   sourceBuffers :: TVar ( IntMap.IntMap (String, SourceBuffer))
                                   }

type EditorInitializer = EditorWindow -> IO ()