module HaskellEditor.Types

where

import Control.Concurrent.STM
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceBuffer
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.Dynamic
import qualified HaskQuery
import qualified HaskQuery.OrdIndex as OrdIndex

data DirectoryEntry = Directory String | PlainFile String String

data ConfigurationProperty = ConfigurationProperty { _name :: String, _value :: String }

data Named a = Named { _identifier :: String, _content :: a}
data Widgets = Widgets {  _widgets :: HaskQuery.Relation (Named Dynamic) (OrdIndex.OrdIndex String)}


emptyWidgets :: Widgets
emptyWidgets = Widgets{  _widgets = HaskQuery.emptyWithIndex $ OrdIndex.ordIndex _identifier }

data EditorWindow = EditorWindow { mainPane:: VPaned, 
                                   _fileTreeStore :: TreeStore DirectoryEntry, 
                                   _fileTreeView:: TreeView, 
                                   notebook :: Notebook, 
                                   _rootPath :: TVar (Maybe FilePath),                                    
                                   nextGuiId :: IORef (Int), 
                                   sourceBuffers :: TVar ( IntMap.IntMap (String, SourceBuffer)),
                                   _properties :: TVar (HaskQuery.Relation ConfigurationProperty ())
                                   }

type EditorInitializer = EditorWindow -> IO ()