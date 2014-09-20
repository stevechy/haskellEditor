module HaskellEditor.Gui.FileTree
where
import Graphics.UI.Gtk
import HaskellEditor.Gui.Util
import HaskellEditor.Types
import HaskellEditor.Dynamic
import Control.Concurrent.STM

fileLabelRenderer :: CellRendererTextClass o => DirectoryEntry -> [AttrOp o]    
fileLabelRenderer label = case label of
  Directory directory -> [cellText := directory]
  PlainFile file _ ->  [cellText := file]

iconLabelRenderer :: CellRendererPixbufClass o => DirectoryEntry -> [AttrOp o]
iconLabelRenderer label = case label of
  Directory directory -> [cellPixbufStockId := stockDirectory]
  _ -> [cellPixbufStockId := stockFile]

makeFileTreeView :: TVar(Widgets) -> IO TreeView
makeFileTreeView widgetTVar = 
    do  treeStore <- fileTreeStoreNew
    
        fileTreeView <- treeViewNewWithModel treeStore
        insertNamedWidget widgetTVar "fileTreeView" fileTreeView
        insertNamedWidget widgetTVar "fileTreeStore" treeStore
    
        treeViewColumn <- treeViewColumnNew
        treeViewColumnSetTitle treeViewColumn "Project Files"
        typePix <- cellRendererPixbufNew
        cellRenderer <- cellRendererTextNew
        treeViewColumnPackStart treeViewColumn typePix True
        treeViewColumnPackStart treeViewColumn cellRenderer True 
        cellLayoutSetAttributes treeViewColumn cellRenderer treeStore fileLabelRenderer
        cellLayoutSetAttributes treeViewColumn typePix treeStore iconLabelRenderer
    
        _ <- treeViewAppendColumn fileTreeView treeViewColumn
        _ <- treeViewGetColumn fileTreeView 0
        return fileTreeView

fileTreeStoreNew :: IO (TreeStore DirectoryEntry)
fileTreeStoreNew = treeStoreNew []