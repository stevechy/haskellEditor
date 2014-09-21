module HaskellEditor.Gui.FileTree
where
import Graphics.UI.Gtk
import HaskellEditor.Gui.Util
import HaskellEditor.Types
import HaskellEditor.Dynamic()
import Control.Concurrent.STM

fileTreeStoreRef :: WidgetRef (TreeStore DirectoryEntry)
fileTreeStoreRef = widgetReference "fileTreeStore"

fileLabelRenderer :: CellRendererTextClass o => DirectoryEntry -> [AttrOp o]    
fileLabelRenderer label = case label of
  Directory directory -> [cellText := directory]
  PlainFile file _ ->  [cellText := file]

iconLabelRenderer :: CellRendererPixbufClass o => DirectoryEntry -> [AttrOp o]
iconLabelRenderer label = case label of
  Directory _directory -> [cellPixbufStockId := stockDirectory]
  _ -> [cellPixbufStockId := stockFile]

makeFileTreeScrolledWindow :: TVar(Widgets) -> IO ScrolledWindow
makeFileTreeScrolledWindow widgetTVar = 
    do  fileTreeScrolledWindow <- scrolledWindowNew Nothing Nothing
        fileTreeView <- makeFileTreeView widgetTVar
        containerAdd fileTreeScrolledWindow fileTreeView
        widgetShowAll fileTreeView
        widgetShowAll fileTreeScrolledWindow
        return fileTreeScrolledWindow

makeFileTreeView :: TVar(Widgets) -> IO TreeView
makeFileTreeView widgetTVar = 
    do  treeStore <- fileTreeStoreNew
    
        fileTreeView <- treeViewNewWithModel treeStore
        insertNamedWidget widgetTVar "fileTreeView" fileTreeView
        insertNamedWidget widgetTVar (_identifier fileTreeStoreRef) treeStore
    
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

