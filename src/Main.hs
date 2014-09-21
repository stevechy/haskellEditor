import Graphics.UI.Gtk


import Control.Concurrent.STM

import HaskellEditor.Files
import HaskellEditor.Types
import HaskellEditor.ShellExecution
import HaskellEditor.Dynamic
import HaskellEditor.Editor
import HaskellEditor.Gui.Components
import HaskellEditor.Gui.FileTree
import HaskellEditor.Gui.SourceTabBook
import HaskellEditor.Gui.Util
import qualified HaskQuery


main :: IO ()
main = do
    _ <- initGUI    

    editor <- makeEditor  
    let widgetTVar = _editorWidgets editor
    createMainWindow widgetTVar
           

    _ <- makeButtonBar widgetTVar $ HaskQuery.insertRows HaskQuery.empty [
                Named { _identifier = "openProjectButton", _content =  newFileChooser $ loadFile editor},
                Named { _identifier = "saveProjectButton", _content =  saveFiles editor},
                Named { _identifier = "refreshProjectButton", _content = refreshFolders editor}
            ]              
        
    widgets <- readTVarIO widgetTVar 

    _ <- HaskQuery.runQueryM $ do
        fileTreeView <- selectWidget widgets "fileTreeView" treeViewProxy  
        HaskQuery.executeM $ onRowActivated fileTreeView $ openFileChooserFile editor   
              
    mainBox <- vBoxNew False 0
    _ <- HaskQuery.runQueryM $ do 
        buttonBar <- selectWidgetRef widgets buttonBarRef
        HaskQuery.executeM $ boxPackStart mainBox buttonBar PackNatural 0
        
    _ <- HaskQuery.runQueryM $ do 
        editorPaneWidget <- selectWidgetRef widgets editorPane
        HaskQuery.executeM $ boxPackStart mainBox editorPaneWidget PackGrow 0
        
    widgetShowAll mainBox

    _ <- HaskQuery.runQueryM $ do
        mainWindow <- selectWidgetRef widgets mainWindowRef
        HaskQuery.executeM $ containerAdd mainWindow mainBox
    _ <- HaskQuery.runQueryM $ do 
        mainWindow <- selectWidgetRef widgets mainWindowRef
        HaskQuery.executeM $ widgetShowAll mainWindow
    mainGUI




openFileChooserFile :: EditorWindow -> TreePath -> t -> IO ()    
openFileChooserFile editor treePath _treeViewColumn = do
    _ <- HaskQuery.runQueryM $ do
        widgets <- getWidgets (_editorWidgets editor)
        treeStore <- selectWidgetRef widgets fileTreeStoreRef 
        HaskQuery.executeM $ do  
            dirEntry <- treeStoreGetValue treeStore treePath
            openFileInSourceTab editor dirEntry
    return ()

openFileInSourceTab :: EditorWindow -> DirectoryEntry -> IO ()
openFileInSourceTab editor directoryEntry = do
    case directoryEntry of
        PlainFile _fileName filePath -> 
            do  _tabNum <- addNotebookTab editor filePath
                return () 
        _ -> return ()    



makeEditor :: IO EditorWindow
makeEditor = do    
    editorWindow <- makeEditorWindow
    let widgetTVar = _editorWidgets editorWindow

    fileTreeScrolledWindow <- makeFileTreeScrolledWindow widgetTVar
    
    
    noteBook <- notebookNew
    notebookSetScrollable noteBook True
    insertWidget sourceTabBook noteBook widgetTVar    
    widgetShow noteBook

    mainHBox <- hPanedNew
    panedSetPosition mainHBox 250
    panedAdd1 mainHBox fileTreeScrolledWindow 
    panedAdd2 mainHBox noteBook    
    widgetShow mainHBox


    consoleBook <- shortcutPage editorWindow


    mainVPane <- vPanedNew
    panedAdd1 mainVPane mainHBox
    panedAdd2 mainVPane consoleBook
    panedSetPosition mainVPane 400
    widgetShow mainVPane

    insertWidget editorPane mainVPane widgetTVar
    
    return editorWindow

shortcutPage :: EditorWindow -> IO (Notebook)
shortcutPage editorWindow = do
    consoleBook <- notebookNew
    notebookSetTabPos consoleBook PosBottom
    shortcutPane <- vBoxNew False 0
    buttonPage <- hBoxNew False 0
    saveButton <- buttonNewWithMnemonic "_Cabal Install"
    cleanButton <- buttonNewWithMnemonic "_Cabal Clean"
    runButton <- buttonNewWithMnemonic "_Cabal Run"
    
    containerAdd buttonPage saveButton
    containerAdd buttonPage cleanButton
    containerAdd buttonPage runButton

    textViewScrolledWindow <- scrolledWindowNew Nothing Nothing    
    consoleOut <- textViewNew
    containerAdd textViewScrolledWindow consoleOut
    boxPackStart shortcutPane buttonPage PackNatural 0
    boxPackStart shortcutPane textViewScrolledWindow PackGrow 0

    _ <- notebookAppendPage consoleBook shortcutPane "Shortcuts"
    widgetShow consoleBook
    
    _ <- onClicked saveButton (runCabal editorWindow consoleOut ("cabal" , ["install"]))
    _ <- onClicked cleanButton (runCabal editorWindow consoleOut ("cabal", ["clean"]))
    _ <- onClicked runButton (runCabal editorWindow consoleOut ("cabal", ["run"]))   
    return consoleBook






