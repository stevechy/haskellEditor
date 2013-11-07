import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Configuration.Types
import Data.Tree
import Data.Yaml
import System.Directory
import System.FilePath
import Control.Concurrent.STM

data EditorWindow = EditorWindow { mainPane:: VPaned, _fileTreeStore :: TreeStore String, _fileTreeView:: TreeView, notebook :: Notebook, _rootPath :: TVar (Maybe FilePath) }

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]
    
    mainBox <- vBoxNew False 0
    _ <- containerAdd window mainBox

    button <- buttonNewWithLabel "Open Project"
   
    boxPackStart mainBox button PackNatural 0
          
    editor <- makeEditor
    
    
    onClicked button $ newFileChooser $ loadFile editor
    
    onRowActivated (_fileTreeView editor) $ openFileChooserFile editor
    
    boxPackStart mainBox (mainPane editor) PackGrow 0
             

    onDestroy window mainQuit
    widgetShowAll button    
    widgetShowAll mainBox
    widgetShowAll window
    mainGUI
    
openFileChooserFile editor treePath treeViewColumn = 
  do
    let treeStore = _fileTreeStore editor
    fileName <- treeStoreGetValue treeStore treePath
    tabNum <- addNotebookTab editor fileName
    widgetQueueDraw (notebook editor)
    return ()
    
makeEditor = do
    mainVPane <- vPanedNew
    
    mainHBox <- hPanedNew
    panedSetPosition mainHBox 250
    fileTreeScrolledWindow <- scrolledWindowNew Nothing Nothing
    treeStore <- treeStoreNew []
    let column = makeColumnIdString 0
    treeModelSetColumn treeStore column id
    fileTreeView <- treeViewNewWithModel treeStore
    treeViewColumn <- treeViewColumnNew
    treeViewColumnSetTitle treeViewColumn "Project Files"
    cellRenderer <- cellRendererTextNew
    treeViewColumnPackStart treeViewColumn cellRenderer True 
    cellLayoutSetAttributes treeViewColumn cellRenderer treeStore $ (\label -> [cellText := label])
    treeViewAppendColumn fileTreeView treeViewColumn
    Just maybeColumn <- treeViewGetColumn fileTreeView 0

    containerAdd fileTreeScrolledWindow fileTreeView
    widgetShowAll fileTreeView
    widgetShowAll fileTreeScrolledWindow
    panedAdd1 mainHBox fileTreeScrolledWindow 
    
    noteBook <- notebookNew
    widgetShow noteBook
    panedAdd2 mainHBox noteBook    
    
    panedAdd1 mainVPane mainHBox
    widgetShow mainHBox
    
    consoleBook <- notebookNew
    notebookSetTabPos consoleBook PosBottom
    buttonPage <- hBoxNew False 0
    notebookAppendPage consoleBook buttonPage "Shortcuts"
    widgetShow consoleBook
    panedAdd2 mainVPane consoleBook
    widgetShow mainVPane
    
    panedSetPosition mainVPane 500
    
    filePath <- atomically $ newTVar Nothing
    
    return EditorWindow { mainPane = mainVPane, _fileTreeView = fileTreeView, _fileTreeStore = treeStore, notebook = noteBook, _rootPath = filePath } 

parseConfig :: FilePath -> IO (Maybe Configuration.Types.Configuration)
parseConfig = Data.Yaml.decodeFile

addNotebookTab editor title = do
    let noteBook = notebook editor
    textViewScrolledWindow <- scrolledWindowNew Nothing Nothing
    sourceBuffer <- sourceBufferNew Nothing
    rootPath <- atomically $ readTVar $ _rootPath editor
    case rootPath of
      Just path -> do
        fileContents <- readFile (combine path title)        
        putStrLn fileContents
        textBufferSetText sourceBuffer fileContents
        return ()
      Nothing -> return ()
    textView <- sourceViewNewWithBuffer sourceBuffer 
    sourceViewSetShowLineNumbers textView True
    font <- fontDescriptionNew
    fontDescriptionSetFamily font "monospace"
    widgetModifyFont textView (Just font)
    containerAdd textViewScrolledWindow textView
    widgetShowAll textViewScrolledWindow
    notebookAppendPage noteBook textViewScrolledWindow title

loadConfigFile path = do
    maybeConfiguration <- parseConfig path    
    return maybeConfiguration

loadFile editor path = do
  configuration <- loadConfigFile path
  case configuration of
    Just config -> do
      putStrLn $ show $ config
      loadConfiguration editor config path
      return ()
    Nothing -> return ()

loadConfiguration editor config filepath = do
  path <- canonicalizePath filepath
  let rootPath = combine (dropFileName path) $ rootFolder config
  canonicalRootPath <- canonicalizePath rootPath
  atomically $ writeTVar (_rootPath editor) $ Just canonicalRootPath
  dirContents <- getDirectoryContents canonicalRootPath
  putStrLn $ show $ dirContents
  let forest = map (\file -> Node { rootLabel = file, subForest = [] }) dirContents
  let fileTreeStore = _fileTreeStore editor 
  treeStoreClear fileTreeStore  
  treeStoreInsertForest fileTreeStore [] 0 forest
  return ()

newFileChooser handleChoice = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]

    fch <- fileChooserWidgetNew FileChooserActionOpen

    containerAdd window fch

    onFileActivated fch $
        do filePath <- fileChooserGetFilename fch
           case filePath of
               Just dpath -> do putStrLn dpath
                                handleChoice dpath
                                widgetDestroy window
               Nothing -> return ()
     
    _ <- fileChooserSetCurrentFolder fch "." 
    
    widgetShowAll fch
    widgetShowAll window
    return ()

