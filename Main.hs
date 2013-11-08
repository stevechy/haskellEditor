import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Configuration.Types
import Data.Tree
import Data.Yaml
import System.Directory
import System.FilePath
import Control.Concurrent.STM
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Control.Monad
import qualified Shelly
import Data.String

data EditorWindow = EditorWindow { mainPane:: VPaned, 
                                   _fileTreeStore :: TreeStore String, 
                                   _fileTreeView:: TreeView, 
                                   notebook :: Notebook, 
                                   _rootPath :: TVar (Maybe FilePath),                                    
                                   nextGuiId :: IORef (Int), 
                                   sourceBuffers :: TVar ( IntMap.IntMap (String, SourceBuffer))
                                   }

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]
    
    mainBox <- vBoxNew False 0
    _ <- containerAdd window mainBox

    buttonBar <- hBoxNew False 0
    
    button <- buttonNewWithLabel "Open Project"
    saveButton <- buttonNewWithLabel "Save Files"
    boxPackStart buttonBar button PackNatural 0
    boxPackStart buttonBar saveButton PackNatural 0
    widgetShowAll buttonBar
    
    boxPackStart mainBox buttonBar PackNatural 0
          
    editor <- makeEditor
    
    
    onClicked button $ newFileChooser $ loadFile editor
    onClicked saveButton $ saveFiles editor
    
    onRowActivated (_fileTreeView editor) $ openFileChooserFile editor
    
    boxPackStart mainBox (mainPane editor) PackGrow 0
             

    onDestroy window mainQuit
    widgetShowAll button    
    widgetShowAll mainBox
    widgetShowAll window
    mainGUI
    
saveFiles editor = do    
  buffers <- atomically $ readTVar $ sourceBuffers editor
  _ <- forM 
           (IntMap.elems buffers) 
           (\ (fileName, buffer) -> do 
               startIter <- textBufferGetStartIter buffer
               endIter <- textBufferGetEndIter buffer
               text <- textBufferGetText buffer startIter endIter True
               putStrLn "Saving file"
               putStrLn fileName
               putStrLn "-----"
               putStrLn text
               return ()
               )
       
  return ()
    
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
    buffers <- atomically $ newTVar IntMap.empty
    
    guiId <- newIORef 0
    
    return EditorWindow { mainPane = mainVPane, 
                          _fileTreeView = fileTreeView, 
                          _fileTreeStore = treeStore, 
                          notebook = noteBook, 
                          _rootPath = filePath, 
                          nextGuiId = guiId,
                          sourceBuffers = buffers
                        } 

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
    
    guiId <- atomicModifyIORef (nextGuiId editor) (\x -> (x+1, x) )
    
    _ <- atomically $ do
      modifyTVar' (sourceBuffers editor) (\bufferMap -> IntMap.insert guiId (title, sourceBuffer) bufferMap)
      return ()
    
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
  
  canonicalRootPath <- getCanonicalRootPath config filepath
  
  atomically $ writeTVar (_rootPath editor) $ Just canonicalRootPath  
  forest <- getDirContentsAsTree canonicalRootPath
  let fileTreeStore = _fileTreeStore editor 
  treeStoreClear fileTreeStore  
  treeStoreInsertForest fileTreeStore [] 0 forest
  return ()

getCanonicalRootPath config filepath = do
  path <- canonicalizePath filepath
  let rootPath = combine (dropFileName path) $ rootFolder config
  canonicalRootPath <- canonicalizePath rootPath
  return canonicalRootPath
  
getDirContentsAsTree rootPath = getDirContentsAsTreeWithRelpath rootPath "."
  
getDirContentsAsTreeWithRelpath rootPath relPath = do
  dirContents <- getDirectoryContents (combine rootPath relPath)
  putStrLn $ show $ dirContents
  forest <- mapM (getFileNode rootPath) $ filter (\x -> x /= "." && x /= "..") $  dirContents
  return forest

getFileNode rootPath filePath = do  
  isDirectory <- Shelly.shelly $ Shelly.test_d $ fromString $ combine rootPath filePath
  
  return $ if isDirectory then Node { rootLabel = filePath, subForest = [ Node {rootLabel =".", subForest=[]} ] } else Node { rootLabel = filePath, subForest = [] }

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

