import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.SourceView.SourceLanguageManager
import Configuration.Types
import Data.Tree
import Data.Yaml
import System.Directory
import System.FilePath
import Control.Concurrent.STM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import qualified Shelly
import Data.String

data EditorWindow = EditorWindow { mainPane:: VPaned, 
                                   _fileTreeStore :: TreeStore DirectoryEntry, 
                                   _fileTreeView:: TreeView, 
                                   notebook :: Notebook, 
                                   _rootPath :: TVar (Maybe FilePath),                                    
                                   nextGuiId :: IORef (Int), 
                                   sourceBuffers :: TVar ( IntMap.IntMap (String, SourceBuffer))
                                   }

data DirectoryEntry = Directory String | PlainFile String String

languageFileExtensions = Map.fromList [(".hs", "haskell")]

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
  rootPathMaybe <- atomically $ readTVar $ _rootPath editor
  case rootPathMaybe of       
    Just rootPath -> do
      saveBuffers rootPath buffers
      return ()
    Nothing -> return ()
  return ()

saveBuffers rootPath buffers = do
  forM 
    (IntMap.elems buffers) 
    (\ (fileName, buffer) -> do 
        startIter <- textBufferGetStartIter buffer
        endIter <- textBufferGetEndIter buffer
        text <- textBufferGetText buffer startIter endIter True
        putStrLn "Saving file"
        putStrLn fileName
        putStrLn "-----"
        putStrLn text
        writeFile (combine rootPath fileName) text
        return ()
    )
    
openFileChooserFile editor treePath treeViewColumn = 
  do
    let treeStore = _fileTreeStore editor
    dirEntry <- treeStoreGetValue treeStore treePath
    case dirEntry of
      PlainFile fileName filePath -> do
        tabNum <- addNotebookTab editor filePath
        widgetQueueDraw (notebook editor)
      _ -> return ()
    return ()
    
fileLabelRenderer label = case label of
  Directory directory -> [cellText := directory]
  PlainFile file _ ->  [cellText := file]

iconLabelRenderer label = case label of
  Directory directory -> [cellPixbufStockId := stockDirectory]
  _ -> [cellPixbufStockId := stockFile]

fileTreeStoreNew :: IO (TreeStore DirectoryEntry)
fileTreeStoreNew = treeStoreNew []

makeEditor = do
    mainVPane <- vPanedNew
    
    mainHBox <- hPanedNew
    panedSetPosition mainHBox 250
    fileTreeScrolledWindow <- scrolledWindowNew Nothing Nothing
    treeStore <- fileTreeStoreNew
    
    fileTreeView <- treeViewNewWithModel treeStore
    


    iconTheme <- iconThemeGetDefault
    
    treeViewColumn <- treeViewColumnNew
    treeViewColumnSetTitle treeViewColumn "Project Files"
    typePix <- cellRendererPixbufNew
    cellRenderer <- cellRendererTextNew
    treeViewColumnPackStart treeViewColumn typePix True
    treeViewColumnPackStart treeViewColumn cellRenderer True 
    cellLayoutSetAttributes treeViewColumn cellRenderer treeStore fileLabelRenderer
    cellLayoutSetAttributes treeViewColumn typePix treeStore iconLabelRenderer
    
    treeViewAppendColumn fileTreeView treeViewColumn
    Just maybeColumn <- treeViewGetColumn fileTreeView 0

    containerAdd fileTreeScrolledWindow fileTreeView
    widgetShowAll fileTreeView
    widgetShowAll fileTreeScrolledWindow
    panedAdd1 mainHBox fileTreeScrolledWindow 
    
    noteBook <- notebookNew
    notebookSetScrollable noteBook True
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
   
    
    sourceLanguageManager <- sourceLanguageManagerGetDefault
    putStrLn $ takeExtension title
    let languageKeyMaybe = Map.lookup (takeExtension title) languageFileExtensions
    languageMaybe <- case languageKeyMaybe of
      Just language ->  sourceLanguageManagerGetLanguage sourceLanguageManager language
      Nothing -> return Nothing
    
    sourceBuffer <- sourceBufferNew Nothing
    
    
    
    _ <- sourceBufferSetLanguage sourceBuffer languageMaybe
    
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
    
    tabBar <- hBoxNew False 0
    tabLabel <- labelNew (Just title)
    closeImage <- imageNewFromStock stockClose IconSizeSmallToolbar
    tabClose <- buttonNew
    
    buttonSetImage tabClose closeImage 
    buttonSetRelief tabClose ReliefNone
    buttonSetFocusOnClick tabClose False
    
    onClicked tabClose $ do 
      pageNumberMaybe <- notebookPageNum noteBook textViewScrolledWindow
      case pageNumberMaybe of
        Just pageNumber -> do
                               notebookRemovePage noteBook pageNumber
                               atomically $ do 
                                 modifyTVar (sourceBuffers editor ) (IntMap.delete guiId)                                 
                                 return ()
                               return ()
        Nothing -> return ()
    
    boxPackStart tabBar tabLabel PackGrow 0
    boxPackStart tabBar tabClose PackNatural 0
    
    widgetShowAll tabBar
    
    menuLabel <- labelNew Nothing
    
    notebookAppendPageMenu noteBook textViewScrolledWindow tabBar menuLabel

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
  forest <- mapM (getFileNode rootPath relPath) $ filter (\x -> x /= "." && x /= "..") $  dirContents
  return forest

getFileNode rootPath relPath filePath = do  
  isDirectory <- Shelly.shelly $ Shelly.test_d $ fromString $ combine rootPath $ combine relPath filePath
  
  if isDirectory 
    then do
         subDirectories <- getDirContentsAsTreeWithRelpath rootPath (combine relPath filePath)
         return Node { rootLabel = Directory filePath, subForest = subDirectories } 
    else return (Node { rootLabel = PlainFile filePath (combine relPath filePath), subForest = [] })
  

newFileChooser handleChoice = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]

    fch <- fileChooserWidgetNew FileChooserActionOpen

    containerAdd window fch

    _ <- onFileActivated fch $
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

