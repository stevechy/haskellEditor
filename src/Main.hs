import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.SourceView.SourceLanguageManager

import System.Directory
import System.FilePath
import Control.Concurrent.STM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.IORef

import HaskellEditor.Files
import HaskellEditor.Types
import HaskellEditor.ShellExecution
import HaskellEditor.Configuration
import HaskellEditor.Configuration.Types
import HaskellEditor.Dynamic
import qualified HaskQuery
import Data.Dynamic
import qualified Control.Monad.Trans.Cont
import Data.Proxy

data ComponentWithInitializer a = ComponentWithInitializer a EditorInitializer



languageFileExtensions :: Map.Map [Char] [Char]
languageFileExtensions = Map.fromList [(".hs", "haskell")]

makeButtons :: IO (Widgets)
makeButtons = do
    button <- fmap (namedDynamic "openProjectButton") $ buttonNewWithLabel "Open Project"
  
    saveButton <- fmap (namedDynamic "saveProjectButton") $ buttonNewWithMnemonic "_Save Files"
   
    refreshButton <- fmap (namedDynamic "refreshProjectButton") $ buttonNewWithMnemonic "S_ynchronize Folders"
    return emptyWidgets { _widgets = HaskQuery.insertRows (_widgets emptyWidgets) [button, saveButton, refreshButton]} 

named :: String -> IO a -> IO (Named a)
named name computation = do
    value <- computation
    return Named {_identifier = name, _content =value }

namedDynamic :: Typeable a => String -> a -> Named Dynamic
namedDynamic name value = Named {_identifier = name, _content = toDyn value }



createMainWindow :: TVar (Widgets) -> IO()
createMainWindow widgetTVar = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]
    _ <- onDestroy window mainQuit 
      

    atomically $ do
        modifyTVar widgetTVar (\currentWidget -> currentWidget { _widgets = HaskQuery.insert (_widgets currentWidget) $ namedDynamic "mainWindow" window} )   
    return () 

main :: IO ()
main = do
    _ <- initGUI
    widgetTVar <- atomically $ newTVar emptyWidgets
    createMainWindow widgetTVar
    
    mainBox <- vBoxNew False 0

    _ <- HaskQuery.runQueryM $ do
        widgets <- getWidgets widgetTVar
        mainWindow <- selectWidget widgets "mainWindow" windowProxy 
        HaskQuery.executeM $ containerAdd mainWindow mainBox

    buttonBar <- hBoxNew False 0
    
    buttonBarButtons <- makeButtons 
    atomically $ do
        modifyTVar widgetTVar (\currentWidget -> currentWidget {_widgets = HaskQuery.insertInto (_widgets currentWidget) (HaskQuery.select (_widgets buttonBarButtons))})
    _ <- HaskQuery.runQueryM $ do
        namedWidget <- HaskQuery.selectM (_widgets buttonBarButtons) 
        button <- HaskQuery.selectDynamicWithTypeM buttonProxy (_content namedWidget)
        _ <- HaskQuery.executeM (boxPackStart buttonBar button PackNatural 0)
        return ()
    
    widgetShowAll buttonBar
    
    boxPackStart mainBox buttonBar PackNatural 0
          
    editor <- makeEditor  

    let buttonCallbacks :: HaskQuery.Relation (Named (Button -> IO (ConnectId Button)) ) ()
        buttonCallbacks = HaskQuery.insertRows HaskQuery.empty [
            Named { _identifier = "openProjectButton", _content = (\namedButton -> onClicked namedButton $ newFileChooser $ loadFile editor)},
            Named { _identifier = "saveProjectButton", _content = (\namedButton -> onClicked namedButton $ saveFiles editor)},
            Named { _identifier = "refreshProjectButton", _content = (\namedButton -> onClicked namedButton $ refreshFolders editor)}
            ]    

    _ <- HaskQuery.runQueryM $ do
        widgets <- getWidgets widgetTVar
        callback <- HaskQuery.selectM buttonCallbacks
        button <- selectWidget widgets (_identifier callback) buttonProxy
        _ <- HaskQuery.executeM $ (_content callback) button
        return ()

    _ <- onRowActivated (_fileTreeView editor) $ openFileChooserFile editor
    
    boxPackStart mainBox (mainPane editor) PackGrow 0
             

    
    widgetShowAll mainBox

    _ <- HaskQuery.runQueryM $ do
        widgets <- HaskQuery.executeM $ readTVarIO widgetTVar
        mainWindow <- selectWidget widgets "mainWindow" windowProxy 
        HaskQuery.executeM $ widgetShowAll mainWindow
    mainGUI

getWidgets :: TVar(Widgets) -> Control.Monad.Trans.Cont.Cont (b -> IO b) Widgets
getWidgets widgetTVar = HaskQuery.executeM $ readTVarIO widgetTVar

selectWidget :: Typeable a => Widgets -> String -> Proxy a -> (Control.Monad.Trans.Cont.Cont (b -> IO b) a)
selectWidget widgets identifier typeProxy = do        
        widget <- HaskQuery.selectM $ _widgets widgets
        HaskQuery.filterM $ (_identifier widget) == identifier
        selectedWidget <- HaskQuery.selectDynamicWithTypeM typeProxy (_content widget)
        return selectedWidget

openFileChooserFile :: EditorWindow -> TreePath -> t -> IO ()    
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

fileLabelRenderer :: CellRendererTextClass o => DirectoryEntry -> [AttrOp o]    
fileLabelRenderer label = case label of
  Directory directory -> [cellText := directory]
  PlainFile file _ ->  [cellText := file]

iconLabelRenderer :: CellRendererPixbufClass o => DirectoryEntry -> [AttrOp o]
iconLabelRenderer label = case label of
  Directory directory -> [cellPixbufStockId := stockDirectory]
  _ -> [cellPixbufStockId := stockFile]

fileTreeStoreNew :: IO (TreeStore DirectoryEntry)
fileTreeStoreNew = treeStoreNew []

makeEditor :: IO EditorWindow
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
    
    _ <- treeViewAppendColumn fileTreeView treeViewColumn
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
    
    ComponentWithInitializer consoleBook consoleBookInitializer <- shortcutPage
    panedAdd2 mainVPane consoleBook
    widgetShow mainVPane
    
    panedSetPosition mainVPane 500
    
    filePath <- atomically $ newTVar Nothing
    buffers <- atomically $ newTVar IntMap.empty
    propertyRelation <- atomically $ newTVar HaskQuery.empty
    
    guiId <- newIORef 0
    
    let editorWindow =  EditorWindow { mainPane = mainVPane, 
                          _fileTreeView = fileTreeView, 
                          _fileTreeStore = treeStore, 
                          notebook = noteBook, 
                          _rootPath = filePath, 
                          nextGuiId = guiId,
                          sourceBuffers = buffers,
                          _properties = propertyRelation
                        } 
    consoleBookInitializer editorWindow
    return editorWindow

shortcutPage :: IO (ComponentWithInitializer Notebook)
shortcutPage = do
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
    return $ ComponentWithInitializer consoleBook (\ editor -> onClicked saveButton (runCabal editor consoleOut ("cabal" , ["install"])) >> onClicked cleanButton (runCabal editor consoleOut ("cabal", ["clean"])) >> onClicked runButton (runCabal editor consoleOut ("cabal", ["run"])) >> return ())   





addNotebookTab :: EditorWindow -> String -> IO (Int)
addNotebookTab editor title = do
    let noteBook = notebook editor
    textViewScrolledWindow <- scrolledWindowNew Nothing Nothing
   
    
    sourceLanguageManager <- sourceLanguageManagerGetDefault
    
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
    
    _ <- onClicked tabClose $ do 
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
    
    let menuLabelText :: Maybe String
        menuLabelText = Nothing
    menuLabel <- labelNew menuLabelText
    
    notebookAppendPageMenu noteBook textViewScrolledWindow tabBar menuLabel



loadFile :: EditorWindow -> FilePath -> IO ()
loadFile editor path = do
  configuration <- loadConfigFile path
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
  
  canonicalRootPath <- getCanonicalRootPath config filepath
  
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
                                    let fileTreeStore = _fileTreeStore editor 
                                    treeStoreClear fileTreeStore  
                                    treeStoreInsertForest fileTreeStore [] 0 forest
                                    return ()
        Nothing -> return ()

getCanonicalRootPath :: Configuration -> FilePath -> IO FilePath
getCanonicalRootPath config filepath = do
  path <- canonicalizePath filepath
  let rootPath = combine (dropFileName path) $ rootFolder config
  canonicalRootPath <- canonicalizePath rootPath
  return canonicalRootPath

newFileChooser ::  (FilePath -> IO t) -> IO ()
newFileChooser handleChoice = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]

    fch <- fileChooserWidgetNew FileChooserActionOpen

    containerAdd window fch

    _ <- on fch fileActivated  $
        do filePath <- fileChooserGetFilename fch
           case filePath of
               Just dpath -> do 
                                _ <- handleChoice dpath
                                widgetDestroy window
               Nothing -> return ()
     
    _ <- fileChooserSetCurrentFolder fch "." 
    
    widgetShowAll fch
    widgetShowAll window
    return ()

