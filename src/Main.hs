import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Gdk.Events
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.SourceView.SourceLanguageManager

import System.Directory
import System.FilePath
import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map

import HaskellEditor.Files
import HaskellEditor.Types
import HaskellEditor.ShellExecution
import HaskellEditor.Configuration
import HaskellEditor.Configuration.Types
import HaskellEditor.Dynamic
import HaskellEditor.Gui.Components
import HaskellEditor.Gui.FileTree
import HaskellEditor.Gui.Util
import qualified HaskQuery



data ComponentWithInitializer a = ComponentWithInitializer a EditorInitializer



languageFileExtensions :: Map.Map [Char] [Char]
languageFileExtensions = Map.fromList [(".hs", "haskell")]


main :: IO ()
main = do
    _ <- initGUI
    widgetTVar <- atomically $ newTVar emptyWidgets

    editor <- makeEditor widgetTVar 
    createMainWindow widgetTVar
        
    mainBox <- vBoxNew False 0

    _ <- makeButtonBar widgetTVar $ HaskQuery.insertRows HaskQuery.empty [
                Named { _identifier = "openProjectButton", _content =  newFileChooser $ loadFile editor},
                Named { _identifier = "saveProjectButton", _content =  saveFiles editor},
                Named { _identifier = "refreshProjectButton", _content = refreshFolders editor}
            ]              
        
    widgets <- readTVarIO widgetTVar 

    _ <- HaskQuery.runQueryM $ do
        fileTreeView <- selectWidget widgets "fileTreeView" treeViewProxy  
        HaskQuery.executeM $ onRowActivated fileTreeView $ openFileChooserFile editor                 
      
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
openFileChooserFile editor treePath treeViewColumn = do
    _ <- HaskQuery.runQueryM $ do
        widgets <- getWidgets (_editorWidgets editor)
        treeStore <- selectWidget widgets "fileTreeStore" directoryEntryTreeStoreProxy 
        HaskQuery.executeM $ do  
            dirEntry <- treeStoreGetValue treeStore treePath
            case dirEntry of
                PlainFile fileName filePath -> do
                    tabNum <- addNotebookTab editor filePath
                    widgetQueueDraw (notebook editor)
                _ -> return ()
            return ()
    return ()





makeEditor :: TVar(Widgets) -> IO EditorWindow
makeEditor widgetTVar = do
    mainVPane <- vPanedNew
    
    mainHBox <- hPanedNew
    panedSetPosition mainHBox 250
    fileTreeScrolledWindow <- scrolledWindowNew Nothing Nothing
    fileTreeView <- makeFileTreeView widgetTVar
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

    insertWidget editorPane mainVPane widgetTVar

    widgetShow mainVPane
    
    panedSetPosition mainVPane 400
    
    filePath <- atomically $ newTVar Nothing
    buffers <- atomically $ newTVar IntMap.empty
    propertyRelation <- atomically $ newTVar HaskQuery.empty
    
   
    guiId <- newIORef 0
    
    let editorWindow =  EditorWindow {   
                          _editorWidgets = widgetTVar, 
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
  configuration <- if (takeExtension path) == ".cabal"
                        then return $ Just $ Configuration { rootFolder =".", cabalFile = takeFileName path, commands = Nothing }
                        else loadConfigFile path
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
                                        _ <- HaskQuery.runQueryM $ do
                                            widgets <- getWidgets (_editorWidgets editor)
                                            fileTreeStore <- selectWidget widgets "fileTreeStore" directoryEntryTreeStoreProxy 
                                            HaskQuery.executeM $ do
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

