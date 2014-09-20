module HaskellEditor.Gui.SourceTabBook

where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.SourceView.SourceLanguageManager
import qualified Data.Map as Map
import System.FilePath
import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap.Strict as IntMap

import HaskellEditor.Types
import HaskellEditor.Dynamic()
import HaskellEditor.Gui.Util
import qualified HaskQuery

languageFileExtensions :: Map.Map [Char] [Char]
languageFileExtensions = Map.fromList [(".hs", "haskell")]

sourceTabBook :: WidgetRef Notebook
sourceTabBook = widgetReference "sourceTabBook"


addNotebookTab :: EditorWindow -> String -> IO ([Int])
addNotebookTab editorWindow title = do
    HaskQuery.runQueryM $ do
        widgets <- getWidgets (_editorWidgets editorWindow)
        noteBook <- selectWidgetRef widgets sourceTabBook
        HaskQuery.executeM $ addTabToNotebook editorWindow noteBook title

addTabToNotebook :: EditorWindow -> Notebook-> String -> IO (Int)
addTabToNotebook editor noteBook title = do
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
    
    tabNumber <- notebookAppendPageMenu noteBook textViewScrolledWindow tabBar menuLabel
    widgetQueueDraw noteBook
    return tabNumber

