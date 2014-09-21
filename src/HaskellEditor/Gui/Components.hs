module HaskellEditor.Gui.Components

where

import Graphics.UI.Gtk
import HaskQuery
import Control.Concurrent.STM
import HaskellEditor.Types
import HaskellEditor.Dynamic
import HaskellEditor.Gui.Util


mainWindowRef :: WidgetRef Window
mainWindowRef = widgetReference "mainWindow"

buttonBarRef :: WidgetRef HBox
buttonBarRef = widgetReference "buttonBar"

editorPane :: WidgetRef VPaned
editorPane = widgetReference "editorPane"


makeButtonBar :: TVar(Widgets) -> HaskQuery.Relation (Named (IO ())) b -> IO (Widgets)
makeButtonBar widgetTVar buttonCallbacks = do
    buttonBar <- hBoxNew False 0
    
    buttonBarButtons <- makeButtons widgetTVar buttonCallbacks 
    
    _ <- HaskQuery.runQueryM $ do
        namedWidget <- HaskQuery.selectM (_widgets buttonBarButtons) 
        button <- HaskQuery.selectDynamicWithTypeM buttonProxy (_content namedWidget)
        _ <- HaskQuery.executeM (boxPackStart buttonBar button PackNatural 0)
        return ()
    
    widgetShowAll buttonBar
    let buttonBarRelation = emptyWidgets { _widgets = HaskQuery.insert (_widgets emptyWidgets) $ namedDynamic (_identifier buttonBarRef) buttonBar}     
    insertWidgets widgetTVar buttonBarRelation
    return buttonBarRelation 

makeButtons :: TVar(Widgets) -> HaskQuery.Relation (Named (IO ())) b -> IO (Widgets)
makeButtons widgetTVar buttonCallbacks = do
    openProjectButton <- fmap (namedDynamic "openProjectButton") $ buttonNewWithLabel "Open Project"
  
    saveButton <- fmap (namedDynamic "saveProjectButton") $ buttonNewWithMnemonic "_Save Files"
   
    refreshButton <- fmap (namedDynamic "refreshProjectButton") $ buttonNewWithMnemonic "S_ynchronize Folders"
    let buttons = emptyWidgets { _widgets = HaskQuery.insertRows (_widgets emptyWidgets) [openProjectButton, saveButton, refreshButton]} 

    insertWidgets widgetTVar buttons
    
    _ <- HaskQuery.runQueryM $ do
        widgets <- getWidgets widgetTVar
        callback <- HaskQuery.selectM buttonCallbacks
        button <- selectWidget widgets (_identifier callback) buttonProxy
        _ <- HaskQuery.executeM $ onClicked button (_content callback) 
        return ()
    return buttons

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

createMainWindow :: TVar (Widgets) -> IO()
createMainWindow widgetTVar = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 500]
    _ <- onDestroy window mainQuit 
    insertWidget mainWindowRef window widgetTVar    
    return () 

