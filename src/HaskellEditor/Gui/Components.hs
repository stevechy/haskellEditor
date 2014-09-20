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
    atomically $ do
        modifyTVar widgetTVar (\currentWidget -> currentWidget {_widgets = HaskQuery.insertInto (_widgets currentWidget) (HaskQuery.select (_widgets buttonBarRelation))})
    return buttonBarRelation 

makeButtons :: TVar(Widgets) -> HaskQuery.Relation (Named (IO ())) b -> IO (Widgets)
makeButtons widgetTVar buttonCallbacks = do
    openProjectButton <- fmap (namedDynamic "openProjectButton") $ buttonNewWithLabel "Open Project"
  
    saveButton <- fmap (namedDynamic "saveProjectButton") $ buttonNewWithMnemonic "_Save Files"
   
    refreshButton <- fmap (namedDynamic "refreshProjectButton") $ buttonNewWithMnemonic "S_ynchronize Folders"
    let buttons = emptyWidgets { _widgets = HaskQuery.insertRows (_widgets emptyWidgets) [openProjectButton, saveButton, refreshButton]} 

    atomically $ do
        modifyTVar widgetTVar (\currentWidget -> currentWidget {_widgets = HaskQuery.insertInto (_widgets currentWidget) (HaskQuery.select (_widgets buttons))})

    _ <- HaskQuery.runQueryM $ do
        widgets <- getWidgets widgetTVar
        callback <- HaskQuery.selectM buttonCallbacks
        button <- selectWidget widgets (_identifier callback) buttonProxy
        _ <- HaskQuery.executeM $ onClicked button (_content callback) 
        return ()
    return buttons



createMainWindow :: TVar (Widgets) -> IO()
createMainWindow widgetTVar = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 500]
    _ <- onDestroy window mainQuit       
    atomically $ do
        modifyTVar widgetTVar (\currentWidget -> currentWidget { _widgets = HaskQuery.insert (_widgets currentWidget) $ namedDynamic (_identifier mainWindowRef) window} )   
    return () 

