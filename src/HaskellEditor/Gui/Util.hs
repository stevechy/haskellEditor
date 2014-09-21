module HaskellEditor.Gui.Util
where

import qualified HaskQuery
import Control.Concurrent.STM
import HaskellEditor.Types
import Data.Typeable
import Data.Proxy


insertNamedWidget :: Typeable a => TVar(Widgets) -> String -> a -> IO () 
insertNamedWidget widgetTVar name widget = 
    atomically $ do
        modifyTVar widgetTVar $ \currentWidget -> 
                 currentWidget {_widgets = HaskQuery.insert (_widgets currentWidget) $ namedDynamic name widget}

insertWidgets :: TVar(Widgets) -> Widgets -> IO ()
insertWidgets widgetTVar widgets = do
    atomically $ do
        modifyTVar widgetTVar (\currentWidget -> currentWidget {_widgets = HaskQuery.insertInto (_widgets currentWidget) (HaskQuery.select (_widgets widgets))})


getWidgets :: TVar(Widgets) -> HaskQuery.Cont (b -> IO b) Widgets
getWidgets widgetTVar = HaskQuery.executeM $ readTVarIO widgetTVar

selectWidget :: Typeable a => Widgets -> String -> Proxy a -> (HaskQuery.Cont (b -> IO b) a)
selectWidget widgets identifier typeProxy = do        
        widget <- HaskQuery.selectM $ _widgets widgets
        HaskQuery.filterM $ (_identifier widget) == identifier
        selectedWidget <- HaskQuery.selectDynamicWithTypeM typeProxy (_content widget)
        return selectedWidget


selectWidgetRef :: Typeable a => Widgets -> WidgetRef a -> (HaskQuery.Cont (b -> IO b) a)
selectWidgetRef widgets widgetRef = selectWidget widgets (_identifier widgetRef) (_content widgetRef)

insertWidget :: Typeable a => WidgetRef a -> a -> TVar(Widgets) -> IO ()
insertWidget widgetRef widget widgetTVar = atomically $ do
        let namedWidget = namedDynamic (_identifier widgetRef) widget
        modifyTVar widgetTVar (\currentWidget -> currentWidget {_widgets = HaskQuery.insert (_widgets currentWidget) namedWidget})
        return ()
