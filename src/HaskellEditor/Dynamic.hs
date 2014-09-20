{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}
module HaskellEditor.Dynamic 

where

import Graphics.UI.Gtk
import Data.Dynamic
import Data.Proxy
import HaskellEditor.Types

deriving instance Typeable Window
deriving instance Typeable Button
deriving instance Typeable VPaned
deriving instance Typeable HBox
deriving instance Typeable TreeView
deriving instance Typeable1 (TreeStore )
deriving instance Typeable Notebook

windowProxy :: Proxy Window
windowProxy = Proxy

buttonProxy :: Proxy Button
buttonProxy = Proxy

vpaneProxy :: Proxy VPaned
vpaneProxy = Proxy

treeViewProxy :: Proxy TreeView
treeViewProxy = Proxy

directoryEntryTreeStoreProxy :: Proxy (TreeStore DirectoryEntry)
directoryEntryTreeStoreProxy = Proxy

fromDynamicProxy :: Typeable a => Proxy a -> Dynamic -> Maybe a
fromDynamicProxy _ dynamic = fromDynamic dynamic

