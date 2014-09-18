{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module HaskellEditor.Dynamic 

where

import Graphics.UI.Gtk
import Data.Dynamic
import Data.Proxy

deriving instance Typeable Window
deriving instance Typeable Button
deriving instance Typeable VPaned
deriving instance Typeable HBox

windowProxy :: Proxy Window
windowProxy = Proxy

buttonProxy :: Proxy Button
buttonProxy = Proxy

vpaneProxy :: Proxy VPaned
vpaneProxy = Proxy

fromDynamicProxy :: Typeable a => Proxy a -> Dynamic -> Maybe a
fromDynamicProxy proxy dynamic = fromDynamic dynamic

