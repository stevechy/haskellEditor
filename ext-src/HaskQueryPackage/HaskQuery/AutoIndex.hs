module HaskQuery.AutoIndex where

import qualified Data.IntMap.Lazy

data InsertSet a = InsertSet { inserted :: Data.IntMap.Lazy.IntMap a }

data DeleteSet a = DeleteSet { deleted :: Data.IntMap.Lazy.IntMap a }

data UpdateSet a = UpdateSet { updated :: Data.IntMap.Lazy.IntMap (a,a) }

data ChangeSet a = Insert (InsertSet a) | Delete (DeleteSet a) | Update (UpdateSet a)

data Auto a b = Auto { updateAuto :: b -> a -> (Auto a b), readAuto :: b} 

instance Show b => Show(Auto a b) where
    show auto = "Auto { " ++ (show $ readAuto auto) ++ " }"


type UpdatableIndex a index = Auto (ChangeSet a) index

updateAutoWithInput :: Auto a b -> a -> Auto a b
updateAutoWithInput auto input = (updateAuto auto) (readAuto auto) input



updateEmptyIndex :: () -> a -> Auto a ()
updateEmptyIndex index changeSet = emptyIndex

emptyIndex = Auto { updateAuto = updateEmptyIndex, readAuto = ()}


