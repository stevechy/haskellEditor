module HaskQuery.OrdIndex where

import qualified Data.IntMap.Lazy
import qualified Data.Map.Strict
import qualified Data.IntSet
import qualified Control.Monad.Trans.Cont
import HaskQuery.AutoIndex

data OrdIndex a = OrdIndex { relationIndex :: Data.Map.Strict.Map a Data.IntSet.IntSet } deriving (Show)

type UpdatableOrdIndex a b = UpdatableIndex a (OrdIndex b)

emptyOrdIndex :: OrdIndex a
emptyOrdIndex = OrdIndex { relationIndex = Data.Map.Strict.empty }

ordIndexSelector :: Ord indexed => OrdIndex indexed -> indexed -> Control.Monad.Trans.Cont.Cont (b -> b) Int
ordIndexSelector index indexValue = 
    let indexSet = Data.Map.Strict.findWithDefault (Data.IntSet.empty) indexValue (relationIndex index) 
        in Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> Data.IntSet.foldl' (\foldSeed rowId -> continuation rowId foldSeed) seed indexSet ))

ordIndex :: Ord indexed => (a -> indexed) -> Auto (ChangeSet a) (OrdIndex indexed)
ordIndex fieldAccessor = Auto { updateAuto = updateOrdIndexAuto fieldAccessor, readAuto = emptyOrdIndex }

updateOrdIndexAuto :: Ord indexed => ( a -> indexed) -> OrdIndex indexed -> ChangeSet a -> Auto (ChangeSet a) (OrdIndex indexed)
updateOrdIndexAuto fieldAccessor autoIndex changeset = 
    let updateAutoFunction = updateOrdIndexAuto fieldAccessor
        updatedIndex = updateOrdIndex autoIndex changeset fieldAccessor in
        Auto { updateAuto = updateAutoFunction, readAuto = updatedIndex}

updateOrdIndex :: Ord indexed => OrdIndex indexed -> ChangeSet a -> (a->indexed) -> OrdIndex indexed
updateOrdIndex index changeset fieldAccessor = case changeset of
    Insert insertSet -> Data.IntMap.Lazy.foldlWithKey' (insertBasicIndexEntry fieldAccessor) index (inserted insertSet)
    Delete deleteSet -> Data.IntMap.Lazy.foldlWithKey' (deleteBasicIndexEntry fieldAccessor) index (deleted deleteSet)
    Update updateSet -> Data.IntMap.Lazy.foldlWithKey' (updateBasicIndexEntry fieldAccessor) index (updated updateSet)    

insertBasicIndexEntry :: Ord indexed => (a->indexed) -> OrdIndex indexed -> Int -> a -> OrdIndex indexed
insertBasicIndexEntry fieldAccessor seed key value = 
    OrdIndex { relationIndex = Data.Map.Strict.insertWith ( \newSet oldSet -> Data.IntSet.union oldSet newSet ) (fieldAccessor value) (Data.IntSet.singleton key) (relationIndex seed) }

deleteBasicIndexEntry :: Ord indexed => (a->indexed) -> OrdIndex indexed -> Int -> a -> OrdIndex indexed
deleteBasicIndexEntry fieldAccessor seed key value = 
    OrdIndex { relationIndex = Data.Map.Strict.update (\set -> let deletedSet = Data.IntSet.delete key set in if Data.IntSet.null deletedSet then Nothing else Just deletedSet ) (fieldAccessor value) (relationIndex seed) } 

updateBasicIndexEntry :: Ord indexed => (a->indexed) -> OrdIndex indexed -> Int -> (a,a) -> OrdIndex indexed
updateBasicIndexEntry fieldAccessor seed key value = 
    let deletedIndex = deleteBasicIndexEntry fieldAccessor seed key (fst value) in
        insertBasicIndexEntry fieldAccessor deletedIndex key (snd value)

