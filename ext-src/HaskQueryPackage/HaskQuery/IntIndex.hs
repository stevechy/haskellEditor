module HaskQuery.IntIndex where

import qualified Data.IntMap.Strict
import qualified Data.IntMap.Lazy
import qualified Data.IntSet
import qualified Control.Monad.Trans.Cont
import HaskQuery.AutoIndex

data IntIndex = IntIndex { relationIndex :: Data.IntMap.Strict.IntMap Data.IntSet.IntSet } deriving (Show)

type UpdatableIntIndex a = UpdatableIndex a IntIndex

emptyIntIndex :: IntIndex 
emptyIntIndex = IntIndex { relationIndex = Data.IntMap.Strict.empty }

intIndexSelector :: IntIndex -> Int -> Control.Monad.Trans.Cont.Cont (b -> b) Int
intIndexSelector index indexValue = 
    let indexSet = Data.IntMap.Strict.findWithDefault (Data.IntSet.empty) indexValue (relationIndex index) 
        in Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> Data.IntSet.foldl' (\foldSeed rowId -> continuation rowId foldSeed) seed indexSet ))

intIndex :: (a -> Int) -> Auto (ChangeSet a) (IntIndex)
intIndex fieldAccessor = Auto { updateAuto = updateIntIndexAuto fieldAccessor, readAuto = emptyIntIndex }

updateIntIndexAuto :: ( a -> Int) -> IntIndex -> ChangeSet a -> Auto (ChangeSet a) IntIndex
updateIntIndexAuto fieldAccessor autoIndex changeset = 
        Auto { updateAuto =  updateIntIndexAuto fieldAccessor, 
               readAuto = updateIntIndex autoIndex changeset fieldAccessor}

updateIntIndex :: IntIndex -> ChangeSet a -> (a->Int) -> IntIndex
updateIntIndex index changeset fieldAccessor = case changeset of
    Insert insertSet -> Data.IntMap.Lazy.foldlWithKey' (insertSingleIndexEntry fieldAccessor) index (inserted insertSet)
    Delete deleteSet -> Data.IntMap.Lazy.foldlWithKey' (deleteSingleIndexEntry fieldAccessor) index (deleted deleteSet)
    Update updateSet -> Data.IntMap.Lazy.foldlWithKey' (updateSingleIndexEntry fieldAccessor) index (updated updateSet)    

insertSingleIndexEntry :: (a->Int) -> IntIndex -> Int -> a -> IntIndex
insertSingleIndexEntry fieldAccessor seed key value = 
    IntIndex { relationIndex = Data.IntMap.Strict.insertWith ( \newSet oldSet -> Data.IntSet.union oldSet newSet ) (fieldAccessor value) (Data.IntSet.singleton key) (relationIndex seed) }

deleteSingleIndexEntry :: (a->Int) -> IntIndex -> Int -> a -> IntIndex
deleteSingleIndexEntry fieldAccessor seed key value = 
    IntIndex { relationIndex = Data.IntMap.Strict.update (\set -> let deletedSet = Data.IntSet.delete key set in if Data.IntSet.null deletedSet then Nothing else Just deletedSet ) (fieldAccessor value) (relationIndex seed) } 

updateSingleIndexEntry :: (a->Int) -> IntIndex -> Int -> (a,a) -> IntIndex
updateSingleIndexEntry fieldAccessor seed key value = 
    let deletedIndex = deleteSingleIndexEntry fieldAccessor seed key (fst value) in
        insertSingleIndexEntry fieldAccessor deletedIndex key (snd value)

