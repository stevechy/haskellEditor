module HaskQuery (
module HaskQuery.AutoIndex, 
Relation,
Cont,
empty,
emptyWithIndex,
reindex,
runQuery,
select,
runQueryM,
executeM,
selectM,
filterM,
selectWithIndex,
selectDynamic,
selectDynamicWithTypeM,
insert,
insertRows,
insertInto,
update,
delete
) where

import qualified Data.IntMap.Lazy
import qualified Data.List
import qualified Control.Monad.Trans.Cont
import qualified Data.Typeable
import qualified Data.Dynamic
import qualified Data.Proxy

import HaskQuery.AutoIndex

data Relation a b = Relation { _relation :: Data.IntMap.Lazy.IntMap a , _lastRowId :: Int, _indices :: UpdatableIndex a b} deriving (Show)

type Cont r a = Control.Monad.Trans.Cont.Cont r a

empty :: Relation a ()
empty = Relation { _relation = Data.IntMap.Lazy.empty, _lastRowId = 0, _indices = emptyIndex }

emptyWithIndex :: UpdatableIndex a c -> Relation a c
emptyWithIndex index = reindex empty index

reindex :: Relation a b -> UpdatableIndex a c -> Relation a c
reindex indexedRelation newIndex =
    let originalRelation = _relation indexedRelation
        in indexedRelation { _relation = originalRelation, _indices = updateAutoWithInput newIndex $ Insert $ InsertSet {inserted = originalRelation} }

runQuery :: Control.Monad.Trans.Cont.Cont ([a] -> [a]) a ->  [a]
runQuery query = (Control.Monad.Trans.Cont.runCont query (\value -> (\list -> value : list))) []

select :: Relation a c -> Control.Monad.Trans.Cont.Cont (b -> b) a
select relation = Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> Data.IntMap.Lazy.foldl (\foldSeed value -> continuation value foldSeed) seed (_relation relation)))

filterM :: Monad m => Bool -> Control.Monad.Trans.Cont.Cont (a ->  m a) () 
filterM predicate = Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> if predicate then (continuation () seed ) else return seed))

runQueryM ::  Monad m => Control.Monad.Trans.Cont.Cont ([a] ->  m [a]) a ->  m [a]
runQueryM query = (Control.Monad.Trans.Cont.runCont query (\value -> (\list -> return (value : list)))) []

selectM :: Monad m => Relation a c -> Control.Monad.Trans.Cont.Cont (b -> m b) a
selectM relation = Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> Data.IntMap.Lazy.foldl (\foldSeed value ->  foldSeed >>= continuation value) (return seed) (_relation relation)))

executeM :: Monad m => m a -> Control.Monad.Trans.Cont.Cont (b -> m b) a
executeM computation = Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> do
    value <- computation
    result <- continuation value seed
    return result))

selectWithIndex :: (c -> indexValue -> Control.Monad.Trans.Cont.Cont (b -> b) Int) -> Relation a c -> indexValue -> Control.Monad.Trans.Cont.Cont (b -> b) a
selectWithIndex indexAccessor relation indexValue = do 
    rowId <- indexAccessor (readAuto $ _indices relation) indexValue
    return $ (_relation relation) Data.IntMap.Lazy.! rowId

selectDynamic :: (Data.Typeable.Typeable a) => Data.Dynamic.Dynamic -> Control.Monad.Trans.Cont.Cont (b->b) a
selectDynamic value = Control.Monad.Trans.Cont.cont (\continuation -> (case Data.Dynamic.fromDynamic value of Just typed -> continuation typed ; Nothing -> id))

selectDynamicWithType :: (Data.Typeable.Typeable a) => Data.Proxy.Proxy a -> Data.Dynamic.Dynamic -> Control.Monad.Trans.Cont.Cont (b->b) a
selectDynamicWithType proxy value = selectDynamic value

selectDynamicWithTypeM :: (Data.Typeable.Typeable a, Monad m) => Data.Proxy.Proxy a -> Data.Dynamic.Dynamic -> Control.Monad.Trans.Cont.Cont (b->m b) a
selectDynamicWithTypeM proxy value = Control.Monad.Trans.Cont.cont (\continuation -> (\seed -> (case Data.Dynamic.fromDynamic value of Just typed -> continuation typed seed ; Nothing -> return seed)))



insert :: Relation a b -> a -> Relation a b
insert indexedRelation item = 
    let newRowId = (_lastRowId indexedRelation) + 1 
        updatedRelation = Data.IntMap.Lazy.insert newRowId item (_relation indexedRelation)               
        updatedIndex = updateAutoWithInput (_indices indexedRelation) (Insert $ InsertSet { inserted = Data.IntMap.Lazy.singleton newRowId item})
        in
            Relation { _relation = updatedRelation, _lastRowId = newRowId, _indices = updatedIndex}

insertRows :: Relation a b -> [a] -> Relation a b
insertRows indexedRelation rows = Data.List.foldl' (insert) indexedRelation rows

insertInto :: Relation a b -> Control.Monad.Trans.Cont.Cont (Relation a b -> Relation a b) a -> Relation a b
insertInto indexedRelation insertContinuation = 
    Control.Monad.Trans.Cont.runCont insertContinuation (\ row -> (\seedIndexedRelation -> insert seedIndexedRelation row)) indexedRelation


update :: Relation a b-> (a -> Bool) -> (a -> a) -> Relation a b
update indexedRelation predicate updateFunction = 
    let originalRelation = (_relation indexedRelation)
        affectedRows = Data.IntMap.Lazy.filter predicate originalRelation 
        updateMap = Data.IntMap.Lazy.map (\row -> (row, updateFunction row)) affectedRows
        updatedRows = Data.IntMap.Lazy.map (\ (row, updatedRow) -> updatedRow) updateMap
        updatedIndex = updateAutoWithInput (_indices indexedRelation) $ Update $ UpdateSet { updated = updateMap }
        in
            indexedRelation { _relation = updatedRows, _indices = updatedIndex }


delete :: Relation a b -> (a -> Bool) -> Relation a b
delete indexedRelation predicate = 
    let 
        originalRelation = (_relation indexedRelation)
        affectedRows = Data.IntMap.Lazy.filter predicate originalRelation          
        updatedRelation =   Data.IntMap.Lazy.difference originalRelation affectedRows
        updatedIndex = updateAutoWithInput (_indices indexedRelation) $ Delete $ DeleteSet { deleted = affectedRows }
        in
            indexedRelation { _relation = updatedRelation, _indices = updatedIndex}

