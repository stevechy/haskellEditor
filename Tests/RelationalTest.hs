{-#LANGUAGE GADTs, EmptyDataDecls #-}

import HaskellEditor.Relational
import Test.HUnit
import Data.List
import System.IO
import qualified Data.DList
import Control.Monad

import System.Directory



data Person = Person { _name :: String, _age :: Integer, _country ::String }

data Country = Country { _countryCode::String, _countryName :: String }

personRelation = Relation { _relations = [ Person { _name = "Alice", _age = 30, _country = "CA" }, Person { _name = "Bob", _age = 45, _country = "US" } ]}

countryRelation = Relation { _relations = [ Country { _countryCode = "CA", _countryName = "Canada"}, Country { _countryCode = "US", _countryName = "United States"} ]}


type SeedConsumer b = Data.DList.DList b -> IO (Data.DList.DList b)
type IOConsumer a b = a -> IO (Data.DList.DList b)
type IOAccumulator a b = a -> SeedConsumer b
type IOSource a b = Data.DList.DList b -> IOAccumulator a b-> IO (Data.DList.DList b)

relationIO :: Relation a -> IOSource a b
relationIO relation seed consumer = foldM (\seed input -> consumer input seed) seed (_relations relation) 

selectIO :: IOSource a b -> Data.DList.DList b -> IOAccumulator a b -> IO ( Data.DList.DList b )
selectIO ioSource seed consumer = do
    ioSource seed consumer

select :: IOSource a b -> IOConsumer a b -> SeedConsumer b
select source consumer = selectPipe source (accum consumer)

selectPipe :: IOSource a b -> IOAccumulator a b -> SeedConsumer b
selectPipe source accumulator = \seed  -> source seed accumulator

applySeed :: Data.DList.DList b-> (Data.DList.DList b -> IO (Data.DList.DList b)) -> IO (Data.DList.DList b)
applySeed seed seedConsumer = seedConsumer seed


accum ::  IOConsumer a b -> IOAccumulator a b
accum consumer = \ input seed -> do
    result <- consumer input
    return $ Data.DList.append seed result

directoryContentsRelation :: FilePath -> IOSource FilePath b
directoryContentsRelation filePath seed consumer = do
    directoryContents <- getDirectoryContents filePath
    relationIO (Relation {_relations=directoryContents}) seed consumer 

data FileNode = PlainFile FilePath | Directory FilePath deriving (Show)

directoryType :: FilePath -> IOSource FileNode b
directoryType filePath seed consumer = do
    isDir <- doesDirectoryExist filePath
    if isDir
        then consumer (Directory filePath) seed
        else consumer (PlainFile filePath) seed

readFileRelation :: IOSource String b
readFileRelation seed consumer = withFile "LICENSE" ReadMode (readFileStep seed consumer)

readFileStep :: Data.DList.DList b -> IOAccumulator String b -> Handle -> IO ( Data.DList.DList b )
readFileStep seed consumer handle = do
   isEof <- hIsEOF handle
   if isEof 
       then return $ seed
       else do 
                 line <- hGetLine handle
                 result <- consumer line seed
                 readFileStep result consumer handle
   


satisfying :: Bool -> Relation a -> Relation a
satisfying True relation = relation
satisfying False relation = Relation { _relations = []}

sat :: Data.DList.DList a -> Bool -> a -> Data.DList.DList a
sat seed True elem = Data.DList.snoc seed elem
sat seed False elem = seed

having :: Bool -> a -> Data.DList.DList a
having True elem = Data.DList.singleton elem
having False elem = Data.DList.empty

yield :: a -> Relation a
yield x = Relation {_relations = [ x]}

emptySeed = Data.DList.empty

relationIOTest :: Data.DList.DList (String,String,String,String) -> IO (Data.DList.DList (String,String,String,String))
relationIOTest = selectPipe (relationIO personRelation) $ \person ->
    select (relationIO countryRelation) $ \country ->
    return $ having ((_country person) == (_countryCode country)) (_name person ,_country person, _countryCode country, _countryName country)

showDlist :: (Show a) => Data.DList.DList a -> String
showDlist list = show $ Data.DList.toList list

tests :: Test
tests = TestList [
        TestLabel "SimpleTest" $ TestCase $ do 
            assertEqual "Stuff" 1 1
        ,TestLabel "Should Query Relationally" $ TestCase $ do 
            x <- relationIOTest emptySeed
            putStrLn $ showDlist x
            assertEqual "Stuff" 1 1
        ,TestLabel "Should Query File" $ TestCase $ do
            fileRelations <- readFileRelation emptySeed (\ line seed  -> return (Data.DList.snoc seed line) ) 
            putStrLn $ showDlist $ fileRelations
            assertEqual "Queried file" 1 1
        ,TestLabel "Should Query File" $ TestCase $ do
            fileRelations <- readFileRelation emptySeed (\ line seed  -> return $ (sat seed (length line <= 10) line) )  
            putStrLn $ showDlist $ fileRelations
            assertEqual "Queried file" 1 1
        ,TestLabel "Should Query Dir" $ TestCase $ do
            dirRelations <- directoryContentsRelation "Tests" emptySeed (\filePath seed  -> return $ Data.DList.snoc seed filePath)
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" 1 1
        ,TestLabel "Should Show Dir types " $ TestCase $ do
            let dirTypes = selectPipe (directoryContentsRelation "Tests") $ \filePath ->
                               select (directoryType filePath) $ \fileNode ->
                               return $ having True fileNode
            dirRelations <- dirTypes emptySeed
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" 1 1
    ]

main :: IO()
main = do
    _ <- runTestTT tests
    return ()