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

directoryContentsRelation :: DirectoryPath -> IOSource DirectoryPath b
directoryContentsRelation (DirectoryPath filePath nodePath) seed consumer = do
    let dirPath = filePath ++ "/" ++ nodePath
    directoryContents <- getDirectoryContents dirPath
    relationIO (Relation {_relations= map (\nodeName -> DirectoryPath dirPath nodeName) directoryContents}) seed consumer 

data DirectoryPath = DirectoryPath String String deriving (Show)

data FileNode = PlainFile DirectoryPath | Directory DirectoryPath deriving (Show)

data FileTree = Leaf FileNode | Tree FileNode [FileTree] deriving (Show)

fileNodePath :: FileNode -> DirectoryPath
fileNodePath (PlainFile path) = path
fileNodePath (Directory path) = path


directoryType :: DirectoryPath -> IOSource FileNode b
directoryType dirPath@(DirectoryPath rootPath nodePath) seed consumer = do
    isDir <- doesDirectoryExist (rootPath ++ "/" ++ nodePath)
    if isDir
        then consumer (Directory dirPath) seed
        else consumer (PlainFile dirPath) seed

flatDirectoryContents :: DirectoryPath -> IOSource FileNode b
flatDirectoryContents dirPath seed consumer = applySeed seed $ selectPipe (directoryContentsRelation dirPath) $ \ filePath -> 
                                                    selectPipe (directoryType filePath) $ consumer

fileTree :: DirectoryPath -> IO( Data.DList.DList FileTree )
fileTree dirPath = applySeed emptySeed $ select (flatDirectoryContents dirPath) $ \fileNode ->
                                           if traversable fileNode
                                                 then do 
                                                           subTrees <- fileTree (fileNodePath fileNode)
                                                           return $ having True $ (Tree fileNode (Data.DList.toList subTrees)) 
                                                 else return $ having True $ (Leaf fileNode) 

traversable :: FileNode -> Bool
traversable (PlainFile _) = False
traversable (Directory (DirectoryPath _ ".")) = False
traversable (Directory (DirectoryPath _ "..")) = False
traversable (Directory (DirectoryPath _ _ )) = True

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
            assertEqual "Stuff"  "a" "a"
        ,TestLabel "Should Query Relationally" $ TestCase $ do 
            x <- relationIOTest emptySeed
            putStrLn $ showDlist x
            assertEqual "Stuff"  "a" "a"
        ,TestLabel "Should Query File" $ TestCase $ do
            fileRelations <- readFileRelation emptySeed (\ line seed  -> return (Data.DList.snoc seed line) ) 
            putStrLn $ showDlist $ fileRelations
            assertEqual "Queried file"  "a" "a"
        ,TestLabel "Should Query File" $ TestCase $ do
            fileRelations <- readFileRelation emptySeed (\ line seed  -> return $ (sat seed (length line <= 10) line) )  
            putStrLn $ showDlist $ fileRelations
            assertEqual "Queried file" "a" "a"
        ,TestLabel "Should Query Dir" $ TestCase $ do
            dirRelations <- directoryContentsRelation (DirectoryPath "." "Tests") emptySeed (\filePath seed  -> return $ Data.DList.snoc seed filePath)
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" "a" "a"
        ,TestLabel "Should Show Dir types " $ TestCase $ do
            let dirTypes = selectPipe (directoryContentsRelation (DirectoryPath "." "Tests")) $ \filePath ->
                               select (directoryType filePath) $ \fileNode ->
                               return $ having True fileNode
            dirRelations <- dirTypes emptySeed
            putStrLn $ ""
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" "a" "a"
        ,TestLabel "Should Recursively traverse dir " $ TestCase $ do
            let dirTypes = selectPipe (directoryContentsRelation (DirectoryPath "." "sandbox")) $ \filePath ->
                               selectPipe (directoryType filePath) $ \fileNode ->
                               if traversable fileNode
                                   then selectPipe (directoryContentsRelation (fileNodePath fileNode)) $ \ secondFilePath ->
                                             select (directoryType secondFilePath) $ \secondFileNode -> return $ having True secondFileNode
                                   else \list -> return $ Data.DList.snoc list fileNode
            dirRelations <- dirTypes emptySeed
            putStrLn $ ""
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" "a" "a"
       , TestLabel "Flat dir types"  $ TestCase $ do
            let dirTypes = select (flatDirectoryContents (DirectoryPath "." "sandbox")) $ \ fileNode -> return $ having True fileNode
            dirRelations <- dirTypes emptySeed
            putStrLn $ ""
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" "a" "a"
       , TestLabel "Flat dir types"  $ TestCase $ do
            dirRelations <- fileTree (DirectoryPath "." "sandbox")
            putStrLn $ ""
            putStrLn $ showDlist $ dirRelations
            assertEqual "Queried Dir" "a" "a"
    ]

main :: IO()
main = do
    _ <- runTestTT tests
    return ()