module HaskellEditor.Relational 

where

data Relation a = Relation { _relations :: [a]} deriving (Show)


emptyRelation :: Relation a
emptyRelation = Relation {_relations = []}

insert :: a -> Relation a -> Relation a
insert entry relation = relation { _relations = entry : (_relations relation)}



