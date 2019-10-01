import Data.Tuple

data Map a = Empty | NonEmpty ((,) a a)  (Map a)


printMap :: Show t => Map t -> String
printMap Empty = ""
printMap (NonEmpty tuple nextkey) = show (fst tuple) ++ " - " ++ show (snd tuple) ++ "; " ++ printMap nextkey


insert :: Ord d => (Map d) -> ((,) d d) -> (Map d)
insert Empty tuple = NonEmpty tuple Empty
insert p@(NonEmpty tuple nextItem) tupleToInsert
	| (fst tuple) == (fst tupleToInsert)		= p
	| otherwise									= NonEmpty tuple (insert nextItem tupleToInsert)


