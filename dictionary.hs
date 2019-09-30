-- data Dictionaty = Empty 
-- 	| Nonemty 

data Map a = Empty | NonEmpty a a (Map a)


printMap :: Show t => Map t -> String
printMap Empty = ""
printMap (NonEmpty key val nextkey) = show key ++ " - " ++ show val ++ "; " ++ printMap nextkey


insert :: Ord d => (Map d) -> d -> d -> (Map d)
insert Empty key val = NonEmpty key val Empty
insert p@(NonEmpty key_p val_p nextItem) key val
	| key_p == key		= p
	| otherwise			= NonEmpty key_p val_p (insert nextItem key val)


