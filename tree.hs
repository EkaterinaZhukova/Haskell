data Tree a = Empty
			| NonEmpty a (Tree a) (Tree a)


count :: Tree tree -> Int
count Empty = 0
count (NonEmpty _ left right) = 1 + (count left) + (count right)

insert :: Ord t => Tree t -> t -> Tree t
insert Empty t = NonEmpty t Empty Empty
insert q@(NonEmpty t left right) a
	| a > t 						= NonEmpty t left (insert right a)
	| a < t 						= NonEmpty t (insert left a) right
	|otherwise						= q


search :: Ord t => Tree t -> t -> Bool
search Empty _ = False
search (NonEmpty t left right) a
	| a > t 	= search right a
	| a < t 	= search left a
	| otherwise = True


printTree :: Show t => Tree t -> String
printTree Empty = ""
printTree (NonEmpty t left right) =
	" <" ++(printTree left) ++ " ." ++ show t ++ " " ++ (printTree right) ++ ">"

instance Show a => Show (Tree a) where
    show = printTree