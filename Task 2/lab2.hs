{-
Zhuravlev Roman 5303

Functional programming 

Lab 2
-}


import Data.List
import Data.Char
import Data.Ratio
import Data.Monoid
import Data.Foldable
 
-- Task 1
circShiftL :: Int -> [a] -> [a]
circShiftL n xs = let (a, b) = splitAt (length xs - (abs n)) xs
                    in b ++ a

-- Task 2
indices :: [a] -> [(Integer, a)]
indices lst = zip [0 .. toInteger (length lst - 1)] lst

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy lst condition = map check lst where
    check x = if (condition x) then x else mempty

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum l1 l2 l3 = zipWith (+) l1 (zipWith (+) l2 l3)

-- Task 3
revRange :: (Char,Char) -> [Char] 
revRange = unfoldr fun 
fun (begin, end) 
    | begin > end = Nothing 
    | minBound == end = Just (end, (succ begin, end)) 
    | otherwise = Just (end, (begin, pred end))

-- Task 4
seriesK :: Int -> [Rational]
seriesK k = let 
    nats n = ( 1 % toInteger(k ^ n) ) : nats (n + 1)
    in ( 1 % 1 ) : nats 1
    
-- Task 5
newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

concatSortedList :: Ord a => (SortedList a) -> (SortedList a) -> (SortedList a)
concatSortedList a b = SortedList $ sort (getSorted a ++ getSorted b)

instance Ord a => Semigroup (SortedList a) where
    (<>) = concatSortedList

instance Ord a => Monoid (SortedList a) where
    mempty = SortedList []


-- Task 6
fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

msort :: Ord a => [a] -> SortedList a
msort [] = SortedList []
msort [x] = SortedList [x]
msort xs = (msort $ fsthalf xs) <> (msort $ sndhalf xs)


-- Task 7
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldMap f Nil = mempty
  foldMap f (Node l v r) =
           (foldMap f l) <> f v <> (foldMap f r)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
instance Foldable Preorder where
    foldMap f (PreO (Nil)) = mempty
    foldMap f (PreO (Node l v r)) = f v <> (foldMap f $ PreO l) <> (foldMap f $ PreO r)

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
instance Foldable Postorder where
    foldMap f (PostO (Nil)) = mempty
    foldMap f (PostO (Node l v r)) = (foldMap f $ PostO l) <> (foldMap f $ PostO r) <> f v

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)
instance Foldable Levelorder where
    foldMap f tree = foldMap' f (tbf [tree]) where
        foldMap' f [] = mempty
        foldMap' f (x:xs) = f x <> foldMap' f xs

        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs)) where
            nodeValue (LevelO (Node _ a _)) = a
            leftAndRightNodes (LevelO (Node Nil _ Nil)) = []
            leftAndRightNodes (LevelO (Node Nil _ b))   = [LevelO b]
            leftAndRightNodes (LevelO (Node a _ Nil))   = [LevelO a]
            leftAndRightNodes (LevelO (Node a _ b))     = [LevelO a, LevelO b] 


inorderTree = Node Nil 2 (Node (Node Nil 4 Nil) 3 (Node Nil 5 Nil))
preorderTree = PreO (inorderTree)
postorderTree = PostO (inorderTree)
levelorderTree = LevelO (inorderTree)

{-
foldedInorderTree    = foldMap (\x -> [x]) inorderTree
foldedPreorderTree   = foldMap (\x -> [x]) preorderTree
foldedPostorderTree  = foldMap (\x -> [x]) postorderTree
foldedLevelorderTree = foldMap (\x -> [x]) levelorderTree
-}
