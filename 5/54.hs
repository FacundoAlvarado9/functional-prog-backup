data Seq a = End | Seq a (Seq a) | Lseq [a] (Seq a) | Pseq (a,a) (Seq a) deriving (Show)

instance Functor Seq where
    fmap f End = End
    fmap f (Seq x subseq) = Seq (f x) (fmap f subseq)
    fmap f (Lseq xs subseq) = Lseq (map f xs) (fmap f subseq)
    fmap f (Pseq (x,y) subseq) = Pseq (f x, f y) (fmap f subseq)

testFunctor = fmap (+3) (Seq 1 (Lseq [1,2,3] (Pseq (1,2) End)))

instance Foldable Seq where
    foldr f b End = b
    foldr f b (Seq x subseq) = f x (foldr f b subseq)
    foldr f b (Lseq xs subseq) = foldr f (foldr f b subseq) xs -- is this Right? -> it is (otherwise would violate the definition)
    foldr f b (Pseq (x,y) subseq) = f x (f y (foldr f b subseq))

testFoldr = foldr (+) 0 (Seq 1 (Lseq [1,2,3] (Pseq (1,2) End)))
testFoldrSeq = foldr (+) 0 (Seq 1 (Seq 5 (Seq 4 End)))
testFoldrLseq = foldr (+) 0 (Lseq [1,4,5] (Seq 1 End))
testFoldrPairs = foldr (+) 0 (Pseq (5,4) (Seq 1 End))