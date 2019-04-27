{-# LANGUAGE GADTs #-}

module PriorityQueue (
    PriorityQueue(..),
    size,
    top,
    push,
    pop
) where

data PriorityQueue a where
    Branch :: Ord a => a -> Integer -> Maybe (PriorityQueue a) -> Maybe (PriorityQueue a) -> PriorityQueue a

top :: Maybe (PriorityQueue a) -> Maybe a
top Nothing  = Nothing
top (Just p) = Just t
    where Branch t _ _ _ = p

size :: Maybe (PriorityQueue a) -> Integer
size Nothing  = 0
size (Just p) = s
    where Branch _ s _ _ = p

push :: (Ord a) => Maybe (PriorityQueue a) -> a -> Maybe (PriorityQueue a)
push Nothing  now_v = Just $ Branch now_v 1 Nothing Nothing
push (Just p) now_v 
    | now_v >  value && lsize <  rsize = 
        let lchild_ = push lchild now_v in
            Just $ Branch value (tsize + 1) lchild_ rchild
    | now_v >  value && lsize >= rsize = 
        let rchild_ = push rchild now_v in
            Just $ Branch value (tsize + 1) lchild  rchild_
    | now_v <= value && lsize <  rsize = 
        let lchild_ = push lchild value in
            Just $ Branch now_v (tsize + 1) lchild_ rchild
    | now_v <= value && lsize >= rsize = 
        let rchild_ = push rchild value in
            Just $ Branch now_v (tsize + 1) lchild  rchild_
    where Branch value tsize lchild rchild = p
          (lsize, rsize) = (size lchild, size rchild)

del :: (Ord a) => Maybe (PriorityQueue a) -> Maybe (PriorityQueue a)
del Nothing  = Nothing
del (Just p) = case (lchild, rchild) of
    (Nothing, Nothing) -> Nothing
    (Just lp, Nothing) -> Just lp
    (Nothing, Just rp) -> Just rp
    (Just lp, Just rp) 
        | ltop < rtop -> Just $ Branch ltop (tsize - 1) (del lchild) rchild
        | otherwise   -> Just $ Branch rtop (tsize - 1) lchild (del rchild)
        where Branch ltop _ _ _ = lp
              Branch rtop _ _ _ = rp
    where Branch value tsize lchild rchild = p
          (lsize, rsize) = (size lchild, size rchild)

pop :: (Ord a) => Maybe (PriorityQueue a) -> (Maybe a, Maybe (PriorityQueue a))
pop p = (top p, del p)

