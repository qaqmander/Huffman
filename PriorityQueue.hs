{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module PriorityQueue(PriorityQueue, size, pop) where

import qualified BinaryTree

data PriorityQueue a where
    PriorityQueue :: Ord a => Maybe (BinaryTree.BinaryTree a) -> PriorityQueue a

maintain :: PriorityQueue a -> PriorityQueue a
maintain (PriorityQueue b) = PriorityQueue b1
    where b1 = case b of
            Nothing                       -> Nothing
            Just (BinaryTree.Branch _ lchild rchild) -> case (lval, rval) of
                (Nothing, Nothing) -> Nothing
                (Nothing, Just v ) -> Just $ BinaryTree.Branch v Nothing new_rchild
                    where PriorityQueue new_rchild = maintain $ PriorityQueue rchild
                (Just v , Nothing) -> Just $ BinaryTree.Branch v new_lchild Nothing
                    where PriorityQueue new_lchild = maintain $ PriorityQueue lchild
                (Just vl, Just vr) | vl < vr   -> Just $ BinaryTree.Branch vl new_lchild rchild
                    where PriorityQueue new_lchild = maintain $ PriorityQueue lchild
                (Just vl, Just vr) | otherwise -> Just $ BinaryTree.Branch vr lchild new_rchild
                    where PriorityQueue new_rchild = maintain $ PriorityQueue rchild
                where lval = BinaryTree.root lchild
                      rval = BinaryTree.root rchild

pop :: PriorityQueue a -> (PriorityQueue a, Maybe a)
pop q@(PriorityQueue b) = (q1, val)
    where val = BinaryTree.root b
          q1 = maintain q

size :: PriorityQueue a -> Integer
size (PriorityQueue b) = BinaryTree.size b

