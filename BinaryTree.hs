{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module BinaryTree(module BinaryTree) where

data BinaryTree a where
    Branch :: a -> Maybe (BinaryTree a) -> Maybe (BinaryTree a) -> BinaryTree a

root :: Maybe (BinaryTree a) -> Maybe a
root x = case x of
    Just (Branch val _ _) -> Just val
    Nothing               -> Nothing

size :: Maybe (BinaryTree a) -> Integer
size x = case x of
    Just (Branch _ lchild rchild) -> size lchild + size rchild + 1
    Nothing                       -> 0

