{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module BinaryTree (
    BinaryTree(..), 
    output,
    size
) where

import Data.List(intercalate)

data BinaryTree a = Branch {
    value  :: Maybe a,
    lchild :: Maybe (BinaryTree a),
    rchild :: Maybe (BinaryTree a)
}

instance (Eq a) => Eq (BinaryTree a) where
    (==) :: BinaryTree a -> BinaryTree a -> Bool
    (==) (Branch v1 _ _) (Branch v2 _ _) = v1 == v2

instance (Ord a) => Ord (BinaryTree a) where
    (<=) :: BinaryTree a -> BinaryTree a -> Bool
    (<=) (Branch v1 _ _) (Branch v2 _ _) = v1 <= v2

toStringList :: (Show a) => Maybe (BinaryTree a) -> Int -> ([String], Int, Int)
toStringList Nothing dwidth                           = ([[' ' | i <- [1 .. dwidth]]], 1, dwidth)
toStringList (Just (Branch val lchild rchild)) dwidth = (
    (mcfrontspaces ++ mchar    ++ mcbackspaces) :
    (frontspaces   ++ skeleton ++ backspaces)   :
    (frontspaces   ++ [ lver ]  ++ middlespaces ++ [ rver ] ++ backspaces) : 
    (map (\(x, y) -> x ++ y) $ zip lstring rstring),
    height + 4, tlength)
    where 
        (lstring_, lheight, llength) = toStringList lchild dwidth
        (rstring_, rheight, rlength) = toStringList rchild dwidth
        mchar = case val of
            Nothing -> "|"
            Just t  -> show t
        mclength = length mchar
        height   = max lheight rheight
        tlength  = llength + rlength
        lstring  = lstring_ ++ [[' ' | i <- [1 .. llength]] | j <- [1 .. height - lheight]]
        rstring  = rstring_ ++ [[' ' | i <- [1 .. rlength]] | j <- [1 .. height - rheight]]
        upfront  = tlength `div` 2
        upback   = tlength - upfront - 1
        front    = llength `div` 2
        middle   = llength - llength `div` 2 + rlength `div` 2 - 1
        back     = tlength - front - middle - 2
        skelen   = middle + 2
        mcfrontspaces = [' ' | i <- [1 .. upfront - mclength `div` 2]]
        mcbackspaces  = [' ' | i <- [1 .. upback  - (mclength - mclength `div` 2 - 1)]]
        upfrontspaces = [' ' | i <- [1 .. upfront]]
        upbackspaces  = [' ' | i <- [1 .. upback ]]
        frontspaces   = [' ' | i <- [1 .. front  ]]
        middlespaces  = [' ' | i <- [1 .. middle ]]
        backspaces    = [' ' | i <- [1 .. back   ]]
        lskeleton     = [generator | i <- [1 .. upfront - front]]
            where generator = case lchild of Nothing   -> ' ' 
                                             otherwise -> '_'
        rskeleton     = [generator | i <- [1 .. skelen - (upfront - front) - 1]]
            where generator = case rchild of Nothing   -> ' ' 
                                             otherwise -> '_'
        lver          = case lchild of Nothing   -> ' '
                                       otherwise -> '|'
        rver          = case rchild of Nothing   -> ' '
                                       otherwise -> '|'
        mver          = case (lchild, rchild) of (Nothing, Nothing) -> ' '
                                                 otherwise          -> '|'
        skeleton      = lskeleton ++ [mver] ++ rskeleton

output :: (Show a) => Maybe (BinaryTree a) -> Int -> IO ()
output b dwidth = do
    putStrLn $ intercalate "\n" stringList
        where (stringList, _, _) = toStringList b dwidth

size :: Maybe (BinaryTree a) -> Integer
size x = case x of
    Nothing                       -> 0
    Just (Branch _ lchild rchild) -> size lchild + size rchild + 1
