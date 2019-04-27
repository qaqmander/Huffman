
module Huffman(
    HuffmanTree(..),
    fromList,
    output
) where

import qualified Term as T
import qualified BinaryTree as B
import qualified PriorityQueue as P

newtype HuffmanTree = HuffmanTree (Maybe (B.BinaryTree (T.Term Char)))

listToPriorityQueue :: [T.Term Char] -> Maybe (P.PriorityQueue (B.BinaryTree (T.Term Char)))
listToPriorityQueue []     = Nothing
listToPriorityQueue (x:xs) = P.push (listToPriorityQueue xs) $ B.Branch (Just x) Nothing Nothing

fromPriorityQueue :: Maybe (P.PriorityQueue (B.BinaryTree (T.Term Char))) -> Int -> HuffmanTree
fromPriorityQueue p 0 = HuffmanTree $ P.top p
fromPriorityQueue p n =
    let (top1, new_p1) = P.pop p in
        let (top2, new_p2) = P.pop new_p1 in
            let (topt1, topt2) = (top1 >>= B.value, top2 >>= B.value)
                new_p3         = P.push new_p2 $ B.Branch (T.combine <$> topt1 <*> topt2) top1 top2 in
                    fromPriorityQueue new_p3 (n - 1)

fromList :: [T.Term Char] -> HuffmanTree
fromList l = fromPriorityQueue p n
    where p = listToPriorityQueue l
          n = length l - 1

output :: HuffmanTree -> Int -> IO ()
output (HuffmanTree b) n = B.output b n
