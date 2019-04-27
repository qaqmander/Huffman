module HuffmanDict (
    HuffmanDict(..),
    fromHuffmanTree,
    encode,
    output
) where

import qualified HuffmanTree as HT
import qualified BinaryTree as B
import qualified Data.Map as M
import qualified Term as T
import qualified Bit
import Data.List(intercalate)

newtype HuffmanDict = HuffmanDict (M.Map Char [Bit.Bit])

treeToList :: HT.HuffmanTree -> [(Char, [Bit.Bit])]
treeToList (HT.HuffmanTree Nothing)  = []
treeToList (HT.HuffmanTree (Just b)) = case b of
    B.Branch (Just (T.Term Nothing  _)) lchild rchild -> lres ++ rres
        where continueToTrans :: Bit.Bit -> Maybe (B.BinaryTree (T.Term Char)) -> [(Char, [Bit.Bit])]
              continueToTrans b = map (\(x, y) -> (x, b : y)) . treeToList . HT.HuffmanTree
              lres = continueToTrans Bit.ZERO lchild 
              rres = continueToTrans Bit.ONE  rchild
    B.Branch (Just (T.Term (Just v) _)) _      _      -> [(v, [])]
    otherwise                                         -> []    -- unreachable

fromHuffmanTree :: HT.HuffmanTree -> HuffmanDict
fromHuffmanTree = HuffmanDict . M.fromList . treeToList

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f res []     = res
foldl' f res (x:xs) = let newres = f res x in
                          seq newres $ foldl' f newres xs

encode :: HT.HuffmanTree -> [Char] -> [Bit.Bit]
encode t plain = foldl' (++) [] $ map (\x -> M.findWithDefault [] x hdict) plain
    where HuffmanDict hdict = fromHuffmanTree t

output :: HuffmanDict -> IO ()
output (HuffmanDict m) = do
    putStrLn $ show m
