module HuffmanDict (
    HuffmanDict(..),
    fromHuffmanTree,
    output
) where

import qualified HuffmanTree as HT
import qualified BinaryTree as B
import qualified Data.Map as M
import qualified Term as T
import qualified Bits

newtype HuffmanDict = HuffmanDict (M.Map Char Bits.Bits) 

treeToList :: HT.HuffmanTree -> [(Char, Bits.Bits)]
treeToList (HT.HuffmanTree Nothing)  = []
treeToList (HT.HuffmanTree (Just b)) = case b of
    B.Branch (Just (T.Term Nothing  _)) lchild rchild -> lres ++ rres
        where continueToTrans :: Bits.Bit -> Maybe (B.BinaryTree (T.Term Char)) -> [(Char, Bits.Bits)]
              continueToTrans b = map (\(x, y) -> (x, Bits.cons b y)) . treeToList . HT.HuffmanTree
              lres = continueToTrans Bits.ZERO lchild 
              rres = continueToTrans Bits.ONE  rchild
    B.Branch (Just (T.Term (Just v) _)) _      _      -> [(v, Bits.create)]
    otherwise                                         -> []    -- unreachable

fromHuffmanTree :: HT.HuffmanTree -> HuffmanDict
fromHuffmanTree = HuffmanDict . M.fromList . treeToList

output :: HuffmanDict -> IO ()
output (HuffmanDict m) = do
    putStrLn $ show m
