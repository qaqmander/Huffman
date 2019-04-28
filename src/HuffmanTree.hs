module HuffmanTree(
    HuffmanTree(..),
    fromPlain,
    decode,
    output
) where

import qualified Term as T
import qualified BinaryTree as B
import qualified PriorityQueue as P
import qualified Data.Map as M
import qualified Bit

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

plainToDict :: [Char] -> M.Map Char Integer
plainToDict plain = iter plain M.empty
    where iter :: [Char] -> M.Map Char Integer -> M.Map Char Integer
          iter []     nowres = nowres
          iter (x:xs) nowres = iter xs nextres
              where nextres = M.insert x (1 + M.findWithDefault 0 x nowres) nowres

fromList :: [T.Term Char] -> HuffmanTree
fromList l = fromPriorityQueue p n
    where p = listToPriorityQueue l
          n = length l - 1

fromPlain :: [Char] -> HuffmanTree
fromPlain plain = fromList $ map (\(x, y) -> T.Term (Just x) y) $ M.toList $ plainToDict plain

decode :: HuffmanTree -> [Bit.Bit] -> Maybe [Char]
decode (HuffmanTree Nothing)  _ = Nothing
decode (HuffmanTree (Just t)) b = iter t t b []
    where iter :: B.BinaryTree (T.Term Char) -> B.BinaryTree (T.Term Char) -> [Bit.Bit] -> [Char] -> Maybe [Char]
          iter t now []     res = case now of
              B.Branch (Just v) Nothing Nothing -> case v of
                  T.Term (Just value) _ -> Just (res ++ [value])
                  otherwise             -> Nothing
              otherwise                         -> Just res
          iter t now (x:xs) res = case now of
              B.Branch (Just v) Nothing  Nothing  -> case v of
                  T.Term (Just value) _ -> iter t t (x:xs) (res ++ [value])
                  otherwise                       -> Nothing
              B.Branch (Just v) (Just l) (Just r)
                  | x == Bit.ZERO -> iter t l xs res
                  | x == Bit.ONE  -> iter t r xs res
              otherwise                           -> Nothing

output :: Int -> HuffmanTree -> IO ()
output n (HuffmanTree b) = B.output b n
