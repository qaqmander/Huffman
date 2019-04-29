module LZ(encode) where

import qualified Data.Map as M
import Data.List(maximum)

data Term = Term Int Char deriving (Show)

type Dict = M.Map String Int

encodeToTerms :: String -> [Term]
encodeToTerms plain = res
    where (_, res) = iter plain (M.fromList [("", 0)]) []
          iter :: String -> Dict -> [Term] -> (Dict, [Term])
          iter []     d res = (d, res)
          iter (x:xs) d res = iter nextxs nextd nextres
              where (nowhead, nowend, nextxs) = findSeg (x:xs) d
                    nowxs = nowhead ++ [nowend]
                    nextd = M.insert nowxs (M.size d) d
                    nextres = res ++ [nowterm]
                        where nowterm = Term (M.findWithDefault 0 nowhead d) nowend
                    findSeg :: String -> Dict -> (String, Char, String)
                    findSeg (x:xs) d = iter xs d "" x
                        where iter :: String -> Dict -> String -> Char -> (String, Char, String)
                              iter []     _ nowhead nowend = (nowhead, nowend, [])
                              iter (x:xs) d nowhead nowend
                                  | res == (-1) = (nowhead, nowend, x:xs)
                                  | otherwise   = iter xs d nexthead nextend
                                  where nexthead = nowhead ++ [nowend]
                                        nextend  = x
                                        res      = M.findWithDefault (-1) nexthead d

bin :: Int -> String
bin 0 = "0"
bin x = iter x
    where iter :: Int -> String
          iter 0 = ""
          iter x = iter (x `div` 2) ++ rem
              where rem = show (x `mod` 2)

rjust :: Int -> String -> String
rjust len s = iter (len - length s) s
    where iter :: Int -> String -> String
          iter len s | len <= 0  = s
                     | otherwise = iter (len - 1) ('0' : s)

termsToString :: [Term] -> String
termsToString ts = iter ts ""
    where len = length $ bin $ maximum $ map (\(Term x y) -> x) ts
          iter :: [Term] -> String -> String
          iter []     res = res
          iter (x:xs) res = iter xs nextres
              where Term head end = x
                    headbin = rjust len $ bin head
                    nextres = res ++ headbin ++ [end]

encode :: String -> String
encode = termsToString . encodeToTerms

