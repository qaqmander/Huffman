module LZ(encode) where

import qualified Data.Map as M
import Data.List(maximum)

data Term = Term Int Char deriving (Show)

fromString :: String -> Term
fromString s = Term op1 op2
    where (head, end) = firstN s (length s - 1)
          op1 = binStringToNum head
          op2 = if end == "1" then '1' else '0'

type Dict = M.Map String Int
type DDict = M.Map Int String

encodeToTerms :: String -> [Term]
encodeToTerms plain = res
    where res = iter plain (M.fromList [("", 0)]) []
          iter :: String -> Dict -> [Term] -> [Term]
          iter []     d res = res
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

binStringToNum :: String -> Int
binStringToNum s = iter s 0
    where iter :: String -> Int -> Int
          iter []     res = res
          iter (x:xs) res = iter xs nextres
              where nextres = res * 2 + (if x == '1' then 1 else 0)

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

stringToTerms :: Int -> String -> [Term]
stringToTerms len s = iter s len []
    where iter :: String -> Int -> [Term] -> [Term]
          iter [] len res = res
          iter s  len res = iter rest len nextres
              where (now, rest) = firstN s len
                    nextres = res ++ [fromString now]

firstN :: String -> Int -> (String, String)
firstN s      0 = ([], s) 
firstN []     _ = ([], [])
firstN (x:xs) n = (x : res0, res1)
    where (res0, res1) = firstN xs (n - 1)

dTermsToString :: [Term] -> String
dTermsToString ts = iter ts (M.fromList [(0, "")]) 1 ""
    where iter :: [Term] -> DDict -> Int -> String -> String
          iter []     _ _   res = res
          iter (t:ts) d num res = iter ts nextd nextnum nextres
              where Term head end = t 
                    nextd = M.insert num nowres d
                    nextnum = num + 1
                    nextres = res ++ nowres
                    nowres = M.findWithDefault "" head d ++ [end]

decode :: Int -> String -> String
decode len = dTermsToString . stringToTerms len
