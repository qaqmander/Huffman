{-# LANGUAGE InstanceSigs #-}

module Bit (
    Bit(..),
    toString,
    fromString
) where

import Data.List(intercalate)

data Bit = ZERO | ONE deriving (Eq)

instance Show Bit where
    show :: Bit -> String
    show ZERO = "0"
    show ONE  = "1"

toString :: [Bit] -> String
toString bs = intercalate "" $ map show bs

fromString :: String -> [Bit]
fromString s = map trans s
    where trans :: Char -> Bit
          trans '0' = ZERO
          trans '1' = ONE
          trans  _  = ZERO
