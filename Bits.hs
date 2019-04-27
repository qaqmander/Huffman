{-# LANGUAGE InstanceSigs #-}

module Bits (
    Bit(..),
    Bits(..),
    create,
    cons,
    append,
    Bits.concat
) where

import qualified Data.List as L

data Bit = ZERO | ONE

instance Show Bit where
    show :: Bit -> String
    show ZERO = "0"
    show ONE  = "1"

data Bits = Bits { value :: [Bit] }

instance Show Bits where
    show :: Bits -> String
    show (Bits value) = L.intercalate "" $ map show value

create :: Bits
create = Bits []

cons :: Bit -> Bits -> Bits
cons b (Bits bs) = Bits (b : bs)

append :: Bits -> Bit -> Bits
append (Bits [])     b = Bits [b]
append (Bits (x:xs)) b = Bits (x : rest)
    where Bits rest = append (Bits xs) b

concat :: Bits -> Bits -> Bits
concat bs (Bits [])     = bs
concat bs (Bits (x:xs)) = Bits.concat newf newb
    where newf = append bs x
          newb = Bits xs
