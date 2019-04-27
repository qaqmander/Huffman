module Test where

import qualified Huffman as H
import HuffmanTree(output) 

plain  = "H4sk3ll loves you, happy hacking! >_<"
htree  = H.fromPlain plain
cipher = H.encode htree plain
Just nplain = H.decode htree cipher

main :: IO ()
main = do
    putStrLn $ H.toString cipher
    putStrLn nplain
