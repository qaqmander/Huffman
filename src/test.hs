module Main where

import qualified Huffman as H
import qualified System.IO

binary = True
en     = True

plainfilename  = if binary 
                 then "../yukino.png"
                 else "../shakespeare.txt"
cipherfilename = plainfilename  ++ ".encrypted"
nplainfilename = cipherfilename ++ ".decrypted"
openFile = if binary 
           then System.IO.openBinaryFile
           else System.IO.openFile

main :: IO ()
main = if en then encode else decode

encode :: IO ()
encode = do
    frhandle <- openFile plainfilename  System.IO.ReadMode
    plain    <- System.IO.hGetContents frhandle
    let htree  = H.fromPlain plain
    let cipher = H.encode htree plain
    fwhandle <- System.IO.openFile cipherfilename System.IO.WriteMode
    System.IO.hPutStr fwhandle $ cipher
    putStrLn $ show $ length plain
    putStrLn $ show $ length cipher `div` 8
    System.IO.hClose frhandle
    System.IO.hClose fwhandle

decode :: IO ()
decode = do
    fphandle <- openFile plainfilename  System.IO.ReadMode
    plain    <- System.IO.hGetContents fphandle
    frhandle <- System.IO.openFile cipherfilename System.IO.ReadMode
    cipher   <- System.IO.hGetContents frhandle
    let htree  = H.fromPlain plain
    let Just nplain = H.decode htree cipher
    fwhandle <- openFile nplainfilename System.IO.WriteMode
    System.IO.hPutStr fwhandle $ nplain
    System.IO.hClose fphandle
    System.IO.hClose frhandle
    System.IO.hClose fwhandle
