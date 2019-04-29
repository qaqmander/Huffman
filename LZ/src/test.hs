module Main where

import qualified System.IO
import LZ(encode)

plainfilename  = "../message.txt"
cipherfilename = plainfilename ++ ".encrypted"

main :: IO ()
main = do
    frhand <- System.IO.openFile plainfilename  System.IO.ReadMode
    plain  <- System.IO.hGetContents frhand
    let cipher = encode plain
    fwhand <- System.IO.openFile cipherfilename System.IO.WriteMode
    System.IO.hPutStr fwhand cipher
    System.IO.hClose frhand
    System.IO.hClose fwhand
