
import qualified Huffman as H
import qualified System.IO

plainfilename  = "../a.txt"
cipherfilename = plainfilename  ++ ".encrypted"
nplainfilename = cipherfilename ++ ".decrypted"

{-
main :: IO ()
main =  then encode else decode
-}

encode :: IO ()
encode = do
    frhandle <- System.IO.openBinaryFile plainfilename  System.IO.ReadMode
    plain    <- System.IO.hGetContents frhandle
    let htree  = H.fromPlain plain
    let cipher = H.encode htree plain
    fwhandle <- System.IO.openBinaryFile cipherfilename System.IO.WriteMode
    System.IO.hPutStr fwhandle $ cipher
    putStrLn $ show $ length plain
    putStrLn $ show $ length cipher `div` 8
    System.IO.hClose frhandle
    System.IO.hClose fwhandle

decode :: IO ()
decode = do
    fphandle <- System.IO.openBinaryFile plainfilename  System.IO.ReadMode
    plain    <- System.IO.hGetContents fphandle
    frhandle <- System.IO.openBinaryFile cipherfilename System.IO.ReadMode
    cipher   <- System.IO.hGetContents frhandle
    let htree  = H.fromPlain plain
    let Just nplain = H.decode htree cipher
    fwhandle <- System.IO.openBinaryFile nplainfilename System.IO.WriteMode
    System.IO.hPutStr fwhandle $ nplain
    System.IO.hClose fphandle
    System.IO.hClose frhandle
    System.IO.hClose fwhandle

