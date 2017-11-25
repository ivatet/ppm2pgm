module Main where

import System.IO
import System.Environment

toInts :: [String] -> [Int]
toInts = map read

toStrings :: [Int] -> [String]
toStrings = map show

grayscale :: [Int] -> [Int]
grayscale [] = []
grayscale (r:g:b:xs) = [gray] ++ grayscale xs
    where fr = fromIntegral(r)
          fg = fromIntegral(g)
          fb = fromIntegral(b)
          gray = floor(fr * 0.3 + fg * 0.59 + fb * 0.11)

convert :: [String] -> [String]
convert (x:xs) = header ++ content
    where header = ["P2"] ++ [show width] ++ [show height]
          width = il !! 0
          height = il !! 1
          il = toInts xs
          content = [show 255] ++ toStrings img
          img = grayscale $ drop 3 il

main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    let inp = words contents
    let outp = convert inp

    handle2 <- openFile "/tmp/dump.pgm" WriteMode
    hPutStr handle2 (unlines outp)

    hClose handle
    hClose handle2
