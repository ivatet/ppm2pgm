module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as CH

import Data.Maybe
import Data.Either
import Data.Either.Utils

import System.Environment

data Magic = Magic

data Pixel = ColourPixel (Int, Int, Int)
           | GrayPixel Int

instance Show Pixel where
    show (GrayPixel p) = show p

data Picture = Picture { width :: Int
                       , height :: Int
                       , pixels :: [Pixel]
                       }

myReadMagic :: BS.ByteString -> Maybe (Magic, BS.ByteString)
myReadMagic s =
    case C8.unpack (C8.take 2 s) of
        "P3" -> Just (Magic, C8.drop 2 s)
        otherwise -> Nothing

myReadInt :: BS.ByteString -> Maybe (Int, BS.ByteString)
myReadInt = C8.readInt . C8.dropWhile CH.isSpace

colourPixels :: BS.ByteString -> Maybe (Pixel, BS.ByteString)
colourPixels s = do
    (red, s)   <- myReadInt s
    (green, s) <- myReadInt s
    (blue, s)  <- myReadInt s

    return (ColourPixel (red, green, blue), s)

myReadAll :: (BS.ByteString -> Maybe (a, BS.ByteString)) -> BS.ByteString -> [a]
myReadAll f s = case f s of
    Nothing -> []
    Just (a, s) -> (a : myReadAll f s)

colourPicture :: BS.ByteString -> Maybe Picture
colourPicture s = do
    (magic, s)  <- myReadMagic s
    (width, s)  <- myReadInt s
    (height, s) <- myReadInt s
    (_, s)      <- myReadInt s
    let pixels  = myReadAll colourPixels s

    return (Picture width height pixels)

toGrayscale :: Pixel -> Pixel
toGrayscale (ColourPixel (r, g, b)) = GrayPixel ((r * 30 + g * 59 + b * 11) `div` 100)

convertPicture :: (Pixel -> Pixel) -> Picture -> Picture
convertPicture f (Picture w h pixels) = Picture w h (map f pixels)

serialisePicture :: Picture -> String
serialisePicture p = unwords $ header ++ content
    where header = ["P2", show (width p), show (height p), show 255]
          content = map show (pixels p)

main :: IO ()
main = do
    args <- getArgs
    let inPath = head args
    let outPath = head (drop 1 args)

    inp <- BS.readFile inPath

    case colourPicture inp of
        Nothing -> print ("Unable to parse input file " ++ inPath)
        Just cp -> do
            let gp = convertPicture toGrayscale cp
            writeFile outPath (serialisePicture gp)
