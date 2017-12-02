module Main where

import System.Environment (getArgs)

import Data.ByteString (readFile, ByteString)
import Data.ByteString.Char8 (words, readInt)

import Prelude hiding (readFile, words)

data Pixel = ColourPixel (Int, Int, Int)
           | GrayPixel Int

instance Show Pixel where
    show (GrayPixel p) = show p

data Picture = Picture { width :: Int
                       , height :: Int
                       , pixels :: [Pixel]
                       }

colourPixels :: [Int] -> [Pixel]
colourPixels [] = []
colourPixels (r:g:b:t) = ColourPixel (r, g, b) : colourPixels(t)

myReadInt :: ByteString -> Int
myReadInt p =
    case readInt p of
        Nothing -> 0
        Just (v, rest) -> v

colourPicture :: [ByteString] -> Maybe Picture
colourPicture (_:w:h:_:p) =
    case readInt w of
        Nothing -> Nothing
        Just (w, rest) ->
            case readInt h of
                Nothing -> Nothing
                Just (h, rest) -> Just $ Picture w h (colourPixels (map myReadInt p))

toGrayscale :: Pixel -> Pixel
toGrayscale (ColourPixel (r, g, b)) = GrayPixel ((r * 30 + g * 59 + b * 11) `div` 100)

convertPicture :: (Pixel -> Pixel) -> Maybe Picture -> Maybe Picture
convertPicture f (Just (Picture w h pixels)) = Just $ Picture w h (map f pixels)
convertPicture _ Nothing = Nothing

serialisePicture :: Maybe Picture -> String
serialisePicture (Just p) = unwords $ header ++ content
    where header = ["P2", show (width p), show (height p), show 255]
          content = map show (pixels p)
serialisePicture Nothing = ""

main :: IO ()
main = do
    args <- getArgs

    inp <- readFile (head args)

    let grayscalePicture = convertPicture toGrayscale (colourPicture (words inp))

    writeFile "/tmp/dump.pgm" (serialisePicture grayscalePicture)
