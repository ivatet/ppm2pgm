module Main where

import System.Environment (getArgs)

import Data.ByteString (readFile, ByteString)
import Data.ByteString.Char8 (words, take, drop, readInt, unpack, dropWhile)
import Data.Char (isSpace)
import Data.Maybe
import Data.Either
import Data.Either.Utils

import Prelude hiding (readFile, words, take, drop, unpack, dropWhile)

data Pixel = ColourPixel (Int, Int, Int)
           | GrayPixel Int

instance Show Pixel where
    show (GrayPixel p) = show p

data Picture = Picture { width :: Int
                       , height :: Int
                       , pixels :: [Pixel]
                       }

colourPixels :: ByteString -> [Pixel]
colourPixels s =
    case readInt (dropWhile isSpace s) of
    Nothing -> []
    Just (r, s) ->
        case readInt (dropWhile isSpace s) of
        Nothing -> []
        Just (g, s) ->
            case readInt (dropWhile isSpace s) of
            Nothing -> []
            Just (b, s) ->
                ColourPixel (r, g, b) : colourPixels s

colourPicture :: ByteString -> Either Picture String
colourPicture s =
    case unpack (take 2 s) of
    "P3" ->
        let s' = drop 2 s in
        case readInt (dropWhile isSpace s') of
        Nothing -> Right "Wrong width"
        Just (w, s) ->
            case readInt (dropWhile isSpace s) of
            Nothing -> Right "Wrong height"
            Just (h, s) ->
                case readInt (dropWhile isSpace s) of
                Nothing -> Right "Wrong max value"
                Just (_, s) -> Left (Picture w h pixels)
                    where pixels = colourPixels s
    otherwise -> Right "Wrong magic number"


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

    inp <- readFile (head args)

    let cp = colourPicture inp

    --print $ case cp of
    --    Left p -> show $ width p
    --    Right s -> s

    let gp = convertPicture toGrayscale (fromLeft cp)
    let outp = serialisePicture gp

    writeFile "/tmp/dump.pgm" outp
