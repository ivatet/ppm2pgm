module Main where

import System.IO
import System.Environment

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

colourPicture :: [String] -> Picture
colourPicture (_:w:h:_:p) = Picture (read w) (read h) (colourPixels (map read p))

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
    inp <- openFile (head args) ReadMode
    outp <- openFile "/tmp/dump.pgm" WriteMode
    content <- hGetContents inp
    let grayscalePicture = convertPicture toGrayscale (colourPicture (words content))
    hPutStr outp (serialisePicture grayscalePicture)
    hClose outp
    hClose inp
