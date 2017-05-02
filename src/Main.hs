module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Picture.Types as PicTypes
import qualified Codec.Picture.Png as Png
import Data.IDX
import Data.List.Split
import Data.Vector.Unboxed hiding (map, head)

import Data.Maybe -- FIXME: for fromJust, which can throw an error

genFooImage x y = PicTypes.PixelRGB8 (fromIntegral x) (fromIntegral y) 128

fooImage = PicTypes.generateImage genFooImage 250 300

matrixToPNG :: Int -> Int -> [Int] -> PicTypes.Image PicTypes.PixelRGB8
matrixToPNG w h matrix = (PicTypes.generateImage rasterize w h) where
  gray = \b -> PicTypes.PixelRGB8 b b b
  rasterize = \x y -> gray (fromIntegral (matrix !! (y * w + x)))

idxToPng :: IDXData -> [PicTypes.Image PicTypes.PixelRGB8]
idxToPng idxData = map (matrixToPNG width height) imageMatrices where
  [numImages, width, height] = toList (idxDimensions idxData)
  imageData = idxIntContent idxData
  imageMatrices = chunksOf (width * height) (toList imageData)

main :: IO ()
main = do
  imagesInIDX <- decodeIDXFile "./data/t10k-images-idx3-ubyte"
  let firstImage = head $ idxToPng (fromJust imagesInIDX) in
    Png.writePng "./first.png" firstImage
  putStrLn "Wrote first test image to ./first.png"
