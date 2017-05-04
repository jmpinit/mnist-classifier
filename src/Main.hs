module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Picture.Types as PicTypes
import qualified Codec.Picture.Png as Png
import Data.IDX
import Data.List.Split
import Data.Vector.Unboxed hiding (map, head, take, length, filter, zip, (++))
import System.Random.Mersenne.Pure64
import Data.Maybe -- FIXME: for fromJust, which can throw an error
import Data.Word (Word64)

-- FIXME: using psuedo-random numbers for test
-- thx http://stackoverflow.com/a/8777494

randomStream :: Unbox a => (PureMT -> (a, PureMT)) -> PureMT -> Vector a
randomStream rndstep = unfoldr (Just . rndstep)

toStream :: Word64 -> [Double]
toStream seed = toList (randomStream randomDouble $ pureMT seed)

genFooImage x y = PicTypes.PixelRGB8 (fromIntegral x) (fromIntegral y) 128

fooImage = PicTypes.generateImage genFooImage 250 300

matrixToPNG :: Int -> Int -> [Int] -> PicTypes.Image PicTypes.PixelRGB8
matrixToPNG w h matrix = PicTypes.generateImage rasterize w h where
  gray b = PicTypes.PixelRGB8 b b b
  rasterize x y = gray (fromIntegral (matrix !! (y * w + x)))

idxToPng :: IDXData -> [PicTypes.Image PicTypes.PixelRGB8]
idxToPng idxData = map (matrixToPNG width height) imageMatrices where
  [numImages, width, height] = toList (idxDimensions idxData)
  imageData = idxIntContent idxData
  imageMatrices = chunksOf (width * height) (toList imageData)

data ImageMatrix = ImageMatrix {
  width :: Int,
  height :: Int,
  pixels :: [Int]
}

-- Agh how do monads actually work!?
imagesFromIDXFile :: String -> IO [ImageMatrix]
imagesFromIDXFile path = do
  idxData <- decodeIDXFile path
  let [imageCount, width, height] = toList (idxDimensions $ fromJust idxData)
  let imageData = idxIntContent $ fromJust idxData
  let imageMatrices = chunksOf (width * height) (toList imageData)
  let images = map (\pixels -> ImageMatrix { width = width, height = height, pixels = pixels }) imageMatrices
  return images

-- FIXME: handle case where IDX data has wrong dimensions for labels
labelsFromIDXFile :: String -> IO [Label]
labelsFromIDXFile path = do
  idxData <- decodeIDXFile path
  let labels = toList (idxIntContent $ fromJust idxData)
  return (map toInteger labels)

getPixel :: Int -> Int -> ImageMatrix -> Int
getPixel x y image = fromIntegral (pixels image !! (y * w + x)) where
  w = width image

type Label = Integer

randomClassifier :: ImageMatrix -> Label
randomClassifier image = 10 * floor (head $ toStream 42)

-- given some images return a classifier
-- or nothing if the number of images does not match the number of labels
trainClassifier :: [ImageMatrix] -> [Label] -> Maybe (ImageMatrix -> Label)
trainClassifier images labels = if length images == length labels then
    Just randomClassifier
  else
    Nothing

main :: IO ()
main = do
  trainingImages <- imagesFromIDXFile "./data/t10k-images-idx3-ubyte"
  trainingLabels <- labelsFromIDXFile "./data/t10k-labels-idx1-ubyte"
  testImages <- imagesFromIDXFile "./data/train-images-idx3-ubyte"
  testLabels <- labelsFromIDXFile "./data/train-labels-idx1-ubyte"

  let classifier = fromJust $ trainClassifier trainingImages trainingLabels
  let guessedLabels = map classifier testImages
  let numberOfCorrectGuesses = length (filter (uncurry (==)) (zip testLabels guessedLabels))
  let ratioCorrect = realToFrac numberOfCorrectGuesses / realToFrac (length testLabels)
  let outputMessage = show (100.0 * ratioCorrect) ++ "% of labels on test images guessed correctly"
    in putStrLn outputMessage
