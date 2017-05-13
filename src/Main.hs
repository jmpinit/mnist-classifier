module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Picture.Types as PicTypes
import qualified Codec.Picture.Png as Png
import Data.IDX
import Data.List.Split
import qualified Data.Vector.Unboxed as DU
import System.Random.Mersenne.Pure64
import Data.Maybe -- FIXME: for fromJust, which can throw an error
import Data.Word (Word64)
import Control.Monad
import System.Random
import Data.List
import Data.Ord

-- FIXME: using psuedo-random numbers for test
-- thx http://stackoverflow.com/a/8777494

randomStream :: DU.Unbox a => (PureMT -> (a, PureMT)) -> PureMT -> DU.Vector a
randomStream rndstep = DU.unfoldr (Just . rndstep)

toStream :: Word64 -> [Double]
toStream seed = DU.toList (randomStream randomDouble $ pureMT seed)

genFooImage x y = PicTypes.PixelRGB8 (fromIntegral x) (fromIntegral y) 128

fooImage = PicTypes.generateImage genFooImage 250 300

matrixToPNG :: Int -> Int -> [Int] -> PicTypes.Image PicTypes.PixelRGB8
matrixToPNG w h matrix = PicTypes.generateImage rasterize w h where
  gray b = PicTypes.PixelRGB8 b b b
  rasterize x y = gray (fromIntegral (matrix !! (y * w + x)))

idxToPng :: IDXData -> [PicTypes.Image PicTypes.PixelRGB8]
idxToPng idxData = map (matrixToPNG width height) imageMatrices where
  [numImages, width, height] = DU.toList (idxDimensions idxData)
  imageData = idxIntContent idxData
  imageMatrices = chunksOf (width * height) (DU.toList imageData)

data ImageMatrix = ImageMatrix {
  width :: Int,
  height :: Int,
  pixels :: [Int]
}

-- Agh how do monads actually work!?
imagesFromIDXFile :: String -> IO [ImageMatrix]
imagesFromIDXFile path = do
  idxData <- decodeIDXFile path
  let [imageCount, width, height] = DU.toList (idxDimensions $ fromJust idxData)
  let imageData = idxIntContent $ fromJust idxData
  let imageMatrices = chunksOf (width * height) (DU.toList imageData)
  let images = map (\pixels -> ImageMatrix { width = width, height = height, pixels = pixels }) imageMatrices
  return images

-- FIXME: handle case where IDX data has wrong dimensions for labels
labelsFromIDXFile :: String -> IO [Label]
labelsFromIDXFile path = do
  idxData <- decodeIDXFile path
  let labels = DU.toList (idxIntContent $ fromJust idxData)
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

-- BEGIN things stolen from https://crypto.stanford.edu/~blynn/haskell/brain.html

gauss :: Float -> IO Float
gauss stdev = do
  x1 <- randomIO
  x2 <- randomIO
  return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- f is activation function
-- ws is weights
-- b is bias
-- as is inputs
output f ws b as = f (sum (zipWith (*) ws as) + b)

-- rectifier: https://en.wikipedia.org/wiki/Rectifier_(neural_networks)
relu = max 0

relu' x | x < 0      = 0
        | otherwise  = 1

dCost a y | y == 1 && a >= y = 0
          | otherwise        = a - y

newBrain :: [Int] -> IO [([Float], [[Float]])]
newBrain szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts

zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs

feed :: [Float] -> [([Float], [[Float]])] -> [Float]
feed = foldl (((relu <$>) . ) . zLayer)

-- xv: vector of inputs
-- Returns a list of (weighted inputs, activations) of each layer,
-- from last layer to first.
revaz :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
revaz xv = foldl (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = zLayer av (bs, wms) in (((relu <$> zs'):avs), (zs':zs))) ([xv], [])

-- xv: vector of inputs
-- yv: vector of desired outputs
-- Returns list of (activations, deltas) of each layer in order.
deltas :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
deltas xv yv layers = let
  (avs@(av:_), zv:zvs) = revaz xv layers
  delta0 = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [(sum $ zipWith (*) row dv) | row <- wm] (relu' <$> zv)

eta = 0.002

descend av dv = zipWith (-) av ((eta *) <$> dv)

learn :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
learn xv yv layers = let (avs, dvs) = deltas xv yv layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
      (snd <$> layers) avs dvs

getImage s n = pixels $ s !! n
getX     s n = (/ 256) . fromIntegral <$> getImage s n
getLabel s n = s !! n
getY     s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0..9]

-- END stolen things

{-
data Neuron = Neuron {
  inputs :: [Neuron],
  weights :: [Double],
  activation :: Double -> Double
}

e = exp 1

sigmoidActivation :: Double -> Double
sigmoidActivation v = 1 / (1 + e ** (- v))

-- Get the output value of a neuron
output :: Neuron -> Double
output neuron = activation neuron $ sum inputValues where
  inputValues = map output (inputs neuron)

testNet = Neuron { inputs = [], weights = [], activation = const 0 }
-}

main :: IO ()
main = do
  trainingImages <- imagesFromIDXFile "./data/t10k-images-idx3-ubyte"
  trainingLabels <- labelsFromIDXFile "./data/t10k-labels-idx1-ubyte"
  testImages <- imagesFromIDXFile "./data/train-images-idx3-ubyte"
  testLabels <- labelsFromIDXFile "./data/train-labels-idx1-ubyte"

  b <- newBrain [784, 30, 10]
  n <- (`mod` (10000 :: Int)) <$> randomIO

  let
    example = getX testImages n
    bs = scanl (foldl' (\b n -> learn (getX trainingImages n) (getY trainingLabels n) b)) b [
     [   0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..9999]]
    smart = last bs
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

  forM_ bs $ putStrLn . unlines . zipWith cute [0..9] . feed example

  putStrLn $ "best guess: " ++ show (bestOf $ feed example smart)

  let answers = getLabel testLabels <$> [0..9999]

  --putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++
   -- " / 10000"

  --let classifier = fromJust $ trainClassifier trainingImages trainingLabels
  let guesses = bestOf . (\n -> feed (getX testImages n) smart) <$> [0..9999]
  let numberOfCorrectGuesses = length (filter (uncurry (==)) (zip answers guesses))
  let ratioCorrect = realToFrac numberOfCorrectGuesses / realToFrac (length answers)
  let outputMessage = show (100.0 * ratioCorrect) ++ "% of labels on test images guessed correctly"
    in putStrLn outputMessage
