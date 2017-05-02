module Main where

import Codec.Picture.Types as PicTypes
import Codec.Picture.Png as Png

genFooImage x y = PicTypes.PixelRGB8 (fromIntegral x) (fromIntegral y) 128

fooImage = PicTypes.generateImage genFooImage 250 300

main :: IO ()
main = do
  writePng "./test.png" fooImage
  putStrLn "hello world"
