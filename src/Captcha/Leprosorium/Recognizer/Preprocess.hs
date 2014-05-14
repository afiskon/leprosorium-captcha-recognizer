module Captcha.Leprosorium.Recognizer.Preprocess(
        imageToWhiteAndBlack,
        removeNoise
    ) where

import Codec.Picture

isBorder x y = x == 0 || x == 249 || y == 0 || y == 59

white = 255 :: Pixel8

black = 0 :: Pixel8

backgroundColor = PixelRGBA8 249 219 165 255

imageToWhiteAndBlack :: Image PixelRGBA8 -> IO (Image Pixel8)
imageToWhiteAndBlack img =
  withImage 250 60 $
    \x y -> return $ 
              let px = pixelAt img x y in
              if isBorder x y || px == backgroundColor
                then white
                else black

removeNoise :: Image Pixel8 -> IO (Image Pixel8)
removeNoise img = do
  let getSum im x y =
        if isBorder x y
          then 255
          else sum [ fromIntegral (pixelAt im i j) :: Int 
                     | i <- [x-1..x+1], j <- [y-1,y+1] ] `div` 9
  withImage 250 60 $ \x y -> return $
                               if getSum img x y > 64 then white
                                                      else black

