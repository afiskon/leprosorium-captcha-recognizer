module Captcha.Leprosorium.Recognizer.Compress (
        cropAndCompress,
        to2DImage,
        compress,
        crop,
    ) where

import qualified Data.List as L
import Data.List.Split (chunksOf)

cropAndCompress = compress 14 . crop . to2DImage

to2DImage = chunksOf 25

crop = crop' . crop'
  where
    crop' = L.transpose . reverse . filt . reverse . filt
    filt = dropWhile (all (== -1))

compress :: Double -> [[Double]] -> [Double]
compress scaledwh lst2d =
  let mh = fromIntegral $ length lst2d - 1
      mw = fromIntegral $ length (head lst2d) - 1
      notneg x = if x < 0 then 0 else x
  in [ sum [ (if x == cx && y == cy then 0.8 else 0.1) * 
                    lst2d !! notneg y !! notneg x 
               | x <- [cx-1 .. cx+1],
                 y <- [cy-1 .. cy+1]
           ] / 1.6
       | i <- [1 :: Double .. scaledwh],
         j <- [1 :: Double .. scaledwh],
         let cx = truncate (mw * i / scaledwh) - 1,
         let cy = truncate (mh * j / scaledwh) - 1
     ]
