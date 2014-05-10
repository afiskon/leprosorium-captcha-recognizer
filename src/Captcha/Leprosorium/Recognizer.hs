module Captcha.Leprosorium.Recognizer where

import Captcha.Leprosorium.Recognizer.Preprocess
import Captcha.Leprosorium.Recognizer.GeneticAlgorithm
import Captcha.Leprosorium.Recognizer.Compress (cropAndCompress)
import Captcha.Leprosorium.Recognizer.NeuralNetwork
import Codec.Picture
import Codec.Picture.RGBA8

recognizeLeprosoriumCaptcha :: FilePath -> IO String
recognizeLeprosoriumCaptcha infile = do
    Right dynimg <- readImage infile 
    wbimg <- imageToWhiteAndBlack $ fromDynamicImage dynimg
    img <- removeNoise wbimg
    (letters, _) <- splitImageOnLetters img
    let letters' = map cropAndCompress letters
    return $ map recognizeLetter letters'
