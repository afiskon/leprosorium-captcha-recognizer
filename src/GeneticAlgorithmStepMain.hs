import Captcha.Leprosorium.Recognizer.Preprocess
import Captcha.Leprosorium.Recognizer.GeneticAlgorithm
import System.Environment
import Data.List (foldl')
import Codec.Picture
import Codec.Picture.RGBA8
import Codec.Picture.Canvas

squareSize = 25

white8 = 255 :: Pixel8

white = PixelRGBA8 255 255 255 255

black = PixelRGBA8 0   0   0   255

red = PixelRGBA8 255 0 0 255

convertToRGBA8 :: Image Pixel8 -> IO (Image PixelRGBA8)
convertToRGBA8 img =
    withImage 250 60 $
        \x y -> return $ if pixelAt img x y == white8 then white else black

process [infile, outfile] = do
    Right dynimg <- readImage infile 
    wbimg <- imageToWhiteAndBlack $ fromDynamicImage dynimg
    img <- removeNoise wbimg
    (_, xs) <- splitImageOnLetters img
    rgba <- convertToRGBA8 img
    let Right canv = imageToCanvas rgba
        canv' = foldl' (\c (x, y) -> drawSquare x y squareSize red c) canv xs
        img' = canvasToImage canv'
    writePng outfile img'

process _ = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " +RTS -N -RTS infile.png outfile.png"

main = do
    args <- getArgs
    process args
