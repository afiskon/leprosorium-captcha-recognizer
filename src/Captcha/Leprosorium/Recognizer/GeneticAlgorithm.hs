module Captcha.Leprosorium.Recognizer.GeneticAlgorithm (
        splitImageOnLetters
    ) where

import System.Random
import Codec.Picture
import Text.Printf
import Control.DeepSeq
import AI.GeneticAlgorithm.Simple
import qualified Data.List as L

data LetPos = LetPos (Image Pixel8) [(Int, Int)]

instance NFData LetPos where
    rnf (LetPos _ xs) = rnf xs `seq` ()

instance Show LetPos where
    show (LetPos _ xs) = "LetPos " ++ show xs

squareSize = 24 :: Int

numberOfLetters = 6 :: Int

white = 255 :: Pixel8

black = 0 :: Pixel8

squareCoordsToWeights xs =
    let sq t = t * t
        sqroot = floor . sqrt . (fromIntegral :: Int -> Double)
        neighborNums = [ sum [ if sqroot (sq (x - x') + sq (y - y')) < squareSize then (1 :: Int) else 0 
                               | (x', y') <- xs -- Note: including (x, y)
                             ]
                        | (x, y) <- xs
                       ]
    in map (\x -> if x > 1 then 0.0 else 1.0 ) neighborNums

coordFitness img x y =
    sum [ if pixelAt img i j == white then 0.0 else 1.0
           | i <- [x .. x + squareSize],
             j <- [y .. y + squareSize],
             i >= 0, j >= 0,
             i < imageWidth img,
             j < imageHeight img
        ]

norm = L.sortBy (\(x1,_) (x2,_) -> x1 `compare` x2)

instance Chromosome LetPos where
    crossover g0 (LetPos im xs) (LetPos _ ys) =
        let (idx, g1) = randomR (1, length xs - 1) g0
            (xs1, xs2) = splitAt idx xs
            (ys1, ys2) = splitAt idx ys
        in ( [ LetPos im (xs1 ++ ys2), LetPos im (ys1 ++ xs2) ], g1)

    mutation g0 (LetPos im xs) =
        let (idx, g1) = randomR (0, length xs - 1) g0
            (dx,  g2) = randomR (-20, 20) g1
            (dy,  g3) = randomR (-20, 20) g2
            (x, y) = xs !! idx
            xs' = take idx xs ++ [(x + dx, y + dy)] ++ drop (idx+1) xs
        in (LetPos im (norm xs'), g3)

    fitness (LetPos im xs) =
        let weights = squareCoordsToWeights xs
            fitnessList = [ if w > 0 then coordFitness im x y else 0 | ((x, y), w) <- zip xs weights ]
        in sum $ zipWith (*) weights fitnessList

randomGenPos img gen =
    let (lst, gen') =
            L.foldl'
                (\(xs, g) _ ->
                    let (x, g') = randomR (0, imageWidth img) g
                        (y, g'') = randomR (0, imageHeight img) g'
                    in ((x,y):xs, g'')
                ) ([], gen) [1..numberOfLetters]
    in (LetPos img (norm lst), gen')

stopf :: LetPos -> Int -> IO Bool
stopf best gnum = do
    let f = fitness best
    _ <- printf "Generation: %02d, Fitness: %.0f\n" gnum f
    return $ gnum >= 8

toVector img (x, y) =
    [ if i < 0 || j < 0 || i >= imageWidth img || j >= imageHeight img
         then (-1.0 :: Double) else if pixelAt img i j == black then 1.0 else -1.0
      | i <- [x .. x + squareSize], j <- [y .. y + squareSize]
    ]

splitImageOnLetters img = do
    (LetPos _ xs) <- runGAIO 50 0.5 (randomGenPos img) stopf
    return (map (toVector img) xs, xs)

