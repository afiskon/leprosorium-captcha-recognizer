module Main where

import System.Random
import AI.NeuralNetworks.Simple
import Text.Printf
import qualified Captcha.Leprosorium.Recognizer.Compress as C
import qualified Captcha.Leprosorium.Data.Trainset as Trainset
import qualified Captcha.Leprosorium.Data.Testset as Testset

maxw = 0.075
learningRate = 0.0003
targeterr = 47.0 :: Double
useCrop = True
scaledwh = 10
maxgnum = 2000
learnAttempts = 50
secondLayerFactor = 2
square x = x * x

newtype CaptchaNN = CaptchaNN (NeuralNetwork Double)
    deriving Show

calcCaptchaNN (CaptchaNN nn) i =
    let lst = runNeuralNetwork nn i
        m = maximum lst
    in map (\x -> if x == m then 1 else -1) lst

str2lst = map (\x -> if x == '1' then 1 else -1)

learnset :: [(String, String)] -> [([Double],[Double])]
learnset set =
    let crop = if useCrop then C.crop else id
        compress = C.compress scaledwh . crop . C.to2DImage
    in map (\(o, i) -> (compress $ str2lst i, str2lst o)) set

trainset = learnset Trainset.examples
testset = learnset Testset.examples

mse :: CaptchaNN -> [([Double],[Double])] -> Double
mse (CaptchaNN nn) set =
    0.5 * sum [ square $ d - y | (i, o) <- set, let o' = runNeuralNetwork nn i, (d, y) <- zip o o' ]

sumerr nn set =
    let t = [ if o == o' then 0 else 1 | (i, o) <- set, let o' = calcCaptchaNN nn i ]
    in 100 * sum t / fromIntegral (length t)

stopf net gnum = do
    let nn = CaptchaNN net
        trainmse = mse nn trainset
        testmse = mse nn testset
        trainerr = sumerr nn trainset
        testerr = sumerr nn testset
        formatstr = "=== Generation: %05d, Train MSE: %.4f, Test MSE: %.4f, Train err: %.2f%%, Test err: %.2f%% ===\n"
    print $ getWeights net
    printf formatstr gnum trainmse testmse trainerr testerr
    return $ gnum >= maxgnum || (trainerr <= targeterr && testerr <= targeterr && gnum >= 1)

learnNetwork :: Int -> IO CaptchaNN
learnNetwork att = do
    gen <- newStdGen
    let (net, _) = randomNeuralNetwork gen [truncate $ square scaledwh, truncate $ square scaledwh / secondLayerFactor, 15] [Tanh, Tanh] maxw
    newnet <- backpropagationBatchParallel net trainset learningRate stopf
    let best = CaptchaNN newnet
        testerr = sumerr best testset 
    if att > 1 && testerr > targeterr
        then do
            putStrLn "=== bad network ==="
            learnNetwork $ att - 1
        else return best

main = do
    (CaptchaNN net) <- learnNetwork learnAttempts
    putStrLn "~~~~~~~~ RESULT NETWORK ~~~~~~~~"
    print net
    putStrLn "~~~~~~~~ WEIGHTS ~~~~~~~~"
    print $ getWeights net
