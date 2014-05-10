import System.Environment
import Captcha.Leprosorium.Recognizer

process [infile] = do
    code <- recognizeLeprosoriumCaptcha infile
    putStrLn $ "RESULT: " ++ code

process _ = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " +RTS -N -RTS captcha.png"

main = do
    args <- getArgs
    process args
