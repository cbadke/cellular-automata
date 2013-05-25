import Data.Bits
import Data.Map
import System.Environment

bitty :: Int -> Int -> [Int]
bitty width n = reverse [ min 1 (n .&. x) | x <- take width (Prelude.map (2^) [0..])]

automata :: Int -> Int -> [Int]
automata rule numLines = iter numLines [1]
    where
    ruleMap = fromList (zip (Prelude.map (bitty 3) [0..7]) (bitty 8 rule))
    generation row = generationR ([0,0] ++ row ++ [0,0])
        where
        generationR row
            | (length row) < 3 = []
            | otherwise = (ruleMap ! (take 3 row)) : (generationR (drop 1 row))
    iter remaining row
        | remaining == 0 = row
        | otherwise = iter (remaining - 1) (generation row)

main = do
    args <- getArgs
    let numLines = read (head args) :: Int
    let rule = read (head (drop 1 args)) :: Int
    putStrLn (show (automata rule numLines))
