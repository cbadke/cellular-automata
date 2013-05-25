import Data.Bits
import Data.Map
import System.Environment

rule = 38

bitty :: Int -> Int -> [Int]
bitty width n = reverse [ min 1 (n .&. x) | x <- take width (Prelude.map (2^) [0..])]


generation :: [Int] -> [Int]
generation row = generationR ([0,0] ++ row ++ [0,0])
    where
    ruleMap = fromList (zip (Prelude.map (bitty 3) [0..7]) (bitty 8 rule))
    generationR row
        | (length row) < 3 = []
        | otherwise = (ruleMap ! (take 3 row)) : (generationR (drop 1 row))

main = do
    args <- getArgs
    let numLines = read (head args) :: Int
    putStrLn (show numLines)
