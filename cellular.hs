import Data.Bits
import Data.Map

rule = 38

bitty width n = reverse [ min 1 (n .&. x) | x <- take width (Prelude.map (2^) [0..])]

ruleMap = fromList (zip (Prelude.map (bitty 3) [0..7]) (bitty 8 rule))
