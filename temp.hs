import Data.Char
let2int :: Char -> Int
let2int c =
    let base = if isLower c then 'a' else 'A'
    in ord c - ord base

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c =
    int2let ((let2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
