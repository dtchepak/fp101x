import System.IO
import Control.Applicative

main :: IO ()
main = do
    putStrLn "Think of a word:"
    word <- sgetLine
    putStrLn "Try to guess it:"
    guess word

sgetLine :: IO String
sgetLine = do
    c <- getChar'
    if c == '\n' then do
        putChar c
        return []
    else do
        putChar '_'
        (c:) <$> sgetLine

getChar' :: IO Char
getChar' = do
    hSetEcho stdin False
    c <- getChar
    hSetEcho stdin True
    return c

guess :: String -> IO ()
guess word = do
    g <- getLine
    if g == word then
        putStrLn "Correct!"
    else do
        putStrLn $ diff word g
        guess word

diff :: String -> String -> String
diff a b = map (\c -> if c `elem` b then c else '_') a

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f acc =
    let f' a b = a >>= \a' -> f a' b
    in foldl f' (return acc) 
{-
foldLeftM _ acc [] = return acc
foldLeftM f acc (b:bs) =
    f acc b >>= \acc' -> foldLeftM f acc' bs
-}

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f acc =
    let f' a b = b >>= f a
    in foldr f' (return acc)
