import Char
import System.Environment

bytesCount str = case str of
                    []  -> 0
                    (c:cs) -> 1+bytesCount cs


linesCount [] = 0
linesCount ('\n':cs) = 1 + linesCount cs
linesCount (c:cs) = linesCount cs

wordsCount str = outWords str
    where wordScan f [] = 0
          wordScan f (c:cs)
            | isAlphaNum c = f (inWord cs)
            | otherwise    = outWords cs
          outWords str = wordScan (¥n -> 1 + n) str
          inWord str = wordScan id str

wcFile filename = do
    contents <- readFile filename
    putStrln ("¥t" ++ show (linesCount contents) ++
              "¥t" ++ show (wordsCount contents) ++
              "¥t" ++ show (bytesCount contents) ++
              "¥t" ++ filename)

main = do
    args <- getArgs
    mapM_ wcFile args
