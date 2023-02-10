import Data.Char (isSpace)
import Data.List (genericLength)
import Data.Maybe (catMaybes)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import System.Environment (getArgs)
import System.IO (IOMode (..), hGetLine, hIsEOF, withFile)
import System.Random (getStdRandom, randomR)
import Text.Read (readMaybe)

typingLines = 5

rankingFile = "ranking.txt"

doWhile :: (Monad m) => m (Maybe a) -> m [a]
doWhile m = do
  mx <- m
  case mx of
    Just x -> (x :) <$> doWhile m
    Nothing -> return []

fileLines :: FilePath -> IO [String]
fileLines fp = withFile fp ReadMode $ \h -> doWhile $ do
  b <- hIsEOF h
  if b then return Nothing else Just <$> hGetLine h

generateProblem :: [String] -> IO [String]
generateProblem lines =
  randomRange
    >>= \i -> return $ take typingLines $ drop i lines
  where
    randomRange = getStdRandom $ randomR (0, length lines - typingLines)

getLineWithTime :: IO (String, Integer)
getLineWithTime = do
  t1 <- getCurrentTime
  s <- getLine
  t2 <- getCurrentTime
  return (s, ceiling $ t2 `diffUTCTime` t1)

countMiss :: String -> String -> Integer
countMiss (c : cs) (d : ds)
  | c == d = countMiss cs ds
  | otherwise = 1 + countMiss cs ds
countMiss cs [] = genericLength cs
countMiss [] ds = genericLength ds

point :: String -> String -> Integer -> Integer
point s0 s1 sc = genericLength s0 * 60 `div` (sc + countMiss s0 s1)

type Record = (String, Integer)

record :: String -> Maybe Record
record str = let n : p : _ = words str in (,) n <$> readMaybe p

fromRecord :: Record -> String
fromRecord (n, p) = n ++ " " ++ show p

insertRecord :: Integer -> [Record] -> [(Maybe String, Integer)]
insertRecord px rrs@((nr, pr) : rs)
  | px >= pr = (Nothing, px) : map (\(n, p) -> (Just n, p)) rrs
  | otherwise = (Just nr, pr) : insertRecord px rs
insertRecord px [] = [(Nothing, px)]

yourName :: [(Maybe String, Integer)] -> IO [Record]
yourName ((Just n, p) : rs) = ((n, p) :) <$> yourName rs
yourName ((Nothing, p) : rs) = do
  putStrLn "What's your name?"
  n <- getLine
  ((filter (not . isSpace) n, p) :) <$> yourName rs
yourName [] = return []

ranking :: FilePath -> Integer -> IO ()
ranking fp p = do
  rs <- catMaybes . map record <$> fileLines fp
  rs' <- yourName . take 10 $ insertRecord p rs
  writeFile fp . unlines $ map fromRecord rs'

main :: IO ()
main = do
  fp : _ <- getArgs
  line <- fileLines fp
  pro <- generateProblem line
  ps <- for pro $ \l -> do
    putStrLn l
    (s, sc) <- getLineWithTime
    let p = point l s sc
    print p
    return p
  let rslt = sum ps `div` genericLength ps
  print rslt
  ranking rankingFile rslt
