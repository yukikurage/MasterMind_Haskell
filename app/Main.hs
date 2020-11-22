module Main where

import           Prelude

import qualified Data.Char     as Char
import           Data.List     as List (nub)
import qualified System.Random as Rand
import           Text.Read     (readMaybe)


boolToNum :: Bool -> Int
boolToNum b = if b then 1 else 0

hitCount :: Eq a => [a] -> [a] -> Int
hitCount xs = trueCount . zipWith (==) xs

trueCount :: [Bool] -> Int --配列に含まれるTrueの個数を返します
trueCount = foldl ((. boolToNum) . (+)) 0

judge :: (Eq a) => [a] -> [a] -> (Int, Int) --(hit,blow)のタプルを返します
judge txs xs =
  (hit,sec - hit)
  where
    sec = trueCount.map (`elem` txs) $ xs
    hit = hitCount txs xs

randomUniqueNum :: Int -> IO [Int]
randomUniqueNum n = take n . nub . Rand.randomRs (0,9::Int) <$> Rand.newStdGen

intListToStr :: [Int] -> String
intListToStr =  map Char.intToDigit

strToIntList :: String -> [Int]
strToIntList =  map Char.digitToInt

maybeStrToIntList :: String -> Maybe [Int]
maybeStrToIntList str =
  if all Char.isDigit str then
    Just $ map Char.digitToInt str
  else Nothing

getValidLine :: (String -> Bool) -> IO String --条件にあった文字列を取得
getValidLine f = do
  input <- getLine
  if f input then
    return input
    else do
      putStr "Invalid String"
      getValidLine f

getValidNums :: Int -> IO [Int] --n桁の数字の並びを取得
getValidNums n = strToIntList <$> getValidLine f where
  f :: String -> Bool
  f x = case maybeStrToIntList x of
    Nothing -> False
    Just y  -> n == length y

youTurn :: [Int] -> [Int] -> [[Int]] -> Int -> IO () --自分のターンの処理
youTurn youNums comNums predictions n = do
  putStrLn "Write prediction"
  input <- getValidNums n
  let (hit, blow) = judge comNums input
  putStrLn $ "You > " ++ intListToStr input
  putStrLn $ "Com > " ++ show hit ++ "H" ++ show blow ++ "B"
  if hit == n
    then do
      putStrLn $ "Your Number is " ++ intListToStr youNums
      putStrLn $ "Com's Number is " ++ intListToStr comNums
      putStrLn "You Win"
      return ()
    else
      comTurn youNums comNums predictions n

comTurn :: [Int] -> [Int] -> [[Int]] -> Int -> IO () --コンピューターのターンの処理(ランダム)
comTurn youNums comNums predictions n = do
  rand <- Rand.randomRIO(0, length predictions - 1)
  let comPred = predictions!!rand
      (hit, blow) = judge youNums comPred
  putStrLn $ "Com > " ++ intListToStr comPred
  putStrLn $ "You > " ++ show hit ++ "H" ++ show blow ++ "B"
  if hit == n
    then do
      putStrLn $ "Your Number is " ++ intListToStr youNums
      putStrLn $ "Com's Number is " ++ intListToStr comNums
      putStrLn "Com Win"
      return ()
    else do
      youTurn youNums comNums (refine (hit, blow) comPred predictions) n

allSort :: Int -> [[Int]] --n桁の考えうるすべての重複がない数字の並びを出力
allSort 1 = [[x] | x <- [0..9]]
allSort n = concatMap (\x -> [y:x | y <- [0..9], y `notElem` x]) $ allSort (n - 1)

refine :: Eq a => (Int, Int) -> [a] -> [[a]] -> [[a]] --選択肢を絞り込みます
refine (hit, blow) comPred =
  filter (\pred -> let p_hitblow = judge pred comPred in p_hitblow == (hit, blow))

getValidNum :: (Int -> Bool) -> IO Int
getValidNum f = read <$> getValidLine g where
  g x = maybe False f (readMaybe x :: Maybe Int)

main :: IO ()
main = do
  putStrLn "Select Digits(2 ~ 4)"
  n <- getValidNum ((&&) . (2<=) <*> (4<=))
  putStrLn "Set your number"
  youNums <- getValidNums n
  comNums <- randomUniqueNum n
  let predictions = allSort n
  youTurn youNums comNums predictions n
  putStrLn "Press any key to close"
  _ <- getLine :: IO String
  return ()
