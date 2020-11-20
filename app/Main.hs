module Main where

import           Prelude

import qualified Data.Char     as Char
import           Data.List     as List (nub)
import qualified System.Random as Rand


boolToNum :: Bool -> Int
boolToNum b = if b then 1 else 0

hitCount :: Eq a => [a] -> [a] -> Int
hitCount = (.) trueCount . zipWith (==)

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

listToStr :: [Int] -> String
listToStr =  map Char.intToDigit

strToIntList :: String -> [Int]
strToIntList = map Char.digitToInt

getValidNum :: Int -> IO [Int] --n桁の数の並びを取得します
getValidNum n = do
  input <- strToIntList <$> getLine
  if length input == n && length (nub input) == n then
    return input
    else do
      putStrLn "invalid number"
      getValidNum n

youTurn :: [Int] -> [Int] -> [[Int]] -> Int -> IO () --自分のターンの処理
youTurn youNums comNums predictions n = do
  putStrLn "Write prediction"
  input <- getValidNum n
  let (hit, blow) = judge comNums input
  putStrLn $ "You > " ++ listToStr input
  putStrLn $ "Com > " ++ show hit ++ "H" ++ show blow ++ "B"
  if hit == n
    then do
      putStrLn $ "Your Number is " ++ listToStr youNums
      putStrLn $ "Com's Number is " ++ listToStr comNums
      putStrLn "You Win"
      return ()
    else
      comTurn youNums comNums predictions n

comTurn :: [Int] -> [Int] -> [[Int]] -> Int -> IO () --コンピューターのターンの処理(ランダム、実験用)
comTurn youNums comNums predictions n = do
  rand <- Rand.randomRIO(0, length predictions - 1)
  let comPred = predictions!!rand
      (hit, blow) = judge youNums comPred
  putStrLn $ "You > " ++ listToStr comPred
  putStrLn $ "Com > " ++ show hit ++ "H" ++ show blow ++ "B"
  if hit == n
    then do
      putStrLn $ "Your Number is " ++ listToStr youNums
      putStrLn $ "Com's Number is " ++ listToStr comNums
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

main :: IO ()
main = do
  putStrLn "Select Digits(2 ~ 4)"
  n <- do
    let
      loop = do
        str <- getLine :: IO String
        let m :: Int
            m = read str
        if m >= 2 && m <= 4 then
          return m
          else do
            putStrLn "invalid number"
            loop
    loop
  putStrLn "Set your number"
  youNums <- getValidNum n
  comNums <- randomUniqueNum n
  let predictions = allSort n
  youTurn youNums comNums predictions n
