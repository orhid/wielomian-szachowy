{-# LANGUAGE TupleSections #-}
import           Control.Monad
import           System.Environment
import           System.IO

wielomianSzachownicy :: String -> [Int]
wielomianSzachownicy = liftM2 ((. (flip map [0..] . join . (((. (product . enumFromTo 1)) . quot . length) .) . (!!) . iterate ((map =<< flip (filter . (not .) . ap (ap . ((||) .) . (. fst) . (==) . fst) ((. snd) . (==) . snd))) =<<) . return)) . take) ((1 +) . minimum . sequence [length, length . head]) (ap ((join .) . zipWith (ap (flip . flip ((. ((filter (('o' ==) . fst) .) . zip)) . (.) . map . (. snd) . (,))) (enumFromTo 0 . length))) (enumFromTo 0 . length)) . lines

main = do
  args <- getArgs
  case args of
    [] -> error "nie podano ścieżki"
    [arg] -> do
      szachownica <- readFile arg
      print $ wielomianSzachownicy szachownica
    _ -> error "podano niespodziewaną liczbę argumentów"
