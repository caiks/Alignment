module AlignmentRandom (
  historiesShuffle,
  historiesShuffle_1,
  histogramsRandomsUniform,
  histogramsRandomsUniform_1,
  histogramsRandomsMultinomial,
  histogramsRandomsMultinomial_1
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import AlignmentUtil
import Alignment
import System.Random

historiesShuffle :: History -> Int -> Maybe History
historiesShuffle hh s 
    | dim hh >= 1 = Just gg
    | otherwise = Nothing
  where
    ext 0 (h:t) ac = (h,ac ++ t)
    ext n (h:t) ac = ext (n-1) t (h:ac)
    lshuff [e] _ = [e]
    lshuff ee (r:rr) = let (s,ss) = ext (r `mod` (length ee)) ee [] in s:(lshuff ss rr)
    ishuff t hh =  
      let (ii, qq) = unzip (hlis hh) 
      in hhis (zip (lshuff ii (drop 1 (randomRs (0,pred (length ii)) (mkStdGen t) :: [Int]))) qq)
    gg = foldl1 join [ishuff t hh | (t,hh) <- zip (drop 1 (randoms (mkStdGen s) :: [Int])) [(hred hh v) | v <- hvars hh]]
    hlis = historiesList
    hhis ll = fromJust (listsHistory ll)
    hvars = Set.toList . historiesVars
    dim hh = length (hvars hh)
    hred hh v = setVarsHistoriesReduce (Set.singleton v) hh
    join = pairHistoriesJoin

histogramsRandomsUniform :: Histogram -> Int -> Histogram
histogramsRandomsUniform aa s = 
    his $ map (\((ss,q),p) -> (ss,q*p)) $ zip (lis aa) (map toRational (drop 1 (randomRs (0,1) (mkStdGen s) :: [Double])))
  where
    lis = histogramsList
    his ll = fromJust $ listsHistogram ll

histogramsRandomsMultinomial :: Histogram -> Int -> Int -> Histogram
histogramsRandomsMultinomial aa z s
    | size aa > 0 = his $ take z $ map ((\ss -> (ss,1)) . find aa . toRational) (drop 1 (randomRs (0,1) (mkStdGen s) :: [Double]))
    | otherwise = aa
  where
    acc aa = qq where (qq,r) = foldr (\(ss,c) (xx,t) -> ((ss,t+c):xx, t+c)) ([],0) $ aall $ resize 1 aa
    find aa x = qq where (qq,s) = foldl1 (\(ss,t) (rr,r) -> if x <= r then (rr,r) else (ss,t)) (acc aa)
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
    resize z aa = fromJust $ histogramsResize z aa
    size = histogramsSize
    his ll = fromJust $ listsHistogram ll

historiesShuffle_1 :: History -> Int -> Maybe History
historiesShuffle_1 hh s 
    | dim hh >= 1 = Just gg
    | otherwise = Nothing
  where
    ext 0 (h:t) ac = (h,ac ++ t)
    ext n (h:t) ac = ext (n-1) t (h:ac)
    lshuff [e] _ = [e]
    lshuff ee (r:rr) = let (s,ss) = ext (r `mod` (length ee)) ee [] in s:(lshuff ss rr)
    ishuff t hh =  
      let (ii, qq) = unzip (hlis hh) 
      in hhis (zip (lshuff ii (randomRs (0,pred (length ii)) (mkStdGen t) :: [Int])) qq)
    gg = foldl1 join [ishuff t hh | (t,hh) <- zip (randoms (mkStdGen s) :: [Int]) [(hred hh v) | v <- hvars hh]]
    hlis = historiesList
    hhis ll = fromJust (listsHistory ll)
    hvars = Set.toList . historiesVars
    dim hh = length (hvars hh)
    hred hh v = setVarsHistoriesReduce (Set.singleton v) hh
    join = pairHistoriesJoin

histogramsRandomsUniform_1 :: Histogram -> Int -> Histogram
histogramsRandomsUniform_1 aa s = 
    his $ map (\((ss,q),p) -> (ss,q*p)) $ zip (lis aa) (map toRational (randomRs (0,1) (mkStdGen s) :: [Double]))
  where
    lis = histogramsList
    his ll = fromJust $ listsHistogram ll

histogramsRandomsMultinomial_1 :: Histogram -> Int -> Int -> Histogram
histogramsRandomsMultinomial_1 aa z s
    | size aa > 0 = his $ take z $ map ((\ss -> (ss,1)) . find aa . toRational) (randomRs (0,1) (mkStdGen s) :: [Double])
    | otherwise = aa
  where
    acc aa = qq where (qq,r) = foldr (\(ss,c) (xx,t) -> ((ss,t+c):xx, t+c)) ([],0) $ aall $ resize 1 aa
    find aa x = qq where (qq,s) = foldl1 (\(ss,t) (rr,r) -> if x <= r then (rr,r) else (ss,t)) (acc aa)
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
    resize z aa = fromJust $ histogramsResize z aa
    size = histogramsSize
    his ll = fromJust $ listsHistogram ll

