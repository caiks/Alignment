module AlignmentDistribution (
  Distribution,
  distributionsSetVar,
  systemsDistributionsIs,
  distributionsIsCongruent,
  distributionsIsProbability,
  distributionsModes,
  systemsDistributionsMean,
  systemsDistributionsVariance,
  pairDistributionsChiSq,
  drawHistoricalsDistribution,
  drawHistoricalsDistribution_1,
  drawHistoricalsSupport,
  systemsDrawHistoricalsDistributionComplete,
  systemsDrawHistoricalsSupportStuffed,
  systemsDrawHistoricalsDistributionStuffed, systemsDrawHistoricalsDistributionStuffed_1,
  drawHistoricalsCardinality,
  systemsDrawCartesiansSupport,
  systemsDrawCartesiansProbabilityMass,
  drawMultinomialsCardinality,
  systemsDrawMultinomialsDistribution, systemsDrawMultinomialsDistribution_1,
  systemsDrawIndependentsSupport,
  systemsDrawIndependentBinomialsProbabilityMass, systemsDrawIndependentBinomialsProbabilityMass_1,
  drawIndependentBinomialsMean,
  drawIndependentBinomialsVarianceSum
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import AlignmentUtil
import Alignment
import GHC.Real

type Distribution = Map.Map Histogram Rational

distributionsSetVar :: Distribution -> Set.Set Variable
distributionsSetVar ff 
  | ff == Map.empty = Set.empty
  | otherwise = histogramsSetVar $ Set.findMin $ Map.keysSet ff

systemsDistributionsIs :: System -> Distribution -> Bool
systemsDistributionsIs uu ff 
  | ff == Map.empty = True
  | otherwise = setsAll isComp (Map.keysSet ff) && minimum (Map.elems ff) >= 0
  where 
    isComp aa = systemsHistogramsIsCartesian uu aa && histogramsSetVar aa == distributionsSetVar ff

distributionsIsCongruent :: Distribution -> Bool
distributionsIsCongruent ff 
  | Map.size ff < 2 = True
  | otherwise = setsAll isCong (Map.keysSet ff)
  where 
    isCong = pairHistogramsCongruent (Set.findMin (Map.keysSet ff))

distributionsIsProbability :: Distribution -> Bool
distributionsIsProbability ff 
  | ff == Map.empty = False
  | otherwise = minimum (Map.elems ff) >= 0 && sum (Map.elems ff) == 1

distributionsModes :: Distribution -> Set.Set Histogram
distributionsModes ff = relationsDomain $ relationsMaximum $ Set.fromList $ Map.assocs ff

systemsDistributionsMean :: System -> Distribution -> Maybe Histogram
systemsDistributionsMean uu ff 
  | ff /= Map.empty && isDist uu ff && sumf ff > 0 = 
    Just $ foldl1 add [scalar f `mul` aa | (aa,f) <- Map.toList ff] `div` scalar (sumf ff)
  | otherwise = Nothing
  where
    isDist = systemsDistributionsIs
    sumf ff = Map.fold (+) 0 ff
    scalar = fromJust . histogramScalar
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    div = pairHistogramsDivide
    mul = pairHistogramsMultiply

systemsDistributionsVariance :: System -> Distribution -> Maybe (Map.Map State Rational)
systemsDistributionsVariance uu ff 
  | ff /= Map.empty && isDist uu ff && sumf ff > 0 = 
    Just $ Map.fromList [(ss, sum [f * (index aa ss - index mm ss)^2 | (aa,f) <- Map.toList ff] / sumf ff) | 
      ss <- states uu (vars ff)]
  | otherwise = Nothing
  where
    mm = fromJust $ systemsDistributionsMean uu ff
    isDist = systemsDistributionsIs
    index aa ss = fromJust $ histogramsStatesCount aa ss
    vars = distributionsSetVar
    states uu vv = Set.toList $ fromJust $ systemsSetVarsSetStateCartesian uu vv
    sumf ff = Map.fold (+) 0 ff

pairDistributionsChiSq :: Distribution -> Distribution -> Maybe Rational
pairDistributionsChiSq ff gg 
  | trim ff /= Map.empty && Map.keysSet (trim ff) `Set.isSubsetOf` Map.keysSet gg =  
    Just $ sum [ 1 / f * (gg Map.! aa - f)^2 | (aa,f) <- Map.toList (trim ff)]
  | otherwise = Nothing
  where
    trim = Map.filter (>0) 

drawHistoricalsDistribution :: Histogram -> Integer -> Maybe (Map.Map Histogram Integer)
drawHistoricalsDistribution ee z 
  | isInt ee && size ee >= z && z > 0 = Just $ count [(hhaa gg,gg) | gg <- histories ee, hsize gg == z]
  | otherwise = Nothing
  where
    isInt = histogramsIsIntegral
    histories = Set.toList . Set.map (llhh . Set.toList) . setsPowerset . Set.fromList . hhll . aahh
    hsize = historiesSize
    size = numerator. histogramsSize
    hhaa = historiesHistogram
    aahh aa = fromJust $ histogramsHistory aa
    llhh ll = fromJust $ listsHistory ll
    hhll = historiesList
    count = relationsCount . Set.fromList

drawHistoricalsDistribution_1 :: Histogram -> Integer -> Maybe (Map.Map Histogram Integer)
drawHistoricalsDistribution_1 ee z 
  | isInt ee && size ee >= z && z > 0 = Just $ relationsCount $ Set.map (\gg -> (hhaa gg,gg)) (drawnr (aahh ee) z empty)
  | otherwise = Nothing
  where
    isInt = histogramsIsIntegral
    empty = historyEmpty
    size = numerator. histogramsSize
    drawnr hh z gg = 
      if hh /= empty && z > 0 then 
        (setSetsUnion $ Set.map (\e -> drawnr (hdel hh e) (z-1) (hins gg e)) (hhqq hh))
      else 
        (Set.singleton gg) 
    hhll = historiesList
    llhh ll = fromJust $ listsHistory ll
    hhqq = Set.fromList . hhll
    qqhh = llhh . Set.toList
    hdel hh e = qqhh (e `Set.delete` hhqq hh)
    hins hh e = qqhh (e `Set.insert` hhqq hh)
    hhaa = historiesHistogram
    aahh aa = fromJust $ histogramsHistory aa

drawHistoricalsSupport :: Histogram -> Integer -> Maybe (Set.Set Histogram)
drawHistoricalsSupport ee z = 
  case drawHistoricalsDistribution ee z of
    Just mm -> Just $ Map.keysSet mm
    Nothing -> Nothing

systemsDrawHistoricalsDistributionComplete :: System -> Histogram -> Integer -> Maybe (Map.Map Histogram Integer)
systemsDrawHistoricalsDistributionComplete uu ee z = 
  case drawHistoricalsDistribution ee z of
    Just mm -> Just $ Map.fromList $ map (\(aa,d) -> (comp uu aa,d))  $ Map.toList mm
    Nothing -> Nothing
  where
    comp uu aa = fromJust $ systemsHistogramsComplete uu aa

systemsDrawHistoricalsSupportStuffed :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set Histogram)
systemsDrawHistoricalsSupportStuffed uu vv z 
  | (vv `Set.isSubsetOf` (systemsVars uu)) && z > 0 = Just $ hisinc (zero uu vv) z
  | otherwise = Nothing 
  where
    hisinc aa 0 = Set.singleton aa
    hisinc aa z = setSetsUnion $ Set.map (\ss -> hisinc (aa `add` unit ss) (z-1)) (vvqq uu (vars aa))
    vvqq uu vv = fromJust $ systemsSetVarsSetStateCartesian uu vv
    vars = histogramsSetVar
    unit ss = fromJust $ setStatesHistogramUnit $ Set.singleton ss 
    cart uu vv = fromJust $ systemsSetVarsHistogramCartesian uu vv
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sub xx yy = fromJust $ pairHistogramsSubtract xx yy
    zero uu vv = (cart uu vv) `sub` (cart uu vv)

systemsDrawHistoricalsDistributionStuffed :: System -> Histogram -> Integer -> Maybe (Map.Map Histogram Integer)
systemsDrawHistoricalsDistributionStuffed uu ee z = 
  case systemsDrawHistoricalsDistributionComplete uu ee z of
    Just mm -> Just $ Map.union mm (zeroes ((supp uu (vars ee) z) Set.\\ (Map.keysSet mm)))
    Nothing -> Nothing
  where
    supp uu vv z = fromJust $ systemsDrawHistoricalsSupportStuffed uu vv z
    vars = histogramsSetVar
    zeroes qq = Map.fromList $ map (\aa -> (aa,0)) $ Set.toList qq

systemsDrawHistoricalsDistributionStuffed_1 :: System -> Histogram -> Integer -> Maybe (Map.Map Histogram Integer)
systemsDrawHistoricalsDistributionStuffed_1 uu ee z = 
  case systemsDrawHistoricalsSupportStuffed uu (vars ee) z of
    Just qq -> Just $ Map.fromList [if aa `leq` ee then (aa, freq ee aa) else (aa,0)  | aa <- Set.toList qq]
    Nothing -> Nothing
  where
    vars = histogramsSetVar
    leq = pairHistogramsLeq
    index aa ss = numerator $ fromJust $ histogramsStatesCount aa ss
    freq ee aa = product [combination (index ee ss) (index aa ss) | ss <- states aa]
    states = Set.toList . histogramsSetState . histogramsTrim

drawHistoricalsCardinality :: Histogram -> Integer -> Maybe Integer
drawHistoricalsCardinality ee z 
  | isInt ee && size ee >= z && z > 0 = Just $ combination (size ee) z
  | otherwise = Nothing
  where
    isInt = histogramsIsIntegral
    size = numerator. histogramsSize

systemsDrawCartesiansSupport :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set Histogram)
systemsDrawCartesiansSupport = systemsDrawHistoricalsSupportStuffed

systemsDrawCartesiansProbabilityMass :: System -> Histogram -> Integer -> Maybe Distribution
systemsDrawCartesiansProbabilityMass uu ee z =
  case systemsDrawCartesiansSupport uu (vars ee) z of
    Just qq -> Just $ Map.fromList [(aa, mlt aa * pp ee aa) | aa <- Set.toList qq]
    Nothing -> Nothing
  where
    vars = histogramsSetVar
    mlt aa = toRational $ fromJust $ histogramIntegralsMult aa
    pp ee aa = fromJust $ histogramsHistogramIntegralsPermutorial ee aa

drawMultinomialsCardinality :: Histogram -> Integer -> Integer
drawMultinomialsCardinality ee z = numerator (size ee) ^ z
  where
    size = histogramsSize

systemsDrawMultinomialsDistribution :: System -> Histogram -> Integer -> Maybe Distribution
systemsDrawMultinomialsDistribution uu ee z = 
  case systemsDrawCartesiansProbabilityMass uu ee z of
    Just mm -> Just $ Map.fromList [(aa, card ee z * p) | (aa,p) <- Map.toList mm]
    Nothing -> Nothing
  where
    card ee z = toRational $ drawMultinomialsCardinality ee z

systemsDrawMultinomialsDistribution_1 :: System -> Histogram -> Integer -> Maybe (Map.Map Histogram Integer)
systemsDrawMultinomialsDistribution_1 uu ee z
  | isInt ee && size ee >= z && z > 0 = 
    case systemsDrawCartesiansSupport uu (vars ee) z of
      Just qq -> Just $ Map.union (dist ee z) (zeroes (qq Set.\\ (Map.keysSet (dist ee z))))
      Nothing -> Nothing
  | otherwise = Nothing
  where
    dist ee z = relationsCount $ Set.map (\gg -> (comp uu (hhaa gg),gg)) (drawwr (aahh ee) z empty)
    isInt = histogramsIsIntegral
    empty = historyEmpty
    size = numerator. histogramsSize
    drawwr hh 0 gg = Set.singleton gg 
    drawwr hh z gg = setSetsUnion $ Set.map (\e -> drawwr hh (z-1) (hins gg e z)) (hhqq hh)
    hhll = historiesList
    llhh ll = fromJust $ listsHistory ll
    hhqq = Set.fromList . hhll
    qqhh = llhh . Set.toList
    hins hh (x,ss) z = qqhh ((IdIntId (z,x),ss) `Set.insert` hhqq hh)
    hhaa = historiesHistogram
    aahh aa = fromJust $ histogramsHistory aa
    zeroes qq = Map.fromList $ map (\aa -> (aa,0)) $ Set.toList qq
    vars = histogramsSetVar
    comp uu aa = fromJust $ systemsHistogramsComplete uu aa

systemsDrawIndependentsSupport :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set Histogram)
systemsDrawIndependentsSupport uu vv z 
  | (vv `Set.isSubsetOf` (systemsVars uu)) && z > 0 = 
      Just $ foldl1 join [ Set.fromList [single ss c | c <- [0..z]] | ss <- states uu vv]
  | otherwise = Nothing 
  where
    join mm nn = Set.fromList [aa `add` bb | aa <- Set.toList mm, bb <- Set.toList nn]
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    single ss c = fromJust $ histogramSingleton ss (fromIntegral c)
    states uu vv = Set.toList $ fromJust $ systemsSetVarsSetStateCartesian uu vv

systemsDrawIndependentBinomialsProbabilityMass :: System -> Histogram -> Integer -> Maybe Distribution
systemsDrawIndependentBinomialsProbabilityMass uu ee z =
  case systemsDrawIndependentsSupport uu (vars ee) z of
    Just qq -> Just $ Map.fromList [(aa, product [bpmf (index pp ss) z a | (ss,a) <- aall aa]) | aa <- Set.toList qq]
    Nothing -> Nothing
  where
    pp = ee `div` scalar (size ee)
    scalar = fromJust . histogramScalar
    size = histogramsSize
    vars = histogramsSetVar
    bpmf p z k = comb z k * p ^ k * (1 - p) ^ (z-k)
    comb n k = toRational $ combination n k
    aall = map (\(ss,c) -> (ss, numerator c)) . histogramsList
    index aa ss = fromJust $ histogramsStatesCount aa ss
    div = pairHistogramsDivide

systemsDrawIndependentBinomialsProbabilityMass_1 :: System -> Histogram -> Integer -> Maybe Distribution
systemsDrawIndependentBinomialsProbabilityMass_1 uu ee z
  | (vars ee) `Set.isSubsetOf` (systemsVars uu) && isInt ee && size ee >= 1 && z > 0 = Just $ norm (dist uu ee z)
  | otherwise = Nothing
  where
    dist uu ee z = relationsCount $ Set.map (\gg -> (unstuff uu (vars ee) (hhaa gg),gg)) $ 
      foldl1 join [drawwr (aahh bb) z empty | bb <- stuff ee]
    stuff ee = [sing (ss `sunion` bin) c `add` sing (ss `sunion` bout) (size ee - c) | (ss,c) <- aall ee] 
    unstuff uu vv aa =  aa `mul` sing bin 1 `red` vv `add` cart uu vv `sub` cart uu vv
    join mm nn = Set.fromList [llhh (hhll hh ++ hhll gg) | hh <- Set.toList mm, gg <- Set.toList nn]
    drawwr hh 0 gg = Set.singleton gg 
    drawwr hh z gg = setSetsUnion $ Set.map (\e -> drawwr hh (z-1) (hins gg e z)) (hhqq hh)
    hins hh (x,ss) z = qqhh ((IdIntId (z,x),ss) `Set.insert` hhqq hh)
    hhqq = Set.fromList . hhll
    hhll = historiesList
    b = VarStr "b"
    bin = stateSingleton b (ValInt 1)
    bout = stateSingleton b (ValInt 0)
    aall = histogramsList
    sing ss c = fromJust $ histogramSingleton ss c
    sunion = pairStatesUnionLeft
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sub xx yy = fromJust $ pairHistogramsSubtract xx yy
    cart uu vv = fromJust $ setStatesHistogramUnit $ fromJust $ systemsSetVarsSetStateCartesian uu vv
    vars = histogramsSetVar
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa
    size = histogramsSize
    llhh ll = fromJust $ listsHistory ll
    norm qq = let t = toRational (Map.fold (+) 0 qq) in Map.map (\f -> toRational f / t) qq 
    isInt = histogramsIsIntegral
    empty = historyEmpty
    qqhh = llhh . Set.toList
    hhaa = historiesHistogram
    aahh aa = fromJust $ histogramsHistory aa

drawIndependentBinomialsMean :: Histogram -> Integer -> Maybe Histogram
drawIndependentBinomialsMean ee z 
  | size ee > 0 && z >= 0 = Just $ scalar (toRational z) `mul` pp
  | otherwise = Nothing
  where
    pp = ee `div` scalar (size ee)
    scalar = fromJust . histogramScalar
    size = histogramsSize
    mul = pairHistogramsMultiply
    div = pairHistogramsDivide

drawIndependentBinomialsVarianceSum :: Histogram -> Integer -> Maybe Rational
drawIndependentBinomialsVarianceSum ee z 
  | size ee > 0 && z >= 0 = Just $ sum [toRational z * c * (1 - c) | (ss,c) <- aall pp]
  | otherwise = Nothing
  where
    pp = ee `div` scalar (size ee)
    scalar = fromJust . histogramScalar
    size = histogramsSize
    aall = histogramsList
    div = pairHistogramsDivide
