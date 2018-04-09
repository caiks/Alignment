module AlignmentApprox (
  isAN,
  linear, loglinear,
  logGamma,
--  logGamma_1,
  drawBinomialsProbability, drawBinomialsProbabilityLog,
  drawHistoricalsProbability, drawHistoricalsProbabilityLog,
  drawMultinomialsMultLog,
  drawMultinomialsPermutorialLog,
  drawMultinomialsProbability, drawMultinomialsProbabilityLog,
  drawIndependentBinomialsProbability, drawIndependentBinomialsProbabilityLog,
  histogramsEntropy,
  histogramsHistogramsEntropyCross,
  setVarsHistogramsSliceEntropy,
  transformsHistogramsEntropyComponent,
  histogramsMultinomialLog,
  histogramsAlignment,
  histogramsAlignmentTerms,
  systemsSetVarsSizesAlignmentMaximum,
  systemsTransformsSizesAlignmentOverlap,
  histogramRegularUniformPivotsAlignment,
  histogramRegularUniformPivotsAlignmentTerms,
  histogramRegularNonPivotUniformPivotsAlignment,
  histogramRegularUniformAntiPivotsAlignment,
  space,
  spaceFixed,
  spaceSubset,
  spacePermutation,
  spaceComposition,
  spaceCompositionWeak,
  spaceBellUpper,
  spaceStirlingSecond,
  systemsDimensionsSpaceVariables,
  identifiersSizesSpace,
  systemsHistoriesSpaceEvents,
  encodingMinimalsSpace,
  encodingHistoryIndexsHistoriesSpace,
  encodingHistoryFixedsHistoriesSpace,
  systemsHistogramsSpaceEffective,
  encodingHistoryIndexEffectivesHistoriesSpace,
  systemsHistogramsSpaceCounts,
  systemsHistogramsSpaceCountsEffective,
  systemsHistogramsSpacePartitionEffective,
  encodingHistogramsHistogramsSpace,
  encodingHistogramEffectivesHistogramsSpace,
  histogramsSpaceClassification,
  encodingClassificationsHistoriesSpace,
  encodingClassificationEffectivesHistoriesSpace,
  encodingClassificationPartitionsHistoriesSpace,
  encodingClassificationVariatesHistoriesSpace,
  encodingHistoryFixedVariatesHistoriesSpace,
  histogramsSpaceCodeEntropy,
  histogramsSpaceEventsVariable,
  encodingHistoryVariablesHistoriesSpace,
  encodingClassificationIndependentsHistoriesSpace,
  encodingClassificationReducingsHistoriesSpace,
  encodingTransformsTransformsSpace,
  encodingTransformUnitsTransformsSpace,
  encodingPartitionsPartitionsSpaceUpper,
  encodingTransformOneFuncsTransformsSpace
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import AlignmentUtil
import Alignment
import AlignmentDistribution(systemsDrawCartesiansSupport)
import GHC.Real(numerator,denominator)
import Foreign.C.Types
import System.IO.Unsafe
import Unsafe.Coerce

-- From statistics package

-- | Positive infinity.
m_pos_inf :: Double
m_pos_inf = 1/0

-- | Negative infinity.
m_neg_inf :: Double
m_neg_inf = -1/0

-- | Not a number.
m_NaN :: Double
m_NaN = 0/0

isAN :: Double -> Bool
isAN x = not (isNaN x || isInfinite x)

linear :: Double -> Double -> Double -> Double -> Double -> Double
linear x1 y1 x2 y2 x 
  | x2 == x1 = y1
  | otherwise = y1 + (x - x1) * (y2 - y1) / (x2 - x1) 

loglinear :: Double -> Double -> Double -> Double -> Double -> Double
loglinear x1 y1 x2 y2 x
  | y1 <= 0 || y2 <= 0 = 0
  | otherwise = exp $ linear x1 (log y1) x2 (log y2) x

{-
foreign import ccall unsafe "alngam" alngam :: CDouble -> IO (CDouble)

logGamma_1 :: Double -> Double
logGamma_1 x = unsafeCoerce $ unsafePerformIO $ alngam (unsafeCoerce x)
-}

-- From statistics package

-- Adapted from http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html

-- | Compute the logarithm of the gamma function &#915;(/x/).  Uses
-- Algorithm AS 245 by Macleod.
--
-- Gives an accuracy of 10&#8211;12 significant decimal digits, except
-- for small regions around /x/ = 1 and /x/ = 2, where the function
-- goes to zero.  For greater accuracy, use 'logGammaL'.
--
-- Returns &#8734; if the input is outside of the range (0 < /x/
-- &#8804; 1e305).
logGamma :: Double -> Double
logGamma x
    | x <= 0    = m_pos_inf
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 5.1e5 = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)
  where
    (a , b , c)
        | x < 0.5   = (-y , x + 1 , x)
        | otherwise = (0  , x     , x - 1)

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
    r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
    r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070

    r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
    r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
    r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514

    r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
    r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
    r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2

    r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103

histogramsMultinomialLog :: Histogram -> Double
histogramsMultinomialLog aa =  
    facln zaa - sum [facln c | c <- counts aa]
  where
    zaa = size aa
    size = fromRational . histogramsSize
    counts = map (\(ss,c) -> fromRational c) . histogramsList
    facln x = logGamma (x + 1)

drawBinomialsProbabilityLog :: Double -> Double -> Double -> Double
drawBinomialsProbabilityLog p z k =  
    facln z - facln k - facln (z - k) + k * log p + (z - k) * log (1 - p)
  where
    facln x = logGamma (x + 1)

drawBinomialsProbability :: Double -> Double -> Double -> Double
drawBinomialsProbability p z k = exp $ drawBinomialsProbabilityLog p z k 

drawHistoricalsProbabilityLog :: Histogram -> Histogram -> Double
drawHistoricalsProbabilityLog ee aa =  
    sum [facln e - facln a - facln (e - a) | (ss,a) <- aall aa, let e = index ee ss] -
    (facln zee - facln zaa - facln (zee - zaa))
  where
    zaa = size aa
    zee = size ee
    size = fromRational . histogramsSize
    index aa ss = fromRational $ fromMaybe 0 $ histogramsStatesCount aa ss
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
    facln x = logGamma (x + 1)

drawHistoricalsProbability :: Histogram -> Histogram -> Double
drawHistoricalsProbability ee aa = exp $ drawHistoricalsProbabilityLog ee aa

drawMultinomialsMultLog :: Histogram -> Histogram -> Double
drawMultinomialsMultLog _  =  histogramsMultinomialLog
 
drawMultinomialsPermutorialLog :: Histogram -> Histogram -> Double
drawMultinomialsPermutorialLog ee aa =  
    sum [a * log (index ee ss) | (ss,a) <- aall aa] - zaa * log zee
  where
    zaa = size aa
    zee = size ee
    size = fromRational . histogramsSize
    index aa ss = fromRational $ fromMaybe 0 $ histogramsStatesCount aa ss
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
 
drawMultinomialsProbabilityLog :: Histogram -> Histogram -> Double
drawMultinomialsProbabilityLog ee aa =  
    facln zaa - sum [facln a | (ss,a) <- aall aa] +
    sum [a * log (index ee ss) | (ss,a) <- aall aa] - zaa * log zee
  where
    zaa = size aa
    zee = size ee
    size = fromRational . histogramsSize
    index aa ss = fromRational $ fromMaybe 0 $ histogramsStatesCount aa ss
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
    facln x = logGamma (x + 1)

drawMultinomialsProbability :: Histogram -> Histogram -> Double
drawMultinomialsProbability ee aa = exp $ drawMultinomialsProbabilityLog ee aa
 
drawIndependentBinomialsProbabilityLog :: Histogram -> Double -> Histogram -> Double
drawIndependentBinomialsProbabilityLog ee z aa = 
    sum [bpmf (index pp ss) z a | (ss,a) <- aall aa]
  where
    pp = ee `div` scalar (size ee)
    scalar = fromJust . histogramScalar
    div = pairHistogramsDivide
    size = fromRational . histogramsSize
    index aa ss = fromRational $ fromMaybe 0 $ histogramsStatesCount aa ss
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList
    facln x = logGamma (x + 1)
    bpmf = drawBinomialsProbabilityLog 

drawIndependentBinomialsProbability :: Histogram -> Double -> Histogram -> Double
drawIndependentBinomialsProbability ee z aa = exp $ drawIndependentBinomialsProbabilityLog ee z aa

histogramsEntropy :: Histogram -> Double
histogramsEntropy aa =
    - sum [a * log a | (ss,a) <- aall (prob aa)]
  where
    prob aa = aa `div` scalar (size aa)
    scalar = fromJust . histogramScalar
    div = pairHistogramsDivide
    size = fromRational . histogramsSize
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim

histogramsHistogramsEntropyCross :: Histogram -> Histogram -> Double
histogramsHistogramsEntropyCross aa bb =
    - sum [size (prob aa `mul` sunit ss) * log b | (ss,b) <- aall (prob bb)]
  where
    prob aa = aa `div` scalar (histogramsSize aa)
    scalar = fromJust . histogramScalar
    div = pairHistogramsDivide
    size = fromRational . histogramsSize
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
    mul = pairHistogramsMultiply
    sunit ss = fromJust $ histogramSingleton ss 1

setVarsHistogramsSliceEntropy :: Set.Set Variable -> Histogram -> Double
setVarsHistogramsSliceEntropy kk aa = 
    sum [zc * entropy cc | rr <- states (aa `red` kk), 
          let cc = aa `mul` single rr `red` vk, let zc = fromRational (size cc), zc > 0]
  where
    vk = vars aa `Set.difference` kk
    single ss = fromJust $ histogramSingleton ss 1
    red aa vv = setVarsHistogramsReduce vv aa
    states =  Set.toList . histogramsSetState
    mul = pairHistogramsMultiply
    size = histogramsSize
    entropy = histogramsEntropy
    vars = histogramsVars

transformsHistogramsEntropyComponent :: Transform -> Histogram -> Double
transformsHistogramsEntropyComponent tt aa =
    sum [size (aa' `mul` sunit rr) * entropy (aa `mul` cc) | (rr,cc) <- inv tt]
  where
    aa' = prob aa `tmul` tt
    prob aa = aa `div` scalar (histogramsSize aa)
    scalar = fromJust . histogramScalar
    div = pairHistogramsDivide
    size = fromRational . histogramsSize
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim
    mul = pairHistogramsMultiply
    sunit ss = fromJust $ histogramSingleton ss 1
    entropy = histogramsEntropy
    inv = Map.toList . transformsInverse
    tmul aa tt = transformsHistogramsApply tt aa

histogramsAlignment :: Histogram -> Double
histogramsAlignment aa =
    sum [facln c | c <- counts aa] - sum [facln c | c <- counts (ind aa)]
  where
    counts = map (\(ss,c) -> fromRational c) . histogramsList
    ind = histogramsIndependent
    facln x = logGamma (x + 1)

histogramsAlignmentTerms :: Histogram -> (Double,Double)
histogramsAlignmentTerms aa =
    (sum [facln c | c <- counts aa],sum [facln c | c <- counts (ind aa)])
  where
    counts = map (\(ss,c) -> fromRational c) . histogramsList
    ind = histogramsIndependent
    facln x = logGamma (x + 1)

systemsSetVarsSizesAlignmentMaximum :: System -> Set.Set Variable -> Rational -> Double
systemsSetVarsSizesAlignmentMaximum uu vv z 
  | n < 2 = 0.0
  | z <= 0.0 = 0.0
  | not (isin uu vv) = 0.0
  | d <= 1 = 0.0
  | otherwise = d * facln (z'/d) - d**n * facln (z'/d**n)
  where
    z' = fromRational z
    d = fromIntegral $ valmin uu vv
    n = fromIntegral $ dim vv
    dim = Set.size 
    isin uu vv = vv `Set.isSubsetOf` vars uu
    vars = systemsVars
    facln x = logGamma (x + 1)
    valency uu v = Set.size $ fromJust $ systemsVarsValues uu v
    valmin uu vv = minimum [valency uu v | v <- qqll vv]
    qqll = Set.toList

systemsTransformsSizesAlignmentOverlap :: System -> Transform -> Rational -> Double
systemsTransformsSizesAlignmentOverlap uu tt z
  | dim (und tt) < 1 = 0.0
  | z <= 0 = 0.0
  | not (isin uu (und tt)) = 0.0
  | otherwise = algn (resize z (cart uu (und tt)) `tmul` tt)
  where
    cart uu vv = fromJust $ systemsSetVarsHistogramCartesian uu vv
    isin uu vv = vv `Set.isSubsetOf` vars uu
    dim = Set.size 
    vars = systemsVars
    resize z aa = fromJust $ histogramsResize z aa
    algn = histogramsAlignment
    tmul aa tt = transformsHistogramsApply tt aa
    und = transformsUnderlying

histogramRegularUniformPivotsAlignment :: Rational -> Integer -> Rational -> Double
histogramRegularUniformPivotsAlignment d n z =
    b * facln (z'/b) - 
      sum [binom n k * (d'-1)**k' * facln (((d'-1)**((n'-1)*k') * z')/(b**n')) | k <- [0..n], let k' = fromIntegral k]
  where
    binom n k = fromIntegral (combination n k) :: Double
    d' = fromRational d :: Double
    z' = fromRational z :: Double
    n' = fromIntegral n :: Double
    b = (d'-1)**n' + 1 :: Double
    facln x = logGamma (x + 1)

histogramRegularUniformPivotsAlignmentTerms :: Rational -> Integer -> Rational -> (Double,Double)
histogramRegularUniformPivotsAlignmentTerms d n z =
    (b * facln (z'/b),
      sum [binom n k * (d'-1)**k' * facln (((d'-1)**((n'-1)*k') * z')/(b**n')) | k <- [0..n], let k' = fromIntegral k])
  where
    binom n k = fromIntegral (combination n k) :: Double
    d' = fromRational d :: Double
    z' = fromRational z :: Double
    n' = fromIntegral n :: Double
    b = (d'-1)**n' + 1 :: Double
    facln x = logGamma (x + 1)

histogramRegularNonPivotUniformPivotsAlignment :: Rational -> Integer -> Rational -> Rational -> Double
histogramRegularNonPivotUniformPivotsAlignment d n z p =
    facln (p'*z') + ((d'-1)**n') * facln (q'*z') - 
      sum [binom n k * (d'-1)**k' * facln ((d'-1)**((n'-1)*k') * q'**k' * p'**(n'-k') * z') | k <- [0..n], 
        let k' = fromIntegral k]
  where
    d' = fromRational d :: Double
    z' = fromRational z :: Double
    n' = fromIntegral n :: Double
    p' = fromRational p :: Double
    q' = (1-p')/((d'-1)**n') :: Double
    binom n k = fromIntegral (combination n k) :: Double
    facln x = logGamma (x + 1)

histogramRegularUniformAntiPivotsAlignment :: Rational -> Integer -> Rational -> Double
histogramRegularUniformAntiPivotsAlignment d n z =
    b' * facln (z'/b') - 
      sum [binom n k * (d'-1)**k' * facln (((d'**(n'-1)-(d'-1)**(n'-1))**k' * (d'**(n'-1)-1)**(n'-k') * z')/(b'**n')) | 
        k <- [0..n], let k' = fromIntegral k]
  where
    d' = fromRational d :: Double
    z' = fromRational z :: Double
    n' = fromIntegral n :: Double
    b' = d'**n' - (d'-1)**n' - 1 :: Double
    binom n k = fromIntegral (combination n k) :: Double
    facln x = logGamma (x + 1)

space :: Double -> Double
space = log

spaceFixed :: Double -> Double -> Double
spaceFixed z v = z * log v

spaceSubset :: Double -> Double -> Double
spaceSubset a b =  
    facln a - facln (a - b)  - facln b
  where
    facln x = logGamma (x + 1)

spacePermutation :: Double -> Double
spacePermutation z = facln z
  where
    facln x = logGamma (x + 1)

spaceCompositionWeak :: Double -> Double -> Double
spaceCompositionWeak z v = facln (z + v -1)  - facln z - facln (v-1)
  where
    facln x = logGamma (x + 1)

spaceComposition :: Double -> Double -> Double
spaceComposition z v = facln (z-1)  - facln (z-v) - facln (v-1)
  where
    facln x = logGamma (x + 1)

spaceBellUpper :: Double -> Double
spaceBellUpper n = n * log (0.792 * n / log (n+1)) 

spaceStirlingSecond :: Integer -> Integer -> Double
spaceStirlingSecond n k = log $ fromIntegral $ stirlingSecond n k

systemsDimensionsSpaceVariables :: System -> Integer -> Double
systemsDimensionsSpaceVariables uu n = 
    log (r+1) + spaceSubset r (fromIntegral n)
  where
    r = fromIntegral (Set.size (systemsSetVar uu))

identifiersSizesSpace :: Set.Set Id -> Integer -> Double 
identifiersSizesSpace xx z = 
    log (y+1) + spaceSubset y (fromIntegral z)
  where
    y = fromIntegral (Set.size xx)

systemsHistoriesSpaceEvents :: System -> History -> Maybe Double
systemsHistoriesSpaceEvents uu hh 
  | hvars hh `Set.isSubsetOf` uvars uu = Just $ spaceFixed (size hh) (vol uu (hvars hh))
  | otherwise = Nothing
  where
    size = fromIntegral . historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    vol uu vv = fromIntegral $ fromJust $ systemsSetVarsVolume uu vv

encodingMinimalsSpace :: Set.Set a -> Double
encodingMinimalsSpace mm = log $ fromIntegral $ Set.size mm

encodingHistoryIndexsHistoriesSpace = encodingHistoryFixedsHistoriesSpace

encodingHistoryFixedsHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingHistoryFixedsHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = Just $ spvar uu (dim hh) + spid xx (size hh) + spev uu hh
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    spev uu hh = fromJust $ systemsHistoriesSpaceEvents uu hh

systemsHistogramsSpaceEffective :: System -> Histogram -> Maybe Double
systemsHistogramsSpaceEffective uu aa 
  | avars aa `Set.isSubsetOf` uvars uu = Just $ space v + spaceSubset v x
  | otherwise = Nothing
  where
    v = vol uu (avars aa)
    x = size (eff aa)
    size = fromRational . histogramsSize
    eff = histogramsEffective
    avars = histogramsSetVar
    uvars = systemsSetVar
    vol uu vv = fromIntegral $ fromJust $ systemsSetVarsVolume uu vv

encodingHistoryIndexEffectivesHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingHistoryIndexEffectivesHistoriesSpace uu xx hh
  | avars aa `Set.isSubsetOf` uvars uu = Just $ 
    spvar uu n + spid xx z + speff uu aa + spaceFixed (fromIntegral z) (fromIntegral x)
  | otherwise = Nothing
  where
    aa = hhaa hh
    n = dim hh
    z = size aa
    x = size $ eff aa
    spid = identifiersSizesSpace
    spvar = systemsDimensionsSpaceVariables
    speff uu aa = fromJust $ systemsHistogramsSpaceEffective uu aa
    hhaa hh = historiesHistogram hh
    dim hh = toInteger $ Set.size $ avars aa
    size = numerator . histogramsSize
    eff = histogramsEffective
    avars = histogramsSetVar
    uvars = systemsSetVar

systemsHistogramsSpaceCounts :: System -> Histogram -> Maybe Double
systemsHistogramsSpaceCounts uu aa 
  | avars aa `Set.isSubsetOf` uvars uu = Just $ spaceCompositionWeak (size aa) (vol uu (avars aa))
  | otherwise = Nothing
  where
    size = fromRational . histogramsSize
    avars = histogramsSetVar
    uvars = systemsSetVar
    vol uu vv = fromIntegral $ fromJust $ systemsSetVarsVolume uu vv

systemsHistogramsSpaceCountsEffective :: System -> Histogram -> Maybe Double
systemsHistogramsSpaceCountsEffective uu aa 
  | avars aa `Set.isSubsetOf` uvars uu = Just $ spaceComposition z x
  | otherwise = Nothing
  where
    z = size aa
    x = size (eff aa)
    size = fromRational . histogramsSize
    eff = histogramsEffective
    avars = histogramsSetVar
    uvars = systemsSetVar
    vol uu vv = fromIntegral $ fromJust $ systemsSetVarsVolume uu vv

systemsHistogramsSpacePartitionEffective :: System -> Histogram -> Maybe Double
systemsHistogramsSpacePartitionEffective uu aa 
  | avars aa `Set.isSubsetOf` uvars uu && denominator z == 1 = Just $ 
    spacePermutation (fromRational x) + spaceStirlingSecond (numerator z) (numerator x)
  | otherwise = Nothing
  where
    z = size aa
    x = size (eff aa)
    size = fromRational . histogramsSize
    eff = histogramsEffective
    avars = histogramsSetVar
    uvars = systemsSetVar
    vol uu vv = fromIntegral $ fromJust $ systemsSetVarsVolume uu vv

encodingHistogramsHistogramsSpace :: System -> Integer -> Histogram -> Maybe Double
encodingHistogramsHistogramsSpace uu y aa
  | avars aa `Set.isSubsetOf` uvars uu = Just $ spvar uu (dim aa) + space (fromIntegral y) + spct uu aa
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim = histogramsDimension
    avars = histogramsSetVar
    uvars = systemsSetVar
    spct uu aa = fromJust $ systemsHistogramsSpaceCounts uu aa

encodingHistogramEffectivesHistogramsSpace :: System -> Integer -> Histogram -> Maybe Double
encodingHistogramEffectivesHistogramsSpace uu y aa
  | avars aa `Set.isSubsetOf` uvars uu = Just $ spvar uu (dim aa) + space (fromIntegral y) + speff uu aa + spct uu aa
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim = histogramsDimension
    avars = histogramsSetVar
    uvars = systemsSetVar
    spct uu aa = fromJust $ systemsHistogramsSpaceCountsEffective uu aa
    speff uu aa = fromJust $ systemsHistogramsSpaceEffective uu aa

histogramsSpaceClassification :: Histogram -> Double
histogramsSpaceClassification aa = facln (size aa) - sum [facln c | c <- counts aa]
  where
    size = fromRational . histogramsSize
    facln x = logGamma (x + 1)
    counts aa = [fromRational c | (ss,c) <- histogramsList aa]

encodingClassificationsHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingClassificationsHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (dim hh) + spid xx (size hh) + spct uu (hhaa hh) + spcl (hhaa hh)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    hhaa = historiesHistogram
    spct uu aa = fromJust $ systemsHistogramsSpaceCounts uu aa
    spcl = histogramsSpaceClassification

encodingClassificationEffectivesHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingClassificationEffectivesHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (dim hh) + spid xx (size hh) + speff uu (hhaa hh)  + spct uu (hhaa hh) + spcl (hhaa hh)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    hhaa = historiesHistogram
    spct uu aa = fromJust $ systemsHistogramsSpaceCountsEffective uu aa
    speff uu aa = fromJust $ systemsHistogramsSpaceEffective uu aa
    spcl = histogramsSpaceClassification

encodingClassificationPartitionsHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingClassificationPartitionsHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (dim hh) + spid xx (size hh) + speff uu (hhaa hh)  + sppt uu (hhaa hh)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    hhaa = historiesHistogram
    sppt uu aa = fromJust $ systemsHistogramsSpacePartitionEffective uu aa
    speff uu aa = fromJust $ systemsHistogramsSpaceEffective uu aa

encodingClassificationVariatesHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingClassificationVariatesHistoriesSpace uu xx hh = 
    case encodingClassificationsHistoriesSpace uu xx hh of
      Just s -> Just $ s - log (dim uu + 1) + log (dim uu) - log (size xx + 1) + log (size xx)
      Nothing -> Nothing
  where
    dim uu = fromIntegral (Set.size (systemsSetVar uu))
    size xx = fromIntegral (Set.size xx)

encodingHistoryFixedVariatesHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingHistoryFixedVariatesHistoriesSpace uu xx hh = 
    case encodingHistoryFixedsHistoriesSpace uu xx hh of
      Just s -> Just $ s - log (dim uu + 1) + log (dim uu) - log (size xx + 1) + log (size xx)
      Nothing -> Nothing
  where
    dim uu = fromIntegral (Set.size (systemsSetVar uu))
    size xx = fromIntegral (Set.size xx)

histogramsSpaceCodeEntropy :: Histogram -> Double
histogramsSpaceCodeEntropy aa =
    - sum [log a | (ss,a) <- aall (prob aa)]
  where
    prob aa = aa `div` scalar (size aa)
    scalar = fromJust . histogramScalar
    div = pairHistogramsDivide
    size = fromRational . histogramsSize
    aall = map (\(ss,c) -> (ss,fromRational c)) . histogramsList . histogramsTrim

histogramsSpaceEventsVariable :: Histogram -> Double
histogramsSpaceEventsVariable aa = 
    size aa * ent aa
  where
    ent = histogramsEntropy
    size = fromRational . histogramsSize

encodingHistoryVariablesHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingHistoryVariablesHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (dim hh) + spid xx (size hh) + spct uu (hhaa hh) + spent (hhaa hh) + spevv (hhaa hh)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    hhaa = historiesHistogram
    spct uu aa = fromJust $ systemsHistogramsSpaceCounts uu aa
    spent = histogramsSpaceCodeEntropy
    spevv = histogramsSpaceEventsVariable

encodingClassificationIndependentsHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingClassificationIndependentsHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (dim hh) + spid xx (size hh) + space 2 + spctind uu (hhaa hh) + spcl (hhaa hh)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    hhaa = historiesHistogram
    msupp uu vv z = fromJust $ systemsDrawCartesiansSupport uu vv z
    msuppi uu vv z = Set.filter ind $ msupp uu vv z
    ind aa = aa `pairHistogramsEquivalent` histogramsIndependent aa
    spctind uu aa
      | z == 0 = 0
      | ind aa = space $ fromIntegral $ Set.size $ msuppi uu vv z
      | otherwise = space $ fromIntegral $ Set.size $ msupp uu vv z Set.\\ msuppi uu vv z
      where 
        vv = histogramsSetVar aa
        z = numerator $ histogramsSize aa
    spcl = histogramsSpaceClassification

encodingClassificationReducingsHistoriesSpace :: System -> Set.Set Id -> History -> Maybe Double
encodingClassificationReducingsHistoriesSpace uu xx hh
  | hvars hh `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (dim hh) + spid xx (size hh) + space 2 + spctind uu (hhaa hh) + spcl (hhaa hh)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    dim hh = toInteger $ Set.size $ hvars hh
    spid = identifiersSizesSpace
    size = historiesSize
    hvars = historiesSetVar
    uvars = systemsSetVar
    vars = Set.toList . histogramsVars
    hhaa = historiesHistogram
    ind aa = aa `pairHistogramsEquivalent` histogramsIndependent aa
    red aa v = setVarsHistogramsReduce (Set.singleton v) aa
    spctind uu aa
      | ind aa = sum [spct uu (aa `red` v) | v <- vars aa]
      | otherwise = spct uu aa
    spct uu aa = fromJust $ systemsHistogramsSpaceCounts uu aa
    spcl = histogramsSpaceClassification

encodingTransformsTransformsSpace :: System -> Integer -> Transform -> Maybe Double
encodingTransformsTransformsSpace uu y tt
  | vars tt `Set.isSubsetOf` uvars uu = 
      Just $ sphis uu y (his tt) + space (dim (vars tt) + 1) + spaceSubset (dim (vars tt)) (dim (der tt))
  | otherwise = Nothing
  where
    dim = fromIntegral . Set.size 
    uvars = systemsSetVar
    vars = transformsVars
    der = transformsDerived
    his = transformsHistogram
    sphis uu y aa = fromJust $ encodingHistogramsHistogramsSpace uu y aa

encodingTransformUnitsTransformsSpace :: System -> Transform -> Maybe Double
encodingTransformUnitsTransformsSpace uu tt
  | vars tt `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (idim (vars tt)) + space (dim (vars tt) + 1) + spaceSubset (dim (vars tt)) (dim (der tt))
        + space (vol uu (his tt) + 1) + spaceSubset (vol uu (his tt)) (size (his tt))
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    idim = toInteger . Set.size 
    dim = fromIntegral . Set.size 
    vars = transformsVars
    uvars = systemsSetVar
    der = transformsDerived
    his = transformsHistogram
    vol uu aa = fromIntegral $ fromJust $ systemsHistogramsVolume uu aa
    size = fromIntegral . numerator. histogramsSize . histogramsEffective

encodingPartitionsPartitionsSpaceUpper :: System -> Partition -> Maybe Double
encodingPartitionsPartitionsSpaceUpper uu pp
  | vars pp `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (idim (vars pp)) + spaceBellUpper (vol uu pp)
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    idim = toInteger . Set.size 
    vars = partitionsVars
    uvars = systemsSetVar
    vol uu pp = fromIntegral $ fromJust $ systemsSetVarsVolume uu (vars pp)

encodingTransformOneFuncsTransformsSpace :: System -> Transform -> Maybe Double
encodingTransformOneFuncsTransformsSpace uu tt
  | vars tt `Set.isSubsetOf` uvars uu = 
      Just $ spvar uu (idim (vars tt)) + space (dim (vars tt) + 1) + spaceSubset (dim (vars tt)) (dim (der tt))
        + space (vol uu (der tt)) + spaceSubset (vol uu (der tt)) (volstder tt)
        + space (ss (ivol uu (und tt)) (ivolstder tt))
  | otherwise = Nothing
  where
    spvar = systemsDimensionsSpaceVariables
    idim = toInteger . Set.size 
    dim = fromIntegral . Set.size 
    vars = transformsVars
    uvars = systemsSetVar
    der = transformsDerived
    und = transformsUnderlying
    ivol uu vv = toInteger $ fromJust $ systemsSetVarsVolume uu vv
    vol uu vv = fromIntegral $ ivol uu vv
    ivolstder tt = toInteger $ Set.size $ transformsStateDeriveds tt
    volstder tt = fromIntegral $ ivolstder tt
    ss n k = fromIntegral $ stirlingSecond n k

