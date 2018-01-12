module AlignmentPracticableIO (
  systemsHistogramsHistogramsFudsAlignmentCsdIO_u,
  systemsHistogramsHistogramsFudsAlignmentCsdIO_u1,
  parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO,
  parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO_1,
  parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighestIO,
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValencyIO,
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValencyIO_1,
  parametersSystemsSamplesShufflesListVariablesSearcherFudHighestIO,
  parametersSystemsSamplesShufflesListVariablesInducerFudHighestIO,
  parametersSystemsSamplesListVariablesInducerDecompFudHighestIO,
  parametersSystemsSamplesListVariablesInducerDecompFudHighestIO_1
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Time
import System.Locale
import Text.Printf
import AlignmentUtil
import Alignment
import AlignmentRandom
import AlignmentSubstrate
import AlignmentApprox
import AlignmentPracticable
import GHC.Real


-- AYOR
systemsHistogramsHistogramsFudsAlignmentCsdIO_u :: System -> Histogram -> Histogram -> Fud -> IO Double
systemsHistogramsHistogramsFudsAlignmentCsdIO_u uu aa aarr ff = 
    do
      print "systemsHistogramsHistogramsFudsAlignmentCsdIO_u ..."
      return $ (algn (aa `fmul` ff) - algn (aarr `fmul` ff)) / (w ** (1/m))
  where
    ww = fder ff
    w = fromIntegral $ vol uu ww
    m = fromIntegral $ Set.size ww
    algn = histogramsAlignment
    fder = fudsDerived
    fmul aa ff = fudsHistogramsApply ff aa
    ppcd = toInteger . Set.size . partitionsSetComponent
    vol uu vv = product [uval uu v | v <- Set.toList vv]
    uval _ (VarPartition pp) =  ppcd pp
    uval uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v

-- AYOR
systemsHistogramsHistogramsFudsAlignmentCsdIO_u1 :: System -> Histogram -> Histogram -> Fud -> IO Double
systemsHistogramsHistogramsFudsAlignmentCsdIO_u1 uu aa aarr ff = 
    do
      print "systemsHistogramsHistogramsFudsAlignmentCsdIO_u1 ..."
      systemsHistogramsHistogramsFudsAlignmentCsdIO_u uu aa aarr ff

parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  
  IO (Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)])
parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO xmax omax uu aa aarr ff
  | xmax < 0 = return $ Nothing
  | omax < 0 = return $ Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = 
      do
        printf ">>> zzffcstupopt\n"
        printf "xmax: %d\n" $ xmax
        printf "volume: %.f\n" $ w
        printf "dimension: %.f\n" $ q
        printf "geometric average valency: %.2f\n" $ d
        printf "estimated kmax floor: %d\n" $ kmaxl
        printf "estimated kmax: %.2f\n" $ kmax
        printf "estimated kmax ceiling: %d\n" $ kmaxu
        printf "estimated search cardinality approx floor: %d\n" $ sscdal
        printf "estimated search cardinality approx interp: %.2f\n" $ sscda
        printf "estimated search cardinality approx ceiling: %d\n" $ sscdau
        printf "estimated search cardinality upper: %d\n" $ zzffcstupoptcdu kmaxu aa ff
        t1 <- getCurrentTime
        printf "max: %.2f\n" $ a
        t2 <- getCurrentTime
        printf "steps: %d\n" $ length xx
        printf "elements: %d\n" $ Map.size ee
        printf "searched: %d\n" $ Map.size ss
        printf "<<< zzffcstupopt %s\n" $ show $ diffUTCTime t2 t1
        return $ Just xx
  | otherwise = return $ Nothing
  where
    ww = fvars ff `cup` vars aa
    w = (fromIntegral $ vol uu ww) :: Double
    q = (fromIntegral $ Set.size ww) :: Double
    d = w ** (1/q)
    kmax = if d > 1 then log (fromIntegral xmax) / log d else 0
    kmaxl = floor kmax
    kmaxu = ceiling kmax
    sscdal = zzffcstupoptcda kmaxl omax aa ff
    sscdau = zzffcstupoptcda kmaxu omax aa ff
    sscda = loglinear (fromInteger kmaxl) (fromInteger sscdal) (fromInteger kmaxu) (fromInteger sscdau) kmax
    xx = opt (zzcstupneigh xmax uu aa aarr ff) (topff omax ff) (zzcstupinit xmax uu aa aarr ff)
    ee = foldl Map.union Map.empty $ fst $ unzip xx
    ss = foldl Map.union Map.empty $ snd $ unzip xx
    a = if ee /= Map.empty then (last $ sort $ Map.elems ee) else 0
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcstupinit xmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesFudsFunctionInitialTuple xmax uu aa aarr ff
    zzcstupneigh xmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple xmax uu aa aarr ff bb
    zzffcstupoptcda kmax omax aa ff = 
      parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox kmax omax (vars aa) ff
    zzffcstupoptcdu kmax aa ff = parametersSetVariablesFudsFunctionOptimiserTupleCardinalityUpper kmax (vars aa) ff
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    subset = Set.isSubsetOf
    qqll = Set.toList
    sgl = Set.singleton
    cup = Set.union

parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO_1 :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  
  IO (Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)])
parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO_1 xmax omax uu aa aarr ff
  | xmax < 0 = return $ Nothing
  | omax < 0 = return $ Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = 
      do
        printf ">>> zzffcstupopt\n"
        printf "xmax: %d\n" $ xmax
        printf "volume: %.f\n" $ w
        printf "dimension: %.f\n" $ q
        printf "geometric average valency: %.2f\n" $ d
        printf "estimated kmax floor: %d\n" $ kmaxl
        printf "estimated kmax: %.2f\n" $ kmax
        printf "estimated kmax ceiling: %d\n" $ kmaxu
        printf "estimated search cardinality approx floor: %d\n" $ zzffcstupoptcda kmaxl omax aa ff
        printf "estimated search cardinality approx ceiling: %d\n" $ zzffcstupoptcda kmaxu omax aa ff
        printf "estimated search cardinality upper: %d\n" $ zzffcstupoptcdu kmaxu aa ff
        t1 <- getCurrentTime
        let xx = opt (zzcstupneigh xmax uu aa aarr ff) (topff omax ff) (zzcstupinit xmax uu aa aarr ff)
        printf "max: %.2f\n" $ maxr xx
        t2 <- getCurrentTime
        printf "steps: %d\n" $ length xx
        printf "elements: %d\n" $ length $ concat $ map Map.toList $ init $ fst $ unzip xx
        printf "searched: %d\n" $ length $ concat $ map Map.toList $ init $ snd $ unzip xx
        printf "<<< zzffcstupopt %s\n" $ show $ diffUTCTime t2 t1
        return $ Just $ xx
  | otherwise = return $ Nothing
  where
    ww = fvars ff `cup` vars aa
    w = (fromIntegral $ vol uu ww) :: Double
    q = (fromIntegral $ Set.size ww) :: Double
    d = w ** (1/q)
    kmax = if d > 1 then log (fromIntegral xmax) / log d else 0
    kmaxl = floor kmax
    kmaxu = ceiling kmax
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcstupinit xmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesFudsFunctionInitialTuple xmax uu aa aarr ff
    zzcstupneigh xmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple xmax uu aa aarr ff bb
    zzffcstupoptcda kmax omax aa ff = 
      parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox kmax omax (vars aa) ff
    zzffcstupoptcdu kmax aa ff = parametersSetVariablesFudsFunctionOptimiserTupleCardinalityUpper kmax (vars aa) ff
    maxr xx = let ll = (sort $ concat $ map Map.elems $ fst $ unzip xx) in if ll /= [] then last ll else 0
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    subset = Set.isSubsetOf
    qqll = Set.toList
    sgl = Set.singleton
    cup = Set.union

parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighestIO :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  
  IO (Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)])
parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighestIO wmax omax uu aa aarr ff
  | wmax < 0 = return $ Nothing
  | omax < 0 = return $ Nothing
  | fvars ff `subset` uvars uu = 
      do
        printf ">>> zzffcsdderhighopt\n"
        printf "wmax: %d\n" $ wmax
        printf "volume: %.f\n" $ w
        printf "dimension: %.f\n" $ r
        printf "geometric average valency: %.2f\n" $ d
        printf "estimated jmax floor: %d\n" $ jmaxl
        printf "estimated jmax: %.2f\n" $ jmax
        printf "estimated jmax ceiling: %d\n" $ jmaxu
        printf "omax: %d\n" $ omax
        printf "estimated search cardinality approx floor: %d\n" $ sscdal
        printf "estimated search cardinality approx interp: %.2f\n" $ sscda
        printf "estimated search cardinality approx ceiling: %d\n" $ sscdau
        printf "estimated search cardinality upper: %d\n" $ sscdu
        t1 <- getCurrentTime
        printf "max: %.2f\n" $ a
        t2 <- getCurrentTime
        printf "steps: %d\n" $ length xx
        printf "elements: %d\n" $ Map.size ee
        printf "searched: %d\n" $ Map.size ss
        printf "<<< zzffcsdderhighopt %s\n" $ show $ diffUTCTime t2 t1
        return $ Just xx
  | otherwise = return $ Nothing
  where
    ww = fvars ff `minus` vars aa
    w = (fromIntegral $ vol uu ww) :: Double
    r = (fromIntegral $ Set.size ww) :: Double
    d = w ** (1/r)
    jmax = if d > 1 then log (fromIntegral wmax) / log d else 0
    jmaxl = floor jmax
    jmaxu = ceiling jmax
    sscdal = zzffcsdderhighoptcda jmaxl omax aa ff
    sscdau = zzffcsdderhighoptcda jmaxu omax aa ff
    sscda = loglinear (fromInteger jmaxl) (fromInteger sscdal) (fromInteger jmaxu) (fromInteger sscdau) jmax
    sscdu = zzffcsdderhighoptcdu jmaxu aa ff
    xx = opt (zzcsdderneigh wmax uu aa aarr ff) (topff omax ff) (zzcsdderinit wmax uu aa aarr ff)
    ee = foldl Map.union Map.empty $ fst $ unzip xx
    ss = foldl Map.union Map.empty $ snd $ unzip xx
    a = if ee /= Map.empty then (last $ sort $ Map.elems ee) else 0
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcsdderinit wmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesFudsFunctionInitialDerivedHighest wmax uu aa aarr ff
    zzcsdderneigh wmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived wmax uu aa aarr ff bb
    zzffcsdderhighoptcda jmax omax aa ff = 
      parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityApprox jmax omax (vars aa) ff
    zzffcsdderhighoptcdu jmax aa ff = 
      parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityUpper jmax (vars aa) ff
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf
    minus = Set.difference

parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValencyIO :: 
  Integer -> Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] ->
  IO (Maybe ([(Map.Map Fud Double, Map.Map Fud Double)],(System,[Variable])))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValencyIO 
  mmax umax pmax uu aa aarr ff kk ll
  | umax < 0 = return $ Nothing
  | mmax < 0 = return $ Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = 
      let
        i = nnvvncpbumaxcd uu kk mmax umax
        (lsl,esl,usl) = fromMaybe (0,0,0) $ nnvvnctnsdbumaxpvchminst uu kk mmax umax
        (esu,usu) = fromMaybe (0,0) $ nnvvnctnsdbumaxpvchst uu kk mmax umax
        xx = if gg == [] then ([],(uu,ll)) else (jj,last gg)
        ee = foldl Map.union Map.empty $ fst $ unzip $ fst xx
        ss = foldl Map.union Map.empty $ snd $ unzip $ fst xx
        a = if ee /= Map.empty then (last $ sort $ Map.elems ee) else 0
      in
      do
        printf ">>> zzllcsddecoptw\n"
        printf "initial searched: %d\n" $ i
        printf "minimum searched lower: %d\n" $ (lsl + i) * pmax
        printf "expected searched lower: %.2f\n" $ ((fromRational esl :: Double) + fromIntegral i) * fromIntegral pmax
        printf "maximum searched lower: %d\n" $ (usl + i) * pmax
        printf "expected searched upper: %.2f\n" $ ((fromRational esu :: Double) + fromIntegral i) * fromIntegral pmax
        printf "maximum searched upper: %d\n" $ (usu + i) * pmax
        t1 <- getCurrentTime
        printf "max: %.2f\n" $ a
        t2 <- getCurrentTime
        printf "steps: %d\n" $ length $ fst xx
        printf "elements: %d\n" $ Map.size ee
        printf "searched: %d\n" $ Map.size ss
        printf "<<< zzllcsddecoptw %s\n" $ show $ diffUTCTime t2 t1
        return $ Just xx
  | otherwise = return $ Nothing
  where
    (jj,gg) = unzip $ opt (zzcsddecneigh aa aarr ff) (top pmax) (zzcsddecinit mmax umax uu aa aarr ff kk ll)
    opt pp ii (rr,(uu',ll')) = let mm = ii rr in ((mm,rr),(uu',ll')) : ls mm uu' ll'
      where
        ls mm uu ll = 
          if isempty mm 
          then [] 
          else let (nn,(uu',ll')) = pp mm uu ll; mm' = ii nn in ((mm',nn),(uu',ll')) : ls mm' uu' ll'
    zzcsddecinit =  
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementingLimitedValency_u
    zzcsddecneigh aa aarr ff mm uu ll = 
      systemsSamplesShufflesFudsFunctionsListVariablesFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff mm ll
    nnvvncpbumaxcd uu vv bmax umax =
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinality 
        uu vv bmax umax
    nnvvnctnsdbumaxpvchst uu vv bmax umax = 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchStat 
        uu vv bmax umax
    nnvvnctnsdbumaxpvchminst uu vv bmax umax = 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchMinStat 
        uu vv bmax umax
    fvars = fudsVars
    fund = fudsUnderlying
    vars = histogramsVars
    uvars = systemsVars
    top pmax mm = llmm $ flip $ take (fromInteger pmax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    isempty = Map.null
    keys = Map.keys
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValencyIO_1 :: 
  Integer -> Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] ->
  IO (Maybe ([(Map.Map Fud Double, Map.Map Fud Double)],(System,[Variable])))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValencyIO_1 
  mmax umax pmax uu aa aarr ff kk ll
  | umax < 0 = return $ Nothing
  | mmax < 0 = return $ Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = 
      do
        printf ">>> zzllcsddecoptw\n"
        printf "initial searched: %d\n" $ i
        printf "minimum searched lower: %d\n" $ (lsl + i) * pmax
        printf "expected searched lower: %.2f\n" $ ((fromRational esl :: Double) + fromIntegral i) * fromIntegral pmax
        printf "maximum searched lower: %d\n" $ (usl + i) * pmax
        printf "expected searched upper: %.2f\n" $ ((fromRational esu :: Double) + fromIntegral i) * fromIntegral pmax
        printf "maximum searched upper: %d\n" $ (usu + i) * pmax
        t1 <- getCurrentTime
        printf "max: %.2f\n" $ a
        t2 <- getCurrentTime
        printf "steps: %d\n" $ length $ fst xx
        printf "elements: %d\n" $ Map.size ee
        printf "searched: %d\n" $ Map.size ss
        printf "<<< zzllcsddecoptw %s\n" $ show $ diffUTCTime t2 t1
        return $ Just xx
  | otherwise = return $ Nothing
  where
    i = nnvvncpbumaxcd uu kk mmax umax
    (lsl,esl,usl) = fromMaybe (0,0,0) $ nnvvnctnsdbumaxpvchminst uu kk mmax umax
    (esu,usu) = fromMaybe (0,0) $ nnvvnctnsdbumaxpvchst uu kk mmax umax
    xx = if gg == [] then ([],(uu,ll)) else (jj,last gg)
    ee = foldl Map.union Map.empty $ fst $ unzip $ fst xx
    ss = foldl Map.union Map.empty $ snd $ unzip $ fst xx
    a = if ee /= Map.empty then (last $ sort $ Map.elems ee) else 0
    (jj,gg) = unzip $ opt (zzcsddecneigh aa aarr ff) (top pmax) (zzcsddecinit mmax umax uu aa aarr ff kk ll)
    opt pp ii (rr,(uu',ll')) = let mm = ii rr in ((mm,rr),(uu',ll')) : ls mm uu' ll'
      where
        ls mm uu ll = 
          if isempty mm 
          then [] 
          else let (nn,(uu',ll')) = pp mm uu ll; mm' = ii nn in ((mm',nn),(uu',ll')) : ls mm' uu' ll'
    zzcsddecinit =  
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementingLimitedValency_u
    zzcsddecneigh aa aarr ff mm uu ll = 
      systemsSamplesShufflesFudsFunctionsListVariablesFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff mm ll
    nnvvncpbumaxcd uu vv bmax umax =
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinality 
        uu vv bmax umax
    nnvvnctnsdbumaxpvchst uu vv bmax umax = 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchStat 
        uu vv bmax umax
    nnvvnctnsdbumaxpvchminst uu vv bmax umax = 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchMinStat 
        uu vv bmax umax
    fvars = fudsVars
    fund = fudsUnderlying
    vars = histogramsVars
    uvars = systemsVars
    top pmax mm = llmm $ flip $ take (fromInteger pmax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    isempty = Map.null
    keys = Map.keys
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union
    subset = Set.isSubsetOf

parametersSystemsListTuplesSearchedStats :: 
  Integer -> Integer -> Integer -> System -> [Set.Set Variable] -> (Integer,Double,Integer,Double,Integer)
parametersSystemsListTuplesSearchedStats
  mmax umax pmax uu bb = (sum ss1, sum ss2, sum ss3, sum ss4, sum ss5)
  where 
    (ss1,ss2,ss3,ss4,ss5) = unzip5 [((lsl + i) * pmax,
        ((fromRational esl :: Double) + fromIntegral i) * fromIntegral pmax, 
        (usl + i) * pmax, 
        ((fromRational esu :: Double) + fromIntegral i) * fromIntegral pmax, 
        (usu + i) * pmax) | 
        kk <- bb, 
        let i = nnvvncpbumaxcd uu kk mmax umax, 
        let (lsl,esl,usl) = fromMaybe (0,0,0) $ nnvvnctnsdbumaxpvchminst uu kk mmax umax,
        let (esu,usu) = fromMaybe (0,0) $ nnvvnctnsdbumaxpvchst uu kk mmax umax]
    nnvvncpbumaxcd uu vv bmax umax =
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinality 
        uu vv bmax umax
    nnvvnctnsdbumaxpvchst uu vv bmax umax = 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchStat 
        uu vv bmax umax
    nnvvnctnsdbumaxpvchminst uu vv bmax umax = 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchMinStat 
        uu vv bmax umax

parametersSystemsSamplesShufflesListVariablesSearcherFudHighestIO :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> [Variable] ->
  IO (Maybe ([Fud],(System,[Variable])))
parametersSystemsSamplesShufflesListVariablesSearcherFudHighestIO 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = return $ Nothing
  | vars aa `subset` uvars uu = 
    do
      printf ">>> zzllfudshigh\n"
      t1 <- getCurrentTime
      qq <- ls fudEmpty uu ll lmax 0 1
      let pp = fst $ unzip $ qq
      let (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
      printf "layers: %d\n" $ length $ pp
      t2 <- getCurrentTime
      printf "<<< zzllfudshigh %s\n" $ show $ diffUTCTime t2 t1
      return $ Just $ (pp,(uu',ll'))
  | otherwise = return $ Nothing
  where
    ls :: Fud -> System -> [Variable] -> Integer -> Double -> Integer -> IO [(Fud,(System,[Variable]))]
    ls _ _ _ 0 _ _ = return []
    ls ff uu ll h a l = 
      do 
        printf "layer: %d\n" $ l
        Just bb <- zzffcstupoptIO xmax omax uu aa aarr ff
        let bb' = topff (bmax `div` mmax) ff (fst $ unzip bb)
        let (s1,s2,s3,s4,s5) = parametersSystemsListTuplesSearchedStats mmax umax pmax uu bb'
        printf ">>> zzllcsddecoptw\n"
        printf "tuples: %d\n" $ length bb'
        printf "minimum searched lower: %d\n" $ s1
        printf "expected searched lower: %.2f\n" $ s2
        printf "maximum searched lower: %d\n" $ s3	
        printf "expected searched upper: %.2f\n" $ s4
        printf "maximum searched upper: %d\n" $ s5
        t3 <- getCurrentTime
        let yy = ls2 bb' uu ll
        let gg = ff `funion` llff (elems (llmm [(rr, tt) | hh <- (concat $ fst $ unzip $ fst $ unzip yy), 
                                             w <- qqll (fder hh), let tt = fftt (depends hh w), 
                                             let rr = ttpp tt, rr `notmem` nn]))
        let (uu',ll') = if yy /= [] then (last $ snd $ unzip yy) else (uu,ll)
        printf "searched: %d\n" $ sum $ snd $ unzip $ fst $ unzip yy
        printf "derived vars cardinality: %d\n" $ card $ fder gg
        t4 <- getCurrentTime
        printf "<<< zzllcsddecoptw %s\n" $ show $ diffUTCTime t4 t3
        Just dd <- zzffcsdderhighoptIO wmax omax uu' aa aarr gg
        let b = maxr $ fst $ unzip dd
        if b > a then 
          do 
            mm <- ls gg uu' ll' (h-1) b (l+1)
            return $ (gg,(uu',ll')) : mm 
          else return []
      where
        nn = llqq [ttpp tt | tt <- (qqll . ffqq) ff]
        ls2 [] _ _ = []
        ls2 (kk:bb) uu ll = ((top pmax (fst $ unzip xx), s),(uu',ll')) : ls2 bb uu' ll'
          where
            (xx,(uu',ll')) = zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll
            s = Map.size $ foldl Map.union Map.empty $ snd $ unzip xx
    zzffcstupoptIO xmax omax uu aa aarr ff = 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserTupleIO xmax omax uu aa aarr ff
    zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll = fromJust $
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk ll
    zzffcsdderhighoptIO wmax omax uu aa rr ff = 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighestIO wmax omax uu aa rr ff
    depends ff w = fudsVarsDepends ff (Set.singleton w)
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttpp = transformsPartition
    vars = histogramsVars
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    maxr ll = let xx = (sort $ snd $ unzip $ concat $ map mmll ll) in if xx /= [] then (last xx) else 0
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList
    llqq = Set.fromList
    elems = Map.elems
    llmm = Map.fromList
    mmll = Map.toList
    notmem = Set.notMember
    subset = Set.isSubsetOf
    sgl = Set.singleton
    card = Set.size

parametersSystemsSamplesShufflesListVariablesSearcherFudHighestIO_1 :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> [Variable] ->
  IO (Maybe ([Fud],(System,[Variable])))
parametersSystemsSamplesShufflesListVariablesSearcherFudHighestIO_1 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = return $ Nothing
  | vars aa `subset` uvars uu = 
    do
      printf ">>> zzllfudshigh\n"
      t1 <- getCurrentTime
      qq <- ls fudEmpty uu ll lmax 0
      let pp = fst $ unzip $ qq
      let (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
      printf "steps: %d\n" $ length $ pp
      t2 <- getCurrentTime
      printf "<<< zzllfudshigh %s\n" $ show $ diffUTCTime t2 t1
      return $ Just $ (pp,(uu',ll'))
  | otherwise = return $ Nothing
  where
    ls _ _ _ 0 _ = return []
    ls ff uu ll h a = 
      do 
        if b > a then 
          do 
            mm <- ls gg uu' ll' (h-1) b
            return $ (gg,(uu',ll')) : mm 
          else return []
      where
        yy = ls2 (topff (bmax `div` mmax) ff (zzffcstupopt xmax omax uu aa aarr ff)) uu ll
        gg = ff `funion` llff (elems (llmm [(rr, tt) | hh <- (concat $ fst $ unzip yy), 
                                             w <- qqll (fder hh), let tt = fftt (depends hh w), 
                                             let rr = ttpp tt, rr `notmem` nn]))
        (uu',ll') = if yy /= [] then (last $ snd $ unzip yy) else (uu,ll)
        b = maxr $ zzffcsdderhighopt wmax omax uu' aa aarr gg
        nn = llqq [ttpp tt | tt <- (qqll . ffqq) ff]
        ls2 [] _ _ = []
        ls2 (kk:bb) uu ll = (top pmax (fst $ unzip xx),(uu',ll')) : ls2 bb uu' ll'
          where
            (xx,(uu',ll')) = zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll
    zzffcstupopt xmax omax uu aa aarr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple xmax omax uu aa aarr ff
    zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll = fromJust $
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk ll
    zzffcsdderhighopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ff
    depends ff w = fudsVarsDepends ff (Set.singleton w)
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttpp = transformsPartition
    vars = histogramsVars
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    maxr ll = let xx = (sort $ snd $ unzip $ concat $ map mmll ll) in if xx /= [] then (last xx) else 0
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList
    llqq = Set.fromList
    elems = Map.elems
    llmm = Map.fromList
    mmll = Map.toList
    notmem = Set.notMember
    subset = Set.isSubsetOf
    sgl = Set.singleton

parametersSystemsSamplesShufflesListVariablesInducerFudHighestIO :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
  System -> Histogram -> Histogram -> [Variable] ->
  IO (Maybe ((Fud,Fud),(System,[Variable])))
parametersSystemsSamplesShufflesListVariablesInducerFudHighestIO 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = return $ Nothing
  | vars aa `subset` uvars uu = 
    do 
      printf ">>> iillfudshigh\n"
      t1 <- getCurrentTime
      Just (nn,(uu',ll')) <- zzllfudshighIO wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
      let ff = if nn /= [] then last nn else fudEmpty
      Just dd <- zzffcsdderhighoptIO wmax omax uu' aa aarr ff
      let kk = maxdff ff $ fst $ unzip dd
      t2 <- getCurrentTime
      printf "<<< iillfudshigh %s\n" $ show $ diffUTCTime t2 t1
      return $ Just $ ((depends ff kk, ff),(uu',ll'))
  | otherwise = return $ Nothing
  where
    zzllfudshighIO wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll =
      parametersSystemsSamplesShufflesListVariablesSearcherFudHighestIO 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    zzffcsdderhighoptIO wmax omax uu aa rr ff =
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighestIO wmax omax uu aa rr ff
    depends = fudsVarsDepends
    vars = histogramsVars
    uvars = systemsVars
    maxdff ff ll = let xx = (sortdescff ff $ concat $ map mmll ll) in 
              if xx /= [] then (snd $ head $ xx) else Set.empty
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    mmll = Map.toList
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf

parametersSystemsSamplesListVariablesInducerDecompFudHighestIO :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> [Variable] ->
  IO (Maybe (DecompFud,(System,[Variable])))
parametersSystemsSamplesListVariablesInducerDecompFudHighestIO 
  wmax lmax xmax omax bmax mmax umax pmax mult seed uu aa ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = return $ Nothing
  | not (isint aa) || mult < 1 = return $ Nothing
  | vars aa `subset` uvars uu = 
    do
      printf ">>> iilldfshigh\n"
      t1 <- getCurrentTime
      xx <- dec emptyTree uu ll seed
      printf "nodes: %d\n" $ card $ treesNodes $ dfzz $ fst xx
      t2 <- getCurrentTime
      printf "<<< iilldfshigh %s\n" $ show $ diffUTCTime t2 t1
      return $ Just $ xx
  | otherwise = return $ Nothing
  where
    dec zz uu ll s
      | zz == emptyTree =
        do
          let aarr = ashuffle aa s mult
          Just ((ffr,_),(uur,llr)) <- iillfudshighIO wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
          if ffr == fudEmpty then
              return $ (decompFudEmpty, (uu, ll))
            else do
              let zzr = tsgl (stateEmpty,ffr)
              dec zzr uur llr (s + mult)
      | otherwise = 
        do
          let mm = [(size bb,nn,ss,bb,bbrr) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 let bbrr = ashuffle bb s mult,
                 size bb > 0]
          if mm == [] then
              return $ (zzdf (zztrim zz), (uu, ll))
            else do
              let (_,nn,ss,bb,bbrr) = last $ sort mm
              Just ((ffc,_),(uuc,llc)) <- iillfudshighIO wmax lmax xmax omax bmax mmax umax pmax uu bb bbrr ll
              let zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
              dec zzc uuc llc (s + mult)
    iillfudshighIO wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll = 
      parametersSystemsSamplesShufflesListVariablesInducerFudHighestIO 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    dfzz = decompFudsTreePairStateFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    fhis = fudsSetHistogram
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    aahh aa = fromJust $ histogramsHistory aa
    hhaa hh = historiesHistogram hh
    hshuffle hh r = fromJust $ historiesShuffle hh (fromInteger r)
    ashuffle aa seed mult = let hh = aahh aa in 
                            resize (size aa) $ foldl1 aadd [hhaa $ hshuffle hh (seed + r) | r <- [0..mult-1]]
    isint = histogramsIsIntegral
    aadd xx yy = fromJust $ pairHistogramsAdd xx yy
    resize z aa = fromJust $ histogramsResize z aa
    unit = fromJust . setStatesHistogramUnit . Set.singleton 
    size = histogramsSize
    vars = histogramsVars
    cart = systemsSetVarsSetStateCartesian_u
    uvars = systemsVars
    tsgl r = Tree $ Map.singleton r emptyTree
    bigcup = setSetsUnion
    dom = relationsDomain
    minus = Set.difference
    add qq x = Set.insert x qq
    qqll = Set.toList
    union = Set.union
    subset = Set.isSubsetOf
    card = Set.size

parametersSystemsSamplesListVariablesInducerDecompFudHighestIO_1 :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> [Variable] ->
  IO (Maybe (DecompFud,(System,[Variable])))
parametersSystemsSamplesListVariablesInducerDecompFudHighestIO_1 
  wmax lmax xmax omax bmax mmax umax pmax mult seed uu aa ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = return $ Nothing
  | not (isint aa) || mult < 1 = return $ Nothing
  | vars aa `subset` uvars uu = 
    do
      printf ">>> iilldfshigh\n"
      t1 <- getCurrentTime
      let xx = dec emptyTree uu ll seed
      printf "nodes: %d\n" $ card $ treesNodes $ dfzz $ fst xx
      t2 <- getCurrentTime
      printf "<<< iilldfshigh %s\n" $ show $ diffUTCTime t2 t1
      return $ Just $ xx
  | otherwise = return $ Nothing
  where
    dec zz uu ll s
      | zz == emptyTree && ffr == fudEmpty = (decompFudEmpty, (uu, ll))
      | zz == emptyTree = dec zzr uur llr (s + mult)
      | mm == [] = (zzdf (zztrim zz), (uu, ll)) 
      | otherwise = dec zzc uuc llc (s + mult)
      where
        aarr = ashuffle aa s mult
        ((ffr,_),(uur,llr)) = iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb,bbrr) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 let bbrr = ashuffle bb s mult,
                 size bb > 0]
        (_,nn,ss,bb,bbrr) = last $ sort mm
        ((ffc,_),(uuc,llc)) = iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu bb bbrr ll
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll = fromJust $
      parametersSystemsSamplesShufflesListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    dfzz = decompFudsTreePairStateFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    fhis = fudsSetHistogram
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    aahh aa = fromJust $ histogramsHistory aa
    hhaa hh = historiesHistogram hh
    hshuffle hh r = fromJust $ historiesShuffle hh (fromInteger r)
    ashuffle aa seed mult = let hh = aahh aa in 
                            resize (size aa) $ foldl1 aadd [hhaa $ hshuffle hh (seed + r) | r <- [0..mult-1]]
    isint = histogramsIsIntegral
    aadd xx yy = fromJust $ pairHistogramsAdd xx yy
    resize z aa = fromJust $ histogramsResize z aa
    unit = fromJust . setStatesHistogramUnit . Set.singleton 
    size = histogramsSize
    vars = histogramsVars
    cart = systemsSetVarsSetStateCartesian_u
    uvars = systemsVars
    tsgl r = Tree $ Map.singleton r emptyTree
    bigcup = setSetsUnion
    dom = relationsDomain
    minus = Set.difference
    add qq x = Set.insert x qq
    qqll = Set.toList
    union = Set.union
    subset = Set.isSubsetOf
    card = Set.size

