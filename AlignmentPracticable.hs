{-# LANGUAGE RankNTypes #-}

module AlignmentPracticable (
  systemsHistogramsHistogramsFudsAlignmentCsd_u,
  parametersSystemsSamplesShufflesFunctionsFunctionNeighbourhoodPartitionLimitedPathModels,
  parametersSystemsSamplesShufflesFudsSetVarsFunctionSearchTuplePartition,
  parametersSystemsSamplesShufflesFudsSubstratePartitionsFunctionSearchNonOverlapTransform,
  parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing,
  parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u,
  parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_1, 
  parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementingLimitedValency,
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u,
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u1,
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u2,
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementingLimitedValency_u,
  parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing,
  parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u,
  systemsSamplesShufflesFudsFunctionsListVariablesFunctionNeighbourhoodFudDecrementing_u,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_1,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_2,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_3,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_4,    
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementing,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingMaximumRoll,
  parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency,  
  parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency,
  parametersSystemsSamplesShufflesFudsFunctionInitialTuple,
  parametersSystemsSamplesShufflesFudsFunctionInitialTuple_1,
  parametersSystemsSamplesShufflesCommonFudsFudsFunctionInitialTuple,
  parametersSystemsSamplesShufflesLevelsFudsFunctionInitialTuple,
  parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple,
  parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple_1,
  parametersSystemsSamplesShufflesCommonFudsFudsFunctionNeighbourhoodTuple,
  parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodTuple,
  parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple,
  parametersSystemsSamplesShufflesCommonFudsFudsFunctionOptimiserTuple,
  parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserTuple,
  parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox,
  parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox_1,
  parametersSetVariablesFudsFunctionOptimiserTupleCardinalityUpper,
  parametersSetVariablesCommonFudsFudsFunctionOptimiserTupleCardinalityUpper,
  parametersSystemsSamplesShufflesSearcherFud,
  parametersSystemsSamplesShufflesSearcherFudHighest,
  parametersSystemsSamplesShufflesSearcherFudExcludedSelf,
  parametersSystemsSamplesShufflesCommonFudsSearcherFudHighest,
  parametersSystemsSamplesShufflesListVariablesSearcherFud,
  parametersSystemsSamplesShufflesListVariablesSearcherFudHighest,
  parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest,
  parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest_d,
  parametersSystemsSamplesShufflesLevelsListVariablesSearcherFudHighest,
  parametersSystemsSamplesShufflesFudsFunctionInitialDerived,
  parametersSystemsSamplesShufflesFudsFunctionInitialDerived_1,
  parametersSystemsSamplesShufflesFudsFunctionInitialDerivedHighest,
  parametersSystemsSamplesShufflesLevelsFudsFunctionInitialDerivedHighest,
  parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived,   
  parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived_1,   
  parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodDerived,
  parametersSystemsSamplesShufflesFudsFunctionOptimiserDerived,
  parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest,
  parametersSetVariablesFudsFunctionOptimiserDerivedCardinalityUpper,
  parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityApprox,
  parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityUpper,
  parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserDerivedHighest,
  parametersSystemsSamplesShufflesListVariablesInducerFud,
  parametersSystemsSamplesShufflesListVariablesInducerFudHighest,
  parametersSystemsSamplesShufflesCommonFudsListVariablesInducerFudHighest,
  parametersSystemsSamplesShufflesLevelsListVariablesInducerFudHighest,
  parametersSystemsSamplesListVariablesInducerDecompFudHighest,
  parametersSystemsSamplesListVariablesInducerDecompFudHighestNoShuffle,
  parametersSystemsSamplesListVariablesInducerDecompFudHighestAccumulating,
  parametersSystemsSamplesListVariablesInducerDecompFudHighestAccumulating_1,
  parametersSystemsSamplesLevelsListVariablesInducerDecompFudHighest_u,
  parametersSystemsSamplesLevelsListVariablesInducerDecompFudHighestGoodness_u,
  parametersSystemsBuilderTuple,
  parametersSystemsBuilderTuple_1,
  parametersSystemsBuilderTupleNoSumlayer,
  parametersSystemsBuilderTupleNoSumlayerMultiEffective,
  parametersSystemsBuilderTupleLevel,
  parametersSystemsBuilderTupleLevelNoSumlayer,
  parametersSystemsBuilderTupleLevelNoSumlayerMultiEffective,
  parametersSystemsBuilderDerivedVarsHighest,
  parametersSystemsBuilderDerivedVarsHighestNoSumlayerIncludeHidden,
  parametersSystemsBuilderDerivedVarsLevelHighestNoSumlayerIncludeHidden,
  parametersSystemsPartitionerBinary,
  parametersSystemsPartitioner,
  parametersSystemsPartitioner_1,
  rollValuer,
  systemsRollValuer_1,
  rollValueAlignmenter_u,
  systemsRollValueAlignmenter_1,
  parametersRoller,
  parametersSystemsLayererHighest,
  parametersSystemsLayererHighest_1,
  parametersSystemsLayererMaximumRollHighest,
  parametersSystemsLayererMaximumRollExcludedSelfHighest,
  parametersSystemsLayererMaximumRollExcludedSelfHighest_1,
  parametersSystemsLayererLevelMaximumRollExcludedSelfHighest,
  parametersSystemsLayererLevelMaximumRollExcludedSelfHighest_1,
  parametersSystemsDecomperHighest,
  parametersSystemsDecomperMaximumRollExcludedSelfHighest,
  parametersSystemsDecomperLevelMaximumRollExcludedSelfHighest,
  parametersSystemsDecomperMaximumRollExcludedSelfHighestGoodness,
  systemsDecompFudsNullablePracticable,
  systemsDecompFudsNullableLeafPracticable
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import AlignmentUtil
import Alignment
import AlignmentRandom
import AlignmentSubstrate
import AlignmentApprox
import GHC.Real

-- AYOR
systemsHistogramsHistogramsFudsAlignmentCsd_u :: System -> Histogram -> Histogram -> Fud -> Double
systemsHistogramsHistogramsFudsAlignmentCsd_u uu aa aarr ff = 
    (algn (aa `fmul` ff) - algn (aarr `fmul` ff)) / (w ** (1/m))
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

parametersSystemsSamplesShufflesFunctionsFunctionNeighbourhoodPartitionLimitedPathModels :: 
  Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> Map.Map Fud Double -> Maybe (Map.Map Fud Double)
parametersSystemsSamplesShufflesFunctionsFunctionNeighbourhoodPartitionLimitedPathModels 
  xmax bmax wmax lmax uu aa aarr qq
  | vars aa `subset` uvars uu && and [fund ff `subset` vars aa | ff <- keys qq] = Just $ llmm 
    [(hh, (algn (aa `fmul` hh) - algn (aarr `fmul` hh)) / (w' ** (1/m))) | 
        ff <- keys qq, layer ff < lmax, let uu2 = (uu `uunion` fsys ff),
        gg <- llfudsll bmax [qqtt pp | kk <- tuplesll (vars aa) ff, vol uu2 kk <= xmax, 
                                       pp <- partsll (cart uu2 kk), dim pp >= 2], 
        gg /= fudEmpty, let hh = ff `funion` gg, 
        let ww = fder hh, let w = vol (uu `uunion` fsys hh) ww, w <= wmax, 
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    tuplesll vv ff = Set.toList $ setVariablesFudsSetTuple vv ff
    llfudsll bmax ll = Set.toList $ Set.map qqff $ setsPowersetLimited (Set.fromList ll) bmax
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    layer ff = fudsSetVarsLayer ff (fder ff)
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    qqtt = pptt . qqpp
    pptt = partitionsTransformVarPartition 
    qqpp qq = fromJust $ setComponentsPartition qq
    ppcd = toInteger . Set.size . partitionsSetComponent
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    partsll = Set.toList . setsPartitionSet
    keys = Map.keys
    llmm = Map.fromList
    subset = Set.isSubsetOf
    dim = toInteger . Set.size

parametersSystemsSamplesShufflesFudsSetVarsFunctionSearchTuplePartition :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable ->  
  Maybe (Map.Map (Set.Set (Set.Set Variable)) Double)
parametersSystemsSamplesShufflesFudsSetVarsFunctionSearchTuplePartition mmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ llmm 
      [(zz, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
        zz <- stirsll kk mmax, dim zz >= 2,
        let gg = depends ff kk `funion` llff [pptt (self uu jj) | jj <- qqll zz],
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    llff = fromJust . setTransformsFud . Set.fromList
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    pptt = partitionsTransformVarPartition 
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsSubstratePartitionsFunctionSearchNonOverlapTransform :: 
  System -> Histogram -> Histogram -> Fud -> Set.Set (Set.Set Variable) -> 
  Maybe (Map.Map Transform Double)
parametersSystemsSamplesShufflesFudsSubstratePartitionsFunctionSearchNonOverlapTransform uu aa aarr ff yy
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && (bigcup yy) `subset` (vars aa `cup` fvars ff) = Just $ llmm 
      [(nntt nn, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) | 
        mm <- qqll (prod [partspl (cart uu jj) | jj <- qqll yy]),
        let nn = llqq [qqpp qq | qq <- mm],
        let gg = depends ff (bigcup yy) `funion` ttff (nntt nn),
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,  
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    ttff = fromJust . setTransformsFud . Set.singleton
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    qqpp qq = fromJust $ setComponentsPartition qq
    llff = fromJust . setTransformsFud . Set.fromList
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    prod = listSetsProduct
    partspl xx = Set.singleton xx `Set.delete` setsPartitionSet xx
    bigcup = setSetsUnion
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> Maybe (Map.Map Fud Double)
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing mmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ llmm 
      [(ttff (nntt nn), (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
        zz <- stirsll kk mmax, dim zz >= 2,
        let nn = llqq [self uu jj | jj <- qqll zz],
        let gg = depends ff kk `funion` ttff (nntt nn),
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = fromJust . setTransformsFud . Set.singleton
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

-- AYOR
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> Map.Map Fud Double
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u mmax uu aa aarr ff kk = llmm 
    [(ii, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
        zz <- stirsll kk mmax, dim zz >= 2,
        let nn = llqq [self uu jj | jj <- qqll zz],
        let ii = ttff (nntt nn),
        let gg = depends ff kk `funion` ii,
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = fromJust . setTransformsFud . Set.singleton
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

-- AYOR
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> Map.Map Fud Double
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u1 mmax uu aa aarr ff kk = llmm 
    [(ttff (nntt nn), (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
        zz <- stirsll kk mmax, dim zz >= 2,
        let nn = llqq [self uu jj | jj <- qqll zz],
        let gg = depends ff kk `funion` ttff (nntt nn),
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = fromJust . setTransformsFud . Set.singleton
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> Maybe (Map.Map Fud Double)
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_1 mmax uu aa aarr ff kk
  | mm' == Nothing = Nothing
  | otherwise = Just $ llmm [(ttff (llnntt [self uu jj | jj <- qqll yy]),a) | (yy,a) <- mmll mm]
  where
    mm' = parametersSystemsSamplesShufflesFudsSetVarsFunctionSearchTuplePartition mmax uu aa aarr ff kk
    mm = fromJust $ mm'
    ttff = fromJust . setTransformsFud . Set.singleton
    llnntt ll = fromJust $ setPartitionsTransformVarPartition $ Set.fromList ll
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList

-- AYOR
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] -> 
  (Map.Map Fud Double,(System,[Variable]))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u mmax uu aa aarr ff kk ll = 
  (mm,(uu',ll'))
  where
    xx = [((i, pptt pp x), (x, ppcd pp)) | 
           ((i,pp),x) <- zip [(i, self uu jj) | (zz,i) <- zip (stirsll kk mmax) [1..], dim zz >= 2, jj <- qqll zz] ll]
    uu' = uu `uunion` (lluu $ map (\(x,c) -> (x,llqq [ValInt i | i <- [1..c]])) $ snd $ unzip xx)
    ll' = drop (length xx) ll
    mm = llmm [(ii, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
                 ii <- (elems $ llmmw funion $ map (\(i,tt) -> (i,ttff tt)) $ fst $ unzip xx), 
                 let gg = depends ff kk `funion` ii,
                 let ww = fder gg, let w = vol uu' ww,
                 let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = setTransformsFud_u . Set.singleton
    qqff = setTransformsFud_u
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    vol = systemsSetVarsVolume_u
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    ppcd = toInteger . Set.size . partitionsSetComponent
    self = systemsSetVarsPartitionSelf_u
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList
    llmmw = Map.fromListWith
    lluu = listsSystem_u
    pptt ::  Partition -> Variable -> Transform
    pptt pp x = trans (unit [ss `sunion` single x (ValInt i) | (cc,i) <- zip (ppqq pp) [1..], ss <- qqll cc]) x
    trans xx w = histogramsSetVarsTransform_u xx (Set.singleton w)
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    ppqq = Set.toList . partitionsSetComponent
    sunion = pairStatesUnionLeft
    single = stateSingleton
    elems = Map.elems

-- AYOR
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] -> 
  (Map.Map Fud Double,(System,[Variable]))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u1 mmax uu aa aarr ff kk ll = 
  (mm,(uu',ll'))
  where
    xx = [((zz, pptt pp x), (x, ppcd pp)) | 
           ((zz,pp),x) <- zip [(zz, self uu jj) | zz <- stirsll kk mmax, dim zz >= 2, jj <- qqll zz] ll]
    uu' = uu `uunion` (lluu $ map (\(x,c) -> (x,llqq [ValInt i | i <- [1..c]])) $ snd $ unzip xx)
    ll' = drop (length xx) ll
    mm = llmm [(ii, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
                 ii <- (elems $ llmmw funion $ map (\(zz,tt) -> (zz,ttff tt)) $ fst $ unzip xx), 
                 let gg = depends ff kk `funion` ii,
                 let ww = fder gg, let w = vol uu' ww,
                 let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = fromJust . setTransformsFud . Set.singleton
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    ppcd = toInteger . Set.size . partitionsSetComponent
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList
    llmmw = Map.fromListWith
    lluu ll = fromJust $ systemFromList ll
    pptt ::  Partition -> Variable -> Transform
    pptt pp x = trans (unit [ss `sunion` single x (ValInt i) | (cc,i) <- zip (ppqq pp) [1..], ss <- qqll cc]) x
    trans xx w = fromJust $ histogramsSetVarsTransform xx (Set.singleton w)
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    ppqq = Set.toList . partitionsSetComponent
    sunion = pairStatesUnionLeft
    single = stateSingleton
    elems = Map.elems

-- AYOR
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u2 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] -> 
  (Map.Map Fud Double,(System,[Variable]))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u2 mmax uu aa aarr ff kk ll = 
  (mm,(uu',ll'))
  where
    xx = [((zz, pptt pp x), (x, ppcd pp)) | 
           ((zz,pp),x) <- zip [(zz, self uu jj) | zz <- stirsll kk mmax, dim zz >= 2, jj <- qqll zz] ll]
    uu' = uu `uunion` (lluu $ map (\(x,c) -> (x,llqq [ValInt i | i <- [1..c]])) $ snd $ unzip xx)
    ll' = drop (length xx) ll
    mm = llmm [(ii, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
                 ii <- (elems $ llmmw funion $ map (\(zz,tt) -> (zz,ttff tt)) $ fst $ unzip xx), 
                 let gg = depends ff kk `funion` ii,
                 let ww = fder gg, let w = vol uu' ww,
                 let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = setTransformsFud_u . Set.singleton
    qqff = setTransformsFud_u
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    vol = systemsSetVarsVolume_u
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    ppcd = toInteger . Set.size . partitionsSetComponent
    self = systemsSetVarsPartitionSelf_u
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList
    llmmw = Map.fromListWith
    lluu = listsSystem_u
    pptt ::  Partition -> Variable -> Transform
    pptt pp x = trans (unit [ss `sunion` single x (ValInt i) | (cc,i) <- zip (ppqq pp) [1..], ss <- qqll cc]) x
    trans xx w = histogramsSetVarsTransform_u xx (Set.singleton w)
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    ppqq = Set.toList . partitionsSetComponent
    sunion = pairStatesUnionLeft
    single = stateSingleton
    elems = Map.elems

parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing :: 
  System -> Histogram -> Histogram -> Fud -> Map.Map Fud Double -> Maybe (Map.Map Fud Double)
parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing uu aa aarr ff qq
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    and [fund hh `subset` (vars aa `cup` fvars ff) | hh <- keys qq] = Just $ llmm 
      [(hh `funion` ttff (nntt nn), (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) | 
        hh <- keys qq, let uu' = (uu `uunion` fsys hh),
        x <- qqll (fder hh), ucard uu' x > 2, qq <- decsll (self uu' x),
        let nn = llqq ([qq] ++ [self uu' u | u <- qqll (fder hh), u /= x]),
        let gg = depends ff (fund hh) `funion` hh `funion` ttff (nntt nn),
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,  
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  | otherwise = Nothing
  where  
    algn = histogramsAlignment
    depends = fudsVarsDepends
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    ttff = fromJust . setTransformsFud . Set.singleton
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ ppqq pp
    self uu v = fromJust $ systemsSetVarsPartitionSelf uu (Set.singleton v)
    ppqq = partitionsSetComponent
    qqpp = setComponentsPartition_u
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    keys = Map.keys
    llmm = Map.fromList

-- AYOR
parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u :: 
  System -> Histogram -> Histogram -> Fud -> Map.Map Fud Double -> Map.Map Fud Double
parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff qq = llmm 
    [(ii, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) | 
        hh <- keys qq, let uu' = (uu `uunion` fsys hh),
        x <- qqll (fder hh), ucard uu' x > 2, qq <- decsll (self uu' x),
        let nn = llqq ([qq] ++ [self uu' u | u <- qqll (fder hh), u /= x]),
        let ii = hh `funion` ttff (nntt nn),
        let gg = depends ff (fund hh) `funion` ii,
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,  
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  where  
    algn = histogramsAlignment
    depends = fudsVarsDepends
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    ttff = fromJust . setTransformsFud . Set.singleton
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ ppqq pp
    self uu v = fromJust $ systemsSetVarsPartitionSelf uu (Set.singleton v)
    ppqq = partitionsSetComponent
    qqpp = setComponentsPartition_u
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    qqll = Set.toList
    llqq = Set.fromList
    keys = Map.keys
    llmm = Map.fromList

-- AYOR
parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u1 :: 
  System -> Histogram -> Histogram -> Fud -> Map.Map Fud Double -> Map.Map Fud Double
parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u1 uu aa aarr ff qq = llmm 
    [(hh `funion` ttff (nntt nn), (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) | 
        hh <- keys qq, let uu' = (uu `uunion` fsys hh),
        x <- qqll (fder hh), ucard uu' x > 2, qq <- decsll (self uu' x),
        let nn = llqq ([qq] ++ [self uu' u | u <- qqll (fder hh), u /= x]),
        let gg = depends ff (fund hh) `funion` hh `funion` ttff (nntt nn),
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,  
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  where  
    algn = histogramsAlignment
    depends = fudsVarsDepends
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    ttff = fromJust . setTransformsFud . Set.singleton
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ ppqq pp
    self uu v = fromJust $ systemsSetVarsPartitionSelf uu (Set.singleton v)
    ppqq = partitionsSetComponent
    qqpp = setComponentsPartition_u
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    qqll = Set.toList
    llqq = Set.fromList
    keys = Map.keys
    llmm = Map.fromList

-- AYOR
systemsSamplesShufflesFudsFunctionsListVariablesFunctionNeighbourhoodFudDecrementing_u :: 
  System -> Histogram -> Histogram -> Fud -> Map.Map Fud Double -> [Variable] ->
  (Map.Map Fud Double,(System,[Variable]))
systemsSamplesShufflesFudsFunctionsListVariablesFunctionNeighbourhoodFudDecrementing_u 
  uu aa aarr ff mm ll = (mm',(uu',ll'))
  where
    xx = [(((hh,y,qq), pptt pp x), (x, ppcd pp)) | 
           (((hh,y,qq), pp),x) <- zip [((hh,y,qq), pp) | 
             hh <- keys mm, let ww = fder hh, y <- qqll ww, ucard uu y > 2, qq <- decsll (self uu y), 
             pp <- ([qq] ++ [self uu u | u <- qqll ww, u /= y])] ll]
    uu' = uu `uunion` (lluu $ map (\(x,c) -> (x,llqq [ValInt i | i <- [1..c]])) $ snd $ unzip xx)
    ll' = drop (length xx) ll
    mm' = llmm [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
                ((hh,_,_),ii) <- (mmll $ llmmw funion $ map (\(zz,tt) -> (zz,ttff tt)) $ fst $ unzip xx), 
                let jj = hh `funion` ii,
                let gg = depends ff (fund hh) `funion` jj,
                let ww = fder gg, let w = vol uu' ww,
                let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = setTransformsFud_u . Set.singleton
    qqff = setTransformsFud_u
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    trans xx w = histogramsSetVarsTransform_u xx (Set.singleton w)
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    pptt ::  Partition -> Variable -> Transform
    pptt pp x = trans (unit [ss `sunion` single x (ValInt i) | (cc,i) <- zip (qqll (ppqq pp)) [1..], ss <- qqll cc]) x
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ ppqq pp
    ppcd = toInteger . Set.size . partitionsSetComponent
    qqpp = setComponentsPartition_u
    ppqq = partitionsSetComponent
    self uu v = systemsSetVarsPartitionSelf_u uu (Set.singleton v)
    lluu = listsSystem_u
    sunion = pairStatesUnionLeft
    single = stateSingleton
    vol = systemsSetVarsVolume_u
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList
    mmll = Map.toList
    llmmw = Map.fromListWith
    elems = Map.elems
    keys = Map.keys

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe [(Map.Map Fud Double, Map.Map Fud Double)]
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing mmax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      opt (zzcsddecneigh uu aa aarr ff) (top pmax) (zzcsddecinit mmax uu aa aarr ff kk)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcsddecinit mmax uu aa aarr ff kk =  
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u mmax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = 
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff mm
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

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_1 :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe [(Map.Map Fud Double, Map.Map Fud Double)]
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_1 mmax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      opt (zzcsddecneigh uu aa aarr ff) (top pmax) (zzcsddecinit mmax uu aa aarr ff kk)
  | otherwise = Nothing
  where
    opt pp ii rr = (ii rr, rr) : ls (ii rr)
      where
        ls yy = if isempty yy then [] else (ii (pp yy), pp yy) : ls (ii (pp yy))
    zzcsddecinit mmax uu aa aarr ff kk = fromJust $ 
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing mmax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing uu aa aarr ff mm
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

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_2 :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe [(Map.Map Fud Double, Map.Map Fud Double)]
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_2 mmax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ (mmit, mmi) : ls mmit
  | otherwise = Nothing
  where
    mmi = zzcsddecinit mmax uu aa aarr ff kk
    mmit = top pmax mmi
    ls mm = if isempty mm then [] else (mmnt,mmn) : ls mmnt
      where
        mmn = zzcsddecneigh uu aa aarr ff mm
        mmnt = top pmax mmn
    zzcsddecinit mmax uu aa aarr ff kk = fromJust $ 
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing mmax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing uu aa aarr ff mm
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

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_3 :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe [(Map.Map Fud Double, Map.Map Fud Double)]
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_3 mmax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ (mmit, mmi) : ls mmit
  | otherwise = Nothing
  where
    mmi = zzcsddecinit mmax uu aa aarr ff kk
    mmit = top pmax mmi
    ls mm = if isempty mm then [] else (mmnt,mmn) : ls mmnt
      where
        mmn = zzcsddecneigh uu aa aarr ff mm
        mmnt = top pmax mmn
    zzcsddecinit mmax uu aa aarr ff kk =  
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u mmax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = 
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff mm
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

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_4 :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe [(Map.Map Fud Double, Map.Map Fud Double)]
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementing_4 mmax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      opt (zzcsddecneigh uu aa aarr ff) (top pmax) (zzcsddecinit mmax uu aa aarr ff kk)
  | otherwise = Nothing
  where
    opt pp ii rr = (ii rr, rr) : ls (ii rr)
      where
        ls yy = if isempty yy then [] else (ii (pp yy), pp yy) : ls (ii (pp yy))
    zzcsddecinit mmax uu aa aarr ff kk = fromJust $ 
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_1 mmax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing uu aa aarr ff mm
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

parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementing :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] ->
  Maybe ([(Map.Map Fud Double, Map.Map Fud Double)],(System,[Variable]))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementing 
  mmax pmax uu aa aarr ff kk ll
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      if gg == [] then ([],(uu,ll)) else (jj,last gg)
  | otherwise = Nothing
  where
    (jj,gg) = unzip $ opt (zzcsddecneigh aa aarr ff) (top pmax) (zzcsddecinit mmax uu aa aarr ff kk ll)
    opt pp ii (rr,(uu',ll')) = let mm = ii rr in ((mm,rr),(uu',ll')) : ls mm uu' ll'
      where
        ls mm uu ll = 
          if isempty mm 
          then [] 
          else let (nn,(uu',ll')) = pp mm uu ll; mm' = ii nn in ((mm',nn),(uu',ll')) : ls mm' uu' ll'
    zzcsddecinit =  
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementing_u
    zzcsddecneigh aa aarr ff mm uu ll = 
      systemsSamplesShufflesFudsFunctionsListVariablesFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff mm ll
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

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingMaximumRoll :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe (Tree (Fud,Double))
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingMaximumRoll mmax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      opt (zzcsddecneigh uu aa aarr ff) (top pmax) (zzcsddecinit mmax uu aa aarr ff kk)
  | otherwise = Nothing
  where
    opt pp ii rr = Tree $ llmm [((gg,a), ts (sgl gg a)) | (gg,a) <- mmll (ii rr)]
      where
        ts yy = Tree $ llmm [((gg,a), ts (sgl gg a)) | (gg,a) <- mmll (top 1 (pp yy))]
    zzcsddecinit mmax uu aa aarr ff kk =  
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementing_u mmax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = 
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing_u uu aa aarr ff mm
    fvars = fudsVars
    fund = fudsUnderlying
    vars = histogramsVars
    uvars = systemsVars
    top pmax mm = llmm $ flip $ take (fromInteger pmax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    sgl = Map.singleton
    isempty = Map.null
    keys = Map.keys
    llmm :: forall k a. (Ord k) => [(k, a)] -> Map.Map k a
    llmm = Map.fromList
    mmll :: forall k a. Map.Map k a -> [(k, a)]
    mmll = Map.toList
    cup = Set.union
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementingLimitedValency :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> Maybe (Map.Map Fud Double)
parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementingLimitedValency mmax umax uu aa aarr ff kk
  | umax < 0 = Nothing
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ llmm 
      [(ttff (nntt nn), (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
        zz <- stirsll kk mmax, dim zz >= 2, and [vol uu jj <= umax | jj <- qqll zz],
        let nn = llqq [self uu jj | jj <- qqll zz],
        let gg = depends ff kk `funion` ttff (nntt nn),
        let ww = fder gg, let w = vol (uu `uunion` fsys gg) ww,
        let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = fromJust . setTransformsFud . Set.singleton
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

-- AYOR
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementingLimitedValency_u :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] -> 
  (Map.Map Fud Double,(System,[Variable]))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionInitialFudDecrementingLimitedValency_u 
   mmax umax uu aa aarr ff kk ll = (mm,(uu',ll'))
  where
    xx = [((i, pptt pp x), (x, ppcd pp)) | 
           ((i,pp),x) <- zip [(i, self uu jj) | (zz,i) <- zip (stirsll kk mmax) [1..], dim zz >= 2, 
                                                and [vol uu ii <= umax | ii <- qqll zz], jj <- qqll zz] ll]
    uu' = uu `uunion` (lluu $ map (\(x,c) -> (x,llqq [ValInt i | i <- [1..c]])) $ snd $ unzip xx)
    ll' = drop (length xx) ll
    mm = llmm [(ii, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (w' ** (1/m))) |
                 ii <- (elems $ llmmw funion $ map (\(i,tt) -> (i,ttff tt)) $ fst $ unzip xx), 
                 let gg = depends ff kk `funion` ii,
                 let ww = fder gg, let w = vol uu' ww,
                 let w' = fromIntegral w, let m = fromIntegral (Set.size ww)]
    algn = histogramsAlignment
    depends = fudsVarsDepends
    ttff = setTransformsFud_u . Set.singleton
    qqff = setTransformsFud_u
    ffqq = fudsSetTransform
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fsys = fudsSystemImplied
    vol = systemsSetVarsVolume_u
    uunion = pairSystemsUnion
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    ppcd = toInteger . Set.size . partitionsSetComponent
    self = systemsSetVarsPartitionSelf_u
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList
    llmmw = Map.fromListWith
    lluu = listsSystem_u
    pptt ::  Partition -> Variable -> Transform
    pptt pp x = trans (unit [ss `sunion` single x (ValInt i) | (cc,i) <- zip (ppqq pp) [1..], ss <- qqll cc]) x
    trans xx w = histogramsSetVarsTransform_u xx (Set.singleton w)
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    ppqq = Set.toList . partitionsSetComponent
    sunion = pairStatesUnionLeft
    single = stateSingleton
    elems = Map.elems

parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency :: 
  Integer -> Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> 
  Maybe [(Map.Map Fud Double, Map.Map Fud Double)]
parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency 
  mmax umax pmax uu aa aarr ff kk
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      opt (zzcsddecneigh uu aa aarr ff) (top pmax) (zzcsddecinit mmax umax uu aa aarr ff kk)
  | otherwise = Nothing
  where
    opt pp ii rr = (ii rr, rr) : ls (ii rr)
      where
        ls yy = if isempty yy then [] else (ii (pp yy), pp yy) : ls (ii (pp yy))
    zzcsddecinit mmax umax uu aa aarr ff kk = fromJust $ 
      parametersSystemsSamplesShufflesFudsTuplesFunctionInitialFudDecrementingLimitedValency mmax umax uu aa aarr ff kk
    zzcsddecneigh uu aa aarr ff mm = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionsFunctionNeighbourhoodFudDecrementing uu aa aarr ff mm
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

parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency :: 
  Integer -> Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Set.Set Variable -> [Variable] ->
  Maybe ([(Map.Map Fud Double, Map.Map Fud Double)],(System,[Variable]))
parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency 
  mmax umax pmax uu aa aarr ff kk ll
  | umax < 0 = Nothing
  | mmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu && vars aarr == vars aa &&
    fund ff `subset` vars aa && kk `subset` (vars aa `cup` fvars ff) = Just $ 
      if gg == [] then ([],(uu,ll)) else (jj,last gg)
  | otherwise = Nothing
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

parametersSystemsSamplesShufflesFudsFunctionInitialTuple :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionInitialTuple xmax uu aa aarr ff
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = Just $ llmm 
      [(kk, algn (apply vv kk xx aa) - algn (apply vv kk xx aarr)) |
        w <- qqll (if ff /= fudEmpty then ww else qq), u <- qqll qq, u /= w, 
        let kk = llqq [w,u], vol uu kk <= xmax]
  | otherwise = Nothing
  where
    ww = fder ff
    vv = vars aa
    qq = vv `cup` fvars ff
    xx = fhis ff
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionInitialTuple_1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionInitialTuple_1 xmax uu aa aarr ff
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = Just $ llmm 
      [(kk, algn (apply (vars aa) kk (fhis ff) aa) - algn (apply (vars aa) kk (fhis ff) aarr)) |
        let qq = vars aa `cup` fvars ff,
        w <- qqll (if ff /= fudEmpty then fder ff else qq), u <- qqll qq, u /= w, 
        let kk = llqq [w,u], vol uu kk <= xmax]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple xmax uu aa aarr ff bb
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, algn (apply vv jj xx aa) - algn (apply vv jj xx aarr)) |
        kk <- keys bb, w <- qqll (qq `minus` kk),  
        let jj = kk `add` w, vol uu jj <= xmax]
  | otherwise = Nothing
  where
    vv = vars aa
    qq = vv `cup` fvars ff
    xx = fhis ff
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    cup = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple_1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple_1 xmax uu aa aarr ff bb
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, algn (apply (vars aa) jj (fhis ff) aa) - algn (apply (vars aa) jj (fhis ff) aarr)) |
        kk <- keys bb, w <- qqll (vars aa `cup` fvars ff `minus` kk),  
        let jj = kk `add` w, vol uu jj <= xmax]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    cup = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  
  Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)]
parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple xmax omax uu aa aarr ff
  | xmax < 0 = Nothing
  | omax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ff `subset` uvars uu = Just $
      opt (zzcstupneigh xmax uu aa aarr ff) (topff omax ff) (zzcstupinit xmax uu aa aarr ff)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcstupinit xmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesFudsFunctionInitialTuple xmax uu aa aarr ff
    zzcstupneigh xmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodTuple xmax uu aa aarr ff bb
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    subset = Set.isSubsetOf
    qqll = Set.toList
    sgl = Set.singleton

parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox :: 
  Integer -> Integer -> Set.Set Variable -> Fud -> Integer
parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox kmax omax vv ff 
  | kmax < 2 = 0
  | otherwise = step 3 e s
  where
    x = toInteger $ Set.size $ if ff == fudEmpty then vv else fder ff
    q = toInteger $ Set.size $ fvars ff `cup` vv
    s = binom q 2 - binom (q-x) 2
    e = min s omax
    step k e s 
      | k > min kmax q = s
      | otherwise = step (k+1) e' (s+s')
      where
        s' = min (binom q k - binom (q-x) k) ((q - k + 1) * e)
        e' = min s' omax
    fder = fudsDerived
    fvars = fudsVars
    binom = combination
    cup = Set.union

parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox_1 :: 
  Integer -> Integer -> Set.Set Variable -> Fud -> Integer
parametersSetVariablesFudsFunctionOptimiserTupleCardinalityApprox_1 kmax omax vv ff 
  | kmax < 2 = 0
  | otherwise = binom q 2 - binom (q-x) 2 + 
                sum [min (binom q i - binom (q-x) i) ((q - i + 1) * omax) | i <- [3.. min kmax q]]
  where
    x = toInteger $ Set.size $ if ff == fudEmpty then vv else fder ff
    q = toInteger $ Set.size $ fvars ff `cup` vv
    fder = fudsDerived
    fvars = fudsVars
    binom = combination
    cup = Set.union

parametersSetVariablesFudsFunctionOptimiserTupleCardinalityUpper :: 
  Integer -> Set.Set Variable -> Fud -> Integer
parametersSetVariablesFudsFunctionOptimiserTupleCardinalityUpper kmax vv ff =
    sum [binom q i - binom (q-x) i | i <- [2.. min kmax q]]
  where
    x = toInteger $ Set.size $ if ff == fudEmpty then vv else fder ff
    q = toInteger $ Set.size $ fvars ff `cup` vv
    fder = fudsDerived
    fvars = fudsVars
    binom = combination
    cup = Set.union

parametersSystemsSamplesShufflesSearcherFud :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram ->
  Maybe [Fud]
parametersSystemsSamplesShufflesSearcherFud lmax xmax omax bmax mmax umax pmax uu aa aarr
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ ls uu fudEmpty lmax
  | otherwise = Nothing
  where
    ls _ _ 0 = []
    ls uu ff h = gg : ls (uu `uunion` fsys gg) gg (h-1)
      where
        gg = ff `funion` llff [(pptt . ttpp . fftt) (depends (fexplode hh) w) | 
               kk <- topff (bmax `div` mmax) ff (zzffcstupopt xmax omax uu aa aarr ff), 
               hh <- top pmax (zzcsddecoptw mmax umax pmax uu aa aarr ff kk), w <- qqll (fder hh)]
    zzffcstupopt xmax omax uu aa aarr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple xmax omax uu aa aarr ff
    zzcsddecoptw mmax umax pmax uu aa aarr ff kk = fst $ unzip $ fromJust $
      parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk
    depends ff w = fudsVarsDepends ff (Set.singleton w)
    fexplode ff = fromJust $ fudVarPartitionsExplode ff
    fsys = fudsSystemImplied
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttpp = transformsPartition
    pptt = partitionsTransformVarPartition 
    vars = histogramsVars
    uunion = pairSystemsUnion
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    flip = map (\(a,b) -> (b,a))
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    mmll = Map.toList
    qqll = Set.toList
    subset = Set.isSubsetOf
    sgl = Set.singleton

parametersSystemsSamplesShufflesListVariablesSearcherFud :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> [Variable] ->
  Maybe ([Fud],(System,[Variable]))
parametersSystemsSamplesShufflesListVariablesSearcherFud lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ (pp,(uu',ll'))
  | otherwise = Nothing
  where
    qq = ls fudEmpty uu ll lmax
    pp = fst $ unzip $ qq
    (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
    ls _ _ _ 0 = []
    ls ff uu ll h = (gg,(uu',ll')) : ls gg uu' ll' (h-1)
      where
        yy = ls2 (topff (bmax `div` mmax) ff (zzffcstupopt xmax omax uu aa aarr ff)) uu ll
        gg = ff `funion` llff (elems (llmm [(rr, tt) | hh <- (concat $ fst $ unzip yy), 
                                             w <- qqll (fder hh), let tt = fftt (depends hh w), 
                                             let rr = ttpp tt, rr `notmem` nn]))
        (uu',ll') = if yy /= [] then (last $ snd $ unzip yy) else (uu,ll)
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
    flip = map (\(a,b) -> (b,a))
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    elems = Map.elems
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    llqq = Set.fromList
    notmem = Set.notMember
    subset = Set.isSubsetOf
    sgl = Set.singleton

parametersSystemsSamplesShufflesSearcherFudExcludedSelf :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram ->
  Maybe [Fud]
parametersSystemsSamplesShufflesSearcherFudExcludedSelf lmax xmax omax bmax mmax umax pmax uu aa aarr
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ ls uu fudEmpty lmax
  | otherwise = Nothing
  where
    ls _ _ 0 = []
    ls uu ff h = gg : ls (uu `uunion` fsys gg) gg (h-1)
      where
        gg = ff `funion` llff [pptt pp | 
               kk <- topff (bmax `div` mmax) ff (zzffcstupopt xmax omax uu aa aarr ff), 
               hh <- top pmax (zzcsddecoptw mmax umax pmax uu aa aarr ff kk), w <- qqll (fder hh),
               let pp = ttpp (fftt (depends (fexplode hh) w)), pp /= self uu (ppvars pp)]
    zzffcstupopt xmax omax uu aa aarr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple xmax omax uu aa aarr ff
    zzcsddecoptw mmax umax pmax uu aa aarr ff kk = fst $ unzip $ fromJust $
      parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk
    depends ff w = fudsVarsDepends ff (Set.singleton w)
    fexplode ff = fromJust $ fudVarPartitionsExplode ff
    fsys = fudsSystemImplied
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttpp = transformsPartition
    pptt = partitionsTransformVarPartition 
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    ppvars = partitionsVars
    vars = histogramsVars
    uunion = pairSystemsUnion
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    flip = map (\(a,b) -> (b,a))
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    mmll = Map.toList
    qqll = Set.toList
    subset = Set.isSubsetOf
    sgl = Set.singleton

parametersSystemsSamplesShufflesFudsFunctionInitialDerived :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionInitialDerived wmax uu aa aarr ff
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        w <- qqll qq, u <- qqll qq, u /= w,  
        let jj = llqq [w,u], let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    qq = fvars ff `minus` vars aa
    algn = histogramsAlignment
    depends = fudsVarsDepends
    llff = fromJust . setTransformsFud . Set.fromList
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionInitialDerived_1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionInitialDerived_1 wmax uu aa aarr ff
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        let qq = fvars ff `minus` vars aa, w <- qqll qq, u <- qqll qq, u /= w,  
        let jj = llqq [w,u], let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    llff = fromJust . setTransformsFud . Set.fromList
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived wmax uu aa aarr ff bb
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        kk <- keys bb, w <- qqll (qq `minus` kk),  
        let jj = kk `add` w, let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    qq = fvars ff `minus` vars aa
    algn = histogramsAlignment
    depends = fudsVarsDepends
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived_1 :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived_1 wmax uu aa aarr ff bb
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        kk <- keys bb, w <- qqll (fvars ff `minus` vars aa `minus` kk),  
        let jj = kk `add` w, let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionOptimiserDerived :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  
  Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)]
parametersSystemsSamplesShufflesFudsFunctionOptimiserDerived wmax omax uu aa aarr ff
  | wmax < 0 = Nothing
  | omax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $
      opt (zzcsdderneigh wmax uu aa aarr ff) (topff omax ff) (zzcsdderinit wmax uu aa aarr ff)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcsdderinit wmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesFudsFunctionInitialDerived wmax uu aa aarr ff
    zzcsdderneigh wmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived wmax uu aa aarr ff bb
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    subset = Set.isSubsetOf
    sgl = Set.singleton

parametersSetVariablesFudsFunctionOptimiserDerivedCardinalityUpper :: 
  Integer -> Set.Set Variable -> Fud -> Integer
parametersSetVariablesFudsFunctionOptimiserDerivedCardinalityUpper jmax vv ff =
    sum [binom r i | i <- [2.. min jmax r]]
  where
    r = toInteger $ Set.size $ fvars ff `minus` vv
    fvars = fudsVars
    binom = combination
    minus = Set.difference

parametersSystemsSamplesShufflesListVariablesInducerFud :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
  System -> Histogram -> Histogram -> [Variable] ->
  Maybe ((Fud,Fud),(System,[Variable]))
parametersSystemsSamplesShufflesListVariablesInducerFud wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ ((depends ff kk, ff),(uu',ll'))
  | otherwise = Nothing
  where
    (nn,(uu',ll')) = zzllfuds lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    ff = if nn /= [] then last nn else fudEmpty
    kk = maxdff ff $ zzffcsdderopt wmax omax uu' aa aarr ff
    zzllfuds lmax xmax omax bmax mmax umax pmax uu aa aarr ll = fromJust $
      parametersSystemsSamplesShufflesListVariablesSearcherFud lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    zzffcsdderopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerived wmax omax uu aa rr ff
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

parametersSystemsSamplesShufflesFudsFunctionInitialDerivedHighest :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesFudsFunctionInitialDerivedHighest wmax uu aa aarr ff
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        w <- qqll (fder ff), u <- qqll (fvars ff `minus` vars aa `minus` fvars (depends ff (sgl w))), u /= w,  
        let jj = llqq [w,u], let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    llff = fromJust . setTransformsFud . Set.fromList
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    minus = Set.difference
    subset = Set.isSubsetOf
    sgl = Set.singleton
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  
  Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)]
parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa aarr ff
  | wmax < 0 = Nothing
  | omax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $
      opt (zzcsdderneigh wmax uu aa aarr ff) (topff omax ff) (zzcsdderinit wmax uu aa aarr ff)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcsdderinit wmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesFudsFunctionInitialDerivedHighest wmax uu aa aarr ff
    zzcsdderneigh wmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesFudsFunctionNeighbourhoodDerived wmax uu aa aarr ff bb
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf

parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityApprox :: 
  Integer -> Integer -> Set.Set Variable -> Fud -> Integer
parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityApprox jmax omax vv ff 
  | jmax < 2 = 0
  | otherwise = step 3 e s
  where
    r = toInteger $ Set.size $ fvars ff `minus` vv
    x = toInteger $ Set.size $ fder ff
    s = binom r 2 - binom (r-x) 2
    e = min s omax
    step j e s 
      | j > min jmax r = s
      | otherwise = step (j+1) e' (s+s')
      where
        s' = min (binom r j - binom (r-x) j) ((r - j + 1) * e)
        e' = min s' omax
    fder = fudsDerived
    fvars = fudsVars
    binom = combination
    minus = Set.difference

parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityUpper :: 
  Integer -> Set.Set Variable -> Fud -> Integer
parametersSetVariablesFudsFunctionOptimiserDerivedHighestCardinalityUpper jmax vv ff =
    sum [binom r i - binom (r-x) i | i <- [2.. min jmax r]]
  where
    r = toInteger $ Set.size $ fvars ff `minus` vv
    x = toInteger $ Set.size $ fder ff
    fvars = fudsVars
    fder = fudsDerived
    binom = combination
    minus = Set.difference

parametersSystemsSamplesShufflesSearcherFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram ->
  Maybe [Fud]
parametersSystemsSamplesShufflesSearcherFudHighest wmax lmax xmax omax bmax mmax umax pmax uu aa aarr
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ ls uu fudEmpty lmax 0
  | otherwise = Nothing
  where
    ls _ _ 0 _ = []
    ls uu ff h a = if b > a then (gg : ls uu' gg (h-1) b) else []
      where
        gg = ff `funion` llff [(pptt . ttpp . fftt) (depends (fexplode hh) w) | 
               kk <- topff (bmax `div` mmax) ff (zzffcstupopt xmax omax uu aa aarr ff), 
               hh <- top pmax (zzcsddecoptw mmax umax pmax uu aa aarr ff kk), w <- qqll (fder hh)]
        uu' = uu `uunion` fsys gg
        b = maxr $ zzffcsdderopt wmax omax uu' aa aarr gg
    zzffcstupopt xmax omax uu aa aarr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserTuple xmax omax uu aa aarr ff
    zzcsddecoptw mmax umax pmax uu aa aarr ff kk = fst $ unzip $ fromJust $
      parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk
    zzffcsdderopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ff
    depends ff w = fudsVarsDepends ff (Set.singleton w)
    fexplode ff = fromJust $ fudVarPartitionsExplode ff
    fsys = fudsSystemImplied
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttpp = transformsPartition
    pptt = partitionsTransformVarPartition 
    vars = histogramsVars
    uunion = pairSystemsUnion
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    maxr ll = let xx = (sort $ snd $ unzip $ concat $ map mmll ll) in if xx /= [] then (last xx) else 0
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList
    elems = Map.elems
    mmll = Map.toList
    subset = Set.isSubsetOf
    sgl = Set.singleton

parametersSystemsSamplesShufflesListVariablesSearcherFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> [Variable] ->
  Maybe ([Fud],(System,[Variable]))
parametersSystemsSamplesShufflesListVariablesSearcherFudHighest wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ (pp,(uu',ll'))
  | otherwise = Nothing
  where
    qq = ls fudEmpty uu ll lmax 0
    pp = fst $ unzip $ qq
    (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
    ls _ _ _ 0 _ = []
    ls ff uu ll h a = if b > a then ((gg,(uu',ll')) : ls gg uu' ll' (h-1) b) else []
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

parametersSystemsSamplesShufflesListVariablesInducerFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
  System -> Histogram -> Histogram -> [Variable] ->
  Maybe ((Fud,Fud),(System,[Variable]))
parametersSystemsSamplesShufflesListVariablesInducerFudHighest wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ ((depends ff kk, ff),(uu',ll'))
  | otherwise = Nothing
  where
    (nn,(uu',ll')) = zzllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    ff = if nn /= [] then last nn else fudEmpty
    kk = maxdff ff $ zzffcsdderhighopt wmax omax uu' aa aarr ff
    zzllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll = fromJust $
      parametersSystemsSamplesShufflesListVariablesSearcherFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    zzffcsdderhighopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ff
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

parametersSystemsSamplesListVariablesInducerDecompFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> [Variable] ->
  Maybe (DecompFud,(System,[Variable]))
parametersSystemsSamplesListVariablesInducerDecompFudHighest 
  wmax lmax xmax omax bmax mmax umax pmax mult seed uu aa ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | vars aa `subset` uvars uu = Just $ dec emptyTree uu ll seed
  | otherwise = Nothing
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

parametersSystemsSamplesListVariablesInducerDecompFudHighestNoShuffle :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> [Variable] ->
  Maybe (DecompFud,(System,[Variable]))
parametersSystemsSamplesListVariablesInducerDecompFudHighestNoShuffle
  wmax lmax xmax omax bmax mmax umax pmax uu aa ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu = Just $ dec emptyTree uu ll
  | otherwise = Nothing
  where
    dec zz uu ll 
      | zz == emptyTree && ffr == fudEmpty = (decompFudEmpty, (uu, ll))
      | zz == emptyTree = dec zzr uur llr
      | mm == [] = (zzdf (zztrim zz), (uu, ll)) 
      | otherwise = dec zzc uuc llc
      where
        ((ffr,_),(uur,llr)) = iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa (ind aa) ll
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 size bb > 0]
        (_,nn,ss,bb) = last $ sort mm
        ((ffc,_),(uuc,llc)) = iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu bb (ind bb) ll
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll = fromJust $
      parametersSystemsSamplesShufflesListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    ffqq = fudsSetTransform
    fder = fudsDerived
    fhis = fudsSetHistogram
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    unit = fromJust . setStatesHistogramUnit . Set.singleton 
    ind = histogramsIndependent
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

parametersSystemsSamplesShufflesCommonFudsFudsFunctionInitialTuple :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesCommonFudsFudsFunctionInitialTuple xmax uu aa aarr ffc ff
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu && fvars ff `subset` uvars uu = Just $ llmm 
      [(kk, algn (apply vv kk xx aa) - algn (apply vv kk xx aarr)) |
        w <- qqll (if ff /= fudEmpty then ww else qq), u <- qqll qq, u /= w, 
        let kk = llqq [w,u], vol uu kk <= xmax]
  | otherwise = Nothing
  where
    ww = fder ff
    vv = vars aa
    qq = vv `cup` fvars ffc `cup` fvars ff
    xx = fhis ff `Set.union` fhis ffc
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    cup = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesCommonFudsFudsFunctionNeighbourhoodTuple :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesCommonFudsFudsFunctionNeighbourhoodTuple xmax uu aa aarr ffc ff bb
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu && fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, algn (apply vv jj xx aa) - algn (apply vv jj xx aarr)) |
        kk <- keys bb, w <- qqll (qq `minus` kk),  
        let jj = kk `add` w, vol uu jj <= xmax]
  | otherwise = Nothing
  where
    vv = vars aa
    qq = vv `cup` fvars ffc `cup` fvars ff
    xx = fhis ff `Set.union` fhis ffc
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    cup = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesCommonFudsFudsFunctionOptimiserTuple :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud -> Fud ->  
  Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)]
parametersSystemsSamplesShufflesCommonFudsFudsFunctionOptimiserTuple xmax omax uu aa aarr ffc ff
  | xmax < 0 = Nothing
  | omax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu && fvars ff `subset` uvars uu = Just $
      opt (zzcstupffcneigh xmax uu aa aarr ffc ff) (topff omax (ff `funion` ffc)) 
          (zzcstupffcinit xmax uu aa aarr ffc ff)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcstupffcinit xmax uu aa aarr ffc ff =  fromJust $
      parametersSystemsSamplesShufflesCommonFudsFudsFunctionInitialTuple xmax uu aa aarr ffc ff
    zzcstupffcneigh xmax uu aa aarr ffc ff bb = fromJust $
      parametersSystemsSamplesShufflesCommonFudsFudsFunctionNeighbourhoodTuple xmax uu aa aarr ffc ff bb
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    layer = fudsSetVarsLayer
    mmll = Map.toList
    llmm = Map.fromList
    isempty = Map.null
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf

parametersSetVariablesCommonFudsFudsFunctionOptimiserTupleCardinalityUpper :: 
  Integer -> Set.Set Variable -> Fud -> Fud -> Integer
parametersSetVariablesCommonFudsFudsFunctionOptimiserTupleCardinalityUpper kmax vv ffc ff =
    sum [binom q i - (if ff /= fudEmpty then binom (q-x) i else 0) | i <- [2.. min kmax q]]
  where
    x = toInteger $ Set.size $ fder ff
    q = toInteger $ Set.size $ fvars ff `cup` fvars ffc `cup` vv
    fder = fudsDerived
    fvars = fudsVars
    binom = combination
    cup = Set.union

parametersSystemsSamplesShufflesCommonFudsSearcherFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> Fud ->
  Maybe [Fud]
parametersSystemsSamplesShufflesCommonFudsSearcherFudHighest 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu = Just $ ls uu fudEmpty lmax 0
  | otherwise = Nothing
  where
    ls _ _ 0 _ = []
    ls uu ff h a = if layer ff < lmax && b > a then (gg : ls uu' gg (h-1) b) else []
      where
        gg = foldl funion ff [ttff tt `funion` depends ffc (und tt)  | 
               kk <- topff (bmax `div` mmax) (ff `funion` ffc) (zzffcstupffcopt xmax omax uu aa aarr ffc ff), 
               hh <- top pmax (zzcsddecoptw mmax umax pmax uu aa aarr (ff `funion` ffc) kk), 
               w <- qqll (fder hh),
               let tt = (pptt . ttpp . fftt) (depends (fexplode hh) (sgl w))]
        uu' = uu `uunion` fsys gg
        b = maxr $ zzffcsdderopt wmax omax uu' aa aarr gg
    zzffcstupffcopt xmax omax uu aa aarr ffc ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesCommonFudsFudsFunctionOptimiserTuple xmax omax uu aa aarr ffc ff
    zzcsddecoptw mmax umax pmax uu aa aarr ff kk = fst $ unzip $ fromJust $
      parametersSystemsSamplesShufflesFudsTuplesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk
    zzffcsdderopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ff
    depends ff vv = fudsVarsDepends ff vv
    fexplode ff = fromJust $ fudVarPartitionsExplode ff
    fsys = fudsSystemImplied
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    layer ff = fudsSetVarsLayer ff (fder ff)
    fvars = fudsVars
    ttff = fromJust . setTransformsFud . Set.singleton
    ttpp = transformsPartition
    pptt = partitionsTransformVarPartition 
    und = transformsUnderlying
    vars = histogramsVars
    uunion = pairSystemsUnion
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    maxr ll = let xx = (sort $ snd $ unzip $ concat $ map mmll ll) in if xx /= [] then (last xx) else 0
    flip = map (\(a,b) -> (b,a))
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [fudsSetVarsLayer ff (sgl w) | w <- qqll kk]
    mmll = Map.toList
    elems = Map.elems
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> Fud -> [Variable] ->
  Maybe ([Fud],(System,[Variable]))
parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu = Just $ (pp,(uu',ll'))
  | otherwise = Nothing
  where
    qq = ls fudEmpty uu ll lmax 0
    pp = fst $ unzip $ qq
    (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
    nnc = llmm [(ttpp tt,tt) | tt <- (qqll . ffqq) ffc]
    ls _ _ _ 0 _ = []
    ls ff uu ll h a = if layer ff < lmax && b > a then ((gg,(uu',ll')) : ls gg uu' ll' (h-1) b) else []
      where
        yy = ls2 (topff (bmax `div` mmax) (ff `funion` ffc) (zzffcstupffcopt xmax omax uu aa aarr ffc ff)) uu ll
        gg = foldl funion ff (elems (llmm [(rr, ttff ttc `funion` depends ffc (und ttc)) | 
                                             hh <- (concat $ fst $ unzip yy), 
                                             w <- qqll (fder hh), let tt = fftt (depends hh (sgl w)), 
                                             let rr = ttpp tt, rr `notmem` nn,
                                             let ttc = findw tt rr nnc]))
        (uu',ll') = if yy /= [] then (last $ snd $ unzip yy) else (uu,ll)
        b = maxr $ zzffcsdderhighopt wmax omax uu' aa aarr gg
        nn = llqq [ttpp tt | tt <- (qqll . ffqq) ff]
        ls2 [] _ _ = []
        ls2 (kk:bb) uu ll = (top pmax (fst $ unzip xx),(uu',ll')) : ls2 bb uu' ll'
          where
            (xx,(uu',ll')) = zzllcsddecoptw mmax umax pmax uu aa aarr (ff `funion` ffc) kk ll
    zzffcstupffcopt xmax omax uu aa aarr ffc ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesCommonFudsFudsFunctionOptimiserTuple xmax omax uu aa aarr ffc ff
    zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll = fromJust $
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk ll
    zzffcsdderhighopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ff
    depends ff vv = fudsVarsDepends ff vv
    layer ff = fudsSetVarsLayer ff (fder ff)
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fvars = fudsVars
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttff = fromJust . setTransformsFud . Set.singleton
    ttpp = transformsPartition
    und = transformsUnderlying
    vars = histogramsVars
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    maxr ll = let xx = (sort $ snd $ unzip $ concat $ map mmll ll) in if xx /= [] then (last xx) else 0
    flip = map (\(a,b) -> (b,a))
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [fudsSetVarsLayer ff (sgl w) | w <- qqll kk]
    qqll = Set.toList
    llqq = Set.fromList
    elems = Map.elems
    llmm = Map.fromList
    mmll = Map.toList
    findw = Map.findWithDefault
    sgl = Set.singleton
    notmem = Set.notMember
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest_d :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> Fud -> [Variable] ->
  Maybe ([Fud],(System,[Variable]))
parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest_d 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu = Just $ (pp,(uu',ll'))
  | otherwise = Nothing
  where
    qq = ls fudEmpty uu ll lmax 0
    pp = fst $ unzip $ qq
    (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
    nnc = llmm [(ttpp tt,tt) | tt <- (qqll . ffqq) ffc]
    ls _ _ _ 0 _ = []
    ls ff uu ll h a = if layer ff < lmax && b > a then ((gg,(uu',ll')) : ls gg uu' ll' (h-1) b) else []
      where
        yy = ls2 (topff (bmax `div` mmax) (ff `funion` ffc) (zzffcstupffcopt xmax omax uu aa aarr ffc ff)) uu ll
        gg = foldl funion ff (elems (llmm [(rr, ttff ttc `funion` depends ffc (und ttc)) | 
                                             hh <- (concat $ fst $ unzip yy), 
                                             w <- qqll (fder hh), let tt = fftt (depends hh (sgl w)), 
                                             let rr = ttpp tt, rr `notmem` nn,
                                             let ttc = tt]))
        (uu',ll') = if yy /= [] then (last $ snd $ unzip yy) else (uu,ll)
        b = maxr $ zzffcsdderhighopt wmax omax uu' aa aarr gg
        nn = llqq [ttpp tt | tt <- (qqll . ffqq) ff]
        ls2 [] _ _ = []
        ls2 (kk:bb) uu ll = (top pmax (fst $ unzip xx),(uu',ll')) : ls2 bb uu' ll'
          where
            (xx,(uu',ll')) = zzllcsddecoptw mmax umax pmax uu aa aarr (ff `funion` ffc) kk ll
    zzffcstupffcopt xmax omax uu aa aarr ffc ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesCommonFudsFudsFunctionOptimiserTuple xmax omax uu aa aarr ffc ff
    zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll = fromJust $
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk ll
    zzffcsdderhighopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ff
    depends ff vv = fudsVarsDepends ff vv
    layer ff = fudsSetVarsLayer ff (fder ff)
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fvars = fudsVars
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    ttff = fromJust . setTransformsFud . Set.singleton
    ttpp = transformsPartition
    und = transformsUnderlying
    vars = histogramsVars
    uvars = systemsVars
    top amax ll = snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ concat $ map mmll ll
    maxr ll = let xx = (sort $ snd $ unzip $ concat $ map mmll ll) in if xx /= [] then (last xx) else 0
    flip = map (\(a,b) -> (b,a))
    topff amax ff ll = snd $ unzip $ take (fromInteger amax) $ sortdescff ff $ concat $ map mmll ll
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [fudsSetVarsLayer ff (sgl w) | w <- qqll kk]
    qqll = Set.toList
    llqq = Set.fromList
    elems = Map.elems
    llmm = Map.fromList
    mmll = Map.toList
    sgl = Set.singleton
    notmem = Set.notMember
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesCommonFudsListVariablesInducerFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
  System -> Histogram -> Histogram -> Fud -> [Variable] ->
  Maybe ((Fud,Fud),(System,[Variable]))
parametersSystemsSamplesShufflesCommonFudsListVariablesInducerFudHighest 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffc `subset` uvars uu = Just $ ((depends ff kk, ff),(uu',ll'))
  | otherwise = Nothing
  where
    (nn,(uu',ll')) = zzffcllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc ll
    ff = if nn /= [] then last nn else fudEmpty
    kk = maxdff ff $ zzffcsdderopt wmax omax uu' aa aarr ff
    zzffcllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc ll = fromJust $
      parametersSystemsSamplesShufflesCommonFudsListVariablesSearcherFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffc ll
    zzffcsdderopt wmax omax uu aa rr ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesFudsFunctionOptimiserDerived wmax omax uu aa rr ff
    depends = fudsVarsDepends
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    maxdff ff ll = let xx = (sortdescff ff $ concat $ map mmll ll) in 
              if xx /= [] then (snd $ head $ xx) else Set.empty
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    mmll = Map.toList
    subset = Set.isSubsetOf
    qqll = Set.toList
    sgl = Set.singleton

parametersSystemsSamplesListVariablesInducerDecompFudHighestAccumulating :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> [Variable] ->
  Maybe ((DecompFud,Fud),(System,[Variable]))
parametersSystemsSamplesListVariablesInducerDecompFudHighestAccumulating 
  wmax lmax xmax omax bmax mmax umax pmax mult seed uu aa ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | vars aa `subset` uvars uu = Just $ dec emptyTree fudEmpty uu ll seed
  | otherwise = Nothing
  where
    dec zz ffa uu ll s
      | zz == emptyTree && ffr == fudEmpty = ((decompFudEmpty,fudEmpty), (uu, ll))
      | zz == emptyTree = dec zzr (fftrim uur ffr ffrl) uur llr (s+mult)
      | mm == [] = ((zzdf (zztrim zz),ffa), (uu, ll)) 
      | otherwise = dec zzc (fftrim uuc ffc ffcl `funion` ffa) uuc llc (s+mult)
      where
        aarr = ashuffle aa s mult
        ((ffr,ffrl),(uur,llr)) = iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb,bbrr) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 let bbrr = ashuffle bb s mult,
                 size bb > 0]
        (_,nn,ss,bb,bbrr) = last $ sort mm
        ((ffc,ffcl),(uuc,llc)) = iiffcllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu bb bbrr ffa ll
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll = fromJust $
      parametersSystemsSamplesShufflesListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    iiffcllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffa ll = fromJust $
      parametersSystemsSamplesShufflesCommonFudsListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffa ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    fftrim uu gg ff 
      | ff == fudEmpty = fudEmpty
      | ff' == ff = ff
      | otherwise = fftrim uu gg ff' 
      where ff' = qqff $ ffqq ff `minus` llqq [tt | tt <- qqll (ffqq ff), der tt `subset` fder ff, 
                        der tt `cap` fvars gg == empty, card (und tt) == 1, ttpp tt == self uu (und tt)] 
    funion ff gg = qqff (ffqq ff `cup` ffqq gg)
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    fhis = fudsSetHistogram
    fvars = fudsVars
    ttpp = transformsPartition
    und = transformsUnderlying
    der = transformsDerived
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
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    tsgl r = Tree $ Map.singleton r emptyTree
    bigcup = setSetsUnion
    dom = relationsDomain
    card = Set.size
    minus :: forall a. (Ord a) => Set.Set a -> Set.Set a -> Set.Set a
    minus = Set.difference
    cap = Set.intersection
    cup = Set.union
    empty = Set.empty
    add qq x = Set.insert x qq
    qqll = Set.toList
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf

parametersSystemsSamplesListVariablesInducerDecompFudHighestAccumulating_1 :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> [Variable] ->
  Maybe ((DecompFud,Fud),(System,[Variable]))
parametersSystemsSamplesListVariablesInducerDecompFudHighestAccumulating_1 
  wmax lmax xmax omax bmax mmax umax pmax mult seed uu aa ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | vars aa `subset` uvars uu = Just $ dec emptyTree fudEmpty uu ll seed
  | otherwise = Nothing
  where
    dec zz ffa uu ll s
      | zz == emptyTree && ffr == fudEmpty = ((decompFudEmpty,fudEmpty), (uu, ll))
      | zz == emptyTree = dec zzr ffrl uur llr (s+mult)
      | mm == [] = ((zzdf (zztrim zz),ffa), (uu, ll)) 
      | otherwise = dec zzc (ffcl `funion` ffa) uuc llc (s+mult)
      where
        aarr = ashuffle aa s mult
        ((ffr,ffrl),(uur,llr)) = iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb,bbrr) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 let bbrr = ashuffle bb s mult,
                 size bb > 0]
        (_,nn,ss,bb,bbrr) = last $ sort mm
        ((ffc,ffcl),(uuc,llc)) = iiffcllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu bb bbrr ffa ll
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll = fromJust $
      parametersSystemsSamplesShufflesListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ll
    iiffcllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffa ll = fromJust $
      parametersSystemsSamplesShufflesCommonFudsListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr ffa ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    qqff = fromJust . setTransformsFud
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

parametersSystemsBuilderTuple :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set ((Set.Set Variable, Histogram, Histogram),Double))
parametersSystemsBuilderTuple xmax omax bmax mmax uu vv ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb vv (init vv) Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `union` vv) (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    init vv = llmm [(((sgl w, histogramEmpty, histogramEmpty),0),(0,0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | (((kk,_,_),_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [(((jj, bb, bbrr), a1-b1), (a1-a2-b1+b2, -l, -b1+b2, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, let l =  sumlayer ff jj, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, 
          let a1 = sumfacln bb, let a2 = sumfacln (ind bb), 
          let b1 = sumfacln bbrr, let b2 = sumfacln (ind bbrr)]
    final nn = llmm [(((kk,aa,bb),y),a) | (((kk,aa,bb),y),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    ind = histogramsIndependent
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderTuple_1 :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set (Set.Set Variable, Histogram, Histogram))
parametersSystemsBuilderTuple_1 xmax omax bmax mmax uu vv ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb vv (init vv) Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `union` vv) (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    init vv = llmm [((sgl w, histogramEmpty, histogramEmpty),(0,0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | ((kk,_,_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [((jj, bb, bbrr), (a-b, -l, -b, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, let l =  sumlayer ff jj, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, let a = algn bb, let b = algn bbrr]
    final nn = llmm [((kk,aa,bb),a) | ((kk,aa,bb),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    algn = histogramsAlignment
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderTupleNoSumlayer :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set ((Set.Set Variable, Histogram, Histogram),Double))
parametersSystemsBuilderTupleNoSumlayer xmax omax bmax mmax uu vv ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb vv (init vv) Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `union` vv) (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    init vv = llmm [(((sgl w, histogramEmpty, histogramEmpty),0),(0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | (((kk,_,_),_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [(((jj, bb, bbrr), a1-b1), (a1-a2-b1+b2, -b1+b2, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, 
          let a1 = sumfacln bb, let a2 = sumfacln (ind bb), 
          let b1 = sumfacln bbrr, let b2 = sumfacln (ind bbrr)]
    final nn = llmm [(((kk,aa,bb),y),a) | (((kk,aa,bb),y),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    ind = histogramsIndependent
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderTupleNoSumlayerMultiEffective :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set ((Set.Set Variable, Histogram, Histogram),Double))
parametersSystemsBuilderTupleNoSumlayerMultiEffective xmax omax bmax mmax uu vv ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | meff xx vv == empty = Just $ Set.empty
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb (meff xx vv) (init (meff xx vv)) Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `union` (meff xx vv)) (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    init vv = llmm [(((sgl w, histogramEmpty, histogramEmpty),0),(0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | (((kk,_,_),_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [(((jj, bb, bbrr), a1-b1), (a1-a2-b1+b2, -b1+b2, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, 
          let a1 = sumfacln bb, let a2 = sumfacln (ind bb), 
          let b1 = sumfacln bbrr, let b2 = sumfacln (ind bbrr)]
    final nn = llmm [(((kk,aa,bb),y),a) | (((kk,aa,bb),y),a) <- mmll nn, card kk > 1]
    meff xx vv = Set.filter (\v -> acard (eff (xx `red` sgl v)) > 1) vv
    fder = fudsDerived
    fvars = fudsVars
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    ind = histogramsIndependent
    red aa vv = setVarsHistogramsReduce vv aa
    eff = histogramsEffective
    acard = histogramsCardinality
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderDerivedVarsHighest :: 
  Integer -> Integer -> System -> Set.Set Variable -> Fud -> Histogram -> Histogram ->  
  Maybe (Map.Map (Set.Set Variable, Histogram, Histogram) Double)
parametersSystemsBuilderDerivedVarsHighest wmax omax uu vv ff xx xxrr
  | wmax < 0 || omax < 0 = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | not (fvars ff `subset` uvars uu) = Nothing
  | otherwise = Just $ maxfst $ buildd (fvars ff `minus` vv) (init (fder ff)) Map.empty
  where
    init vv = llmm [((sgl w, histogramEmpty, histogramEmpty),(0,0,0,0)) | w <- qqll vv]
    buildd ww qq nn = if mm /= Map.empty then buildd ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | ((kk,_,_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [((jj, bb, bbrr), ((a-b)/c,-l,-b/c,-u)) |
          jj <- qqll pp, let u = vol uu jj, u <= wmax, fder (depends ff jj) == jj,
          let l =  sumlayer ff jj, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj,
          let u' = fromIntegral u, let m = fromIntegral (Set.size jj),
          let a = algn bb, let b = algn bbrr, let c = u' ** (1/m)]
    final nn = llmm [((kk,aa,bb),a) | ((kk,aa,bb),a) <- mmll nn, card kk > 1]
    depends = fudsVarsDepends
    fder = fudsDerived
    fvars = fudsVars
    algn = histogramsAlignment
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    maxfst mm = llmm $ map (\((a,_,_,_),x) -> (x,a)) $ take 1 $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderDerivedVarsHighestNoSumlayerIncludeHidden :: 
  Integer -> Integer -> System -> Set.Set Variable -> Fud -> Histogram -> Histogram ->  
  Maybe (Map.Map (Set.Set Variable, Histogram, Histogram) Double)
parametersSystemsBuilderDerivedVarsHighestNoSumlayerIncludeHidden wmax omax uu vv ff xx xxrr
  | wmax < 0 || omax < 0 = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | not (fvars ff `subset` uvars uu) = Nothing
  | otherwise = Just $ maxfst $ buildd (fvars ff `minus` vv) (init (fder ff)) Map.empty
  where
    init vv = llmm [((sgl w, histogramEmpty, histogramEmpty),(0,0,0)) | w <- qqll vv]
    buildd ww qq nn = if mm /= Map.empty then buildd ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | ((kk,_,_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [((jj, bb, bbrr), ((a-b)/c,-b/c,-u)) |
          jj <- qqll pp, let u = vol uu jj, u <= wmax, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj,
          let u' = fromIntegral u, let m = fromIntegral (Set.size jj),
          let a = algn bb, let b = algn bbrr, let c = u' ** (1/m)]
    final nn = llmm [((kk,aa,bb),a) | ((kk,aa,bb),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    algn = histogramsAlignment
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    maxfst mm = llmm $ map (\((a,_,_),x) -> (x,a)) $ take 1 $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsPartitionerBinary :: 
  Integer -> Integer -> System -> Set.Set Variable -> Histogram -> Histogram -> 
  Maybe (Set.Set ([Set.Set (State,Int)], Histogram, Histogram))
parametersSystemsPartitionerBinary umax pmax uu kk bb bbrr
  | umax < 0 || pmax < 0 = Nothing
  | not (vars bb `subset` uvars uu && vars bb == vars bbrr && kk `subset` vars bb) = Nothing
  | otherwise = Just $ topd pmax $ llmm [((nn, cc, ccrr), (b-a, b)) |
        yy <- stirsll kk 2, and [vol uu jj <= umax | jj <- qqll yy],
        let nn = [llqq (zip (qqll (cart uu jj)) [0..]) | jj <- qqll yy],
        let tt = trans (unit [foldl sunion sempty [ss `sunion` ssgl (VarIndex w) (ValIndex u) | 
                       (w,(ss,u)) <- zip [0..] ll] | ll <- qqll (prod nn)]) 
                       (llqq [VarIndex (fromInteger w) | w <- [0 .. dim yy - 1]]),
        let cc = bb `tmul` tt, let ccrr = bbrr `tmul` tt,
        let a = sumfacln (ind cc), let b = sumfacln (ind ccrr)]
  where
    tmul aa tt = transformsHistogramsApply tt aa
    trans = histogramsSetVarsTransform_u
    ind = histogramsIndependent
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    aall = histogramsList
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    ssgl = stateSingleton
    sempty = stateEmpty
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    prod = listSetsProduct
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    llmm = Map.fromList
    mmll = Map.toList

parametersSystemsPartitioner :: 
  Integer -> Integer -> Integer -> System -> Set.Set Variable -> Histogram -> Histogram -> Double ->
  Maybe (Set.Set ([Set.Set (State,Int)], Histogram, Histogram))
parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
  | umax < 0 || mmax < 0 || pmax < 0 = Nothing
  | not (vars bb `subset` uvars uu && vars bb == vars bbrr && kk `subset` vars bb) = Nothing
  | otherwise = Just $ topd pmax $ llmm [((nn, cc, ccrr), ((y1-a2+b2)/c, b2, -m)) |
        yy <- stirsll kk mmax, dim yy >= 2, and [vol uu jj <= umax | jj <- qqll yy],
        let m = fromIntegral $ dim yy,
        let nn = [llqq (zip (qqll (cart uu jj)) [0..]) | jj <- qqll yy],
        let tt = trans (unit [foldl sunion sempty [ss `sunion` ssgl (VarIndex w) (ValIndex u) | 
                       (w,(ss,u)) <- zip [0..] ll] | ll <- qqll (prod nn)]) 
                       (llqq [VarIndex (fromInteger w) | w <- [0 .. dim yy - 1]]),
        let cc = bb `tmul` tt, let ccrr = bbrr `tmul` tt,
        let a2 = sumfacln (ind cc), let b2 = sumfacln (ind ccrr), let c = v ** (1/m)]
  where
    v = fromIntegral $ vol uu kk
    tmul aa tt = transformsHistogramsApply tt aa
    trans = histogramsSetVarsTransform_u
    ind = histogramsIndependent
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    aall = histogramsList
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    ssgl = stateSingleton
    sempty = stateEmpty
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    prod = listSetsProduct
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    llmm = Map.fromList
    mmll = Map.toList

parametersSystemsPartitioner_1 :: 
  Integer -> Integer -> Integer -> System -> Set.Set Variable -> Histogram -> Histogram -> 
  Maybe (Set.Set ([Set.Set (State,Int)], Histogram, Histogram))
parametersSystemsPartitioner_1 mmax umax pmax uu kk bb bbrr
  | umax < 0 || mmax < 0 || pmax < 0 = Nothing
  | not (vars bb `subset` uvars uu && vars bb == vars bbrr && kk `subset` vars bb) = Nothing
  | otherwise = Just $ topd pmax $ llmm [((nn, cc, ccrr), ((a-b)/c, -b, -m)) |
        yy <- stirsll kk mmax, dim yy >= 2, and [vol uu jj <= umax | jj <- qqll yy],
        let m = fromIntegral $ dim yy,
        let nn = [llqq (zip (qqll (cart uu jj)) [0..]) | jj <- qqll yy],
        let tt = trans (unit [foldl sunion sempty [ss `sunion` ssgl (VarIndex w) (ValIndex u) | 
                       (w,(ss,u)) <- zip [0..] ll] | ll <- qqll (prod nn)]) 
                       (llqq [VarIndex (fromInteger w) | w <- [0 .. dim yy - 1]]),
        let cc = bb `tmul` tt, let ccrr = bbrr `tmul` tt,
        let a = algn cc, let b = algn ccrr, let c = v ** (1/m)]
  where
    v = fromIntegral $ vol uu kk
    tmul aa tt = transformsHistogramsApply tt aa
    trans = histogramsSetVarsTransform_u
    ind = histogramsIndependent
    algn = histogramsAlignment
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    aall = histogramsList
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    ssgl = stateSingleton
    sempty = stateEmpty
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    prod = listSetsProduct
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    llmm = Map.fromList
    mmll = Map.toList

rollValuer :: RollValue -> Histogram -> Histogram -> (Histogram, Histogram) 
rollValuer rv aa bb
  | s==t = (aa, bb)
  | otherwise = (rollv v s t aa, rollv v s t bb)
  where
    (_,v,s,t) = rvvvvst rv
    aa' = rollv v s t aa
    rollv v s t aa = aa `mul` unit (ssgl v t) `add` 
          llaa [(ss `sminus` ssgl v s `sunion` ssgl v t, c) | (ss,c) <- aall (aa `mul` unit (ssgl v s))] `add` 
          (aa `sub` (aa `mul` (unit (ssgl v s) `add` unit (ssgl v t))))
    rvvvvst = rollValuesSetVariableVariableValueValue
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sub xx yy = fromJust $ pairHistogramsSubtract xx yy
    mul = pairHistogramsMultiply
    llaa ll = fromJust $ listsHistogram ll
    aall = histogramsList
    sunion = pairStatesUnionLeft
    sminus ss rr = qqss $ ssqq ss `minus` ssqq rr
    qqss = llss . Set.toList
    ssqq = Set.fromList . ssll
    unit ss = fromJust $ setStatesHistogramUnit (Set.singleton ss)
    ssll = statesList
    llss = listsState
    ssgl = stateSingleton
    minus = Set.difference

systemsRollValuer_1 :: System -> RollValue -> Histogram -> Histogram -> (Histogram, Histogram) 
systemsRollValuer_1 uu rv aa _ = ((aa `rvmul` rv), ind (aa `rvmul` rv))
  where
    rvmul aa rv = aa `rmul` vrrr uu rv
    vrrr = systemsRollValuesRoll
    rmul aa rr = rollsHistogramsRoll rr aa
    ind = histogramsIndependent

rollValueAlignmenter_u :: RollValue -> Map.Map Variable (Map.Map State Double) -> Double -> Histogram -> Histogram -> 
  (Double, Histogram, Histogram) 
rollValueAlignmenter_u rv yy a aa bb 
  | s==t = (a, aa, bb)
  | otherwise = (a', aa', bb')
  where
    (vv,v,s,t) = rvvvvst rv
    (aa',bb') = rollValuer rv aa bb
    r' = sumfacln (aa' `mul` unit (ssgl v t)) - 
         sumfacln (bb' `mul` unit (ssgl v t))
    a' = a - (yy Map.! v Map.! ssgl v s) - (yy Map.! v Map.! ssgl v t) + r'
    rvvvvst = rollValuesSetVariableVariableValueValue
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    mul = pairHistogramsMultiply
    unit ss = fromJust $ setStatesHistogramUnit (Set.singleton ss)
    ssgl = stateSingleton

systemsRollValueAlignmenter_1 :: System -> RollValue -> Map.Map Variable (Map.Map State Double) -> 
  Double -> Histogram -> Histogram -> 
  (Double, Histogram, Histogram) 
systemsRollValueAlignmenter_1 uu rv _ _ aa _ = (algn aa', aa', ind aa')
  where
    aa' = aa `rvmul` rv
    rvmul aa rv = aa `rmul` vrrr uu rv
    vrrr = systemsRollValuesRoll
    rmul aa rr = rollsHistogramsRoll rr aa
    algn = histogramsAlignment
    ind = histogramsIndependent

parametersRoller :: 
  Integer -> Set.Set ([Set.Set (State,Int)], Histogram, Histogram) -> Maybe (Set.Set [Set.Set (State,Int)])
parametersRoller pmax qq
  | pmax < 0 = Nothing
  | otherwise = Just $ llqq [nn' | (nn',_,_) <- qqll (topd pmax (rollb mm mm))]
  where
    mm = llmm [((nn,rraa,rrbb),(a-b)/c) | (nn,aa,bb) <- qqll qq, let a = algn aa, let b = algn bb,
               let w = fromIntegral (product [card (ran ii) | ii <- nn]), let m = fromIntegral (length nn),
               let c = w ** (1/m), let rraa = (a, aa, ind aa), let rrbb = (b, bb, ind bb)]
    rollb qq pp
      | mm /= empty = rollb mm (pp `union` mm)
      | otherwise = pp         
      where
        mm = top pmax $ llmm [((nn',rraa',rrbb'),(a'-b')/c') | ((nn,rraa,rrbb),_) <- mmll qq, 
               let vv = llqq (map VarIndex [0 .. length nn - 1]), 
               let (_,aa,aaxx) = rraa, let (_,bb,bbxx) = rrbb,
               let yyaa = rals nn aa aaxx, let yybb = rals nn bb bbxx,
               (v,ii) <- zip [0..] nn, card (ran ii) > 2, s <- qqll (ran ii), t <- qqll (ran ii), s > t, 
               let nn' = take v nn ++ [(ii `join` sgl (s,t))] ++ drop (v+1) nn,              
               let rv = vvvstrv (vv, VarIndex v, ValIndex s, ValIndex t),
               let rraa' = algner rv yyaa rraa, let rrbb' = algner rv yybb rrbb,
               let (a',_,_) = rraa', let (b',_,_) = rrbb',
               let w = fromIntegral (product [card (ran ii') | ii' <- nn']), let m = fromIntegral (card vv),
               let c' = w ** (1/m)]
    rals nn aa aaxx = llmm [(v, llmm [(ss, sumfacln (aa `mul` unit (sgl ss)) - sumfacln (aaxx `mul` unit (sgl ss))) | 
                              u <- (map ValIndex . snd . unzip . qqll) ii, let ss = ssgl v u]) | 
                               (v,ii) <- zip (map VarIndex [0..]) nn]
    algner rv yy (a,aa,aaxx) = rollValueAlignmenter_u rv yy a aa aaxx
    vvvstrv xx = fromJust $ setVariablesVariablesValuesValuesRollValue xx
    algn = histogramsAlignment
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    ind = histogramsIndependent
    mul = pairHistogramsMultiply
    states = histogramsStates
    red aa vv = setVarsHistogramsReduce vv aa
    unit qq = fromJust $ setStatesHistogramUnit qq
    aall = histogramsList
    vars = histogramsVars
    ssgl = stateSingleton
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    --join = pairRelationsJoinOuter
    join xx yy = Set.fromList (
      [(s1,t2) | (s1,t1) <- Set.toList xx, (s2,t2) <- Set.toList yy, s2==t1] ++
      [(s1,t1) | (s1,t1) <- Set.toList xx, t1 `Set.notMember` (dom yy)])
    ran = llqq . snd . unzip . qqll
    dom = llqq . fst . unzip . qqll
    empty = Map.empty
    llmm :: forall k a. (Ord k) => [(k, a)] -> Map.Map k a
    llmm = Map.fromList
    mmll :: forall k a. Map.Map k a -> [(k, a)]
    mmll = Map.toList
    union = Map.union
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    card = Set.size
    sgl = Set.singleton

parametersSystemsLayererHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Histogram -> Histogram -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererHighest wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && vv `subset` vars aa) = Nothing
  | otherwise = Just $ layer vv uu fudEmpty Map.empty aa aarr f 1
  where
    layer vv uu ff mm xx xxrr f l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer vv uu' gg mm' xx' xxrr' f (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | ((kk,bb,bbrr),y1) <- qqll (buildfftup uu vv ff xx xxrr), 
               nn <- qqll (roller (parter uu kk bb bbrr y1)), ii <- nn] [1..], 
               let w = VarPair (VarPair (VarInt f, VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh
        mm' = buildffdervar uu' vv gg xx' xxrr'
    buildfftup uu vv ff xx xxrr = fromJust $ parametersSystemsBuilderTuple xmax omax bmax mmax uu vv ff xx xxrr
    parter uu kk bb bbrr y1 = fromJust $ parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
    roller qq = fromJust $ parametersRoller pmax qq
    buildffdervar uu vv ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
                                       parametersSystemsBuilderDerivedVarsHighest wmax omax uu vv ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsLayererHighest_1 :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Histogram -> Histogram -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererHighest_1 wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && vv `subset` vars aa) = Nothing
  | otherwise = Just $ layer vv uu fudEmpty Map.empty aa aarr f 1
  where
    layer vv uu ff mm xx xxrr f l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer vv uu' gg mm' xx' xxrr' f (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | (kk,bb,bbrr) <- qqll (buildfftup uu vv ff xx xxrr), 
               nn <- qqll (roller (parter uu kk bb bbrr)), ii <- nn] [1..], 
               let w = VarPair (VarPair (VarInt f, VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh
        mm' = buildffdervar uu' vv gg xx' xxrr'
    buildfftup uu vv ff xx xxrr = fromJust $ parametersSystemsBuilderTuple_1 xmax omax bmax mmax uu vv ff xx xxrr
    parter uu kk bb bbrr = fromJust $ parametersSystemsPartitioner_1 mmax umax pmax uu kk bb bbrr
    roller qq = fromJust $ parametersRoller pmax qq
    buildffdervar uu vv ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
                                       parametersSystemsBuilderDerivedVarsHighest wmax omax uu vv ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsLayererMaximumRollHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Histogram -> Histogram -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererMaximumRollHighest wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && vv `subset` vars aa) = Nothing
  | otherwise = Just $ layer vv uu fudEmpty Map.empty aa aarr f 1
  where
    layer vv uu ff mm xx xxrr f l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer vv uu' gg mm' xx' xxrr' f (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | ((kk,bb,bbrr),y1) <- qqll (buildfftup uu vv ff xx xxrr), 
               pp <- qqll (parter uu kk bb bbrr y1), nn <- qqll (roller (sgl pp)), ii <- nn] [1..], 
               let w = VarPair (VarPair (VarInt f, VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh
        mm' = buildffdervar uu' vv gg xx' xxrr'
    buildfftup uu vv ff xx xxrr = fromJust $ parametersSystemsBuilderTuple xmax omax bmax mmax uu vv ff xx xxrr
    parter uu kk bb bbrr y1 = fromJust $ parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
    roller qq = fromJust $ parametersRoller 1 qq
    buildffdervar uu vv ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
                                       parametersSystemsBuilderDerivedVarsHighest wmax omax uu vv ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsLayererMaximumRollExcludedSelfHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Histogram -> Histogram -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererMaximumRollExcludedSelfHighest wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && vv `subset` vars aa) = Nothing
  | otherwise = Just $ layer vv uu fudEmpty Map.empty aa aarr f 1
  where
    layer vv uu ff mm xx xxrr f l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer vv uu' gg mm' xx' xxrr' f (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | ((kk,bb,bbrr),y1) <- qqll (buildfftup uu vv ff xx xxrr), 
               pp <- qqll (parter uu kk bb bbrr y1), nn <- qqll (roller (sgl pp)), ii <- nn, rancd ii < domcd ii] [1..], 
               let w = VarPair (VarPair (VarInt f, VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh
        mm' = buildffdervar uu' vv gg xx' xxrr'
    buildfftup uu vv ff xx xxrr = fromJust $ 
      parametersSystemsBuilderTupleNoSumlayerMultiEffective xmax omax bmax mmax uu vv ff xx xxrr
    parter uu kk bb bbrr y1 = fromJust $ parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
    roller qq = fromJust $ parametersRoller 1 qq
    buildffdervar uu vv ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
      parametersSystemsBuilderDerivedVarsHighestNoSumlayerIncludeHidden wmax omax uu vv ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    domcd = Set.size . Set.map fst
    rancd = Set.size . Set.map snd
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsLayererMaximumRollExcludedSelfHighest_1 :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Histogram -> Histogram -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererMaximumRollExcludedSelfHighest_1 wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && vv `subset` vars aa) = Nothing
  | otherwise = Just $ layer vv uu fudEmpty Map.empty aa aarr f 1
  where
    layer vv uu ff mm xx xxrr f l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer vv uu' gg mm' xx' xxrr' f (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | ((kk,bb,bbrr),y1) <- qqll (buildfftup uu vv ff xx xxrr), 
               pp <- qqll (parter uu kk bb bbrr y1), nn <- qqll (roller (sgl pp)), ii <- nn, rancd ii < domcd ii] [1..], 
               let w = VarPair (VarPair (VarInt f, VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh
        mm' = buildffdervar uu' vv gg xx' xxrr'
    buildfftup uu vv ff xx xxrr = fromJust $ parametersSystemsBuilderTupleNoSumlayer xmax omax bmax mmax uu vv ff xx xxrr
    parter uu kk bb bbrr y1 = fromJust $ parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
    roller qq = fromJust $ parametersRoller 1 qq
    buildffdervar uu vv ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
      parametersSystemsBuilderDerivedVarsHighestNoSumlayerIncludeHidden wmax omax uu vv ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    domcd = Set.size . Set.map fst
    rancd = Set.size . Set.map snd
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsDecomperHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Set.Set Variable -> Histogram -> 
  Maybe (System, DecompFud)
parametersSystemsDecomperHighest wmax lmax xmax omax bmax mmax umax pmax mult seed uu vv aa
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | not (vars aa `subset` uvars uu && vv `subset` vars aa) = Nothing
  | otherwise = Just $ decomp uu emptyTree 1 seed
  where
    decomp uu zz f s
      | zz == emptyTree && (ffr == fudEmpty || nnr == mempty || ar <= 0) = (uu, decompFudEmpty)
      | zz == emptyTree = decomp uur zzr (f+1) (s + mult)
      | mm == [] = (uu, zzdf (zztrim zz)) 
      | otherwise = decomp uuc zzc (f+1) (s + mult)
      where
        aarr = ashuffle aa s mult
        (uur,ffr,nnr) = layerer uu aa aarr f
        (ar,kkr) = maxd nnr
        ffr' = if ar > 0 then depends ffr kkr else fudEmpty
        zzr = tsgl (stateEmpty,ffr')
        mm = [(size bb,nn,ss,bb) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)),
                 let xx = hhc `union` rrc `add` unit ss,
                 let bb = apply vv vv xx aa,
                 size bb > 0]
        (_,nn,ss,bb) = last $ sort mm
        bbrr = ashuffle bb s mult
        (uuc,ffc,nnc) = layerer uu bb bbrr f
        (ac,kkc) = maxd nnc
        ffc' = if ac > 0 then depends ffc kkc else fudEmpty
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc')])
    layerer uu aa aarr f = fromJust $ 
      parametersSystemsLayererHighest wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    depends = fudsVarsDepends
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
    maxd mm = if mm /= mempty then (head $ take 1 $ reverse $ sort $ flip $ mmll mm) else (0,empty)
    bigcup = setSetsUnion
    dom = relationsDomain
    mempty = Map.empty
    mmll = Map.toList
    minus = Set.difference
    add qq x = Set.insert x qq
    qqll = Set.toList
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    flip = map (\(a,b) -> (b,a))

parametersSystemsDecomperMaximumRollExcludedSelfHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Set.Set Variable -> Histogram -> 
  Maybe (System, DecompFud)
parametersSystemsDecomperMaximumRollExcludedSelfHighest wmax lmax xmax omax bmax mmax umax pmax mult seed uu vv aa
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | not (vars aa `subset` uvars uu && vv `subset` vars aa) = Nothing
  | otherwise = Just $ decomp uu emptyTree 1 seed
  where
    decomp uu zz f s
      | zz == emptyTree && (ffr == fudEmpty || nnr == mempty || ar <= 0) = (uu, decompFudEmpty)
      | zz == emptyTree = decomp uur zzr (f+1) (s + mult)
      | mm == [] = (uu, zzdf (zztrim zz)) 
      | otherwise = decomp uuc zzc (f+1) (s + mult)
      where
        aarr = ashuffle aa s mult
        (uur,ffr,nnr) = layerer uu aa aarr f
        (ar,kkr) = maxd nnr
        ffr' = if ar > 0 then depends ffr kkr else fudEmpty
        zzr = tsgl (stateEmpty,ffr')
        mm = [(size bb,nn,ss,bb) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)),
                 let xx = hhc `union` rrc `add` unit ss,
                 let bb = apply vv vv xx aa,
                 size bb > 0]
        (_,nn,ss,bb) = last $ sort mm
        bbrr = ashuffle bb s mult
        (uuc,ffc,nnc) = layerer uu bb bbrr f
        (ac,kkc) = maxd nnc
        ffc' = if ac > 0 then depends ffc kkc else fudEmpty
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc')])
    layerer uu aa aarr f = fromJust $ 
      parametersSystemsLayererMaximumRollExcludedSelfHighest wmax lmax xmax omax bmax mmax umax pmax uu vv aa aarr f
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    depends = fudsVarsDepends
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
    maxd mm = if mm /= mempty then (head $ take 1 $ reverse $ sort $ flip $ mmll mm) else (0,empty)
    bigcup = setSetsUnion
    dom = relationsDomain
    mempty = Map.empty
    mmll = Map.toList
    minus = Set.difference
    add qq x = Set.insert x qq
    qqll = Set.toList
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    flip = map (\(a,b) -> (b,a))

parametersSystemsSamplesShufflesLevelsFudsFunctionInitialTuple :: 
  Integer -> System -> Histogram -> Histogram -> Set.Set Variable -> Fud -> Fud -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesLevelsFudsFunctionInitialTuple xmax uu aa aarr vvg ffg ff
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffg `subset` uvars uu && fvars ff `subset` uvars uu &&
    vvg `subset` vars aa && fund ffg `subset` vars aa = Just $ llmm 
      [(kk, algn (apply vv kk xx aa) - algn (apply vv kk xx aarr)) |
        w <- qqll (if ff /= fudEmpty then ww else qq), u <- qqll qq, u /= w, 
        let kk = llqq [w,u], vol uu kk <= xmax]
  | otherwise = Nothing
  where
    ww = fder ff
    vv = vars aa
    qq = fvars ff `minus` fvars ffg `cup` vvg `cup` fder ffg
    xx = fhis ff `Set.union` fhis ffg
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fder = fudsDerived
    fund = fudsUnderlying
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    cup = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodTuple :: 
  Integer -> System -> Histogram -> Histogram -> Set.Set Variable -> Fud -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodTuple xmax uu aa aarr vvg ffg ff bb
  | xmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffg `subset` uvars uu && fvars ff `subset` uvars uu &&
    vvg `subset` vars aa && fund ffg `subset` vars aa = Just $ llmm 
      [(jj, algn (apply vv jj xx aa) - algn (apply vv jj xx aarr)) |
        kk <- keys bb, w <- qqll (qq `minus` kk),  
        let jj = kk `add` w, vol uu jj <= xmax]
  | otherwise = Nothing
  where
    vv = vars aa
    qq = fvars ff `minus` fvars ffg `cup` vvg `cup` fder ffg
    xx = fhis ff `Set.union` fhis ffg
    algn = histogramsAlignment
    depends = fudsVarsDepends
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    fder = fudsDerived
    fvars = fudsVars
    fund = fudsUnderlying
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    cup = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserTuple :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Set.Set Variable -> Fud -> Fud ->  
  Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)]
parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserTuple xmax omax uu aa aarr vvg ffg ff
  | xmax < 0 = Nothing
  | omax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffg `subset` uvars uu && fvars ff `subset` uvars uu &&
    vvg `subset` vars aa && fund ffg `subset` vars aa = Just $ 
      opt (zzcstupneigh xmax uu aa aarr vvg ffg ff) (topff omax ff) (zzcstupinit xmax uu aa aarr vvg ffg ff)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcstupinit xmax uu aa aarr vvg ffg ff =  fromJust $
      parametersSystemsSamplesShufflesLevelsFudsFunctionInitialTuple xmax uu aa aarr vvg ffg ff
    zzcstupneigh xmax uu aa aarr vvg ffg ff bb = fromJust $
      parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodTuple xmax uu aa aarr vvg ffg ff bb
    fvars = fudsVars
    fund = fudsUnderlying
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    layer = fudsSetVarsLayer
    mmll = Map.toList
    llmm = Map.fromList
    isempty = Map.null
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesLevelsFudsFunctionInitialDerivedHighest :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Fud -> Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesLevelsFudsFunctionInitialDerivedHighest wmax uu aa aarr ffg ff
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        w <- qqll (fder ff), 
        u <- qqll (fvars ff `minus` vars aa `minus` fvars ffg `minus` fvars (depends ff (sgl w))), u /= w,  
        let jj = llqq [w,u], let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    algn = histogramsAlignment
    depends = fudsVarsDepends
    llff = fromJust . setTransformsFud . Set.fromList
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    minus = Set.difference
    subset = Set.isSubsetOf
    sgl = Set.singleton
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList

parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodDerived :: 
  Integer -> System -> Histogram -> Histogram -> Fud -> Fud -> Map.Map (Set.Set Variable) Double -> 
  Maybe (Map.Map (Set.Set Variable) Double)
parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodDerived wmax uu aa aarr ffg ff bb
  | wmax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $ llmm 
      [(jj, (algn (aa `fmul` gg) - algn (aarr `fmul` gg)) / (x' ** (1/m))) |
        kk <- keys bb, w <- qqll (qq `minus` kk),  
        let jj = kk `add` w, let x = vol uu jj, x <= wmax, 
        let gg = depends ff jj, fder gg == jj,
        let x' = fromIntegral x, let m = fromIntegral (Set.size jj)]
  | otherwise = Nothing
  where
    qq = fvars ff `minus` vars aa `minus` fvars ffg
    algn = histogramsAlignment
    depends = fudsVarsDepends
    fmul aa ff = fudsHistogramsApply ff aa
    fder = fudsDerived
    fvars = fudsVars
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    keys = Map.keys
    add xx x = x `Set.insert` xx
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    llmm = Map.fromList

parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserDerivedHighest :: 
  Integer -> Integer -> System -> Histogram -> Histogram -> Fud ->  Fud ->  
  Maybe [(Map.Map (Set.Set Variable) Double, Map.Map (Set.Set Variable) Double)]
parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserDerivedHighest wmax omax uu aa aarr ffg ff
  | wmax < 0 = Nothing
  | omax < 0 = Nothing
  | fvars ff `subset` uvars uu = Just $
      opt (zzcsdderneigh wmax uu aa aarr ff) (topff omax ff) (zzcsdderinit wmax uu aa aarr ff)
  | otherwise = Nothing
  where
    opt pp ii rr = let qq = ii rr in (qq, rr) : ls qq
      where
        ls yy = if isempty yy then [] else let nn = pp yy; qq = ii nn in (qq, nn) : ls qq
    zzcsdderinit wmax uu aa aarr ff =  fromJust $
      parametersSystemsSamplesShufflesLevelsFudsFunctionInitialDerivedHighest wmax uu aa aarr ffg ff
    zzcsdderneigh wmax uu aa aarr ff bb = fromJust $
      parametersSystemsSamplesShufflesLevelsFudsFunctionNeighbourhoodDerived wmax uu aa aarr ffg ff bb
    fvars = fudsVars
    vars = histogramsVars
    uvars = systemsVars
    topff amax ff mm = llmm $ flipff $ take (fromInteger amax) $ sortdescff ff $ mmll mm
    flipff = map (\((a,_),b) -> (b,a))
    sortdescff ff ll = reverse $ sort $ map (\(kk,a) -> ((a,-(sumlayer ff kk)),kk)) ll 
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    isempty = Map.null
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    sgl = Set.singleton
    subset = Set.isSubsetOf

parametersSystemsSamplesShufflesLevelsListVariablesSearcherFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Histogram -> Histogram -> Set.Set Variable -> Fud -> [Variable] ->
  Maybe ([Fud],(System,[Variable]))
parametersSystemsSamplesShufflesLevelsListVariablesSearcherFudHighest 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffg `subset` uvars uu && vvg `subset` vars aa && fund ffg `subset` vars aa =
    Just $ (pp,(uu',ll'))
  | otherwise = Nothing
  where
    qq = ls fudEmpty uu ll lmax 0
    pp = fst $ unzip $ qq
    (uu',ll') = if qq /= [] then (last $ snd $ unzip qq) else (uu,ll)
    ls _ _ _ 0 _ = []
    ls ff uu ll h a = if b > a then ((gg,(uu',ll')) : ls gg uu' ll' (h-1) b) else []
      where
        yy = ls2 (topff (bmax `div` mmax) ff (zzffcstupopt xmax omax uu aa aarr vvg ffg ff)) uu ll
        gg = foldl funion ff (elems (llmm [(rr, ttff tt `funion` depends ffg (und tt)) | 
                                             hh <- (concat $ fst $ unzip yy), 
                                             w <- qqll (fder hh), let tt = fftt (depends hh (sgl w)), 
                                             let rr = ttpp tt, rr `notmem` nn]))
        (uu',ll') = if yy /= [] then (last $ snd $ unzip yy) else (uu,ll)
        b = maxr $ zzffcsdderhighopt wmax omax uu' aa aarr ffg gg
        nn = llqq [ttpp tt | tt <- (qqll . ffqq) ff]
        ls2 [] _ _ = []
        ls2 (kk:bb) uu ll = (top pmax (fst $ unzip xx),(uu',ll')) : ls2 bb uu' ll'
          where
            (xx,(uu',ll')) = zzllcsddecoptw mmax umax pmax uu aa aarr (ff `funion`ffg) kk ll
    zzffcstupopt xmax omax uu aa aarr vvg ffg ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserTuple xmax omax uu aa aarr vvg ffg ff
    zzllcsddecoptw mmax umax pmax uu aa aarr ff kk ll = fromJust $
      parametersSystemsSamplesShufflesFudsTuplesListVariablesFunctionOptimiserFudDecrementingLimitedValency 
        mmax umax pmax uu aa aarr ff kk ll
    zzffcsdderhighopt wmax omax uu aa rr ffg ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ffg ff
    depends ff ww = fudsVarsDepends ff ww
    fftt = fudsTransform
    llff = fromJust . setTransformsFud . Set.fromList
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    fvars = fudsVars
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    und = transformsUnderlying
    ttff = fromJust . setTransformsFud . Set.singleton
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

parametersSystemsSamplesShufflesLevelsListVariablesInducerFudHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer ->
  System -> Histogram -> Histogram -> Set.Set Variable -> Fud -> [Variable] ->
  Maybe ((Fud,Fud),(System,[Variable]))
parametersSystemsSamplesShufflesLevelsListVariablesInducerFudHighest 
  wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | vars aa `subset` uvars uu && fvars ffg `subset` uvars uu && vvg `subset` vars aa && fund ffg `subset` vars aa =
    Just $ ((depends ff kk, ff),(uu',ll'))
  | otherwise = Nothing
  where
    (nn,(uu',ll')) = zzllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll
    ff = if nn /= [] then last nn else fudEmpty
    kk = maxdff ff $ zzffcsdderhighopt wmax omax uu' aa aarr ffg ff
    zzllfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll = fromJust $
      parametersSystemsSamplesShufflesLevelsListVariablesSearcherFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll
    zzffcsdderhighopt wmax omax uu aa rr ffg ff = fst $ unzip $ fromJust $ 
      parametersSystemsSamplesShufflesLevelsFudsFunctionOptimiserDerivedHighest wmax omax uu aa rr ffg ff
    depends = fudsVarsDepends
    fvars = fudsVars
    fund = fudsUnderlying
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

-- AYOC - no check of zzg
parametersSystemsSamplesLevelsListVariablesInducerDecompFudHighest_u :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> Tree (Integer, Set.Set Variable, Fud) -> [Variable] ->
  Maybe (DecompFud,(System,[Variable]))
parametersSystemsSamplesLevelsListVariablesInducerDecompFudHighest_u
  lmax xmax omax bmax mmax umax pmax mult seed uu aa zzg ll
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | vars aa `subset` uvars uu = Just $ dec emptyTree uu ll seed
  | otherwise = Nothing
  where
    dec zz uu ll s
      | zz == emptyTree && ffr == fudEmpty = (decompFudEmpty, (uu, ll))
      | zz == emptyTree = dec zzr uur llr (s + mult)
      | mm == [] = (zzdf (zztrim zz), (uu, ll)) 
      | otherwise = dec zzc uuc llc (s + mult)
      where
        aarr = ashuffle aa s mult
        (ffr,(uur,llr)) = level uu aa aarr zzg ll
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb,bbrr) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 let bbrr = ashuffle bb s mult,
                 size bb > 0]
        (_,nn,ss,bb,bbrr) = last $ sort mm
        (ffc,(uuc,llc)) = level uu bb bbrr zzg ll
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    level uu aa aarr (Tree ttg) ll = foldl next (fudEmpty,(uu,ll)) (Map.toList ttg)
      where       
        next (ff,(uu,ll)) ((wmaxg,vvg,ffg),xxg) = (ff `funion` gg,(uu',ll'))
          where
            (ffh,(uuh,llh)) = level uu aa aarr xxg ll
            ((gg,_),(uu',ll')) = 
              iillfudshigh wmaxg lmax xmax omax bmax mmax umax pmax uuh aa aarr vvg (ffg `funion` ffh) llh
    iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll = fromJust $
      parametersSystemsSamplesShufflesLevelsListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    fhis = fudsSetHistogram
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
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

-- AYOC - no check of zzg
parametersSystemsSamplesLevelsListVariablesInducerDecompFudHighestGoodness_u :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Histogram -> Tree (Integer, Set.Set Variable, Fud) -> 
  (System -> Histogram -> Histogram -> Fud -> Double) -> 
  [Variable] ->
  Maybe (DecompFud,(System,[Variable]))
parametersSystemsSamplesLevelsListVariablesInducerDecompFudHighestGoodness_u
  lmax xmax omax bmax mmax umax pmax mult seed uu aa zzg good ll
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | vars aa `subset` uvars uu = Just $ dec emptyTree uu ll seed
  | otherwise = Nothing
  where
    dec zz uu ll s
      | zz == emptyTree && ffr == fudEmpty = (decompFudEmpty, (uu, ll))
      | zz == emptyTree = dec zzr uur llr (s + mult)
      | mm == [] = (zzdf (zztrim zz), (uu, ll)) 
      | otherwise = dec zzc uuc llc (s + mult)
      where
        aarr = ashuffle aa s mult
        (_,(ffr,(uur,llr))) = best uu aa aarr zzg ll
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb,bbrr) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)), 
                 let bb = apply (vars aa) (vars aa) (hhc `union` rrc `add` unit ss) aa,
                 let bbrr = ashuffle bb s mult,
                 size bb > 0]
        (_,nn,ss,bb,bbrr) = last $ sort mm
        (_,(ffc,(uuc,llc))) = best uu bb bbrr zzg ll
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    best uu aa aarr (Tree ttg) ll
      | qq /= [] && gg' /= fudEmpty && g' > g = (g', (gg',(uu'',ll'')))
      | qq /= [] = (g, (gg,(uu',ll')))
      | otherwise = (0,(fudEmpty,(uu,ll)))
      where       
        qq = [(good uu' aa aarr gg, (gg,(uu',ll')), xxg) | ((wmaxg,vvg,ffg),xxg) <- Map.toList ttg, 
               let ((gg,_),(uu',ll')) = iillfudshigh wmaxg lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll]
        (g, (gg,(uu',ll')), xxg) = last $ sort qq
        (g', (gg',(uu'',ll''))) = best uu' aa aarr xxg ll'            
    iillfudshigh wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll = fromJust $
      parametersSystemsSamplesShufflesLevelsListVariablesInducerFudHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu aa aarr vvg ffg ll
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    fhis = fudsSetHistogram
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
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

parametersSystemsBuilderTupleLevel :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set ((Set.Set Variable, Histogram, Histogram),Double))
parametersSystemsBuilderTupleLevel xmax omax bmax mmax uu vvg ffg ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb vv (init vv) Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `minus` fvars ffg `union` vv) (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    vv = vvg `union` fder ffg
    init vv = llmm [(((sgl w, histogramEmpty, histogramEmpty),0),(0,0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | (((kk,_,_),_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [(((jj, bb, bbrr), a1-b1), (a1-a2-b1+b2, -l, -b1+b2, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, let l =  sumlayer ff jj, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, 
          let a1 = sumfacln bb, let a2 = sumfacln (ind bb), 
          let b1 = sumfacln bbrr, let b2 = sumfacln (ind bbrr)]
    final nn = llmm [(((kk,aa,bb),y),a) | (((kk,aa,bb),y),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    ind = histogramsIndependent
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    sumlayer ff kk = sum [layer ff (sgl w) | w <- qqll kk]
    layer = fudsSetVarsLayer
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderTupleLevelNoSumlayer :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set ((Set.Set Variable, Histogram, Histogram),Double))
parametersSystemsBuilderTupleLevelNoSumlayer xmax omax bmax mmax uu vvg ffg ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb vv (init vv) Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `minus` fvars ffg `union` vv) (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    vv = vvg `union` fder ffg
    init vv = llmm [(((sgl w, histogramEmpty, histogramEmpty),0),(0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | (((kk,_,_),_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [(((jj, bb, bbrr), a1-b1), (a1-a2-b1+b2, -b1+b2, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, 
          let a1 = sumfacln bb, let a2 = sumfacln (ind bb), 
          let b1 = sumfacln bbrr, let b2 = sumfacln (ind bbrr)]
    final nn = llmm [(((kk,aa,bb),y),a) | (((kk,aa,bb),y),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    ind = histogramsIndependent
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderTupleLevelNoSumlayerMultiEffective :: 
  Integer -> Integer -> Integer -> Integer -> System -> Set.Set Variable -> Fud -> Fud -> Histogram -> Histogram ->  
  Maybe (Set.Set ((Set.Set Variable, Histogram, Histogram),Double))
parametersSystemsBuilderTupleLevelNoSumlayerMultiEffective xmax omax bmax mmax uu vvg ffg ff xx xxrr
  | xmax < 0 || omax < 0 || mmax < 1 || bmax < mmax = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | vv' == empty = Just $ Set.empty
  | ff == fudEmpty = 
      Just $ topd (bmax `div` mmax) $ buildb vv' (init vv') Map.empty
  | fvars ff `subset` vars xx = 
      Just $ topd (bmax `div` mmax) $ buildb (fvars ff `minus` fvars ffg `union` vv') (init (fder ff)) Map.empty
  | otherwise = Nothing
  where
    vv = vvg `union` fder ffg
    vv' = meff xx vv
    init vv = llmm [(((sgl w, histogramEmpty, histogramEmpty),0),(0,0,0)) | w <- qqll vv]
    buildb ww qq nn = if mm /= Map.empty then buildb ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | (((kk,_,_),_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [(((jj, bb, bbrr), a1-b1), (a1-a2-b1+b2, -b1+b2, -u)) |
          jj <- qqll pp, let u = vol uu jj, u <= xmax, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj, 
          let a1 = sumfacln bb, let a2 = sumfacln (ind bb), 
          let b1 = sumfacln bbrr, let b2 = sumfacln (ind bbrr)]
    final nn = llmm [(((kk,aa,bb),y),a) | (((kk,aa,bb),y),a) <- mmll nn, card kk > 1]
    meff xx vv = Set.filter (\v -> acard (eff (xx `red` sgl v)) > 1) vv
    fder = fudsDerived
    fvars = fudsVars
    sumfacln aa = sum [facln (fromRational c) | (_,c) <- aall aa]
    facln x = logGamma (x + 1)
    aall = histogramsList
    ind = histogramsIndependent
    red aa vv = setVarsHistogramsReduce vv aa
    eff = histogramsEffective
    acard = histogramsCardinality
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    topd amax mm = llqq $ snd $ unzip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    union = Set.union
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsBuilderDerivedVarsLevelHighestNoSumlayerIncludeHidden :: 
  Integer -> Integer -> System -> Set.Set Variable -> Fud -> Fud -> Histogram -> Histogram ->  
  Maybe (Map.Map (Set.Set Variable, Histogram, Histogram) Double)
parametersSystemsBuilderDerivedVarsLevelHighestNoSumlayerIncludeHidden wmax omax uu vv ffg ff xx xxrr
  | wmax < 0 || omax < 0 = Nothing
  | not (vars xx `subset` uvars uu && vars xx == vars xxrr && vv `subset` vars xx) = Nothing
  | not (fvars ff `subset` uvars uu) = Nothing
  | otherwise = Just $ maxfst $ buildd (fvars ff `minus` vv `minus` fvars ffg) (init (fder ff)) Map.empty
  where
    init vv = llmm [((sgl w, histogramEmpty, histogramEmpty),(0,0,0)) | w <- qqll vv]
    buildd ww qq nn = if mm /= Map.empty then buildd ww mm (nn `Map.union` mm) else (final nn) 
      where
        pp = llqq [jj | ((kk,_,_),_) <- mmll qq, w <- qqll (ww `minus` kk), let jj = kk `add` w]
        mm = top omax $ llmm [((jj, bb, bbrr), ((a-b)/c,-b/c,-u)) |
          jj <- qqll pp, let u = vol uu jj, u <= wmax, 
          let bb = xx `red` jj, let bbrr = xxrr `red` jj,
          let u' = fromIntegral u, let m = fromIntegral (Set.size jj),
          let a = algn bb, let b = algn bbrr, let c = u' ** (1/m)]
    final nn = llmm [((kk,aa,bb),a) | ((kk,aa,bb),a) <- mmll nn, card kk > 1]
    fder = fudsDerived
    fvars = fudsVars
    algn = histogramsAlignment
    red aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    top amax mm = llmm $ flip $ take (fromInteger amax) $ reverse $ sort $ flip $ mmll mm
    maxfst mm = llmm $ map (\((a,_,_),x) -> (x,a)) $ take 1 $ reverse $ sort $ flip $ mmll mm
    flip = map (\(a,b) -> (b,a))
    llmm = Map.fromList
    mmll = Map.toList
    add xx x = x `Set.insert` xx
    minus = Set.difference
    subset = Set.isSubsetOf
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
    sgl = Set.singleton
    empty = Set.empty

parametersSystemsLayererLevelMaximumRollExcludedSelfHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Fud -> Histogram -> Histogram -> Integer -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererLevelMaximumRollExcludedSelfHighest 
  wmax lmax xmax omax bmax mmax umax pmax uu vvg ffg aa aarr f g
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && fvars ffg `subset` uvars uu && 
    vvg `subset` vars aa && fund ffg `subset` vars aa) = Nothing
  | otherwise = Just $ layer uu fudEmpty Map.empty xx xxrr 1
  where
    xx = apply (vars aa) (vars aa `union` fvars ffg) (fhis ffg) aa
    xxrr = apply (vars aa) (vars aa `union` fvars ffg) (fhis ffg) aarr
    layer uu ff mm xx xxrr l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer uu' gg mm' xx' xxrr' (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | ((kk,bb,bbrr),y1) <- qqll (buildfftup uu vvg ffg ff xx xxrr), 
               pp <- qqll (parter uu kk bb bbrr y1), nn <- qqll (roller (sgl pp)), ii <- nn, rancd ii < domcd ii] [1..], 
               let w = VarPair (VarPair (VarPair (VarInt f, VarInt g), VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh `funion` depends ffg (fund hh)
        mm' = buildffdervar uu' (vars aa) ffg gg xx' xxrr'
    buildfftup uu vvg ffg ff xx xxrr = 
      fromJust $ parametersSystemsBuilderTupleLevelNoSumlayerMultiEffective xmax omax bmax mmax uu vvg ffg ff xx xxrr
    parter uu kk bb bbrr y1 = fromJust $ parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
    roller qq = fromJust $ parametersRoller 1 qq
    buildffdervar uu vv ffg ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
      parametersSystemsBuilderDerivedVarsLevelHighestNoSumlayerIncludeHidden wmax omax uu vv ffg ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    depends = fudsVarsDepends
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fund = fudsUnderlying
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    domcd = Set.size . Set.map fst
    rancd = Set.size . Set.map snd
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsLayererLevelMaximumRollExcludedSelfHighest_1 :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  System -> Set.Set Variable -> Fud -> Histogram -> Histogram -> Integer -> Integer ->
  Maybe (System, Fud, Map.Map (Set.Set Variable) Double)
parametersSystemsLayererLevelMaximumRollExcludedSelfHighest_1 
  wmax lmax xmax omax bmax mmax umax pmax uu vvg ffg aa aarr f g
  | wmax < 0 || lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (vars aa `subset` uvars uu && vars aa == vars aarr && fvars ffg `subset` uvars uu && 
    vvg `subset` vars aa && fund ffg `subset` vars aa) = Nothing
  | otherwise = Just $ layer uu fudEmpty Map.empty xx xxrr 1
  where
    xx = apply (vars aa) (vars aa `union` fvars ffg) (fhis ffg) aa
    xxrr = apply (vars aa) (vars aa `union` fvars ffg) (fhis ffg) aarr
    layer uu ff mm xx xxrr l = 
      if l <= lmax && hh /= fudEmpty && (mm == Map.empty || maxr mm' > maxr mm) then 
        layer uu' gg mm' xx' xxrr' (l+1) else (uu,ff,mm) 
      where
        ll = [(tt,(w,ww)) | (ii,b) <- zip [ii | ((kk,bb,bbrr),y1) <- qqll (buildfftup uu vvg ffg ff xx xxrr), 
               pp <- qqll (parter uu kk bb bbrr y1), nn <- qqll (roller (sgl pp)), ii <- nn, rancd ii < domcd ii] [1..], 
               let w = VarPair (VarPair (VarPair (VarInt f, VarInt g), VarInt l), VarInt b), 
               let ww = Set.map (\(_,u) -> (nnww u)) ii, 
               let tt = trans (unit [ss `sunion` ssgl w (nnww u) | (ss,u) <- qqll ii]) (sgl w)]
        ll' = [(tt,(w,ww)) | (tt,(w,ww)) <- ll, 
                and [Set.size ww /= Set.size ww' || und tt /= und tt' || ttpp tt /= ttpp tt' | (tt',(w',ww')) <- ll, w > w']]
        hh = qqff $ llqq $ fst $ unzip ll'
        uu' = uu `uunion` (lluu $ snd $ unzip ll')
        xx' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xx
        xxrr' = apply (vars xx) (vars xx `union` fvars hh) (fhis hh) xxrr
        gg = ff `funion` hh `funion` depends ffg (fund hh)
        mm' = buildffdervar uu' (vars aa) ffg gg xx' xxrr'
    buildfftup uu vvg ffg ff xx xxrr = 
      fromJust $ parametersSystemsBuilderTupleLevelNoSumlayer xmax omax bmax mmax uu vvg ffg ff xx xxrr
    parter uu kk bb bbrr y1 = fromJust $ parametersSystemsPartitioner mmax umax pmax uu kk bb bbrr y1
    roller qq = fromJust $ parametersRoller 1 qq
    buildffdervar uu vv ffg ff xx xxrr = llmm $ map (\((kk,_,_),a) -> (kk,a)) $ mmll $ fromJust $
      parametersSystemsBuilderDerivedVarsLevelHighestNoSumlayerIncludeHidden wmax omax uu vv ffg ff xx xxrr
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    fhis = fudsSetHistogram
    qqff = fromJust . setTransformsFud
    depends = fudsVarsDepends
    ffqq = fudsSetTransform
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fund = fudsUnderlying
    ttpp = transformsPartition
    und = transformsUnderlying
    trans = histogramsSetVarsTransform_u
    unit qq = listsHistogram_u $ map (\ss -> (ss,1)) $ qq
    vars = histogramsVars
    sunion = pairStatesUnionLeft
    ssgl = stateSingleton
    uvars = systemsVars
    uunion = pairSystemsUnion
    lluu = listsSystem_u
    nnww = ValInt . toInteger
    maxr mm = if mm /= Map.empty then (fst $ head $ take 1 $ reverse $ sort $ flip $ mmll mm) else 0
    llmm = Map.fromList
    mmll = Map.toList
    domcd = Set.size . Set.map fst
    rancd = Set.size . Set.map snd
    llqq = Set.fromList
    union = Set.union
    subset = Set.isSubsetOf
    sgl = Set.singleton
    flip = map (\(a,b) -> (b,a))
    qqll = Set.toList

parametersSystemsDecomperLevelMaximumRollExcludedSelfHighest :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Set.Set Variable -> Histogram -> Tree (Integer, Set.Set Variable, Fud) -> 
  Maybe (System, DecompFud)
parametersSystemsDecomperLevelMaximumRollExcludedSelfHighest 
  lmax xmax omax bmax mmax umax pmax mult seed uu vv aa zzg
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | not (vars aa `subset` uvars uu && vv `subset` vars aa) = Nothing
  | not (okLevel zzg) = Nothing
  | otherwise = Just $ decomp uu emptyTree 1 seed
  where
    decomp uu zz f s
      | zz == emptyTree && ffr == fudEmpty = (uu, decompFudEmpty)
      | zz == emptyTree = decomp uur zzr (f+1) (s + mult)
      | mm == [] = (uu, zzdf (zztrim zz)) 
      | otherwise = decomp uuc zzc (f+1) (s + mult)
      where
        aarr = ashuffle aa s mult
        (uur,ffr,_) = level uu aa aarr zzg f 1
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)),
                 let xx = hhc `union` rrc `add` unit ss,
                 let bb = apply vv vv xx aa,
                 size bb > 0]
        (_,nn,ss,bb) = last $ sort mm
        bbrr = ashuffle bb s mult
        (uuc,ffc,_) = level uu bb bbrr zzg f 1
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    level uu aa aarr (Tree ttg) f g = foldl next (uu,fudEmpty,g) (Map.toList ttg)
      where       
        next (uu,ff,g) ((wmaxg,vvg,ffg),xxg) = (uu',ff `funion` gg',gh+1)
          where
            (uuh,ffh,gh) = level uu aa aarr xxg f g
            (uu',gg,nn) = layerer wmaxg uuh vvg (ffg `funion` ffh) aa aarr f gh
            (a,kk) = maxd nn
            gg' = if a > 0 then depends gg kk else fudEmpty
    okLevel zzg = and [wmaxg >= 0 && vvg `subset` vars aa && fvars ffg `subset` uvars uu && fund ffg `subset` vars aa |
                       (wmaxg,vvg,ffg) <- Set.toList (treesElements zzg)]
    layerer wmax uu vvg ffg aa aarr f g = fromJust $ 
      parametersSystemsLayererLevelMaximumRollExcludedSelfHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu vvg ffg aa aarr f g
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    qqff = fromJust . setTransformsFud
    depends = fudsVarsDepends
    ffqq = fudsSetTransform
    fvars = fudsVars
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
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
    maxd mm = if mm /= mempty then (head $ take 1 $ reverse $ sort $ flip $ mmll mm) else (0,empty)
    bigcup = setSetsUnion
    dom = relationsDomain
    mempty = Map.empty
    mmll = Map.toList
    minus = Set.difference
    add qq x = Set.insert x qq
    qqll = Set.toList
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    flip = map (\(a,b) -> (b,a))

parametersSystemsDecomperMaximumRollExcludedSelfHighestGoodness :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> 
  Integer -> Integer ->
  System -> Set.Set Variable -> Histogram -> Tree (Integer, Set.Set Variable, Fud) -> 
  (System -> Histogram -> Histogram -> Fud -> Double) -> 
  Maybe (System, DecompFud)
parametersSystemsDecomperMaximumRollExcludedSelfHighestGoodness 
  lmax xmax omax bmax mmax umax pmax mult seed uu vv aa zzg good
  | lmax < 0 || xmax < 0 || omax < 0 || bmax < 0 || mmax < 1 || umax < 0 || pmax < 0 = Nothing
  | not (isint aa) || mult < 1 = Nothing
  | not (vars aa `subset` uvars uu && vv `subset` vars aa) = Nothing
  | not (okLevel zzg) = Nothing
  | otherwise = Just $ decomp uu emptyTree 1 seed
  where
    decomp uu zz f s
      | zz == emptyTree && ffr == fudEmpty = (uu, decompFudEmpty)
      | zz == emptyTree = decomp uur zzr (f+1) (s + mult)
      | mm == [] = (uu, zzdf (zztrim zz)) 
      | otherwise = decomp uuc zzc (f+1) (s + mult)
      where
        aarr = ashuffle aa s mult
        (_,(uur,ffr)) = best uu aa aarr zzg f 1
        zzr = tsgl (stateEmpty,ffr)
        mm = [(size bb,nn,ss,bb) | (nn,yy) <- qqll (treesPlaces zz), 
                 let rrc = llsthis nn, let hhc = llfhis nn, let (_,ff) = last nn, ff /= fudEmpty,
                 ss <- qqll (cart uu (fder ff) `minus` dom (treesRoots yy)),
                 let xx = hhc `union` rrc `add` unit ss,
                 let bb = apply vv vv xx aa,
                 size bb > 0]
        (_,nn,ss,bb) = last $ sort mm
        bbrr = ashuffle bb s mult
        (_,(uuc,ffc)) = best uu bb bbrr zzg f 1
        zzc = pathsTree $ treesPaths zz `add` (nn ++ [(ss,ffc)])
    best uu aa aarr (Tree ttg) f h
      | qq /= [] && gg' /= fudEmpty && g' > g = (g', (uu'',gg'))
      | qq /= [] = (g, (uu',gg))
      | otherwise = (0,(uu,fudEmpty))
      where       
        qq = [(good uu' aa aarr gg, (gg,uu'), xxg) | ((wmaxg,vvg,ffg),xxg) <- Map.toList ttg, 
               let (uu',ff,nn) = layerer wmaxg uu vvg ffg aa aarr f h,
               let (a,kk) = maxd nn, a > 0, let gg = depends ff kk]
        (g, (gg,uu'), xxg) = last $ sort qq
        (g', (uu'',gg')) = best uu' aa aarr xxg f (h+1)
    okLevel zzg = and [wmaxg >= 0 && vvg `subset` vars aa && fvars ffg `subset` uvars uu && fund ffg `subset` vars aa |
                       (wmaxg,vvg,ffg) <- Set.toList (treesElements zzg)]
    layerer wmax uu vvg ffg aa aarr f g = fromJust $ 
      parametersSystemsLayererLevelMaximumRollExcludedSelfHighest 
        wmax lmax xmax omax bmax mmax umax pmax uu vvg ffg aa aarr f g
    zztrim = pathsTree . Set.map lltrim . treesPaths
    lltrim ll = let (_,ff) = last ll in if ff == fudEmpty then init ll else ll
    llsthis = Set.fromList . map unit . fst . unzip
    llfhis = bigcup . Set.fromList . map fhis . snd . unzip
    zzdf zz = fromJust $ treePairStateFudsDecompFud zz
    qqff = fromJust . setTransformsFud
    depends = fudsVarsDepends
    ffqq = fudsSetTransform
    fvars = fudsVars
    fund = fudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
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
    maxd mm = if mm /= mempty then (head $ take 1 $ reverse $ sort $ flip $ mmll mm) else (0,empty)
    bigcup = setSetsUnion
    dom = relationsDomain
    mempty = Map.empty
    mmll = Map.toList
    minus = Set.difference
    add qq x = Set.insert x qq
    qqll = Set.toList
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    flip = map (\(a,b) -> (b,a))

systemsDecompFudsNullablePracticable :: System -> DecompFud -> Integer -> Maybe Fud
systemsDecompFudsNullablePracticable uu df g 
  | and [okvar v | v <- qqll (dfvars df `minus` dfund df)] = Just $ trff fudEmpty fudEmpty (dfzz df)
  | otherwise = Nothing
  where
    trff :: Fud -> Fud -> Tree (State,Fud) -> Fud
    trff ffp ffc zz = foldl funion fudEmpty [ggc `funion` ffn `funion` trff ff ggc xx | 
      (((rrp,ff),xx),i) <- zip (qqll (rel zz)) [1..], let ggc = cont ffp rrp ffc i, let ffn = nullable ff ggc]
    okvar (VarPair (VarPair (_, v), _)) = v /= gs && v /= gc && v /= gn
    okvar _ = False
    cont :: Fud -> State -> Fud -> Integer -> Fud
    cont ffp rrp ffc i
      | ffp == fudEmpty = fudEmpty
      | ffc == fudEmpty = ffp `funion` ttff tts
      | otherwise = ffc `funion` ffp `funion` ttff tts `funion` ttff ttc
      where
        ((VarPair (VarPair (f,_),_)),_) = head $ ssll rrp
        ws = VarPair (VarPair (f, gs), VarInt i)
        aaso = unit $ sgl $ llss [(ws,uo)]
        aasi = unit $ sgl $ llss [(ws,ui)]
        tts = trans ((unit (cart uu (fder ffp) `minus` sgl rrp) `mul` aaso) `add` (unit (sgl rrp) `mul` aasi)) (sgl ws)
        vc = Set.findMin $ fder ffc
        wc = VarPair (VarPair (f, gc), VarInt i)
        aac = unit $ llqq [llss [(ws,uo),(vc,uo),(wc,uo)],llss [(ws,uo),(vc,ui),(wc,uo)],
                           llss [(ws,ui),(vc,uo),(wc,uo)],llss [(ws,ui),(vc,ui),(wc,ui)]]
        ttc = trans aac (sgl wc)
    nullable :: Fud -> Fud -> Fud
    nullable ff ggc
      | ggc == fudEmpty = ff `funion` qqff (Set.map wwttr (fder ff))
      | otherwise = ggc `funion` ff `funion` qqff (Set.map wwttc (fder ff))
      where
        wwttr w = trans aa (sgl w') 
          where
            VarPair (VarPair (f, _), i) = w
            w' = VarPair (VarPair (f, gn), i)
            aa = unit $ Set.map (\ss -> let [(_,u)] = ssll ss in llss [(w,u),(w',u)]) (cart uu (sgl w))
        wwttc w = trans (aao `add` aai) (sgl w') 
          where
            VarPair (VarPair (f, _), i) = w
            w' = VarPair (VarPair (f, gn), i)
            vc = Set.findMin $ fder ggc
            aao = unit $ Set.map ((\u -> llss [(vc,uo),(w,u),(w',un)]) . snd . head . ssll) (cart uu (sgl w))
            aai = unit $ Set.map ((\u -> llss [(vc,ui),(w,u),(w',u)]) . snd . head . ssll) (cart uu (sgl w))
    uo = ValStr "out"
    ui = ValStr "in"
    un = ValStr "null"
    gs = VarStr (show g ++ ";s")
    gc = VarStr (show g ++ ";c")
    gn = VarStr (show g ++ ";n")
    dfzz = decompFudsTreePairStateFud
    dfvars = fudsVars . decompFudsFud
    dfund = decompFudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    ttff = fromJust . setTransformsFud . Set.singleton
    trans = histogramsSetVarsTransform_u
    unit qq = fromJust $ setStatesHistogramUnit qq
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    mul = pairHistogramsMultiply
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    llss = listsState
    ssll = statesList
    rel = treesRelation
    sgl :: a -> Set.Set a
    sgl = Set.singleton
    minus :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
    minus = Set.difference
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList

systemsDecompFudsNullableLeafPracticable :: System -> DecompFud -> Maybe Fud
systemsDecompFudsNullableLeafPracticable uu df 
  | and [okvar v | v <- qqll (dfvars df `minus` dfund df)] = Just $ trff fudEmpty fudEmpty (dfzz df)
  | otherwise = Nothing
  where
    trff :: Fud -> Fud -> Tree (State,Fud) -> Fud
    trff ffp ffc zz = foldl funion fudEmpty [if xx == emptyTree then ggc `funion` ffn  else trff ff ggc xx | 
      (((rrp,ff),xx),i) <- zip (qqll (rel zz)) [1..], let ggc = cont ffp rrp ffc i, let ffn = nullable ff ggc]
    okvar (VarPair (VarPair (_, VarInt _), _)) = True
    okvar _ = False
    cont :: Fud -> State -> Fud -> Integer -> Fud
    cont ffp rrp ffc i
      | ffp == fudEmpty = fudEmpty
      | ffc == fudEmpty = ffp `funion` ttff tts
      | otherwise = ffc `funion` ffp `funion` ttff tts `funion` ttff ttc
      where
        ((VarPair (VarPair (f,_),_)),_) = head $ ssll rrp
        ws = VarPair (VarPair (f, VarStr "s"), VarInt i)
        aaso = unit $ sgl $ llss [(ws,uo)]
        aasi = unit $ sgl $ llss [(ws,ui)]
        tts = trans ((unit (cart uu (fder ffp) `minus` sgl rrp) `mul` aaso) `add` (unit (sgl rrp) `mul` aasi)) (sgl ws)
        vc = Set.findMin $ fder ffc
        wc = VarPair (VarPair (f, VarStr "c"), VarInt i)
        aac = unit $ llqq [llss [(ws,uo),(vc,uo),(wc,uo)],llss [(ws,uo),(vc,ui),(wc,uo)],
                           llss [(ws,ui),(vc,uo),(wc,uo)],llss [(ws,ui),(vc,ui),(wc,ui)]]
        ttc = trans aac (sgl wc)
    nullable :: Fud -> Fud -> Fud
    nullable ff ggc
      | ggc == fudEmpty = ff `funion` qqff (Set.map wwttr (fder ff))
      | otherwise = ggc `funion` ff `funion` qqff (Set.map wwttc (fder ff))
      where
        wwttr w = trans aa (sgl w') 
          where
            VarPair (VarPair (f, _), i) = w
            w' = VarPair (VarPair (f, VarStr "n"), i)
            aa = unit $ Set.map (\ss -> let [(_,u)] = ssll ss in llss [(w,u),(w',u)]) (cart uu (sgl w))
        wwttc w = trans (aao `add` aai) (sgl w') 
          where
            VarPair (VarPair (f, _), i) = w
            w' = VarPair (VarPair (f, VarStr "n"), i)
            vc = Set.findMin $ fder ggc
            aao = unit $ Set.map ((\u -> llss [(vc,uo),(w,u),(w',un)]) . snd . head . ssll) (cart uu (sgl w))
            aai = unit $ Set.map ((\u -> llss [(vc,ui),(w,u),(w',u)]) . snd . head . ssll) (cart uu (sgl w))
    uo = ValStr "out"
    ui = ValStr "in"
    un = ValStr "null"
    dfzz = decompFudsTreePairStateFud
    dfvars = fudsVars . decompFudsFud
    dfund = decompFudsUnderlying
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    ttff = fromJust . setTransformsFud . Set.singleton
    trans = histogramsSetVarsTransform_u
    unit qq = fromJust $ setStatesHistogramUnit qq
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    mul = pairHistogramsMultiply
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    llss = listsState
    ssll = statesList
    rel = treesRelation
    sgl :: a -> Set.Set a
    sgl = Set.singleton
    minus :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
    minus = Set.difference
    qqll :: forall a. Set.Set a -> [a]
    qqll = Set.toList
    llqq :: forall a. (Ord a) => [a] -> Set.Set a
    llqq = Set.fromList

