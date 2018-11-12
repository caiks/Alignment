module AlignmentDev
where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import GHC.Real
import System.Random
import AlignmentUtil
import Alignment
import AlignmentSubstrate
import AlignmentDistribution
import AlignmentApprox
import AlignmentRandom
import AlignmentPracticable
import AlignmentPracticableIO

pv x = represent x
sunion = pairStatesUnionLeft

ssll = statesList
llss = listsState

ssplit = setVarsSetStatesSplit 

cart uu vv = fromJust $ systemsVarsCartesian uu vv
sysreg d n = fromJust $ systemRegular d n
syscart d n = fromJust $ let uu = sysreg d n in systemsVarsCartesian uu (systemsVars uu)
uunion = pairSystemsUnion
uvars = systemsVars
vol uu vv = fromJust $ systemsVarsVolume uu vv
uull = systemsList
umap uu ll = Map.fromList [(v,(v',Map.fromList (zip (Set.toList ww) (map ValInt [1..])))) | ((v,ww),v') <- zip [(v,ww) | (v,ww) <- uull uu, variablesIsPartition v] ll]

ph x = represent x
aat aa ss = fromJust $ histogramsStatesCount aa ss
aall = histogramsList
llaa ll = fromJust $ listsHistogram ll
aarr aa = map (\(ss,c) -> (ss,fromRational c)) (histogramsList aa) :: [(State,Double)]
unit qq = fromJust $ setStatesHistogramUnit qq
equiv = pairHistogramsEquivalent
add xx yy = fromJust $ pairHistogramsAdd xx yy
sub xx yy = fromJust $ pairHistogramsSubtract xx yy
mul = pairHistogramsMultiply
divide = pairHistogramsDivide
apply = setVarsSetVarsSetHistogramsHistogramsApply
leq = pairHistogramsLeq
size = histogramsSize
resize z aa = fromJust $ histogramsResize z aa
norm aa = resize 1 aa
vars = histogramsVars
cdvars ll = Set.fromList $ map VarInt ll
states = histogramsStates
dim = histogramsDimension
card = histogramsCardinality
recip = histogramsReciprocal
red aa vv = setVarsHistogramsReduce vv aa
ared aa vv = setVarsHistogramsReduce vv aa
cdred aa ll = red aa $ Set.fromList $ map VarInt ll
scalar q = fromJust $ histogramScalar q
single ss c = fromJust $ histogramSingleton ss c
trim = histogramsTrim
ceil = histogramsCeiling
eff = histogramsEffective
ind = histogramsIndependent
empty = histogramEmpty
sys = histogramsSystemImplied 
regsing d n = fromJust $ histogramRegularUnitSingleton d n
regcart d n = fromJust $ histogramRegularCartesian d n
regdiag d n = fromJust $ histogramRegularUnitDiagonal d n
regcrown d n = fromJust $ histogramRegularUnitCrown d n
regplanar d n = fromJust $ histogramRegularUnitPlanar d n
regline d n = fromJust $ histogramRegularUnitLine d n
regaxial d n = fromJust $ histogramRegularUnitAxial d n
regpivot d n = fromJust $ histogramRegularUnitPivot d n
regantipivot d n = trim $ regcart d n `sub` regpivot d n
reframe aa ll = fromJust $ histogramsMapVarsFrame aa (Map.fromList ll)
cdtp aa ll = reframe aa (zip (Set.toList (vars aa)) (map VarInt ll))
cdaa ll = llaa [(llss (map (\(i,j) -> (VarInt i, ValInt j)) (zip [1..] ss)),1) | ss <- ll]
frame f aa = fromJust $ histogramsMapVarsFrame aa (Map.fromList $ map (\(VarInt i) -> (VarInt i, VarInt (f i))) $ Set.toList $ vars aa)
regtranspose ll aa = frame (\ i -> toInteger (ll !! fromInteger (i-1))) aa
cdrefr = regtranspose
regpermute i ll aa = llaa [(llss [(VarInt k, if fromInteger k == i then ValInt (toInteger (ll !! fromInteger (j-1))) else ValInt j) | (VarInt k,ValInt j) <- ssll ss],c) | (ss,c) <- aall aa]
cdperm = regpermute
regcopy aa = frame (+ (dim aa)) aa
dot aa bb = llaa $ map (\((ss,q),(rr,p)) -> (sunion ss rr, q*p)) $ zip (aall aa) (cycle (aall bb)) 
runiform = histogramsRandomsUniform
raa aa n = [aa] ++ [runiform aa i | i <- [1..n]]
rreg d n i = let qq = [regsing d n, regcart d n, regdiag d n, regcrown d n, regplanar d n, regline d n, regaxial d n, regpivot d n]
                 pp = qq ++ map (\aa -> regcart d n `sub` aa) qq
             in concat (map (\aa -> raa aa i) pp)
rregz d n i z = [resize z aa | aa <- rreg d n i, z >= 0, size aa > 0]
perturb aa e = [aa `add` single ss e `sub` single tt e | (ss,c) <- aall (trim aa), (tt,d) <- aall (trim aa), tt /= ss, d >= e]

llhh = fromJust . listsHistory  
hhll = historyToList
hvars = historiesSetVar
hsize = historiesSize
hred hh vv = setVarsHistoriesReduce vv hh
hadd hh gg = fromJust $ pairHistoriesAdd hh gg
hmul = pairHistoriesMultiply
aahh aa = fromJust $ histogramsHistory aa
hhaa hh = historiesHistogram hh
hshuffle hh r = fromJust $ historiesShuffle hh r
ashuffle aa r = hhaa $ hshuffle (aahh aa) r

rrll = rollsList
llrr ll = fromJust $ listsRoll ll
rvars = rollsSetVar
rempty = rollEmpty
rdom = rollsDomain
rran = rollsRange
rmul aa rr = rollsHistogramsRoll rr aa
rrnn uu vv rr = fromJust $ systemsSetVariablesRollsSetPartitionVariable uu vv rr

vvvstvr uu xx = fromJust $ systemsSetVariablesVariablesValuesValuesRollValue uu xx
vrrr = systemsRollValuesRoll
jjrrll = systemsListRollValuesListRoll
jjrr = systemsListRollValuesRoll

qqpp qq = fromJust $ setComponentsPartition qq
ppqq = partitionsSetComponent
pptt = partitionsTransformVarPartition 
ppvars = partitionsVars
nntt nn = fromJust $ setPartitionsTransformVarPartition nn

ppppp = partitionPointedsPartition 
pppqq = partitionPointedsPoint 
ppqqppp = partitionsComponentsPartitionPointed 
ppptt = partitionPointedsTransformVarPartition 
ppptts = partitionPointedsStringsCartesianTransformVarString

pt x = represent x
und = transformsUnderlying
der = transformsDerived
tvars = transformsVars
ttaa = transformsHistogram
tmul aa tt = transformsHistogramsApply tt aa
inv = Map.toList . transformsInverse
trans xx ww = fromJust $ histogramsSetVarsTransform xx ww
ttpp = transformsPartition
ttnn = transformsSetPartition

cdtt pp ll = trans (regtranspose pp (cdaa ll)) (Set.singleton (VarInt (last pp)))
treg d n = [let yy = frame (+n) (regcart i 1) in trans (dot (regcart d n) yy) (vars yy) | i <- [1..d^n]]
treg2 d n = [let yy = frame (+n) (regcart i 1) in trans (dot (regcart d n) yy) (vars yy) | i <- [2..d^n]]
treg2b d n b = [let yy = frame (+n) (regcart i 1) in trans (dot (regcart d n) yy) (vars yy) | i <- [2..b]]
tregm d n m = map (fftt. qqff) $ foldl (\qq pp -> [Set.insert tt ff | ff <- qq, tt <- pp]) [Set.empty] [map (\tt -> tframell tt [(n+1,n+k)]) (treg2 d n) | k <- [1 .. m]]
tregmb d n b m = map (fftt. qqff) $ foldl (\qq pp -> [Set.insert tt ff | ff <- qq, tt <- pp]) [Set.empty] [map (\tt -> tframell tt [(n+1,n+k)]) (treg2b d n b) | k <- [1 .. m]]

tframe f tt = fromJust $ transformsMapVarsFrame tt (Map.fromList $ map (\(VarInt i) -> (VarInt i, VarInt (f i))) $ Set.toList $ tvars tt)
tframell tt ll = fromJust $ transformsMapVarsFrame tt (Map.fromList $ map (\(i,j) -> (VarInt i, VarInt j)) ll)

conind aa tt = fromJust $ histogramsTransformsConverseIndependent aa tt
conact aa tt = fromJust $ histogramsTransformsConverseActual aa tt

qqff = fromJust . setTransformsFud
llff = qqff . Set.fromList
ttff = fromJust . setTransformsFud . Set.singleton

pf ff = represent ff
ffqq = fudsSetTransform
fvars = fudsVars
fder = fudsDerived
fund = fudsUnderlying
fhis = fudsSetHistogram
fftt = fudsTransform
funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
fapply aa ff = fudsHistogramsApply ff aa
fmul aa ff = fudsHistogramsMultiply ff aa
ffaa  = ttaa . fftt
layer ff = fudsSetVarsLayer ff (fder ff)
fsys = fudsSystemImplied
fmp ff ll = qqff (Set.map (\tt -> fromJust (transformsMapVarMapValsFrame tt (umap (fsys ff) ll))) (ffqq ff))
fmpi ff = fmp ff [VarInt i | i <- [1..], VarInt i `Set.notMember` fvars ff]

fregb d n b m = map qqff $ foldl (\qq pp -> [Set.insert tt ff | ff <- qq, tt <- pp]) [Set.empty] [map (\tt -> tframell tt [(n+1,n+k)]) (treg2b d n b) | k <- [1 .. m]]
fregbno d n b m = map qqff $ foldl (\qq pp -> [Set.insert tt ff | ff <- qq, tt <- pp]) [Set.empty] [map (\tt -> tframell tt ([(i,n*(k-1)+i) | i <- [1..n]] ++ [(n+1,n*m+k)])) (treg2b d n b) | k <- [1 .. m]]

zzdd zz = fromJust $ treePairStateTransformsDecomp zz
ddzz = decompsTreePairStateTransform
ddff = decompsFud
dund = decompsUnderlying
dmul aa dd = decompsHistogramsApply dd aa

zzdf zz = fromJust $ treePairStateFudsDecompFud zz
dfzz = decompFudsTreePairStateFud
dfdd df = fromJust $ decompFudsDecomp df
dfff = decompFudsFud
dfund = decompFudsUnderlying
dfapply aa df = decompFudsHistogramsApply df aa
dfmul aa df = decompFudsHistogramsMultiply df aa

ent = histogramsEntropy 
lent aa ww vvl = ent (aa `red` (ww `Set.union` vvl)) - ent (aa `red` ww)
algn = histogramsAlignment
algnden aa = let v = fromIntegral (vol (sys aa) (vars aa)); n = fromIntegral (dim aa) in algn aa  / (v ** (1/n))


pd x = represent x

rp x = represent x
rj (Just x) = represent x
rj Nothing = "Nothing"

rpln ll = mapM_ (print . rp) ll
