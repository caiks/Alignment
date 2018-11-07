{-# LANGUAGE RankNTypes #-}

module Alignment (
  Value(..),
  Variable(..),
  System,
  State,
  Id(..),
  History,
  Histogram,
  Classification,
  Transform,
  Fud,
  Component,
  Partition,
  SetPartition,
  PartitionPointed,
  SetPartitionPointed,
  Decomp,
  DecompFud,
  Roll,
  RollValue,
  Model,
  derived,
  variablesIsPartition,
  systemEmpty,
  systemFromList, listsSystem, listsSystem_u,
  systemToList, systemsList,
  systemsVarsValues, systemsVarsSetValue,
  systemsVars, systemsSetVar,
  systemsVarsVolume, systemsSetVarsVolume, systemsSetVarsVolume_u,
  systemsMapVarsFrame, systemsMapVarsIsFrame,
  systemRegular,
  pairSystemsUnion,
  stateFromList, listsState,
  stateSingleton,
  statesVars, statesSetVar,
  varsStatesFilter, setVarsStatesStateFiltered,
  varsSetStatesSplit, setVarsSetStatesSplit,
  statesMapVarsFrame,
  statesMapVarMapValsFrame,
  stateToList, statesList,
  statesVarsValue,
  stateEmpty,
  systemsStatesIs,
  systemsVarsCartesian, systemsSetVarsSetStateCartesian, systemsSetVarsSetStateCartesian_u,
  pairStatesIsSubstate,
  pairStatesIntersection,
  pairStatesUnionLeft,
  pairStatesUnionRight,
  pairStatesIsJoin,
  statesCardinality,
  historyFromList, historyFromList_u,
  listsHistory,
  historyToList, historiesList,
  historyEmpty,
  historiesSize,
  historiesIds, historiesSetId,
  historiesStates, historiesSetState,
  historiesVars, historiesSetVar,
  historiesIdsState,
  varsHistoriesReduce, setVarsHistoriesReduce,
  pairHistoriesJoin,
  pairHistoriesAdd,
  pairHistoriesMultiply,
  histogramFromList, listsHistogram, listsHistogram_u,
  histogramToList, histogramsList,
  histogramsStates, histogramsSetState,
  histogramsVars, histogramsSetVar,
  histogramsMapVarsFrame, histogramsMapVarsFrame_1,
  histogramsMapVarMapValsFrame,
  histogramsStatesCount,
  histogramsSize,
  histogramsResize,
  histogramsTrim,
  histogramsCeiling,
  histogramsFloor,
  histogramsUnit,
  histogramsIsUnit,
  setStatesHistogramUnit,
  histogramsEffective,
  pairHistogramsCongruent,
  pairHistogramsEquivalent,
  pairHistogramsLeq,
  histogramsSystemImplied,
  systemsHistogramsIs, 
  systemsHistogramsVolume,
  histogramEmpty,
  histogramsZero, histogramsIsZero,
  histogramsUniform, histogramsIsUniform,
  histogramsIsSingleton,
  histogramSingleton,
  histogramScalar,
  histogramsDimension,
  systemsHistogramsIsRegular,
  systemsHistogramsValency,
  histogramsStatesIncidence,
  histogramsCardinality,
  histogramsIsCardinal,
  histogramRegularCartesian,
  histogramRegularUnitSingleton,
  histogramsIsCausal,
  histogramsIsPlanar,
  histogramRegularUnitPlanar,
  histogramsIsAntiPlanar,
  histogramsIsDiagonal,
  histogramRegularUnitDiagonal,
  systemsHistogramsIsDiagonalFull,
  systemsHistogramsIsAlignmentMaximum,
  histogramsIsAntiDiagonal,
  histogramsIsLine,
  histogramRegularUnitLine,
  histogramsIsCrown,
  histogramRegularUnitCrown,
  histogramsIsAxial,
  histogramAxialsPivot,
  histogramRegularUnitAxial,
  histogramsIsSkeletal,
  histogramsIsPivot,
  histogramsIsAntiPivot,
  histogramRegularUnitPivot,
  histogramRegularUnitOptional,
  histogramsIsCartesianSub,
  histogramRegularUnitContingentlyPermutedDiagonal,
  setVarsHistogramsSlices,
  setVarsHistogramsSliceModal, setVarsHistogramsSliceModal_1,
  systemsSetVarsHistogramCartesian,
  systemsHistogramsCartesian,
  systemsHistogramsIsCartesian,
  systemsHistogramsComplete,
  histogramsIsIntegral,  
  historiesHistogram,
  histogramsHistory,
  historiesClassification,
  classificationsHistory,
  classificationsList,
  listsClassification,
  classificationsSetState,
  classificationsSetVar,
  classificationEmpty,
  pairHistogramsAdd,
  pairHistogramsSubtract,
  pairHistogramsMultiply,
  histogramsReciprocal,
  pairHistogramsDivide,
  varsHistogramsReduce, setVarsHistogramsReduce, setVarsHistogramsReduce_1,
  histogramsReduceScalar,
  histogramIntegralsMult,
  histogramsHistogramIntegralsPermutorial,
  histogramsHistogramsProductTransformed,
  histogramsHistogramsProductConditionalTransformed,
  histogramsSetVarsTransform, histogramsSetVarsTransform_u,
  transformEmpty,
  histogramsTransformDisjoint,
  histogramsTransformNull,
  transformsHistogram,
  transformsDerived,
  transformsUnderlying,
  transformsVars,
  transformsMapVarsFrame,
  transformsMapVarMapValsFrame,
  transformsHistogramsApply,
  transformsHistoriesApply,
  transformsIsFunc,
  transformsIsBijective,
  transformsInverse,
  transformsStateDeriveds,
  histogramsTransformsEffective,
  histogramsTransformsReductions,
  systemsTransformsIs,
  systemsTransformsIsOneFunc,
  transformsConverseSimple,
  transformsConverseNatural,
  histogramsTransformsConverseActual,
  histogramsIndependent,
  histogramsTransformsConverseIndependent,
  transformsIsAllCartesian,
  histogramsTransformsIsReal,
  histogramsTransformsIsIdeal,
  transformsTransformMultiValent,
  transformsIsOverlap,
  transformsIsTautology,
  systemsTransformsHistogramsHistogramsProductTransformed,
  transformsHistogramsHistogramsProductConditionalTransformed,
  transformsHistogramsHistogramsSetVariablesSetVariablesProductConditionalTransformed,
  setTransformsFud, setTransformsFud_u,
  fudEmpty,
  fudsSetTransform,
  fudsSetHistogram,
  fudsVars, fudsSetVar,
  fudsDerived,
  fudsUnderlying,
  fudsTransform,
  fudsHistogramsApply, fudsHistogramsApply_1,
  fudsHistogramsMultiply,
  setVarsFudHistogramsApply,
  setVarsSetVarsSetHistogramsHistogramsApply, setVarsSetVarsSetHistogramsHistogramsApply_1,
  fudsDefinitions,
  fudsVarsDepends,
  fudsVarsDepends_1,
  fudsSetVarsDepends,
  fudsIsCircular,
  fudsMono,
  fudsTreeVar,
  fudsTreeTransform,
  fudsSetVarsLayer,
  fudsListLayer,
  fudsTop,
  fudsOverlap,
  fudVarPartitionsExplode,
  setVariablesFudsSetTuple,
  fudsSystemImplied,
  fudsFlatten,
--  histogramsFudsLinearity,
  setComponentsPartition, setComponentsPartition_u,
  partitionsSetComponent,
  partitionEmpty,
  partitionScalar,
  partitionsVars,
  partitionsIsBinary,
  systemsPartitionsIs,
  systemsSetPartition,
  systemsSetVarsPartitionsExpand,
  systemsSetVarsPartitionsContract,
  systemsSetVarsPartitionUnary,
  systemsSetVarsPartitionSelf, systemsSetVarsPartitionSelf_u,
  systemsSetVarsSetPartitionFull,
  transformsPartition,
  partitionsVariablesTransform,
  partitionsTransformVarPartition,
  setPartitionsTransformVarPartition,
  transformsSetPartition,
  partitionsCartesian,
  systemsSetVarsTransformVarPartitionsExpand,
  systemsTransformVarPartitionsContract,
  transformVarPartitionsExplode,
  partitionsComponentsPartitionPointed,
  partitionPointedsPartition,
  partitionPointedsPoint,
  partitionPointedScalar,
  partitionPointedsVars,
  partitionPointedsIsPointSingleton,
  partitionPointedsIsPartitionBinary,
  partitionBinaryPointedsComplement,
  partitionPointedsCartesian,
  partitionPointedsPointState,
  systemsSetVarsPartitionPointedUnary,
  systemsSetVarsSetPartitionPointedOne,
  partitionPointedsTransformVarPartition,
  partitionBinaryPointedsEqual,
  partitionBinaryPointedsNot,
  setPartitionBinaryPointedsAnd,
  setPartitionBinaryPointedsOr,
  treePartitionBinaryPointedsAnd,
  systemsVarsPartitionBinaryPointedsNullable,
  partitionPointedsStringsCartesianTransformVarString,
  partitionPointedsVariablesCartesianTransform,
  decompsFud,
  decompsUnderlying,
  decompEmpty,
  treePairStateTransformsDecomp,
  decompsTreePairStateTransform,
  decompsHistogramsApply,
  decompsHistogramsMultiply,
  decompsHistogramsHistogramsQuery,
  decompsSetDecompDistinct,
  decompsIsDistinct,
  systemsDecompsIs,
  systemsDecompsApplication, systemsDecompsApplication_1,
  applicationsDecomp,
  applicationsSymmetryApplications,
  applicationsLocations,
  applicationsSymmetryTransforms,
  systemsDecompsIsWellBehaved,
  systemsDecompsPartition, systemsDecompsPartition_1,
  systemsDecompsComponents, systemsDecompsComponents_1,
  decompsContingents, decompsContingents_1,
  systemsApplicationsTreeSliceTransform,
  systemsApplicationsListVariablesTreeSliceTransform,
  systemsApplicationsTreeSliceContingent,
  systemsTreeSliceTransformsListVariablesTreeSliceContingent,
  systemsApplicationsSlices,
  systemsApplicationsListVariablesSlices,
  systemsDecompsTransformCrown,
  systemsDecompsListVariablesTransformCrown,
  systemsDecompsListVariablesTransformCrown_1,
  systemsApplicationsMapVariablesSliceAlternate,
  systemsTreeSliceContingentsListVariablesMapVariablesSliceAlternate,
  systemsApplicationsSetNullable,
  systemsMapVariablesSliceAlternatesListVariablesFudNullable,
  systemsApplicationsNullable,
  systemsApplicationsListVariablesNullable,
  systemsDecompsNullable,
  systemsDecompsTransform, systemsDecompsTransform_1,
  systemsDecompsListVariablesNullable,
  systemsDecompsListVariablesTransform, systemsDecompsListVariablesTransform_1,
  systemsDecompsOriginals,
  decompsNullablesOriginals,
  histogramsDecompsSetReduction,
  decompFudsSetFud, decompFudsFuds,
  decompFudsFud,
  decompFudsUnderlying,
  decompFudEmpty,
  treePairStateFudsDecompFud,
  decompFudsTreePairStateFud,
  decompFudsDecomp,
  decompsDecompFud,
  decompFudsSetVariablesRemove,
  decompFudsHistogramsApply,
  decompFudsHistogramsMultiply,
  decompFudsHistogramsHistogramsQuery,
  decompFudsIsDistinct,
  systemsDecompFudsIs,
  systemsDecompFudsApplication,
  systemsDecompFudsListVariablesNullable,
  systemsDecompFudsListVariablesFud,
  systemsIdentifiersHistorySet,
  systemsIdentifiersClassificationSet, systemsIdentifiersClassificationSet_1,
  systemsIdentifiersHistoryVariateSet,
  systemsIdentifierCardinalitysHistogramSet,
  systemsIdentifierCardinalitysTransformSet,
  systemsTransformUnitSet,
  systemsPartitionSet,
  rollEmpty,
  listsRoll,
  rollsList,
  rollsDomain,
  rollsRange,
  rollsSetState,
  rollsSetVar,
  rollsHistogramsRoll,
  systemsSetVarsRollIdentity,
  rollsIsCircular,
  rollsIsCircular_1,
  listRollsIsUnique,
  listRollsIsFunc,
  listRollsIsCircular,
  listRollsHistogramsRoll,
  pairRollsCompose,
  listRollsCompose,
  systemsRollsPartitionState,
  systemsSetVariablesRollsSetPartitionVariable,
  systemsSetVariablesRollsTransform,
  setVariablesVariablesValuesValuesRollValue,
  rollValuesSetVariableVariableValueValue,
  systemsSetVariablesVariablesValuesValuesRollValue,
  systemsRollValuesRoll,
  systemsListRollValuesListRoll,
  variablesListRollValuesFilter,
  systemsListRollValuesRoll,
  systemsVariablesListRollValuesPartitionValue,
  systemsSetVariablesListRollValuesSetPartitionVariable,
  systemsSetVariablesListRollValuesTransform)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import GHC.Real
import AlignmentUtil

data Value = ValStr String | ValInt Integer | ValDouble Double | ValComponent Component | ValIndex Int
               deriving (Eq, Ord, Read, Show)

data Variable = VarStr String | VarInt Integer | VarPartition Partition | VarIndex Int | VarPair (Variable,Variable) 
               deriving (Eq, Ord, Read, Show)

newtype System = System (Map.Map Variable (Set.Set Value)) 
               deriving (Eq, Ord, Read, Show)

newtype State = State (Map.Map Variable Value)
               deriving (Eq, Ord, Read, Show)

data Id = IdStr String | IdInt Integer | IdStateInteger (State, Integer) | IdListId [Id] | IdIntId (Integer,Id) | IdPair (Id,Id) | IdNull
               deriving (Eq, Ord, Read, Show)

newtype History = History (Map.Map Id State) 
               deriving (Eq, Ord, Read, Show)

newtype Histogram = Histogram (Map.Map State Rational)
               deriving (Eq, Ord, Read, Show)

newtype Classification = Classification (Map.Map State (Set.Set Id)) 
               deriving (Eq, Ord, Read, Show)

newtype Transform = Transform (Histogram, (Set.Set Variable))
               deriving (Eq, Ord, Read, Show)

newtype Fud = Fud (Set.Set Transform)
               deriving (Eq, Ord, Read, Show)

type Component = Set.Set State

newtype Partition = Partition (Set.Set Component)
               deriving (Eq, Ord, Read, Show)

type SetPartition = Set.Set Partition

newtype PartitionPointed = PartitionPointed (Partition, Component)
               deriving (Eq, Ord, Read, Show)

type SetPartitionPointed = Set.Set PartitionPointed

newtype Decomp = Decomp (Tree (State,Transform))
               deriving (Eq, Ord, Read, Show)

newtype DecompFud = DecompFud (Tree (State,Fud))
               deriving (Eq, Ord, Read, Show)
               
newtype Roll = Roll (Map.Map State State)
               deriving (Eq, Ord, Read, Show)
         
newtype RollValue = RollValue (Set.Set Variable, Variable, Value, Value)
               deriving (Eq, Ord, Read, Show)

class Model a where
  derived :: a -> Set.Set Variable

instance Represent Value where
  represent (ValStr s) = s
  represent (ValInt i) = represent i
  represent (ValDouble i) = represent i
  represent (ValIndex i) = represent i
  represent (ValComponent cc) = represent cc

instance Represent Variable where
  represent (VarStr s) = s
  represent (VarInt i) = represent i
  represent (VarIndex i) = represent i
  represent (VarPartition pp) = represent pp
  represent (VarPair (v,w)) = "<" ++ represent v ++ "," ++ represent w ++ ">"
  
variablesIsPartition :: Variable -> Bool
variablesIsPartition (VarPartition _) = True
variablesIsPartition _ = False
  
instance Represent System where
  represent (System mm) = represent mm

systemEmpty :: System
systemEmpty = System $ Map.empty

systemFromList :: [(Variable, Set.Set Value)] -> Maybe System
systemFromList ll
  | ok = Just (System mm)
  | otherwise = Nothing
  where 
    mm = Map.fromList ll
    ok = and $ Map.elems $ Map.map (not . Set.null) mm

-- AYOR
listsSystem_u :: [(Variable, Set.Set Value)] -> System
listsSystem_u ll = System (Map.fromList ll)

listsSystem = systemFromList

systemToList :: System -> [(Variable, Set.Set Value)]
systemToList (System mm) = Map.toList mm

systemsList = systemToList

systemsVarsValues :: System -> Variable -> Maybe (Set.Set Value)
systemsVarsValues (System mm) v = Map.lookup v mm 

systemsVarsSetValue = systemsVarsValues

systemsVars :: System -> Set.Set Variable
systemsVars (System mm) = Map.keysSet mm

systemsSetVar = systemsVars

systemsVarsVolume :: System -> Set.Set Variable -> Maybe Integer
systemsVarsVolume (System mm) vv 
  | vv /= Set.filter (\v -> Map.member v mm) vv = Nothing
  | otherwise = Just $ product [toInteger (Set.size (mm Map.! v)) | v <- Set.toList vv]

-- AYOR
systemsSetVarsVolume_u :: System -> Set.Set Variable -> Integer
systemsSetVarsVolume_u (System mm) vv = product [toInteger (Set.size (mm Map.! v)) | v <- Set.toList vv]

systemsSetVarsVolume = systemsVarsVolume

systemsMapVarsFrame :: System -> Map.Map Variable Variable -> Bool
systemsMapVarsFrame uu nn =
  (ww `Set.isSubsetOf` vv && xx `Set.isSubsetOf` vv && Set.size xx == Set.size ww
    && and [mm Map.! v == mm Map.! w | (v,w) <- Map.assocs nn])
  where
    System mm = uu
    vv = systemsVars uu
    ww = Map.keysSet nn
    xx = Set.fromList $ Map.elems nn

systemsMapVarsIsFrame = systemsMapVarsFrame

systemRegular :: Integer -> Integer -> Maybe System
systemRegular d n
    | d >= 1 && n >= 1 = listsSystem [(VarInt i, Set.fromList [ValInt j | j <- [1..d]]) | i <- [1..n]]
    | otherwise = Nothing
    
pairSystemsUnion :: System -> System -> System
pairSystemsUnion uu xx =
    System $ Map.unionWith Set.union mm nn
  where
    System mm = uu
    System nn = xx

instance Represent State where
  represent (State mm) = represent mm

stateFromList :: [(Variable, Value)] -> State
stateFromList ll = State (Map.fromList ll)

listsState = stateFromList

stateSingleton :: Variable -> Value -> State
stateSingleton v w = State (Map.singleton v w)

statesVars :: State -> Set.Set Variable
statesVars (State mm) = Map.keysSet mm

statesSetVar = statesVars

stateToList :: State -> [(Variable, Value)]
stateToList (State mm) = Map.toList mm

statesList = stateToList

varsStatesFilter :: Set.Set Variable -> State -> State 
varsStatesFilter vv (State mm) = State $ Map.filterWithKey (\k _ -> k `Set.member` vv) mm

setVarsStatesStateFiltered = varsStatesFilter

varsSetStatesSplit :: Set.Set Variable -> Set.Set State -> Set.Set (State,State)
varsSetStatesSplit vv qq = 
  Set.map (\ (State mm) -> let (nn,pp) = Map.partitionWithKey (\ v _ -> v `Set.member` vv) mm in (State nn, State pp)) qq 

setVarsSetStatesSplit = varsSetStatesSplit

statesMapVarsFrame :: State -> Map.Map Variable Variable -> Maybe State
statesMapVarsFrame ss nn 
    | isBi nn &&  isDisjoint ss nn = Just $ repl ss nn
    | otherwise = Nothing
  where
    isBi nn = Set.size (dom nn) == Set.size (ran nn)
    isDisjoint ss nn =  (ran nn) `Set.intersection` ((vars ss) Set.\\ (dom nn)) == Set.empty
    repl ss nn = llss $ [(nn Map.! v,w) | (v,w) <- ssll ss, v `Set.member` (dom nn)] ++ 
                        [(v,w) | (v,w) <- ssll ss, v `Set.notMember` (dom nn)]
    vars = statesVars
    dom = Map.keysSet
    ran mm = Set.fromList (Map.elems mm)
    ssll = statesList
    llss = listsState

statesMapVarMapValsFrame :: State -> Map.Map Variable (Variable,(Map.Map Value Value)) -> Maybe State
statesMapVarMapValsFrame ss xx 
    | isBi xx && isDisjoint (vars ss) xx = Just $ repl ss xx
    | otherwise = Nothing
  where
    isBi xx = functionsIsBijective (Map.fromList [(v,w) | (v,(w,_)) <- Map.toList xx]) && 
                and [functionsIsBijective ww | (_,(_,ww)) <- Map.toList xx]
    isDisjoint vv xx = relationsDomain (ran xx) `Set.intersection` (vv Set.\\ dom xx) == Set.empty
    repl ss xx = llss $ [(w, ww `findu` u) | (v,u) <- ssll ss, let (w,ww) = xx `findv` v]
    findu mm x = Map.findWithDefault x x mm
    findv mm x = Map.findWithDefault (x,Map.empty) x mm
    vars = statesVars
    dom = functionsDomain
    ran = functionsRange
    ssll = statesList
    llss = listsState

statesVarsValue :: State -> Variable -> Maybe Value
statesVarsValue (State mm) v = Map.lookup v mm

stateEmpty :: State
stateEmpty = State $ Map.empty

systemsStatesIs :: System -> State -> Bool
systemsStatesIs (System mm) ss =
    ((statesVars ss) `Set.isSubsetOf` (systemsVars uu) 
           && and [w `Set.member` (mm Map.! v) | (v,w) <- stateToList ss])
  where 
    uu = System mm

systemsVarsCartesian :: System -> Set.Set Variable -> Maybe (Set.Set State)
systemsVarsCartesian uu vv
  | not (vv `Set.isSubsetOf` (systemsVars uu)) = Nothing
  | Set.size vv == 0 = Just $ Set.singleton stateEmpty
  | otherwise = Just $ foldl1 mul [Set.map (\w -> stateFromList [(v,w)]) (mm Map.! v) | v <- Set.toList vv]
  where
    System mm = uu
    mul aa bb = Set.fromList [stateFromList ((stateToList ss) ++ (stateToList tt)) | 
                  ss <- Set.toList aa, tt <- Set.toList bb]

-- AYOR
systemsSetVarsSetStateCartesian_u :: System -> Set.Set Variable -> Set.Set State
systemsSetVarsSetStateCartesian_u (System mm) vv = 
  foldl1 mul [Set.map (\w -> stateFromList [(v,w)]) (mm Map.! v) | v <- Set.toList vv]
  where
    mul aa bb = Set.fromList [stateFromList ((stateToList ss) ++ (stateToList tt)) | 
                  ss <- Set.toList aa, tt <- Set.toList bb]

systemsSetVarsSetStateCartesian = systemsVarsCartesian

pairStatesIsSubstate :: State -> State -> Bool
pairStatesIsSubstate ss tt =
    xx `Set.isSubsetOf` yy
  where
    lis = statesList
    xx = Set.fromList $ lis ss
    yy = Set.fromList $ lis tt
   
pairStatesIntersection :: State -> State -> State
pairStatesIntersection ss tt =
    State $ Map.fromList $ Set.toList $ xx `Set.intersection` yy
  where
    lis = statesList
    xx = Set.fromList $ lis ss
    yy = Set.fromList $ lis tt
   
pairStatesUnionLeft :: State -> State -> State
pairStatesUnionLeft ss tt =
    State $ Map.union mm nn
  where
    State mm = ss
    State nn = tt

pairStatesUnionRight :: State -> State -> State
pairStatesUnionRight ss tt = pairStatesUnionLeft tt ss

pairStatesIsJoin :: State -> State -> Bool
pairStatesIsJoin ss tt = pairStatesUnionLeft ss tt == pairStatesUnionRight ss tt
   
statesCardinality :: State -> Integer
statesCardinality (State mm) = toInteger $ Map.size mm

instance Represent Id where
  represent (IdStr s) = s
  represent (IdInt i) = represent i
  represent (IdStateInteger (ss, i)) = represent (ss, i)
  represent (IdListId ll) = represent ll
  represent (IdIntId (i,x)) = represent (i,x)
  represent (IdPair (i, j)) = represent (i, j)
  represent (IdNull) = "_"

instance Represent History where
  represent (History mm) = represent mm

historyFromList :: [(Id, State)] -> Maybe History
historyFromList ll
  | ok = Just (History mm)
  | otherwise = Nothing
  where 
    mm = Map.fromList ll
    ok = length ll == 0 || length (nub [statesVars ss | (i, ss) <- ll]) == 1

-- AYOR
historyFromList_u :: [(Id, State)] -> History
historyFromList_u = History . Map.fromList

listsHistory = historyFromList

historyToList :: History -> [(Id, State)]
historyToList (History mm) = Map.toList mm

historiesList = historyToList

historyEmpty :: History
historyEmpty = History $ Map.empty

historiesSize :: History -> Integer
historiesSize (History mm) = toInteger $ Map.size mm

historiesIds :: History -> Set.Set Id
historiesIds (History mm) = Map.keysSet mm

historiesSetId = historiesIds

historiesStates :: History -> Set.Set State
historiesStates (History mm) = Set.fromList $ Map.elems mm

historiesSetState = historiesStates

historiesVars :: History -> Set.Set Variable
historiesVars (History mm) 
  | Map.size mm == 0 = Set.empty
  | otherwise = statesVars $ head $ Map.elems mm

historiesSetVar = historiesVars

historiesIdsState :: History -> Id -> Maybe State
historiesIdsState (History mm) v = Map.lookup v mm

setVarsHistoriesReduce :: Set.Set Variable -> History -> History 
setVarsHistoriesReduce vv hh = 
    fromJust $ his $ [(i, filt vv ss) | (i,ss) <- lis hh]
  where
    filt = varsStatesFilter
    his = listsHistory
    lis = historiesList

varsHistoriesReduce = setVarsHistoriesReduce

pairHistoriesJoin :: History -> History -> History
pairHistoriesJoin hh gg = 
    fromJust $ his [(i, sunion ss (mmgg Map.! i)) | (i, ss) <- lis hh, Map.member i mmgg, isJoin ss (mmgg Map.! i)]
  where
    History mmgg = gg
    his = listsHistory
    lis = historiesList
    isJoin = pairStatesIsJoin
    sunion = pairStatesUnionLeft

pairHistoriesAdd :: History -> History -> Maybe History
pairHistoriesAdd hh gg
  | hh == empty = Just gg
  | gg == empty = Just hh
  | vars hh == vars gg = Just ff
  | otherwise = Nothing
  where
    ff = llhh $ [(IdPair (x,IdNull), ss) | (x,ss) <- hhll hh] ++ [(IdPair (IdNull,x), ss) | (x,ss) <- hhll gg]
    llhh = fromJust . listsHistory  
    hhll = historyToList
    empty = historyEmpty
    vars = historiesSetVar

pairHistoriesMultiply :: History -> History -> History
pairHistoriesMultiply hh gg = ff
  where
    ff = llhh $ [(IdPair (x,y), ss `sjoin` tt) | (x,ss) <- hhll hh, (y,tt) <- hhll gg, ss `isjoin` tt]
    llhh = fromJust . listsHistory  
    hhll = historyToList
    sjoin = pairStatesUnionLeft
    isjoin = pairStatesIsJoin

instance Represent Histogram where
  represent (Histogram mm) = represent mm

histogramFromList :: [(State, Rational)] -> Maybe Histogram
histogramFromList ll
  | ok = Just (Histogram mm)
  | otherwise = Nothing
  where 
    mm = Map.fromListWith (+) ll
    ok = length ll == 0 || (length (nub [statesVars ss | (ss, q) <- ll]) == 1 && and [q >= 0 | (ss, q) <- ll])

-- AYOR
listsHistogram_u :: [(State, Rational)] -> Histogram
listsHistogram_u ll = Histogram (Map.fromListWith (+) ll)

listsHistogram = histogramFromList

histogramToList :: Histogram -> [(State, Rational)]
histogramToList (Histogram mm) = Map.toList mm

histogramsList = histogramToList

histogramsStates :: Histogram -> Set.Set State
histogramsStates (Histogram mm) = Map.keysSet mm

histogramsSetState = histogramsStates

histogramsVars :: Histogram -> Set.Set Variable
histogramsVars (Histogram mm) 
  | Map.size mm == 0 = Set.empty
  | otherwise = statesVars $ head $ Map.keys mm

histogramsSetVar = histogramsVars

histogramsMapVarsFrame :: Histogram -> Map.Map Variable Variable -> Maybe Histogram
histogramsMapVarsFrame aa nn 
    | isBi &&  isDisjoint = Just $ bb
    | otherwise = Nothing
  where
    vv = histogramsVars aa
    ww = Map.keysSet nn
    xx = Set.fromList (Map.elems nn)
    isBi = Set.size ww == Set.size xx
    isDisjoint =  xx `Set.intersection` (vv Set.\\ ww) == Set.empty
    srepl ss = llss $ map (\(v,w) -> if v `Map.member` nn then (nn Map.! v,w) else (v,w)) $ ssll ss
    bb = llaa $ map (\(ss,q) -> (srepl ss, q)) $ aall aa
    ssll = statesList
    llss = listsState
    aall = histogramsList
    llaa = fromJust . listsHistogram

histogramsMapVarsFrame_1 :: Histogram -> Map.Map Variable Variable -> Maybe Histogram
histogramsMapVarsFrame_1 aa nn 
    | isBi nn &&  isDisjoint aa nn = Just $ hrepl aa nn
    | otherwise = Nothing
  where
    isBi nn = Set.size (dom nn) == Set.size (ran nn)
    isDisjoint aa nn =  (ran nn) `Set.intersection` ((vars aa) Set.\\ (dom nn)) == Set.empty
    sframe ss nn = fromJust $ statesMapVarsFrame ss nn 
    hrepl aa nn = llaa $ [(sframe ss nn, q) | (ss,q) <- aall aa]
    vars = histogramsVars
    dom = Map.keysSet
    ran mm = Set.fromList (Map.elems mm)
    aall = histogramsList
    llaa = fromJust . listsHistogram
    
histogramsMapVarMapValsFrame :: Histogram -> Map.Map Variable (Variable,(Map.Map Value Value)) -> Maybe Histogram
histogramsMapVarMapValsFrame aa xx 
    | isBi xx && isDisjoint (vars aa) xx = Just $ llaa $ map (\(ss,q) -> (srepl ss xx, q)) $ aall aa
    | otherwise = Nothing
  where
    srepl ss xx = fromJust $ statesMapVarMapValsFrame ss xx
    isBi xx = functionsIsBijective (Map.fromList [(v,w) | (v,(w,_)) <- Map.toList xx]) && 
                and [functionsIsBijective ww | (_,(_,ww)) <- Map.toList xx]
    isDisjoint vv xx = relationsDomain (ran xx) `Set.intersection` (vv Set.\\ dom xx) == Set.empty
    vars = histogramsVars
    dom = functionsDomain
    ran = functionsRange
    aall = histogramsList
    llaa = fromJust . listsHistogram    

histogramsStatesCount :: Histogram -> State -> Maybe Rational
histogramsStatesCount (Histogram mm) v = Map.lookup v mm

histogramsSize :: Histogram -> Rational
histogramsSize (Histogram mm) = Map.fold (+) 0 mm

histogramsResize :: Rational -> Histogram -> Maybe Histogram
histogramsResize z aa 
    | z >= 0 && size aa > 0 = Just $ aa `mul` scalar (z / size aa)
    | otherwise = Nothing
  where
    size = histogramsSize
    mul = pairHistogramsMultiply
    scalar = fromJust . histogramScalar

histogramsTrim :: Histogram -> Histogram
histogramsTrim (Histogram mm) = Histogram $ Map.filter (> 0) mm

histogramsUnit :: Histogram -> Histogram
histogramsUnit (Histogram mm) = Histogram $ Map.map (\x -> 1) mm

histogramsCeiling :: Histogram -> Histogram
histogramsCeiling (Histogram mm) = Histogram $ Map.map (toRational . ceiling) mm

histogramsFloor :: Histogram -> Histogram
histogramsFloor (Histogram mm) = Histogram $ Map.map (toRational . floor) mm

histogramsIsUnit :: Histogram -> Bool
histogramsIsUnit aa = histogramsUnit aa == aa

setStatesHistogramUnit :: Set.Set State -> Maybe Histogram
setStatesHistogramUnit = listsHistogram . map (\ss -> (ss,1)) . Set.toList

histogramsEffective :: Histogram -> Histogram
histogramsEffective = unit . trim
  where
    unit = histogramsUnit
    trim = histogramsTrim

pairHistogramsCongruent :: Histogram -> Histogram -> Bool
pairHistogramsCongruent aa bb = 
    vars aa == vars bb && size aa == size bb
  where
    vars = histogramsSetVar
    size = histogramsSize

pairHistogramsIsCongruent = pairHistogramsCongruent

pairHistogramsEquivalent :: Histogram -> Histogram -> Bool
pairHistogramsEquivalent aa bb = 
    trim aa == trim bb
  where
    trim = histogramsTrim

pairHistogramsIsEquivalent = pairHistogramsEquivalent

pairHistogramsLeq :: Histogram -> Histogram -> Bool
pairHistogramsLeq aa bb = 
    states aat `Set.isSubsetOf` states bb && Map.fold (&&) True (Map.intersectionWith (<=) mm nn)
  where
    trim = histogramsTrim
    states = histogramsSetState
    aat = trim aa
    Histogram mm = aat
    Histogram nn = bb

systemsHistogramsIs :: System -> Histogram -> Bool
systemsHistogramsIs uu aa =
    Set.fold (&&) True $ Set.map (systemsStatesIs uu) (states aa)
  where 
    states = histogramsSetState

systemsHistogramsVolume :: System -> Histogram -> Maybe Integer
systemsHistogramsVolume uu aa = systemsVarsVolume uu (histogramsSetVar aa)

histogramEmpty :: Histogram
histogramEmpty = Histogram $ Map.empty

histogramsZero :: Histogram -> Bool
histogramsZero aa = aa /= histogramEmpty && histogramsEffective aa == histogramEmpty

histogramsIsZero = histogramsZero

histogramsUniform :: Histogram -> Bool
histogramsUniform aa = 
    Map.size mm == 0 || (Set.size $ Set.fromList $ Map.elems mm) == 1
  where
    Histogram mm = histogramsTrim aa

histogramsIsUniform = histogramsUniform

histogramsIsSingleton :: Histogram -> Bool
histogramsIsSingleton aa = 
    Map.size mm == 1
  where
    Histogram mm = histogramsTrim aa

histogramSingleton :: State -> Rational -> Maybe Histogram
histogramSingleton ss q = listsHistogram [(ss,q)]

histogramScalar :: Rational -> Maybe Histogram
histogramScalar q = listsHistogram [(listsState [], q)]

histogramsDimension :: Histogram -> Integer
histogramsDimension aa = toInteger $ Set.size $ histogramsSetVar aa

systemsHistogramsIsRegular :: System -> Histogram -> Bool
systemsHistogramsIsRegular uu aa 
    | systemsHistogramsIs uu aa = 
        vars aa /= Set.empty && (Set.size $ Set.map (\v -> Set.size (mm Map.! v)) (vars aa)) == 1
    | otherwise = False
  where
    System mm = uu
    vars = histogramsSetVar

systemsHistogramsValency :: System -> Histogram -> Maybe Integer
systemsHistogramsValency uu aa 
    | systemsHistogramsIsRegular uu aa = Just $ toInteger $ Set.size $ mm Map.! (head  $ Set.toList  $ vars aa)
    | otherwise = Nothing
  where
    System mm = uu
    vars = histogramsSetVar

histogramsStatesIncidence :: Histogram -> State -> Integer -> Histogram
histogramsStatesIncidence (Histogram mm) ss i = 
    Histogram (Map.filterWithKey (\tt _ -> statesCardinality (pairStatesIntersection tt ss) == i) mm)

histogramsCardinality :: Histogram -> Integer
histogramsCardinality (Histogram mm) = toInteger $ Map.size mm

histogramsIsCardinal :: Histogram -> Bool
histogramsIsCardinal aa = 
    setvars aa == nnvv (dim aa) && 
      and [qqww qq == nnww (Set.size qq) | v <- vars aa, let qq = states (reduce v aa)]
  where
    qqww qq = Set.fromList [u | ss <- Set.toList qq, (_,u) <- ssll ss]
    nnvv i = Set.fromList [VarInt (toInteger j)  | j <- [1..i]]
    nnww i = Set.fromList [ValInt (toInteger j)  | j <- [1..i]]
    ssll = stateToList
    dim = histogramsDimension    
    reduce v = setVarsHistogramsReduce (Set.singleton v)
    setvars = histogramsSetVar
    vars = Set.toList . setvars
    states = histogramsSetState

histogramRegularCartesian :: Integer -> Integer -> Maybe Histogram
histogramRegularCartesian d n
    | d >= 1 && n >= 1 = setStatesHistogramUnit xx
    | otherwise = Nothing
  where
    Just uu = systemRegular d n
    Just xx = systemsVarsCartesian uu (systemsVars uu)
    
histogramRegularUnitSingleton :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitSingleton d n
    | d >= 1 && n >= 1 = Just bb
    | otherwise = Nothing
  where
    bb = Histogram $ Map.fromList $ [(State (Map.fromList [(VarInt j, ValInt 1) | j <-[1..n]]), 1)]  

histogramRegularUnitSingleton_2 :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitSingleton_2 d n = histogramRegularCartesian 1 n
    
histogramsIsCausal :: Histogram -> Bool
histogramsIsCausal aa = 
    dim aa >= 2 && 
      or [isfunc (split kk (states aa)) | kk <- Set.toList (power (vars aa)), kk /= vars aa]
  where
    isfunc = relationsIsFunc
    split = setVarsSetStatesSplit
    states = histogramsSetState . histogramsTrim
    dim = histogramsDimension
    vars = histogramsSetVar
    power = setsPowerset

histogramsIsPlanar :: Histogram -> Bool
histogramsIsPlanar aa = 
    or [and [val rr w == u | rr <- states aa] | ss <- states aa, (w,u) <- ssll ss]
  where
    states = Set.toList . histogramsSetState . histogramsTrim
    val rr w = fromJust $ statesVarsValue rr w
    ssll = statesList

histogramRegularUnitPlanar :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitPlanar d n
    | d >= 1 && n == 1 = Just $ regsing d n
    | d >= 1 && n > 1 = Just $ regsing d 1 `mul` frame (+ 1) (regcart d (n-1)) 
    | otherwise = Nothing
  where
    regcart d n = fromJust $ histogramRegularCartesian d n
    regsing d n = fromJust $ histogramRegularUnitSingleton d n
    mul = pairHistogramsMultiply
    frame f aa = fromJust $ histogramsMapVarsFrame aa (Map.fromList $ map (\(VarInt i) -> (VarInt i, VarInt (f i))) $ Set.toList $ vars aa)
    vars = histogramsVars
    dim = histogramsDimension

histogramsIsAntiPlanar :: Histogram -> Bool
histogramsIsAntiPlanar aa = 
    dim aa >= 2 && and [card (trim (reduce v aa)) > 1 | v <- vars aa] 
  where
    dim = histogramsDimension
    reduce v = setVarsHistogramsReduce (Set.singleton v)
    trim = histogramsTrim
    card = histogramsCardinality
    vars = Set.toList . histogramsSetVar
    
histogramsIsDiagonal :: Histogram -> Bool
histogramsIsDiagonal aa = 
    dim aa >= 2 && and [ss `intersect` tt == stateEmpty | ss <- states aa, tt <- states aa, ss /= tt]
  where
    states = Set.toList . histogramsSetState . histogramsTrim
    intersect = pairStatesIntersection
    dim = histogramsDimension

histogramRegularUnitDiagonal :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitDiagonal d n
    | d >= 1 && n >= 1 = Just bb
    | otherwise = Nothing
  where
    bb = Histogram $ Map.fromList $ [(State (Map.fromList [(VarInt j, ValInt i) | j <-[1..n]]), 1) | i <- [1..d]]

systemsHistogramsIsDiagonalFull :: System -> Histogram -> Bool
systemsHistogramsIsDiagonalFull uu aa = 
    (systemsHistogramsIs uu aa && histogramsIsDiagonal aa 
      && d == minimum [(toInteger $ Set.size $ mm Map.! v) | v <- vars aa])
  where
    d = histogramsCardinality $ histogramsTrim aa
    vars = Set.toList . histogramsSetVar
    System mm = uu

systemsHistogramsIsAlignmentMaximum :: System -> Histogram -> Bool
systemsHistogramsIsAlignmentMaximum uu aa = isuniform aa && isdiag uu aa
  where
    isdiag = systemsHistogramsIsDiagonalFull
    isuniform = histogramsIsUniform
    
histogramsIsAntiDiagonal :: Histogram -> Bool
histogramsIsAntiDiagonal aa = 
    and [ss `intersect` tt /= stateEmpty | ss <- states aa, tt <- states aa]
  where
    states = Set.toList . histogramsSetState . histogramsTrim
    intersect = pairStatesIntersection    
    
histogramsIsLine :: Histogram -> Bool
histogramsIsLine aa = 
    dim aa >= 1 && and [stcard (ss `intersect` tt) == dim aa - 1 | ss <- states aa, tt <- states aa, ss /= tt]
  where
    states = Set.toList . histogramsSetState . histogramsTrim
    intersect = pairStatesIntersection
    dim = histogramsDimension    
    stcard = statesCardinality
    
histogramRegularUnitLine :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitLine d n
    | d >= 1 && n >= 1 = Just bb
    | otherwise = Nothing
  where
    bb = Histogram $ Map.fromList $ [(State (Map.fromList [(VarInt j, ValInt (if j==1 then i else 1)) | j <-[1..n]]), 1) | i <- [1..d]]    

histogramsIsCrown :: Histogram -> Bool
histogramsIsCrown aa = 
    antiplanar aa && and [stcard (ss `intersect` tt) == dim aa - 2 | ss <- states aa, tt <- states aa, ss /= tt]
  where
    antiplanar = histogramsIsAntiPlanar
    trim = histogramsTrim
    card = histogramsCardinality
    vars = Set.toList . histogramsSetVar
    states = Set.toList . histogramsSetState . trim
    intersect = pairStatesIntersection
    stcard = statesCardinality
    dim = histogramsDimension        

histogramRegularUnitCrown :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitCrown d n
    | d >= 1 && n >= 1 = Just bb
    | otherwise = Nothing
  where
    bb = Histogram $ Map.fromList [(State (Map.fromList [(VarInt j, ValInt (if j==i then d else 1)) | j <-[1..n]]), 1) | i <-[1..n]]

histogramsIsAxial :: Histogram -> Bool
histogramsIsAxial aa = 
    Set.size (Set.fromList [pp | pp <- states (ind aa), and [stcard (ss `intersect` pp) >= dim aa - 1 | ss <- states aa]]) == 1
  where
    trim = histogramsTrim
    states = Set.toList . histogramsSetState . trim
    ind = histogramsIndependent
    intersect = pairStatesIntersection
    stcard = statesCardinality
    dim = histogramsDimension    
    
histogramAxialsPivot :: Histogram -> Maybe State
histogramAxialsPivot aa 
  | isaxial aa = Just $ head [pp | pp <- states (ind aa), and [stcard (ss `intersect` pp) >= dim aa - 1 | ss <- states aa]]
  | otherwise = Nothing
  where
    trim = histogramsTrim
    states = Set.toList . histogramsSetState . trim
    ind = histogramsIndependent
    intersect = pairStatesIntersection
    stcard = statesCardinality
    dim = histogramsDimension     
    isaxial = histogramsIsAxial 

histogramRegularUnitAxial :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitAxial d n
    | d >= 1 && n >= 1 = Just bb
    | otherwise = Nothing
  where
    bb = Histogram $ Map.fromList $ [llss [(v, 1) | v <-[1..n]]] ++ 
      [llss [(v, (if v==w then i else 1)) | v <-[1..n]] | w <-[1..n], i <- [2..d]]
    llss ll = (State (Map.fromList (map (\(v,u) -> (VarInt v, ValInt u)) ll)), 1)
    
histogramsIsSkeletal :: Histogram -> Bool
histogramsIsSkeletal aa = 
    and [line bb || axial bb | v <- vars aa, w <- vars aa, v /= w, let bb = aa `red` [v,w]]
  where
    line = histogramsIsLine
    axial = histogramsIsAxial
    vars = Set.toList . histogramsSetVar
    red aa vv = setVarsHistogramsReduce (Set.fromList vv) aa
    
histogramsIsPivot :: Histogram -> Bool
histogramsIsPivot aa = 
    Set.size (Set.fromList [pp | pp <- states aa, and [stcard (ss `intersect` pp) == 0 | ss <- states aa, ss /= pp]]) == 1
  where
    trim = histogramsTrim
    states = Set.toList . histogramsSetState . trim
    intersect = pairStatesIntersection
    stcard = statesCardinality
    dim = histogramsDimension    
    
histogramsIsAntiPivot :: Histogram -> Bool
histogramsIsAntiPivot aa = 
    Set.size (Set.fromList [pp | pp <- ineff aa, and [stcard (ss `intersect` pp) > 0 | ss <- states aa]]) == 1
  where
    trim = histogramsTrim
    states = Set.toList . setstates
    setstates = histogramsSetState . trim
    ineff aa = Set.toList $ setstates (ind aa) Set.\\ setstates aa
    ind = histogramsIndependent
    intersect = pairStatesIntersection
    stcard = statesCardinality

histogramRegularUnitPivot :: Integer -> Integer -> Maybe Histogram
histogramRegularUnitPivot d n
    | d == 1 && n >= 1 = Just $ bb
    | d >= 1 && n >= 1 = Just $ bb `add` cc
    | otherwise = Nothing
  where
    bb = fromJust $ histogramRegularUnitSingleton d n
    uu = fromJust $ listsSystem [(VarInt i, Set.fromList [ValInt j | j <- [2..d]]) | i <- [1..n]]
    xx = fromJust $ systemsVarsCartesian uu (systemsVars uu)
    cc = fromJust $ setStatesHistogramUnit xx
    add xx yy = fromJust $ pairHistogramsAdd xx yy
     
histogramRegularUnitOptional :: Histogram -> Histogram -> Maybe Histogram
histogramRegularUnitOptional aa bb
    | dim aa > 0 && dim bb > 0 && iscard aa && iscard bb = Just cc
    | otherwise = Nothing
  where
    cc = (aa `mul` single (minstate aa) `mul` frame (+ (dim aa)) bb) `add` 
      ((aa `sub` (aa `mul` (single (minstate aa)))) `mul` frame (+ (dim aa)) (resize (size bb) (regsing (dim bb))))
    trim = histogramsTrim
    minstate = Set.findMin . histogramsSetState . trim
    iscard = histogramsIsCardinal 
    frame f aa = fromJust $ histogramsMapVarsFrame aa (Map.fromList $ map (\(VarInt i) -> (VarInt i, VarInt (f i))) $ Set.toList $ vars aa)
    dim = histogramsDimension  
    single ss = fromJust $ histogramSingleton ss 1 
    mul = pairHistogramsMultiply
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sub xx yy = fromJust $ pairHistogramsSubtract xx yy
    regsing n = fromJust $ histogramRegularCartesian 1 n
    resize z aa = fromJust $ histogramsResize z aa
    size = histogramsSize
    vars = histogramsVars
    
histogramRegularUnitContingentlyPermutedDiagonal :: Integer -> Integer -> Integer -> Maybe Histogram
histogramRegularUnitContingentlyPermutedDiagonal d n h
    | d >= 1 && n >= 1 && h >= 1 = Just $ bb
    | otherwise = Nothing
  where
    bb = unit $ llqq $ map (foldl1 sadd) $ qqll $ treesPaths $ funcsTreesMap st $ treeRegular d h
    st ll = let l = toInteger (length ll); q = last ll; p = if l>1 then (last (init ll)) - 1 else 0; in 
      llss ([(VarInt (n*(l-1)+n), ValInt (((q+p-1) `mod` d) + 1))] ++ [(VarInt (n*(l-1)+i), ValInt q) | i <- [1..n-1]])
    sadd = pairStatesUnionLeft
    llss = listsState
    unit qq = fromJust $ setStatesHistogramUnit qq
    llqq = Set.fromList
    qqll = Set.toList

setVarsHistogramsSlices :: Set.Set Variable -> Histogram -> Map.Map State Histogram
setVarsHistogramsSlices kk aa = 
    Map.fromList [(rr, aa `mul` single rr) | rr <- states (aa `red` kk)]
  where
    single ss = fromJust $ histogramSingleton ss 1
    red aa vv = setVarsHistogramsReduce vv aa
    states =  Set.toList . histogramsSetState
    mul = pairHistogramsMultiply

setVarsHistogramsSliceModal :: Set.Set Variable -> Histogram -> Rational
setVarsHistogramsSliceModal kk aa = 
    sum [aamax cc | (rr,cc) <- Map.toList (slices kk aa)]
  where
    slices = setVarsHistogramsSlices
    size = histogramsSize
    aall = histogramsList
    aamax aa = if size aa > 0 then (last $ sort $ snd $ unzip $ aall aa) else 0

setVarsHistogramsSliceModal_1 :: Set.Set Variable -> Histogram -> Rational
setVarsHistogramsSliceModal_1 kk aa = 
    sum [aamax cc | rr <- states (aa `red` kk), let cc = aa `mul` single rr `red` vk]
  where
    vk = vars aa `Set.difference` kk
    single ss = fromJust $ histogramSingleton ss 1
    red aa vv = setVarsHistogramsReduce vv aa
    states =  Set.toList . histogramsSetState
    size = histogramsSize
    mul = pairHistogramsMultiply
    aall = histogramsList
    aamax aa = if size aa > 0 then (last $ sort $ snd $ unzip $ aall aa) else 0
    vars = histogramsVars

histogramsIsCartesianSub :: Histogram -> Bool
histogramsIsCartesianSub aa = 
    dim aa >= 1 && xx == Set.fromList (states aa)
  where
    dim = histogramsDimension
    vars = Set.toList . histogramsSetVar
    states =  Set.toList . histogramsSetState . histogramsTrim
    Just uu = listsSystem [(v, Set.fromList [mm Map.! v | (State mm) <- states aa]) | v <- vars aa]
    Just xx = systemsVarsCartesian uu (systemsVars uu)

histogramsSystemImplied :: Histogram -> System
histogramsSystemImplied aa = 
    lluu [(v, llqq [val ss v | ss <- states aa]) | v <- vars aa]
  where
    lluu ll = fromJust $ systemFromList ll
    val ss v = fromJust $ statesVarsValue ss v
    states = qqll . histogramsStates
    vars = qqll . histogramsVars
    llqq = Set.fromList
    qqll = Set.toList

systemsSetVarsHistogramCartesian :: System -> Set.Set Variable -> Maybe Histogram
systemsSetVarsHistogramCartesian uu vv = 
  case systemsSetVarsSetStateCartesian uu vv of
    Just xx -> setStatesHistogramUnit xx
    Nothing -> Nothing

systemsHistogramsCartesian :: System -> Histogram -> Maybe Histogram
systemsHistogramsCartesian uu aa = systemsSetVarsHistogramCartesian uu $ histogramsSetVar aa

systemsHistogramsIsCartesian :: System -> Histogram -> Bool
systemsHistogramsIsCartesian uu aa = 
    case systemsVarsCartesian uu (vars aa) of
      Just qq -> qq == states aa
      Nothing -> False
  where
    states = histogramsStates
    vars = histogramsVars

systemsHistogramsComplete :: System -> Histogram -> Maybe Histogram
systemsHistogramsComplete uu aa = 
  case systemsHistogramsCartesian uu aa of
    Just cc -> Just $ aa `add` cc `sub` cc
    Nothing -> Nothing
  where
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sub xx yy = fromJust $ pairHistogramsSubtract xx yy

histogramsIsIntegral :: Histogram -> Bool
histogramsIsIntegral (Histogram mmaa) = 
    and $ map (\q -> denominator q == 1) $ Map.elems mmaa

historiesHistogram :: History -> Histogram
historiesHistogram hh = aa
  where 
    History mmhh = hh
    Just aa = histogramFromList $ map (\ss -> (ss,1)) $ Map.elems mmhh

histogramsHistory :: Histogram -> Maybe History
histogramsHistory aa
    | histogramsIsIntegral aa = listsHistory $ concat $ Map.elems nn
    | otherwise = Nothing
  where 
    Histogram mmaa = aa
    nn = Map.mapWithKey (\ss q -> [(IdStateInteger (ss, i),ss) | i <- [1..(truncate q)]]) mmaa

historiesClassification :: History -> Classification
historiesClassification (History mmhh) = Classification mmgg
  where 
    mmgg = Map.fromListWith (Set.union) $ map (\(i,ss) -> (ss, Set.singleton i)) $ Map.assocs mmhh

classificationsHistory :: Classification -> History
classificationsHistory (Classification mmgg) = hh
  where 
    Just hh = listsHistory $ concat $ map (\(ss,ii) -> [(i,ss) | i <- Set.toList ii]) $ Map.assocs mmgg

classificationsList :: Classification -> [(State, (Set.Set Id))]
classificationsList (Classification mm) = Map.toList mm

listsClassification :: [(State, (Set.Set Id))] -> Maybe Classification
listsClassification ll
  | length ll == 0 = Just $ Classification $ Map.empty
  | ok ll = Just $ Classification $ Map.fromList ll
  | otherwise = Nothing
  where
    ok ll = length ll == Map.size (Map.fromList ll)
      && and [svars ss == vars ll && ii /= empty | (ss,ii) <- ll]
      && and [ii `inter` jj == empty | (ss,ii) <- ll, (tt,jj) <- ll, ss /= tt]
    vars ll = svars $ fst $ head ll
    empty = Set.empty
    inter = Set.intersection
    svars = statesSetVar

classificationsSetState :: Classification -> Set.Set State
classificationsSetState (Classification mm) = Map.keysSet mm

classificationsSetVar :: Classification -> Set.Set Variable
classificationsSetVar gg
  | gg == empty = Set.empty
  | otherwise = svars $ Set.findMin $ states gg
  where
    states = classificationsSetState
    empty = classificationEmpty
    svars = statesSetVar

classificationEmpty :: Classification
classificationEmpty = Classification $ Map.empty

instance Represent Classification where
  represent (Classification mm) = represent mm

pairHistogramsAdd :: Histogram -> Histogram -> Maybe Histogram
pairHistogramsAdd aa bb 
    | aa == empty || bb == empty || vars aa == vars bb = Just cc
    | otherwise = Nothing
  where
    Histogram mmaa = aa
    Histogram mmbb = bb
    cc = Histogram $ Map.unionWith (+) mmaa mmbb
    empty = histogramEmpty
    vars = histogramsSetVar

pairHistogramsSubtract :: Histogram -> Histogram -> Maybe Histogram
pairHistogramsSubtract aa bb 
    | aa == empty || bb == empty || vars aa == vars bb = Just cc
    | otherwise = Nothing
  where
    Histogram mmaa = aa
    Histogram mmbb = bb
    cc = Histogram $ Map.map flr $ Map.unionWith (+) mmaa $ Map.map neg mmbb
    empty = histogramEmpty
    vars = histogramsSetVar
    neg q = -q
    flr q = if q >= 0 then q else 0

pairHistogramsMultiply :: Histogram -> Histogram -> Histogram
pairHistogramsMultiply aa bb = cc
  where
    Histogram mmaa = aa
    Histogram mmbb = bb
    cc = Histogram $ Map.fromList [(pairStatesUnionLeft ss tt, q*r) | (ss, q) <- Map.toList mmaa, 
           (tt, r) <- Map.toList mmbb, pairStatesIsJoin ss tt]

histogramsReciprocal :: Histogram -> Histogram
histogramsReciprocal aa =
    Histogram $ Map.map (\q -> 1/q) mmaa
  where
    Histogram mmaa = histogramsTrim aa

pairHistogramsDivide :: Histogram -> Histogram -> Histogram
pairHistogramsDivide aa bb = mul aa (recip bb)
  where
    recip = histogramsReciprocal
    mul = pairHistogramsMultiply

setVarsHistogramsReduce :: Set.Set Variable -> Histogram -> Histogram 
setVarsHistogramsReduce vv aa = cc
  where
    cc = his $ [(varsStatesFilter vv ss, q) | (ss,q) <- lis aa]
    his = listsHistogram_u  
    lis = histogramsList

setVarsHistogramsReduce_1 :: Set.Set Variable -> Histogram -> Histogram 
setVarsHistogramsReduce_1 vv aa = cc
  where
    Just cc = his $ [(varsStatesFilter vv ss, q) | (ss,q) <- lis aa]
    his = listsHistogram  
    lis = histogramsList

varsHistogramsReduce = setVarsHistogramsReduce

histogramsReduceScalar :: Histogram -> Histogram 
histogramsReduceScalar aa = cc
  where
    Just cc = scalar $ size aa
    scalar = histogramScalar
    size = histogramsSize

instance Model Transform where
  derived = transformsDerived

instance Represent Transform where
  represent (Transform (xx,ww)) = represent (xx,ww)

histogramsSetVarsTransform :: Histogram -> Set.Set Variable -> Maybe Transform
histogramsSetVarsTransform xx ww 
    | ww `Set.isSubsetOf` vv = Just $ Transform (xx,ww)
    | otherwise = Nothing
  where
    vv = histogramsSetVar xx

-- AYOR
histogramsSetVarsTransform_u :: Histogram -> Set.Set Variable -> Transform
histogramsSetVarsTransform_u xx ww = Transform (xx,ww)

transformEmpty :: Transform
transformEmpty = Transform (histogramEmpty, Set.empty)

histogramsTransformDisjoint :: Histogram -> Transform
histogramsTransformDisjoint xx = Transform (xx,ww)
  where
    ww = histogramsSetVar xx

histogramsTransformNull :: Histogram -> Transform
histogramsTransformNull xx = Transform (xx,Set.empty)

transformsHistogram :: Transform -> Histogram
transformsHistogram (Transform (xx,ww)) = xx

transformsDerived :: Transform -> Set.Set Variable
transformsDerived (Transform (xx,ww)) = ww

transformsUnderlying :: Transform -> Set.Set Variable
transformsUnderlying (Transform (xx,ww)) = 
    vv Set.\\ ww
  where
    vv = histogramsSetVar xx

transformsVars :: Transform -> Set.Set Variable
transformsVars (Transform (xx,ww)) = 
    vv
  where
    vv = histogramsSetVar xx

transformsMapVarsFrame :: Transform -> Map.Map Variable Variable -> Maybe Transform
transformsMapVarsFrame (Transform (aa,ww)) nn =
    case hframe aa nn of
      Just bb -> trans bb xx
      Nothing -> Nothing
  where
    hframe = histogramsMapVarsFrame
    trans = histogramsSetVarsTransform
    xx = Set.map (\ v -> if v `Map.member` nn then nn Map.! v else v) ww

transformsMapVarMapValsFrame :: Transform -> Map.Map Variable (Variable,(Map.Map Value Value)) -> Maybe Transform
transformsMapVarMapValsFrame (Transform (aa,ww)) nn 
  | bb' /= Nothing = trans bb xx
  | otherwise = Nothing
  where
    bb' = histogramsMapVarMapValsFrame aa nn
    Just bb = bb'
    xx = Set.map (\ v -> if v `Map.member` nn then fst (nn Map.! v) else v) ww
    trans = histogramsSetVarsTransform

transformsHistogramsApply :: Transform -> Histogram -> Histogram
transformsHistogramsApply tt aa = 
    aa `mul` xx `red` ww
  where
    Transform (xx,ww) = tt
    mul = pairHistogramsMultiply
    red = flip setVarsHistogramsReduce

transformsHistoriesApply :: Transform -> History -> History
transformsHistoriesApply tt hh =
    llhh $ [(i, rr) | (i,ss) <- hhll hh, 
                      let rr' = states (eff (sunit ss `tmul` tt)), Set.size rr' == 1, let rr = Set.findMax rr']
  where
    llhh = fromJust .listsHistory
    hhll = historiesList
    sunit ss = fromJust $ histogramSingleton ss 1
    states = histogramsStates
    eff = histogramsEffective
    tmul aa tt = transformsHistogramsApply tt aa

transformsIsFunc :: Transform -> Bool
transformsIsFunc tt = 
    relationsIsFunc $ Set.map (\ss -> (filt yy ss, filt ww ss)) (states xx)
  where
    ww = der tt
    yy = und tt
    xx = trim (his tt)
    filt = varsStatesFilter
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    trim = histogramsTrim
    states = histogramsStates

transformsIsBijective :: Transform -> Bool
transformsIsBijective tt = 
    relationsIsBijective $ Set.map (\ss -> (filt yy ss, filt ww ss)) (states xx)
  where
    ww = der tt
    yy = und tt
    xx = trim (his tt)
    filt = varsStatesFilter
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    trim = histogramsTrim
    states = histogramsStates

transformsInverse :: Transform -> Map.Map State Histogram
transformsInverse tt =
    Map.fromList $ Set.toList $ Set.map ind $ states (xx `red` ww)
  where
    ww = der tt
    yy = und tt
    xx = his tt
    ind rr = (rr, Histogram (Map.fromList [(filt yy ss, q) | (ss,q) <- (lis xx), rr `substate` ss]))
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    trim = histogramsTrim
    states = histogramsStates
    red = flip setVarsHistogramsReduce
    lis = histogramsList
    substate = pairStatesIsSubstate
    filt = varsStatesFilter

transformsStateDeriveds :: Transform -> Set.Set State
transformsStateDeriveds tt = states ((his tt) `red` (der tt))
  where
    states = histogramsStates
    red = flip setVarsHistogramsReduce
    der = transformsDerived
    his = transformsHistogram
    
histogramsTransformsEffective :: Histogram -> Transform -> Maybe Transform
histogramsTransformsEffective aa tt
    | isfunc tt && vars aa == und tt = 
        trans (foldl add empty [eff (aa `mul` cc) `mul` (unit rr) | (rr,cc) <- inv tt]) (der tt)
    | otherwise = Nothing
  where
    der = transformsDerived
    inv = Map.toList . transformsInverse
    add xx yy = fromJust (pairHistogramsAdd xx yy)
    mul = pairHistogramsMultiply
    unit rr = Histogram (Map.singleton rr 1)
    empty = histogramEmpty
    und = transformsUnderlying
    vars = histogramsVars
    eff = histogramsEffective
    trans = histogramsSetVarsTransform
    isfunc = transformsIsFunc

histogramsTransformsReductions :: Histogram -> Transform -> Maybe (Set.Set Transform)
histogramsTransformsReductions aa tt
    | isfunc tt && und tt `subset` vars aa = 
        Just $ Set.fromList [rr | kk <- powset (der tt), let rr = trans (his tt `red` (und tt `Set.union` kk)) kk, comps aa rr == comps aa tt]
    | otherwise = Nothing
  where
    isfunc = transformsIsFunc
    vars = histogramsVars
    und = transformsUnderlying
    powset qq = Set.toList $ setsPowerset qq
    der = transformsDerived
    trans xx ww = fromJust $ histogramsSetVarsTransform xx ww
    his = transformsHistogram
    comps aa tt = llqq [eff (aa `mul` cc) | (_,cc) <- inv tt] `del` histogramEmpty
    eff = histogramsEffective
    mul = pairHistogramsMultiply
    inv = Map.toList . transformsInverse
    red aa vv = setVarsHistogramsReduce vv aa   
    subset = Set.isSubsetOf 
    llqq = Set.fromList
    del xx x = Set.delete x xx

systemsTransformsIs :: System -> Transform -> Bool
systemsTransformsIs uu tt = isHis uu (his tt)
  where 
    isHis = systemsHistogramsIs
    his = transformsHistogram

systemsTransformsIsOneFunc :: System -> Transform -> Bool
systemsTransformsIsOneFunc uu tt = 
    isTran uu tt && isFunc tt && isUnit aa && isCart uu aa
  where
    ww = der tt
    yy = und tt
    aa = his tt `red` yy
    isTran = systemsTransformsIs
    isCart = systemsHistogramsIsCartesian
    isFunc = transformsIsFunc
    isUnit = histogramsIsUnit
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    red = flip setVarsHistogramsReduce

transformsConverseSimple :: Transform -> Transform
transformsConverseSimple tt = 
    Transform (recip (his tt), und tt)
  where
    und = transformsUnderlying
    his = transformsHistogram
    recip = histogramsReciprocal

transformsConverseNatural :: Transform -> Transform
transformsConverseNatural tt = 
    Transform ((eff xx) `div` (xx `red` ww), yy)
  where
    ww = der tt
    xx = his tt
    yy = und tt    
    der = transformsDerived
    und = transformsUnderlying
    his = transformsHistogram
    red = flip setVarsHistogramsReduce
    div = pairHistogramsDivide
    eff = histogramsEffective

histogramsIndependent :: Histogram -> Histogram
histogramsIndependent aa
    | aa == empty = empty
    | dim aa == 0 = aa
    | z == 0 = foldl1 mul [aa `red` v | v <- vars aa]
    | otherwise = foldl mul zz  [aa `red` v | v <- vars aa]
  where
    z = size aa
    d = dim aa
    Just zz = scalar (z / z^d)
    vars = Set.toList . histogramsVars
    red aa v = setVarsHistogramsReduce (Set.singleton v) aa
    mul = pairHistogramsMultiply
    scalar = histogramScalar
    size = histogramsSize
    dim = histogramsDimension
    empty = histogramEmpty

histogramsTransformsConverseActual :: Histogram -> Transform -> Maybe Transform
histogramsTransformsConverseActual bb tt
    | isfunc tt && vars bb == yy = aatt ee yy
    | otherwise = Nothing
  where
    ee = foldl add empty [resize dd `mul` (unit rr) | (rr,cc) <- inv tt, let dd = bb `mul` cc, size dd > 0]
    yy = und tt    
    isfunc = transformsIsFunc 
    aatt = histogramsSetVarsTransform
    resize aa = fromJust $ histogramsResize 1 aa
    size = histogramsSize
    inv = Map.toList . transformsInverse
    add xx yy = fromJust (pairHistogramsAdd xx yy)
    mul = pairHistogramsMultiply
    unit rr = Histogram (Map.singleton rr 1)
    empty = histogramEmpty
    und = transformsUnderlying
    vars = histogramsVars

histogramsTransformsConverseIndependent :: Histogram -> Transform -> Maybe Transform
histogramsTransformsConverseIndependent bb tt
    | transformsIsFunc tt && vars bb == yy = histogramsSetVarsTransform ee yy
    | otherwise = Nothing
  where
    ee = foldl add empty [let dd = bb `mul` cc in (ind dd) `div` (hscalar dd) `mul` (unit rr) | (rr,cc) <- inv tt]
    yy = und tt    
    inv = Map.toList . transformsInverse
    ind = histogramsIndependent
    add xx yy = fromJust (pairHistogramsAdd xx yy)
    mul = pairHistogramsMultiply
    div = pairHistogramsDivide
    hscalar = histogramsReduceScalar
    unit rr = Histogram (Map.singleton rr 1)
    empty = histogramEmpty
    und = transformsUnderlying
    vars = histogramsVars

transformsIsAllCartesian :: Transform -> Bool
transformsIsAllCartesian tt = 
    and [eff cc == eff (ind cc) | (rr,cc) <- inv tt]
  where
    inv = Map.toList . transformsInverse
    eff = histogramsEffective
    ind = histogramsIndependent

histogramsTransformsIsReal :: Histogram -> Transform -> Bool
histogramsTransformsIsReal aa tt = 
    case conact aa tt of 
      Just ttd -> ind aa `tmul` tt `tmul` ttd `equiv` aa
      Nothing -> False
  where
    conact = histogramsTransformsConverseActual
    tmul aa tt = transformsHistogramsApply tt aa
    equiv = pairHistogramsEquivalent
    ind = histogramsIndependent

histogramsTransformsIsIdeal :: Histogram -> Transform -> Bool
histogramsTransformsIsIdeal aa tt = 
    case conind aa tt of 
      Just ttd -> aa `tmul` tt `tmul` ttd `equiv` aa
      Nothing -> False
  where
    conind = histogramsTransformsConverseIndependent
    tmul aa tt = transformsHistogramsApply tt aa
    equiv = pairHistogramsEquivalent

transformsTransformMultiValent :: Transform -> Transform
transformsTransformMultiValent tt = 
    Transform (eff (his tt `red` (und tt `Set.union` mder tt)), mder tt)
  where
    mder tt = llqq [w | w <- qqll (der tt), card (eff (his tt `red` single w)) > 1]
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    red aa vv = setVarsHistogramsReduce vv aa
    card = histogramsCardinality
    eff = histogramsEffective
    single = Set.singleton
    llqq = Set.fromList
    qqll = Set.toList

transformsIsOverlap :: Transform -> Bool
transformsIsOverlap tt = 
    not $ der (mlt tt) == empty || or [llff [tred tt kk w | (w,kk) <- qqll rr] == mlt tt 
              | qq <- parts (und tt), Set.size qq == Set.size (der (mlt tt)), rr <- dots (der (mlt tt)) qq]
  where
    mlt = transformsTransformMultiValent
    tred tt kk w = trans (eff (his tt `red` (w `Set.insert` kk))) w
    parts vv = Set.toList (setsPartitionSet vv)
    dots xx yy = Set.toList (setsSetsDot xx yy)
    llff = fudsTransform . fromJust . setTransformsFud . llqq
    trans xx w = fromJust $ histogramsSetVarsTransform xx (Set.singleton w)
    red aa vv = setVarsHistogramsReduce vv aa
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    eff = histogramsEffective
    empty = Set.empty
    llqq = Set.fromList
    qqll = Set.toList

transformsIsTautology :: Transform -> Bool
transformsIsTautology tt = Set.size (llqq [ttpp (tred tt (und tt) w) | w <- qqll (der tt)]) == 1
  where
    tred tt kk w = trans (his tt `red` (w `Set.insert` kk)) w
    llqq = Set.fromList
    qqll = Set.toList
    trans xx w = fromJust $ histogramsSetVarsTransform xx (Set.singleton w)
    red aa vv = setVarsHistogramsReduce vv aa
    und = transformsUnderlying
    der = transformsDerived
    his = transformsHistogram
    ttpp = transformsPartition

setTransformsFud :: Set.Set Transform -> Maybe Fud
setTransformsFud qq
    | ok = Just $ Fud qq
    | otherwise = Nothing
  where
    ok = and [Set.intersection ww xx == Set.empty 
      | Transform (aa,ww) <- Set.toList qq, Transform (bb,xx) <- Set.toList qq, (aa,ww) /= (bb,xx)]

-- AYOR
setTransformsFud_u :: Set.Set Transform -> Fud
setTransformsFud_u = Fud

fudsSetTransform :: Fud -> Set.Set Transform
fudsSetTransform (Fud qq) = qq

instance Model Fud where
  derived = fudsDerived

instance Represent Fud where
  represent (Fud qq) = represent qq

fudsSetHistogram :: Fud -> Set.Set Histogram 
fudsSetHistogram (Fud qq) = Set.map (\(Transform (aa,ww)) -> aa) qq

fudEmpty :: Fud
fudEmpty = Fud (Set.empty)

fudsVars :: Fud -> Set.Set Variable
fudsVars (Fud qq) = Set.fold (\(Transform (aa,ww)) vv -> Set.union (vars aa) vv) Set.empty qq
  where
    vars = histogramsSetVar

fudsSetVar = fudsVars

fudsDerived :: Fud -> Set.Set Variable
fudsDerived (Fud qq) = 
    (Set.fold (\tt vv -> Set.union (tder tt) vv) Set.empty qq) Set.\\ 
    (Set.fold (\tt vv -> Set.union (tund tt) vv) Set.empty qq)
  where
    tder = transformsDerived
    tund = transformsUnderlying

fudsUnderlying :: Fud -> Set.Set Variable
fudsUnderlying (Fud qq) = 
    (Set.fold (\tt vv -> Set.union (tund tt) vv) Set.empty qq) Set.\\ 
    (Set.fold (\tt vv -> Set.union (tder tt) vv) Set.empty qq)
  where
    tder = transformsDerived
    tund = transformsUnderlying

fudsTransform :: Fud -> Transform
fudsTransform ff 
    | ff == fudEmpty = transformEmpty
    | otherwise = Transform ((foldl1 mul (fhis ff) `red` (fder ff `Set.union` fund ff)), fder ff)
  where
    fhis = Set.toList . fudsSetHistogram
    fder = fudsDerived
    fund = fudsUnderlying
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa

fudsHistogramsApply :: Fud -> Histogram -> Histogram
fudsHistogramsApply ff aa = 
    if fund ff `subset` vars aa 
      then applyFud (fder ff) ff aa 
      else apply (vars aa `cup` fund ff) (fder ff) (ffqq ff) aa
  where
    fder = fudsDerived
    fund = fudsUnderlying
    ffqq = fudsSetHistogram
    vars = histogramsVars
    applyFud ww ff aa = fromJust $ setVarsFudHistogramsApply ww ff aa
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    subset = Set.isSubsetOf
    cup = Set.union

fudsHistogramsApply_1 :: Fud -> Histogram -> Histogram
fudsHistogramsApply_1 ff aa = 
    apply (fund ff) (fder ff) (ffqq ff) aa
  where
    fder = fudsDerived
    fund = fudsUnderlying
    ffqq = fudsSetHistogram
    apply = setVarsSetVarsSetHistogramsHistogramsApply

fudsHistogramsMultiply :: Fud -> Histogram -> Histogram
fudsHistogramsMultiply ff aa = 
    if fund ff `subset` vars aa 
      then applyFud (vars aa `cup` fder ff) ff aa 
      else apply (vars aa `cup` fund ff) (vars aa `cup` fder ff) (ffqq ff) aa
  where
    fder = fudsDerived
    fund = fudsUnderlying
    ffqq = fudsSetHistogram
    vars = histogramsVars
    applyFud ww ff aa = fromJust $ setVarsFudHistogramsApply ww ff aa
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    subset = Set.isSubsetOf
    cup = Set.union

setVarsFudHistogramsApply :: Set.Set Variable -> Fud -> Histogram -> Maybe Histogram
setVarsFudHistogramsApply ww ff aa 
  | ff == fudEmpty = Just $ aa `red` ww
  | xx == Set.empty = Nothing
  | otherwise = apply ww gg cc
  where
    xx = Set.filter (\tt -> und tt `subset` vars aa) (ffqq ff)
    tt = Set.findMin xx
    gg = Fud (ffqq ff `del` tt)
    cc = aa `mul` his tt `red` (ww `cup` fvars gg)
    apply = setVarsFudHistogramsApply
    fvars = fudsVars
    ffqq = fudsSetTransform
    und = transformsUnderlying
    his = transformsHistogram
    mul = pairHistogramsMultiply
    red aa vv = if vars aa `subset` vv then aa else setVarsHistogramsReduce vv aa
    vars = histogramsVars
    cup = Set.union
    del xx x = Set.delete x xx
    subset = Set.isSubsetOf

setVarsSetVarsSetHistogramsHistogramsApply :: Set.Set Variable -> Set.Set Variable -> Set.Set Histogram -> Histogram -> Histogram
setVarsSetVarsSetHistogramsHistogramsApply vv ww mm aa 
  | mm == empty = aa `red` ww
  | xx == [] = aa `red` ww
  | otherwise = apply vv ww nn cc
  where
    xx = [(card bb,bb,qq) | 
           dd <- qqll mm, vars dd `cap` (vars aa `cup` vv) /= empty,
           let qq = mm `del` dd, let bb = aa `mul` dd `red` (ww `cup` qvars qq)]
    (_,cc,nn) = head $ sort xx
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    mul = pairHistogramsMultiply
    red aa vv = if vars aa `Set.isSubsetOf` vv then aa else setVarsHistogramsReduce vv aa
    qvars qq = setSetsUnion (Set.map histogramsVars qq)
    card = histogramsCardinality
    vars = histogramsVars
    cup = Set.union
    cap = Set.intersection
    empty = Set.empty
    del mm dd = Set.delete dd mm
    qqll = Set.toList
    
setVarsSetVarsSetHistogramsHistogramsApply_1 :: Set.Set Variable -> Set.Set Variable -> Set.Set Histogram -> Histogram -> Histogram
setVarsSetVarsSetHistogramsHistogramsApply_1 vv ww mm aa 
  | mm == empty = aa `red` ww
  | xx == [] = aa `red` ww
  | otherwise = apply vv ww nn cc
  where
    xx = [(card bb,bb,qq) | 
           dd <- qqll mm, vars dd `cap` (vars aa `cup` vv) /= empty,
           let qq = mm `del` dd, let bb = aa `mul` dd `red` (ww `cup` qvars qq)]
    (_,cc,nn) = head $ sort xx
    apply = setVarsSetVarsSetHistogramsHistogramsApply_1
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa
    qvars qq = setSetsUnion (Set.map histogramsVars qq)
    card = histogramsCardinality
    vars = histogramsVars
    cup = Set.union
    cap = Set.intersection
    empty = Set.empty
    del mm dd = Set.delete dd mm
    qqll = Set.toList
    
    
{-
setVarsSetHistogramsHistogramsApply :: Set.Set Variable -> Set.Set Histogram -> Histogram -> Histogram
setVarsSetHistogramsHistogramsApply ww ff aa 
    | ff == Set.empty = aa `reduce` ww
    | otherwise = apply ww hh cc
  where
    (c,(hh,cc)) = head $ sort $ map aac $ Set.toList ff
    aac xx = let gg = xx `Set.delete` ff
                 bb = aa `mul` xx `reduce` (ww `Set.union` vars gg)
             in (card bb,(gg,bb))
    mul = pairHistogramsMultiply
    reduce aa vv = setVarsHistogramsReduce vv aa
    vars qq = setSetsUnion (Set.map histogramsVars qq)
    card = histogramsCardinality
    apply = setVarsSetHistogramsHistogramsApply
    
setVarsSetHistogramsHistogramsApply_2 :: Set.Set Variable -> Set.Set Histogram -> Histogram -> Histogram
setVarsSetHistogramsHistogramsApply_2 ww mm aa 
    | mm == Set.empty = aa `reduce` ww
    | otherwise = apply ww nn cc
  where
    (_,cc,nn) = head $ sort [(card bb,bb,qq) | dd <- Set.toList mm, let qq = Set.delete dd mm, 
                                               let bb = aa `mul` dd `reduce` (ww `Set.union` vars qq)]
    mul = pairHistogramsMultiply
    reduce aa vv = setVarsHistogramsReduce vv aa
    vars qq = setSetsUnion (Set.map histogramsVars qq)
    card = histogramsCardinality
    apply = setVarsSetHistogramsHistogramsApply    
-}

fudsDefinitions :: Fud -> Map.Map Variable Transform
fudsDefinitions ff = Map.fromList $ [(v,tt) | tt <- trans ff, v <- der tt]
  where
    trans = Set.toList . fudsSetTransform
    der = Set.toList . transformsDerived

fudsVarsDepends :: Fud -> Set.Set Variable -> Fud
fudsVarsDepends ff ww = deps ww Set.empty
  where
    deps ww xx = 
        Set.fold funion fudEmpty $ Set.map wff $ ww `Set.intersection` yy Set.\\ xx
      where
        wff w = let tt = dd Map.! w in tt `finsert` deps (und tt) (w `Set.insert` xx)
    dd = fudsDefinitions ff
    yy = Map.keysSet dd
    und = transformsUnderlying
    finsert tt (Fud qq) =  Fud (tt `Set.insert` qq)
    funion (Fud pp) (Fud qq) =  Fud (pp `Set.union` qq)

fudsVarsDepends_1 :: Fud -> Set.Set Variable -> Fud
fudsVarsDepends_1 ff ww = fudsVarsVarsDepends ff ww Set.empty

fudsSetVarsDepends = fudsVarsDepends
  
fudsVarsVarsDepends :: Fud -> Set.Set Variable -> Set.Set Variable -> Fud
fudsVarsVarsDepends ff ww xx = 
    Set.fold funion fudEmpty $ Set.map wff $ ww `Set.intersection` dom (def ff) Set.\\ xx
  where
    wff w = let tt = def ff Map.! w in tt `finsert` deps ff (und tt) (w `Set.insert` xx)
    def = fudsDefinitions
    dom = Map.keysSet
    deps = fudsVarsVarsDepends
    und = transformsUnderlying
    finsert tt (Fud qq) =  Fud (tt `Set.insert` qq)
    funion (Fud pp) (Fud qq) =  Fud (pp `Set.union` qq)
    
fudsIsCircular :: Fud -> Bool
fudsIsCircular ff = 
    Set.filter (\w -> w `Set.member` vars ((def ff Map.! w) `Set.delete` deps ff (Set.singleton w))) (dom (def ff)) /= Set.empty
  where
    def = fudsDefinitions
    dom = Map.keysSet
    deps ff ww = fudsSetTransform $ fudsVarsDepends ff ww
    vars = fudsVars . Fud
    
fudsMono :: Fud -> Fud
fudsMono ff = 
    Fud $ Set.fromList [Transform (xx `reduce` (vars xx Set.\\ ww `Set.union` uu), uu) | 
                          Transform (xx,ww) <- ffqq ff, w <- Set.toList ww, let uu = Set.singleton w]
  where
    ffqq = Set.toList . fudsSetTransform
    reduce aa vv = setVarsHistogramsReduce vv aa
    vars = histogramsVars
    
fudsTreeVar :: Fud -> Tree Variable
fudsTreeVar ff =
    tree [(v, treev ff v Set.empty) | v <- Set.toList (fder ff)]
  where
    treev ff v xx 
      | v `Map.member` def ff = tree [(w, treev ff w (Set.insert w xx)) | w <- Set.toList (und (def ff Map.! v) Set.\\ xx)]
      | otherwise = tree []
    tree = Tree . Map.fromList
    def = fudsDefinitions
    und = transformsUnderlying
    fder = fudsDerived

fudsTreeTransform :: Fud -> Tree Transform
fudsTreeTransform ff =
    tree [(tt, treet ff tt (fund ff)) | v <- Set.toList (fder ff), let tt = def ff Map.! v]
  where
    treet ff tt xx = tree [(rr, treet ff rr (Set.insert w xx)) | w <- Set.toList (und tt Set.\\ xx), let rr = def ff Map.! w]
    tree = Tree . Map.fromList
    def = fudsDefinitions
    und = transformsUnderlying
    fder = fudsDerived
    fund = fudsUnderlying

{-
fudsTreeTransform :: Fud -> Tree Transform
fudsTreeTransform ff =
    tree $ Set.map ttnn $ Set.map vtt $ fder ff
  where
    treet tt = tree $ Set.map ttnn $ Set.map vtt $ Set.filter visdef $ und tt
    ttnn tt = (tt, treet tt)
    visdef v = v `member` def ff
    vtt v = def ff Map.! v
    tree = Tree . Map.fromList . Set.toList
    fder = fudsDerived
    def = fudsDefinitions
    und = transformsUnderlying
    member = Map.member
-}

fudsSetVarsLayer :: Fud -> Set.Set Variable -> Integer
fudsSetVarsLayer ff ww =
    layer ff ww (fund ff)
  where
    layer ff ww xx = maximum $ [layer ff (und tt) (Set.insert w xx) + 1 | 
                                 w <- Set.toList (ww `cap` dom (def ff) Set.\\ xx), let tt = def ff Map.! w] ++ [0]
    def = fudsDefinitions
    dom = Map.keysSet
    und = transformsUnderlying
    fder = fudsDerived
    fund = fudsUnderlying
    cap = Set.intersection

fudsListLayer :: Fud -> [Fud]
fudsListLayer ff =
    map Fud $ Map.elems $ inv $ Map.fromList [(tt,layer ff (der tt)) | tt <- ffqq ff]
  where
    ffqq = Set.toList . fudsSetTransform
    layer = fudsSetVarsLayer
    der = transformsDerived
    inv = functionsInverse

fudsTop :: Fud -> Maybe Transform
fudsTop ff 
  | Set.size qq == 1 = Just $ Set.findMin qq
  | otherwise = Nothing
  where
    qq = Set.map (\v -> def ff Map.! v) $ fder ff
    fder = fudsDerived
    def = fudsDefinitions

fudsOverlap :: Fud -> Bool
fudsOverlap ff =
    not $ and [fvars (dep ff v) `Set.intersection` fvars (dep ff w) == Set.empty | v <- fder ff, w <- fder ff, v /= w]
  where
    fvars = fudsVars
    fder = Set.toList . fudsDerived
    dep ff v = fudsVarsDepends ff (Set.singleton v)

fudVarPartitionsExplode :: Fud -> Maybe Fud
fudVarPartitionsExplode ff
  | not (and [ispart w | tt <- ffqq ff, w <- der tt]) = Nothing
  | otherwise = Just $ qqff [rr | tt <- ffqq ff, rr <- ffqq (texplode tt)]
  where
    ispart = variablesIsPartition
    der = Set.toList . transformsDerived
    qqff = fromJust . setTransformsFud . Set.fromList
    ffqq = Set.toList . fudsSetTransform
    texplode tt = fromJust $ transformVarPartitionsExplode tt

fudsSystemImplied :: Fud -> System
fudsSystemImplied ff = 
    Set.fold uunion empty (Set.map sys (his ff))
  where
    sys = histogramsSystemImplied
    his = fudsSetHistogram
    empty = systemEmpty
    uunion = pairSystemsUnion
    
fudsFlatten :: Fud -> Fud
fudsFlatten ff  = 
    llqqff [pptt (ttpp (Transform (apply vv (vv `union` ww) xx,ww))) | tt <- ffqqll ff, let ww = der tt]
  where
    vv = fund ff
    xx = his ff
    pptt = partitionsTransformVarPartition 
    ttpp = transformsPartition
    apply vv ww xx = setVarsSetVarsSetHistogramsHistogramsApply vv ww xx (scalar 1)
    his = fudsSetHistogram
    fund = fudsUnderlying
    der = transformsDerived
    llqqff = fromJust . setTransformsFud . Set.fromList
    ffqqll = Set.toList . fudsSetTransform
    scalar q = fromJust $ histogramScalar q
    union = Set.union

setVariablesFudsSetTuple :: Set.Set Variable -> Fud -> Set.Set (Set.Set Variable)
setVariablesFudsSetTuple vv ff  = 
    llqq [kk | kk <- powerll (fvars ff `union` vv), (fder ff == empty) || (kk `cap` fder ff /= empty)]
  where
    fvars = fudsVars
    fder = fudsDerived
    powerll = Set.toList . setsPowerset
    llqq = Set.fromList
    union = Set.union
    cap = Set.intersection
    empty = Set.empty

{-
histogramsFudsLinearity :: Histogram -> Fud -> Bool
histogramsFudsLinearity aa ff =
    (fund ff `Set.isSubsetOf` vars aa) && (not $ fudsOverlap ff) && ((card $ trim $ fapply ff aa) > 1) && 
      (diag $ fapply ff aa)
  where
    fund = fudsUnderlying
    vars = histogramsVars
    fvars = fudsVars
    fapply = fudsHistogramsApply
    trim = histogramsTrim
    card = histogramsCardinality
    diag = histogramsIsDiagonal
-}

instance Represent Partition where
  represent (Partition qq) = represent qq

setComponentsPartition :: Set.Set Component -> Maybe Partition
setComponentsPartition qq
  | qq == Set.empty = Just (Partition qq)
  | ispart qq && okvars qq = Just (Partition qq)
  | otherwise = Nothing
  where 
    okvars qq = setsAll (\ss -> statesVars ss == pvars qq) (setSetsUnion qq)
    pvars qq = partitionsVars (Partition qq)
    ispart = setsIsPartition

-- AYOR
setComponentsPartition_u :: Set.Set Component -> Partition
setComponentsPartition_u = Partition

partitionsSetComponent :: Partition -> Set.Set Component
partitionsSetComponent (Partition qq) = qq

partitionEmpty :: Partition
partitionEmpty = Partition (Set.empty)

partitionScalar :: Partition
partitionScalar = Partition (Set.singleton (Set.singleton (stateEmpty)))

partitionsVars :: Partition -> Set.Set Variable
partitionsVars (Partition qq) 
  | qq == Set.empty = Set.empty
  | otherwise = statesVars $ Set.findMin $ Set.findMin qq
  
partitionsIsBinary :: Partition -> Bool
partitionsIsBinary (Partition qq) = Set.size qq == 2
  
systemsPartitionsIs :: System -> Partition -> Bool
systemsPartitionsIs uu pp = 
    case systemsVarsCartesian uu (pvars pp) of
      Just qq -> bigcup (ppqq pp) == qq
      Nothing -> False
  where
    bigcup = setSetsUnion
    pvars = partitionsVars
    ppqq = partitionsSetComponent
    
systemsSetPartition :: System -> Set.Set Partition
systemsSetPartition uu = 
    Set.fromList [Partition pp | ww <- power (uvars uu), pp <- parts (cart uu ww)]
  where
    power = Set.toList . setsPowerset
    uvars = systemsVars
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    parts = Set.toList . setsPartitionSet

systemsSetVarsPartitionsExpand :: System -> Set.Set Variable -> Partition -> Maybe Partition
systemsSetVarsPartitionsExpand uu vv pp = 
    case systemsVarsCartesian uu (vv Set.\\ (pvars pp)) of
      Just qq -> Just (Partition $ Set.map (\cc -> Set.fromList [ss `cup` rr | ss <- Set.toList cc, rr <- Set.toList qq]) $ ppqq pp)
      Nothing -> Nothing
  where
    pvars = partitionsVars
    ppqq = partitionsSetComponent
    cup = pairStatesUnionLeft

systemsSetVarsPartitionsContract :: System -> Partition -> Maybe Partition
systemsSetVarsPartitionsContract uu pp = 
    case cart (pvars pp) of
      Just _ -> Just $ snd $ head $ sort [(Set.size kk, rr) | kk <- power (pvars pp), 
                  let rr = reduce kk pp, expand (pvars pp) rr == pp]
      Nothing -> Nothing
  where
    power = Set.toList . setsPowerset
    cart = systemsVarsCartesian uu
    expand vv rr = fromJust (systemsSetVarsPartitionsExpand uu vv rr)
    reduce kk pp = Partition $ Set.map (\cc -> Set.map (\ss -> filt kk ss) cc) $ ppqq pp 
    filt = varsStatesFilter
    pvars = partitionsVars
    ppqq = partitionsSetComponent
    
systemsSetVarsPartitionUnary :: System -> Set.Set Variable -> Maybe Partition
systemsSetVarsPartitionUnary uu vv 
  | qq' /= Nothing = Just (Partition (single qq))
  | otherwise = Nothing
  where
    qq' = systemsVarsCartesian uu vv
    Just qq = qq'
    single = Set.singleton
   
systemsSetVarsPartitionPointedUnary :: System -> Set.Set Variable -> Maybe PartitionPointed
systemsSetVarsPartitionPointedUnary uu vv 
  | vv `subset` uvars uu = Just $ PartitionPointed (Partition (single (cart uu vv)), cart uu vv)
  | otherwise = Nothing
  where
    cart uu vv = fromJust $ systemsVarsCartesian uu vv  
    uvars = systemsVars
    subset = Set.isSubsetOf
    single = Set.singleton
        
systemsSetVarsPartitionSelf :: System -> Set.Set Variable -> Maybe Partition
systemsSetVarsPartitionSelf uu vv 
  | qq' /= Nothing = Just (Partition (Set.map single qq))
  | otherwise = Nothing
  where
    qq' = systemsVarsCartesian uu vv
    Just qq = qq'
    single = Set.singleton

systemsSetVarsPartitionSelf_u :: System -> Set.Set Variable -> Partition
systemsSetVarsPartitionSelf_u uu vv = Partition (Set.map single (cart uu vv))
  where
    cart = systemsSetVarsSetStateCartesian_u
    single = Set.singleton

systemsSetVarsSetPartitionFull :: System -> Set.Set Variable -> Maybe SetPartition
systemsSetVarsSetPartitionFull uu vv 
  | vv `subset` uvars uu = Just $ llqq [self uu (sgl v) | v <- qqll vv]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv  
    uvars = systemsVars
    subset = Set.isSubsetOf
    sgl = Set.singleton        
    qqll = Set.toList
    llqq = Set.fromList

systemsSetVarsSetPartitionPointedOne :: System -> Set.Set Variable -> Maybe SetPartitionPointed
systemsSetVarsSetPartitionPointedOne uu vv 
  | vv `subset` uvars uu = Just $ llqq [unary uu (sgl v) | v <- qqll vv]
  | otherwise = Nothing
  where
    unary uu vv = fromJust $ systemsSetVarsPartitionPointedUnary uu vv  
    uvars = systemsVars
    subset = Set.isSubsetOf
    sgl = Set.singleton        
    qqll = Set.toList
    llqq = Set.fromList

{-
partitionsTransform ::  Partition -> Transform
partitionsTransform pp =
    Transform (unit [ss `sunion` single w (ValInt i) | (i, cc) <- zip [1..] (ppqq pp), ss <- Set.toList cc],
      Set.singleton w)
  where
    w = VarInt $ (Set.findMax $ Set.map vvi $ pvars pp) + 1
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    pvars = partitionsVars
    ppqq = Set.toList . partitionsSetComponent
    vvi (VarInt i) = i
    vvi _ = 0
    sunion = pairStatesUnionLeft
    single = stateSingleton
-}

partitionsVariablesTransform ::  Partition -> Variable -> (Transform, System)
partitionsVariablesTransform pp w =
    (Transform (unit [ss `sunion` single w (ValInt i) | (i, cc) <- zip [1..] (ppll pp), ss <- qqll cc], ssing w), 
      System (msing w (llqq [ValInt i | i <- [1..card pp]])))
  where
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    ppqq = partitionsSetComponent
    sunion = pairStatesUnionLeft
    single = stateSingleton
    ppll = Set.toList . ppqq
    card = toInteger . Set.size . ppqq
    llqq = Set.fromList
    qqll = Set.toList
    ssing = Set.singleton
    msing = Map.singleton

partitionsTransformVarPartition ::  Partition -> Transform
partitionsTransformVarPartition pp =
    Transform (unit [ss `sunion` single w (ValComponent cc) | cc <- ppqq pp, ss <- Set.toList cc], Set.singleton w)
  where
    w = VarPartition pp
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    ppqq = Set.toList . partitionsSetComponent
    sunion = pairStatesUnionLeft
    single = stateSingleton

setPartitionsTransformVarPartition ::  SetPartition -> Maybe Transform
setPartitionsTransformVarPartition nn 
  | ff' /= Nothing = Just $ fftt ff
  | otherwise = Nothing
  where
    ff' = qqff $ llqq [pptt pp | pp <- qqll nn]
    Just ff = ff'
    fftt = fudsTransform
    qqff = setTransformsFud
    pptt = partitionsTransformVarPartition 
    qqll = Set.toList
    llqq = Set.fromList

transformsSetPartition ::  Transform -> SetPartition
transformsSetPartition tt = llqq [ttpp (Transform (his tt `red` (und tt `ins` w), sgl w)) | w <- qqll (der tt)]
  where
    his = transformsHistogram
    und = transformsUnderlying
    der = transformsDerived
    red aa vv = setVarsHistogramsReduce vv aa
    ttpp = transformsPartition
    ins qq x = Set.insert x qq
    sgl = Set.singleton
    qqll = Set.toList
    llqq = Set.fromList
    
partitionsCartesian ::  Partition -> Set.Set State
partitionsCartesian pp =
    Set.fromList [single (VarPartition pp) (ValComponent cc) | cc <- ppqq pp]
  where
    ppqq = Set.toList . partitionsSetComponent
    single = stateSingleton

transformsPartition :: Transform -> Partition
transformsPartition tt =
    Partition $ Set.map states $ inv tt
  where
    inv = Set.fromList . Map.elems . transformsInverse
    states = histogramsStates
    
systemsSetVarsTransformVarPartitionsExpand :: System -> Set.Set Variable -> Transform -> Maybe Transform
systemsSetVarsTransformVarPartitionsExpand uu vv tt
  | not (vv `subset` (uvars uu)) = Nothing
  | not (and [ispart w | w <- der tt]) = Nothing
  | otherwise = Just $ fftt $ qqff [pptt (expand pp) | VarPartition pp <- der tt]
  where
    ispart = variablesIsPartition
    subset = Set.isSubsetOf
    uvars = systemsVars
    expand rr = fromJust (systemsSetVarsPartitionsExpand uu vv rr)
    der = Set.toList . transformsDerived
    fftt = fudsTransform
    qqff = fromJust . setTransformsFud . Set.fromList
    pptt = partitionsTransformVarPartition 

systemsTransformVarPartitionsContract :: System -> Transform -> Maybe Transform
systemsTransformVarPartitionsContract uu tt
  | not (und tt `subset` (uvars uu)) = Nothing
  | not (and [ispart w | w <- der tt]) = Nothing
  | otherwise = Just $ fftt $ qqff [pptt (contract pp) | VarPartition pp <- der tt]
  where
    ispart = variablesIsPartition
    subset = Set.isSubsetOf
    uvars = systemsVars
    contract rr = fromJust (systemsSetVarsPartitionsContract uu rr)
    der = Set.toList . transformsDerived
    und = transformsUnderlying
    fftt = fudsTransform
    qqff = fromJust . setTransformsFud . Set.fromList
    pptt = partitionsTransformVarPartition 

transformVarPartitionsExplode :: Transform -> Maybe Fud
transformVarPartitionsExplode tt
  | not (and [ispart w | w <- der tt]) = Nothing
  | otherwise = Just $ qqff [pptt pp | VarPartition pp <- der tt]
  where
    ispart = variablesIsPartition
    der = Set.toList . transformsDerived
    qqff = fromJust . setTransformsFud . Set.fromList
    pptt = partitionsTransformVarPartition 
    
instance Represent PartitionPointed where
  represent (PartitionPointed qq) = represent qq

partitionsComponentsPartitionPointed :: Partition -> Component -> Maybe PartitionPointed
partitionsComponentsPartitionPointed pp ccp  
  | ccp `Set.member` (ppqq pp) = Just (PartitionPointed (pp,ccp))
  | otherwise = Nothing
  where 
    ppqq = partitionsSetComponent

partitionPointedsPartition :: PartitionPointed -> Partition
partitionPointedsPartition (PartitionPointed (pp,_)) = pp

partitionPointedsPoint :: PartitionPointed -> Component
partitionPointedsPoint (PartitionPointed (_,ccp)) = ccp

partitionPointedScalar :: PartitionPointed
partitionPointedScalar = PartitionPointed (Partition (Set.singleton (Set.singleton (stateEmpty))), Set.singleton (stateEmpty))

partitionPointedsVars :: PartitionPointed -> Set.Set Variable
partitionPointedsVars (PartitionPointed (pp,_)) = partitionsVars pp 

partitionPointedsIsPointSingleton :: PartitionPointed -> Bool
partitionPointedsIsPointSingleton (PartitionPointed (_,ccp)) = Set.size ccp == 1

partitionPointedsIsPartitionBinary :: PartitionPointed -> Bool
partitionPointedsIsPartitionBinary (PartitionPointed (pp,_)) = partitionsIsBinary pp

partitionBinaryPointedsComplement :: PartitionPointed -> Maybe PartitionPointed
partitionBinaryPointedsComplement ppp 
  | isbinary ppp = Just $ PartitionPointed (pp,ccp')
  | otherwise = Nothing
  where
    PartitionPointed (pp,ccp) = ppp
    ccp' = Set.findMin $ ccp `Set.delete` ppqq pp 
    isbinary = partitionPointedsIsPartitionBinary
    ppqq = partitionsSetComponent

partitionPointedsCartesian :: PartitionPointed -> Set.Set State
partitionPointedsCartesian (PartitionPointed (pp,_)) = partitionsCartesian pp

partitionPointedsPointState :: PartitionPointed -> State
partitionPointedsPointState (PartitionPointed (pp,ccp)) = stateSingleton (VarPartition pp) (ValComponent ccp)

partitionPointedsTransformVarPartition :: PartitionPointed -> Transform
partitionPointedsTransformVarPartition (PartitionPointed (pp,_)) = partitionsTransformVarPartition pp

partitionBinaryPointedsEqual :: PartitionPointed -> Maybe PartitionPointed
partitionBinaryPointedsEqual ppp 
  | isbinary ppp = Just $ PartitionPointed (Partition (Set.map sing (cart ppp)), sing (point ppp))
  | otherwise = Nothing
  where
    isbinary = partitionPointedsIsPartitionBinary
    ppqq = partitionsSetComponent
    cart = partitionPointedsCartesian
    point = partitionPointedsPointState
    sing = Set.singleton
    
partitionBinaryPointedsNot :: PartitionPointed -> Maybe PartitionPointed
partitionBinaryPointedsNot ppp 
  | isbinary ppp = Just $ PartitionPointed (Partition (Set.map sing (cart ppp)), sing (point (comp ppp)))
  | otherwise = Nothing
  where
    isbinary = partitionPointedsIsPartitionBinary
    ppqq = partitionsSetComponent
    cart = partitionPointedsCartesian
    point = partitionPointedsPointState
    sing = Set.singleton
    comp ppp = fromJust $ partitionBinaryPointedsComplement ppp
   
setPartitionBinaryPointedsAnd :: Set.Set PartitionPointed -> Maybe PartitionPointed
setPartitionBinaryPointedsAnd xx 
  | xx == Set.empty = Nothing
  | and [isbinary ppp | ppp <- qqll xx] = 
      Just $ PartitionPointed (Partition (Set.fromList [spoint xx, scart xx Set.\\ spoint xx]), spoint xx)
  | otherwise = Nothing
  where
    isbinary = partitionPointedsIsPartitionBinary
    scart xx = foldl1 mul [cart ppp | ppp <- qqll xx]
    spoint xx = foldl1 mul [sing (point ppp) | ppp <- qqll xx]
    mul aa bb = llqq [llss ((ssll ss) ++ (ssll tt)) | ss <- qqll aa, tt <- qqll bb]
    cart = partitionPointedsCartesian
    point = partitionPointedsPointState
    ppqq = partitionsSetComponent
    ssll = stateToList
    llss = stateFromList
    qqll = Set.toList
    llqq = Set.fromList
    sing = Set.singleton
        
setPartitionBinaryPointedsOr :: Set.Set PartitionPointed -> Maybe PartitionPointed
setPartitionBinaryPointedsOr xx 
  | xx == Set.empty = Nothing
  | and [isbinary ppp | ppp <- qqll xx] = 
      Just $ PartitionPointed (Partition (Set.fromList [spoint' xx, scart xx Set.\\ spoint' xx]), scart xx Set.\\ spoint' xx)
  | otherwise = Nothing
  where
    isbinary = partitionPointedsIsPartitionBinary
    scart xx = foldl1 mul [cart ppp | ppp <- qqll xx]
    spoint' xx = foldl1 mul [sing (point (comp ppp)) | ppp <- qqll xx]
    mul aa bb = llqq [llss ((ssll ss) ++ (ssll tt)) | ss <- qqll aa, tt <- qqll bb]
    cart = partitionPointedsCartesian
    point = partitionPointedsPointState
    ppqq = partitionsSetComponent
    ssll = stateToList
    llss = stateFromList
    qqll = Set.toList
    llqq = Set.fromList
    sing = Set.singleton
    comp ppp = fromJust $ partitionBinaryPointedsComplement ppp
            
treePartitionBinaryPointedsAnd :: Tree PartitionPointed -> Maybe (Tree PartitionPointed)
treePartitionBinaryPointedsAnd zz 
  | zz == emptyTree = Nothing
  | and [isbinary ppp | ppp <- zzll zz] = 
      Just $ Tree $ llmm [(ppp, trand ppp xx) | (ppp, xx) <- rel zz]
  | otherwise = Nothing
  where
    isbinary = partitionPointedsIsPartitionBinary
    zzll = Set.toList . treesElements 
    rel = Set.toList . treesRelation
    trand :: PartitionPointed -> Tree PartitionPointed -> Tree PartitionPointed
    trand ppp zz = Tree $ llmm [(mmp, trand mmp xx) | (rrp, xx) <- rel zz, let mmp = ppp `pppand` rrp]
    pppand :: PartitionPointed -> PartitionPointed -> PartitionPointed
    pppand ppp qqp = fromJust $ setPartitionBinaryPointedsAnd $ llqq [ppp, qqp]
    qqll = Set.toList
    llqq = Set.fromList
    llmm =  Map.fromList       
    
systemsVarsPartitionBinaryPointedsNullable :: System -> Variable -> PartitionPointed -> Maybe PartitionPointed    
systemsVarsPartitionBinaryPointedsNullable uu w ppp
  | not (w `mem` uvars uu) = Nothing
  | not (isbinary ppp) = Nothing
  | otherwise = Just $ PartitionPointed (Partition (qq `ins` ddp), ddp)
  where
    qq = Set.map sing (sing (point ppp) `mul` ucart w)
    ddp = sing (point (comp ppp)) `mul` ucart w
    uvars = systemsVars
    ucart w = fromJust $ systemsVarsCartesian uu (sing w)
    isbinary = partitionPointedsIsPartitionBinary
    comp ppp = fromJust $ partitionBinaryPointedsComplement ppp
    cart = partitionPointedsCartesian
    point = partitionPointedsPointState
    mul aa bb = llqq [llss ((ssll ss) ++ (ssll tt)) | ss <- qqll aa, tt <- qqll bb]
    ssll = stateToList
    llss = stateFromList
    qqll = Set.toList
    llqq = Set.fromList
    sing = Set.singleton    
    mem = Set.member
    ins qq e = Set.insert e qq
            
partitionPointedsStringsCartesianTransformVarString ::  PartitionPointed -> String -> Transform
partitionPointedsStringsCartesianTransformVarString ppp s =
    Transform (unit [ss `sunion` single w (val ss) | ss <- ppqq ppp], Set.singleton w)
  where
    w = VarStr s
    val ss = if ss==(point ppp) then ValStr "in" else ValStr "out"
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    ppqq = Set.toList . partitionPointedsCartesian
    point = partitionPointedsPointState
    sunion = pairStatesUnionLeft
    single = stateSingleton

partitionPointedsVariablesCartesianTransform ::  PartitionPointed -> Variable -> (Transform, System)
partitionPointedsVariablesCartesianTransform ppp w =
    (Transform (unit [ss `sunion` single w (val ss) | ss <- ppqq ppp], ssing w), 
      System (msing w (llqq [ValStr "in",ValStr "out"])))
  where
    val ss = if ss==(point ppp) then ValStr "in" else ValStr "out"
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    ppqq = Set.toList . partitionPointedsCartesian
    point = partitionPointedsPointState
    sunion = pairStatesUnionLeft
    llqq = Set.fromList
    single = stateSingleton
    ssing = Set.singleton
    msing = Map.singleton
    
instance Represent Decomp where
  represent (Decomp zz) = represent zz

decompsFud :: Decomp -> Fud 
decompsFud (Decomp zz) = Fud $ ran $ elem zz
  where
    ran = relationsRange
    elem = treesElements

decompsUnderlying :: Decomp -> Set.Set Variable
decompsUnderlying dd = fund $ ddff dd
  where
    ddff = decompsFud
    fund = fudsUnderlying

decompEmpty :: Decomp
decompEmpty = Decomp (Tree (Map.singleton (stateEmpty,transformEmpty) emptyTree))

treePairStateTransformsDecomp :: Tree (State,Transform) -> Maybe Decomp
treePairStateTransformsDecomp zz
    | ok zz = Just $ Decomp zz
    | otherwise = Nothing
  where
    ok zz = okVars zz && okRoots zz && okStates zz
    okVars zz = and [tder tt1 `cap` tvars tt2 == empty | tt1 <- qqll (ran (elem zz)), tt2 <- qqll (ran (elem zz)), tt1 /= tt2]
    okRoots zz = dom (roots zz) == sing stateEmpty
    okStates zz = and [ss `mem` std tt | ((_,tt),(ss,_)) <- qqll (steps zz)]
    tder = transformsDerived
    tvars = transformsVars
    std = transformsStateDeriveds
    dom = relationsDomain
    ran = relationsRange
    elem = treesElements
    roots = treesRoots
    steps = treesSteps
    qqll = Set.toList
    cap = Set.intersection
    empty = Set.empty
    sing = Set.singleton
    mem = Set.member
    
decompsTreePairStateTransform :: Decomp -> Tree (State,Transform)
decompsTreePairStateTransform (Decomp zz) = zz

decompsHistogramsApply :: Decomp -> Histogram -> Tree (State,Histogram)
decompsHistogramsApply (Decomp zz) aa = apply zz (vars aa) aa
  where
    apply :: Tree (State,Transform) -> Set.Set Variable -> Histogram -> Tree (State,Histogram)
    apply zz vv aa = Tree $ llmm $ [((ss, bb `red` ww), apply yy vv bb) | 
         ((ss,Transform (xx,ww)),yy) <- zzll zz, let bb = aa `mul` (single ss 1) `mul` xx `red` (vv `cup` vars xx)]
    single ss c = fromJust $ histogramSingleton ss c
    mul = pairHistogramsMultiply
    red = flip setVarsHistogramsReduce
    vars = histogramsVars
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union

decompsHistogramsMultiply :: Decomp -> Histogram -> Tree (State,Histogram)
decompsHistogramsMultiply (Decomp zz) aa = apply zz (vars aa) aa
  where
    apply :: Tree (State,Transform) -> Set.Set Variable -> Histogram -> Tree (State,Histogram)
    apply zz vv aa = Tree $ llmm $ [((ss, bb), apply yy vv bb) | 
         ((ss,Transform (xx,ww)),yy) <- zzll zz, let bb = aa `mul` (single ss 1) `mul` xx `red` (vv `cup` ww)]
    single ss c = fromJust $ histogramSingleton ss c
    mul = pairHistogramsMultiply
    red = flip setVarsHistogramsReduce
    vars = histogramsVars
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union

decompsHistogramsHistogramsQuery :: Decomp -> Histogram -> Histogram -> Tree (State,Histogram)
decompsHistogramsHistogramsQuery (Decomp zz) aa qq = query zz (vars aa) aa qq
  where
    query :: Tree (State,Transform) -> Set.Set Variable -> Histogram -> Histogram -> Tree (State,Histogram)
    query zz vv aa qq = Tree $ llmm $ [((ss, bb `red` vv), query yy vv bb rr) | 
         ((ss,Transform (xx,ww)),yy) <- zzll zz, 
         let rr = qq `mul` (single ss 1) `mul` xx `red` (vv `cup` ww), size rr > 0,
         let bb = aa `mul` (single ss 1) `mul` xx `mul` (rr `red` ww) `red` (vv `cup` ww)]
    single ss c = fromJust $ histogramSingleton ss c
    mul = pairHistogramsMultiply
    red = flip setVarsHistogramsReduce
    size = histogramsSize
    vars = histogramsVars
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union

decompsSetDecompDistinct :: Decomp -> Set.Set Decomp
decompsSetDecompDistinct (Decomp zz) = Set.map Decomp (treesDistinct zz)
    
decompsIsDistinct :: Decomp -> Bool
decompsIsDistinct (Decomp zz) = treesIsDistinct zz
    
systemsDecompsIs :: System -> Decomp -> Bool
systemsDecompsIs uu dd =
    and [isState uu ss | ss <- states dd] && and [isOneFunc uu tt | tt <- ddqq dd]
  where 
    ddqq = Set.toList . fudsSetTransform . decompsFud
    isOneFunc = systemsTransformsIsOneFunc
    isState = systemsStatesIs
    states = Set.toList . relationsDomain . treesElements . ddzz
    ddzz = decompsTreePairStateTransform
    
systemsDecompsApplication :: System -> Decomp -> Maybe (Tree (Transform,State))
systemsDecompsApplication uu dd 
  | issys uu dd && isdistinct dd = Just $ app zz stateEmpty stateEmpty fudEmpty
  | otherwise = Nothing
  where 
    Decomp zz = dd
    issys = systemsDecompsIs
    isdistinct = decompsIsDistinct
    app :: Tree (State,Transform) -> State -> State -> Fud -> Tree (Transform,State)
    app dd qq rr ff = Tree $ llmm $ 
       [((tt,ss), app ee ss (rr `scup` ss) (ff `fins` tt)) | 
         ((pp,tt),ee) <- zzll dd, pp==qq, ss <- qqll (dom (roots ee)), isjoin rr ss,
         (rr `scup` ss) `mem` states (scalar 1 `fmul` (ff `fins` tt `fins` tdis (single rr 1)))] ++
       [((tt,ss), emptyTree) | 
         ((pp,tt),ee) <- zzll dd, pp==qq, ss <- qqll (cart (der tt) Set.\\ dom (roots ee)), isjoin rr ss,
         (rr `scup` ss) `mem` states (scalar 1 `fmul` (ff `fins` tt `fins` tdis (single rr 1)))]
    isjoin = pairStatesIsJoin
    scup = pairStatesUnionLeft         
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    std ff = states (ttaa (fftt ff) `red` fder ff)
    tdis =  histogramsTransformDisjoint
    single ss c = fromJust $ histogramSingleton ss c
    fmul aa ff = fudsHistogramsApply ff aa
    scalar q = fromJust $ histogramScalar q
    states = histogramsStates
    ttaa = transformsHistogram
    red aa vv = setVarsHistogramsReduce vv aa
    fder = fudsDerived
    fftt = fudsTransform
    zzll (Tree mm) = mmll mm
    roots = treesRoots
    ffqq = fudsSetTransform
    cart vv = fromJust $ systemsVarsCartesian uu vv
    der = transformsDerived
    dom = relationsDomain
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    mem = Set.member
    
systemsDecompsApplication_1 :: System -> Decomp -> Maybe (Tree (Transform,State))
systemsDecompsApplication_1 uu dd 
  | issys uu dd && isdistinct dd = Just $ app zz stateEmpty stateEmpty fudEmpty
  | otherwise = Nothing
  where 
    Decomp zz = dd
    issys = systemsDecompsIs
    isdistinct = decompsIsDistinct
    app :: Tree (State,Transform) -> State -> State -> Fud -> Tree (Transform,State)
    app dd qq rr ff = Tree $ llmm $ 
       [((tt,ss), app ee ss (rr `scup` ss) (ff `fins` tt)) | 
         ((pp,tt),ee) <- zzll dd, pp==qq, ss <- qqll (dom (roots ee)), isjoin rr ss,
         (rr `scup` ss) `mem` std (ff `fins` tt)] ++
       [((tt,ss), emptyTree) | 
         ((pp,tt),ee) <- zzll dd, pp==qq, ss <- qqll (cart (der tt) Set.\\ dom (roots ee)), isjoin rr ss,
         (rr `scup` ss) `mem` std (ff `fins` tt)]
    isjoin = pairStatesIsJoin
    scup = pairStatesUnionLeft         
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    std ff = states (ttaa (fftt ff) `red` fder ff)
    states = histogramsStates
    ttaa = transformsHistogram
    red aa vv = setVarsHistogramsReduce vv aa
    fder = fudsDerived
    fftt = fudsTransform
    zzll (Tree mm) = mmll mm
    roots = treesRoots
    ffqq = fudsSetTransform
    cart vv = fromJust $ systemsVarsCartesian uu vv
    der = transformsDerived
    dom = relationsDomain
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    mem = Set.member    
    
applicationsDecomp :: Tree (Transform,State) -> Maybe Decomp
applicationsDecomp zz = dec $ decomp stateEmpty zz
  where 
    dec = treePairStateTransformsDecomp
    decomp :: State -> Tree (Transform,State) -> Tree (State,Transform)
    decomp rr dd = Tree $ llmm $ [((rr,tt), (Tree $ Map.unions [nn | 
      ((_,ss),ee) <- zzll dd, let Tree nn = decomp ss ee])) | tt <- qqll (dom (roots dd))]
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    roots = treesRoots
    dom = relationsDomain
    qqll = Set.toList

systemsDecompsIsWellBehaved :: System -> Decomp -> Bool
systemsDecompsIsWellBehaved uu dd =
    aa' /= Nothing && ee' /= Nothing && ee == dd
  where 
    aa' = systemsDecompsApplication uu dd
    aa = fromJust aa'
    ee' = applicationsDecomp aa
    ee = fromJust ee'
    
applicationsSymmetryApplications :: Tree (Transform,State) -> Set.Set (Tree (Transform,State), Set.Set [(Transform,State)])
applicationsSymmetryApplications = 
    llqq . mmll . Map.filter (\qq -> Set.size qq > 1) . inv . Map.filter (\zz -> zz /= empty) . llmm . qqll . places
  where 
    places = treesPlaces
    empty = emptyTree
    inv = functionsInverse
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    llqq = Set.fromList
        
applicationsLocations :: Tree (Transform,State) -> Set.Set ([(Transform,State)],Transform)
applicationsLocations zz = 
    llqq [(ll,tt) | (ll,ee) <- qqll (places zz), ((tt,_),_) <- zzll ee]
  where 
    places = treesPlaces
    zzll (Tree mm) = mmll mm
    mmll = Map.toList
    qqll = Set.toList
    llqq = Set.fromList
    
applicationsSymmetryTransforms :: Tree (Transform,State) -> Set.Set (Transform, Set.Set [(Transform,State)])
applicationsSymmetryTransforms = 
    llqq . mmll . Map.filter (\qq -> Set.size qq > 1) . inv . llmm . qqll . loc
  where 
    loc = applicationsLocations
    inv = functionsInverse
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    llqq = Set.fromList
    
systemsDecompsPartition :: System -> Decomp -> Maybe Partition
systemsDecompsPartition uu dd 
  | zz' /= Nothing = Just $ Partition $ 
      llqq [states (apply vv vv (his ff `ins` cart uu vv `ins` single ss 1) (scalar 1)) | 
        ll <- qqll (paths zz), let ff = fud ll, let ss = state ll]
  | otherwise = Nothing
  where 
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    vv = dund dd
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    state ll = foldl1 scup $ map snd ll
    fud ll = foldl fins fudEmpty $ map fst ll
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    ffqq = fudsSetTransform
    ins qq aa = Set.insert aa qq
    scalar q = fromJust $ histogramScalar q
    his = fudsSetHistogram
    unit qq = fromJust $ setStatesHistogramUnit qq
    cart uu vv = unit $ fromJust $ systemsVarsCartesian uu vv
    scup = pairStatesUnionLeft         
    paths = treesPaths
    dund = decompsUnderlying
    states = histogramsStates
    qqll = Set.toList
    llqq = Set.fromList
    single ss c = fromJust $ histogramSingleton ss c

systemsDecompsPartition_1 :: System -> Decomp -> Maybe Partition
systemsDecompsPartition_1 uu dd 
  | zz' /= Nothing = Just $ Partition $ 
      llqq [states (ddaa dd `mul` single (state ll) 1 `red` dund dd) | ll <- qqll (paths zz)]
  | otherwise = Nothing
  where 
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ddaa =  ttaa . fftt . ddff
    ddff = decompsFud
    fftt = fudsTransform
    ttaa = transformsHistogram
    paths = treesPaths
    dund = decompsUnderlying
    states = histogramsStates
    state ll = foldl1 scup $ map snd ll
    scup = pairStatesUnionLeft         
    qqll = Set.toList
    llqq = Set.fromList
    mul = pairHistogramsMultiply
    single ss c = fromJust $ histogramSingleton ss c
    red aa vv = setVarsHistogramsReduce vv aa

systemsDecompsComponents :: System -> Decomp -> Maybe (Tree Component)
systemsDecompsComponents uu dd 
  | zz' /= Nothing = Just $ funcsTreesMapAccum comp zz
  | otherwise = Nothing
  where 
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    vv = dund dd
    comp :: [(Transform,State)] -> Component
    comp ll = states (apply vv vv (his (fud ll) `ins` cart uu vv `ins` single (state ll) 1) (scalar 1))
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    state ll = foldl1 scup $ map snd ll
    fud ll = foldl fins fudEmpty $ map fst ll
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    ffqq = fudsSetTransform
    ins qq aa = Set.insert aa qq
    scalar q = fromJust $ histogramScalar q
    his = fudsSetHistogram
    unit qq = fromJust $ setStatesHistogramUnit qq
    cart uu vv = unit $ fromJust $ systemsVarsCartesian uu vv
    scup = pairStatesUnionLeft         
    dund = decompsUnderlying
    states = histogramsStates
    single ss c = fromJust $ histogramSingleton ss c      
   
systemsDecompsComponents_1 :: System -> Decomp -> Maybe (Tree Component)
systemsDecompsComponents_1 uu dd 
  | zz' /= Nothing = Just $ funcsTreesMapAccum comp zz
  | otherwise = Nothing
  where 
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    comp :: [(Transform,State)] -> Component
    comp ll = states (inv (fftt (fud ll)) Map.! state ll `mul` cart uu (dund dd))
    inv = transformsInverse
    state ll = foldl1 scup $ map snd ll
    fud ll = foldl fins fudEmpty $ map fst ll
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    ffqq = fudsSetTransform
    fftt = fudsTransform
    dund = decompsUnderlying
    cart uu vv = fromJust $ setStatesHistogramUnit $ fromJust $ systemsVarsCartesian uu vv
    mul = pairHistogramsMultiply
    states = histogramsStates
    scup = pairStatesUnionLeft         
   
decompsContingents :: Decomp -> Tree (Histogram,Transform)
decompsContingents dd = 
    funcsTreesMapAccum cont (ddzz dd)
  where 
    vv = dund dd
    ddzz = decompsTreePairStateTransform
    cont :: [(State,Transform)] -> (Histogram,Transform)
    cont [(_,tt)] = (scalar 1,tt)
    cont ll = (eff (apply vv vv (his (fud ll) `ins` single (state ll) 1) (scalar 1)), snd (last ll))
    apply = setVarsSetVarsSetHistogramsHistogramsApply    
    state ll = foldl1 scup $ map fst ll
    scup = pairStatesUnionLeft         
    fud ll = foldl fins fudEmpty $ map snd (init ll)
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    ffqq = fudsSetTransform
    ins qq aa = Set.insert aa qq
    scalar q = fromJust $ histogramScalar q
    his = fudsSetHistogram
    unit qq = fromJust $ setStatesHistogramUnit qq
    cart uu vv = unit $ fromJust $ systemsVarsCartesian uu vv
    dund = decompsUnderlying
    eff = histogramsEffective
    single ss c = fromJust $ histogramSingleton ss c          
   
decompsContingents_1 :: Decomp -> Tree (Histogram,Transform)
decompsContingents_1 dd = 
    funcsTreesMapAccum cont (ddzz dd)
  where 
    ddzz = decompsTreePairStateTransform
    cont :: [(State,Transform)] -> (Histogram,Transform)
    cont [(_,tt)] = (scalar 1,tt)
    cont ll = (inv (fftt (fud ll)) Map.! state ll, snd (last ll))
    inv = transformsInverse
    state ll = foldl1 scup $ map fst ll
    fud ll = foldl fins fudEmpty $ map snd (init ll)
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    ffqq = fudsSetTransform
    fftt = fudsTransform
    scup = pairStatesUnionLeft       
    scalar q = fromJust $ histogramScalar q
    
systemsApplicationsTreeSliceTransform :: System -> Tree (Transform,State) -> Maybe (Tree PartitionPointed)
systemsApplicationsTreeSliceTransform uu zz 
  | ok zz = Just $ funcsTreesMap slice zz
  | otherwise = Nothing
  where
    slice :: (Transform,State) -> PartitionPointed
    slice (tt,ss) = ppppp (qqpp (llqq [sing ss, cart uu (tder tt) `del` ss])) (sing ss)
    ok zz = and [(tder tt) `subs` (uvars uu) && ss `mem` cart uu (tder tt) | (tt,ss) <- zzll zz]
    qqpp = fromJust . setComponentsPartition
    ppppp pp cc = fromJust $ partitionsComponentsPartitionPointed pp cc
    zzll = Set.toList . treesElements 
    tder = transformsDerived
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    subs = Set.isSubsetOf
    llqq = Set.fromList
    sing = Set.singleton
    del qq x = Set.delete x qq
    mem = Set.member
  
systemsApplicationsListVariablesTreeSliceTransform :: 
  forall m . (Ord m, Model m) =>  System -> Tree (m,State) -> [Variable] -> 
  Maybe (Tree (Transform,(m,State)),(System,[Variable]))
systemsApplicationsListVariablesTreeSliceTransform uu zz ll
  | ok zz = Just $ (yy, (uu', kk))
  | otherwise = Nothing
  where
    (yy,kk) = funcsListsTreesTraversePreOrder slice ll zz
    uu' = uunion uu (System $ Map.fromList [(w, Set.fromList [ValStr "in", ValStr "out"]) | (tt,x) <- qqll (treesElements yy), w <- qqll (derived tt)])
    slice :: forall m . (Ord m, Model m) =>  (m,State) -> Variable -> (Transform,(m,State))
    slice (tt,ss) w = (Transform (unit [rr `sunion` ssing w (val ss rr) | rr <- qqll (cart uu (derived tt))], sing w),(tt,ss))
    val ss rr = if rr==ss then ValStr "in" else ValStr "out"
    ok zz = and [(derived tt) `subs` (uvars uu) && ss `mem` cart uu (derived tt) | (tt,ss) <- qqll (treesElements zz)]
    uunion = pairSystemsUnion
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    sunion = pairStatesUnionLeft
    ssing = stateSingleton
    tder = transformsDerived
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    subs = Set.isSubsetOf
    qqll = Set.toList
    sing = Set.singleton
    mem = Set.member
  
systemsApplicationsTreeSliceContingent :: System -> Tree (Transform,State) -> Maybe (Tree PartitionPointed)
systemsApplicationsTreeSliceContingent uu zz 
  | yy' /= Nothing = treePartitionBinaryPointedsAnd yy
  | otherwise = Nothing
  where
    yy' = systemsApplicationsTreeSliceTransform uu zz
    yy = fromJust yy'
  
-- not checked AYOR 
systemsTreeSliceTransformsListVariablesTreeSliceContingent :: 
  forall m . Ord m => System -> Tree (Transform,(m,State)) -> [Variable] -> 
  (Tree (Transform,(Transform,(m,State))),(System,[Variable]))
systemsTreeSliceTransformsListVariablesTreeSliceContingent uu zz ll = (yy, (uu', kk))
  where
    (rr,kk) = funcsListsTreesTraversePreOrder (,) (take (Set.size (treesRoots zz)) ll ++ ll) zz
    uu' = uunion uu (System $ Map.fromList [(w, Set.fromList [ValStr "in", ValStr "out"]) | (_,w) <- qqll (treesElements rr)])
    yy = Tree $ Map.fromList [((tt,(tt,x)), trand tt xx) | (((tt,x),_), xx) <- Set.toList (treesRelation rr)]
    trand :: forall x . Ord x => Transform -> Tree ((Transform,x),Variable) -> Tree (Transform,(Transform,x))
    trand tt1 qq = Tree $ Map.fromList [((tt3,(tt2,x)), trand tt3 xx) | 
      (((tt2,x),w), xx) <- Set.toList (treesRelation qq), let tt3 = wand tt1 tt2 w]
    wand :: Transform -> Transform -> Variable -> Transform
    wand tt1 tt2 w = Transform (unit [ss3 `sunion` ssing w (val ss3) | 
      ss1 <- qqll (cart uu' (tder tt1)), ss2 <- qqll (cart uu' (tder tt2)), let ss3 = ss1 `sunion` ss2], sing w)
    val (State mm) = if ran (mmqq mm) == sing (ValStr "in") then ValStr "in" else ValStr "out"
    uunion = pairSystemsUnion
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    sunion = pairStatesUnionLeft
    ssing = stateSingleton
    tder = transformsDerived
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqll = Set.toList
    sing = Set.singleton
    mmqq = functionsRelation
    ran = relationsRange
        
systemsApplicationsSlices :: System -> Tree (Transform,State) -> Maybe Fud
systemsApplicationsSlices uu zz 
  | yy' /= Nothing && xx' /= Nothing = Just $ Fud $ Set.map ppptt (elem yy `union` elem xx)
  | otherwise = Nothing
  where
    yy' = systemsApplicationsTreeSliceTransform uu zz
    yy = fromJust yy'
    xx' = treePartitionBinaryPointedsAnd yy
    xx = fromJust xx'
    ppptt = partitionPointedsTransformVarPartition 
    elem = treesElements 
    union = Set.union
    
systemsApplicationsListVariablesSlices :: System -> Tree (Transform,State) -> [Variable] -> Maybe (Fud,(System,[Variable]))
systemsApplicationsListVariablesSlices uu zz ll
  | yy' /= Nothing = Just $ (Fud (relationsDomain (treesElements yy) `union` relationsDomain (treesElements xx)),(uu2,ll2))
  | otherwise = Nothing
  where
    yy' = systemsApplicationsListVariablesTreeSliceTransform uu zz ll
    (yy,(uu1,ll1)) = fromJust yy'
    (xx,(uu2,ll2)) = systemsTreeSliceTransformsListVariablesTreeSliceContingent uu1 yy ll1
    ppptt = partitionPointedsTransformVarPartition 
    union = Set.union
        
systemsDecompsTransformCrown :: System -> Decomp -> Maybe Transform
systemsDecompsTransformCrown uu dd 
  | zz' /= Nothing && ff' /= Nothing = Just $ fftt $ Fud $ ffqq ff `union` (ffqq (ddff dd))
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsSlices uu zz
    ff = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt = fudsTransform
    union = Set.union
    
systemsDecompsListVariablesTransformCrown :: System -> Decomp -> [Variable] -> Maybe (Transform,(System,[Variable]))
systemsDecompsListVariablesTransformCrown uu dd ll
  | zz' /= Nothing && ff' /= Nothing = Just ((fftt $ Fud $ ffqq ff `Set.union` (ffqq (ddff dd))),(uu1,ll1))
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsListVariablesSlices uu zz ll
    (ff,(uu1,ll1)) = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt gg = Transform (eff (apply (fund gg) (fder gg `Set.union` fund gg) (fudsSetHistogram gg) (scalar 1)), fder gg)
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    scalar q = fromJust $ histogramScalar q
    fder = fudsDerived
    fund = fudsUnderlying
    eff = histogramsEffective

systemsDecompsListVariablesTransformCrown_1 :: System -> Decomp -> [Variable] -> Maybe (Transform,(System,[Variable]))
systemsDecompsListVariablesTransformCrown_1 uu dd ll
  | zz' /= Nothing && ff' /= Nothing = Just ((fftt $ Fud $ ffqq ff `union` (ffqq (ddff dd))),(uu1,ll1))
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsListVariablesSlices uu zz ll
    (ff,(uu1,ll1)) = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt = fudsTransform
    union = Set.union
    
systemsApplicationsMapVariablesSliceAlternate :: System -> Tree (Transform,State) -> Maybe (Map.Map Variable PartitionPointed)
systemsApplicationsMapVariablesSliceAlternate uu zz 
  | yy' /= Nothing = Just $ Map.map pppor $ inv $ slv yy
  | otherwise = Nothing
  where
    yy' = systemsApplicationsTreeSliceContingent uu zz
    yy = fromJust yy'
    slv yy = llqq [(ppp,w) | (ppp,qqp) <- qqll (steps yy), let pp = ppppp ppp, 
      xx <- qqll (pppvars qqp), xx /= VarPartition pp, w <- qqll (ppvars xx)]
    ppppp = partitionPointedsPartition
    pppvars = partitionPointedsVars
    ppvars (VarPartition pp) = partitionsVars pp
    steps = treesSteps
    der = transformsDerived
    qqll = Set.toList
    llqq = Set.fromList
    inv = relationsInverse  
    pppor qq = fromJust $ setPartitionBinaryPointedsOr qq
   
-- not checked AYOR     
systemsTreeSliceContingentsListVariablesMapVariablesSliceAlternate :: 
  forall m . (Ord m, Model m) => System -> Tree (Transform,(Transform,(m,State))) -> [Variable] -> 
  ((Map.Map Variable Transform),(System,[Variable]))
systemsTreeSliceContingentsListVariablesMapVariablesSliceAlternate uu zz ll = (yy, (uu', ll'))
  where
    (pp,rr) = Set.partition (\(_,nn) -> Set.size nn > 1) (rel (inv (slv zz)))
    qq = relationsRange pp
    xx = Map.fromList [(nn, (wor vv w,w)) | (nn,w) <- zip (qqll qq) ll, let vv = setSetsUnion (Set.map der nn)]
    ll' = drop (Set.size qq) ll
    uu' = uunion uu (System $ Map.fromList [(w, Set.fromList [ValStr "in", ValStr "out"]) | (_,w) <- zip (qqll qq) ll]) 
    yy = Map.fromList ([(u,tt) | (u,nn) <- qqll pp, let (tt,w) = xx Map.! nn] ++ [(u,tt) | (u,nn) <- qqll rr, tt <- qqll nn])
    slv zz = llqq [(tt1,u) | ((tt1,_),(_,(_,(tt2,_)))) <- qqll (steps zz), u <- qqll (derived tt2)]
    wor :: Set.Set Variable -> Variable -> Transform
    wor ww w = Transform (unit [ss `sunion` ssing w (val ss) | ss <- qqll (cart uu ww)], sing w)
    val (State mm) = if (ValStr "in") `mem` ran (mmqq mm) then ValStr "in" else ValStr "out"    
    uunion = pairSystemsUnion
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    sunion = pairStatesUnionLeft
    ssing = stateSingleton
    der = transformsDerived
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    und = transformsUnderlying
    inv = relationsInverse  
    rel = functionsRelation
    steps = treesSteps
    llqq = Set.fromList
    qqll = Set.toList
    mmqq = functionsRelation
    ran = relationsRange
    mem = Set.member
    sing = Set.singleton
    
systemsApplicationsSetNullable :: System -> Tree (Transform,State) -> Maybe (Set.Set PartitionPointed)
systemsApplicationsSetNullable uu zz 
  | mm' /= Nothing && keys mm `subs` uvars uu = Just $ Set.map nullable (mmqq mm)
  | otherwise = Nothing
  where
    mm' = systemsApplicationsMapVariablesSliceAlternate uu zz
    mm = fromJust mm'
    nullable (w,ppp) = fromJust $ systemsVarsPartitionBinaryPointedsNullable uu w ppp
    uvars = systemsVars
    keys = Map.keysSet
    subs = Set.isSubsetOf
    mmqq = functionsRelation
    
-- not checked AYOR     
systemsMapVariablesSliceAlternatesListVariablesFudNullable :: System -> Map.Map Variable Transform -> [Variable] -> (Fud,(System,[Variable]))
systemsMapVariablesSliceAlternatesListVariablesFudNullable uu mm ll = (ff, (uu', ll'))
  where
    ll' = drop (Map.size mm) ll
    uu' = uunion uu (System $ llmm [(w, ValStr "null" `Set.insert` uvals uu u) | ((u,_),w) <- zip (mmll mm) ll]) 
    ff = Fud $ Set.fromList [null u (der tt) w | ((u,tt),w) <- zip (mmll mm) ll]
    null :: Variable -> Set.Set Variable -> Variable -> Transform
    null u ww w = Transform (unit [ss `sunion` rr `sunion` ssing w (val ss (sval rr u)) | 
      ss <- qqll (cart uu ww), rr <- qqll (cart uu (sing u))], sing w)
    val (State mm) x = if (ValStr "in") `mem` ran (mmqq mm) then x else ValStr "null"   
    uvals uu v = fromJust $ systemsVarsSetValue uu v
    sval rr u = fromJust $ statesVarsValue rr u 
    uunion = pairSystemsUnion
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    sunion = pairStatesUnionLeft
    ssing = stateSingleton
    der = transformsDerived
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqll = Set.toList
    mmll = Map.toList
    llmm = Map.fromList
    sing = Set.singleton    
    mmqq = functionsRelation
    ran = relationsRange
    mem = Set.member
 
systemsApplicationsNullable :: System -> Tree (Transform,State) -> Maybe Fud
systemsApplicationsNullable uu zz 
  | ok = Just $ Fud $ 
      rtfull zz `Set.union` Set.map ppptt (nonleaves slt `union` nonleaves slc `union` ran vsla `union` nl)
  | otherwise = Nothing
  where
    ok = slt' /= Nothing && slc' /= Nothing && vsla' /= Nothing && nl' /= Nothing
    slt' = systemsApplicationsTreeSliceTransform uu zz
    slt = fromJust slt'
    slc' = systemsApplicationsTreeSliceContingent uu zz
    slc = fromJust slc'
    vsla' = systemsApplicationsMapVariablesSliceAlternate uu zz
    vsla = fromJust vsla'
    nl' = systemsApplicationsSetNullable uu zz
    nl = fromJust nl'
    rtfull zz = llqq [full w | (tt,_) <- qqll (roots zz), w <- qqll (der tt)]
    full w = pptt $ qqpp $ Set.map sing (cart uu w)
    cart uu w = fromJust $ systemsVarsCartesian uu (sing w)
    qqpp qq = fromJust $ setComponentsPartition qq
    pptt = partitionsTransformVarPartition 
    ppptt = partitionPointedsTransformVarPartition 
    nonleaves zz = elem zz `minus` leaves zz
    elem = treesElements 
    union = Set.union
    minus = Set.difference
    roots = treesRoots
    leaves = treesLeaves
    ran = Set.fromList . Map.elems
    qqll = Set.toList
    llqq = Set.fromList
    der = transformsDerived
    sing = Set.singleton
       
systemsApplicationsListVariablesNullable :: 
  forall m . (Ord m, Model m) => System -> Tree (m,State) -> [Variable] -> Maybe (Fud,(System,[Variable]))
systemsApplicationsListVariablesNullable uu zz ll
  | ok = Just $ (ff, (uu4, ll4))
  | otherwise = Nothing
  where
    ok = Set.size (dom (roots zz))==1 && slt' /= Nothing
    ff = Fud $ ffqq ffr `union` Set.map fst (nonleaves1 slt) `union` Set.map fst (nonleaves2 slc) `union` ran vsla `union` ffqq nl
    (ttr,_) = Set.findMin (roots zz)
    ll0 = drop (Set.size (derived ttr)) ll
    uu0 = uunion uu (System $ llmm [(w, uvals uu u) | (u,w) <- zip (qqll (derived ttr)) ll]) 
    ffr = Fud $ Set.fromList [full u w | (u,w) <- zip (qqll (derived ttr)) ll]
    full :: Variable -> Variable -> Transform
    full u w = Transform (unit [ss `sunion` ssing w (sval ss u) | ss <- qqll (cart uu (sing u))], sing w)   
    slt' = systemsApplicationsListVariablesTreeSliceTransform uu0 zz ll0
    Just (slt,(uu1,ll1)) = slt'
    (slc,(uu2,ll2)) = systemsTreeSliceTransformsListVariablesTreeSliceContingent uu1 slt ll1
    (vsla,(uu3,ll3)) = systemsTreeSliceContingentsListVariablesMapVariablesSliceAlternate uu2 slc ll2
    (nl, (uu4, ll4)) = systemsMapVariablesSliceAlternatesListVariablesFudNullable uu3 vsla ll3
    ffqq = fudsSetTransform
    uvals uu v = fromJust $ systemsVarsSetValue uu v
    sval rr u = fromJust $ statesVarsValue rr u 
    uunion = pairSystemsUnion
    unit qq = fromJust $ setStatesHistogramUnit $ Set.fromList qq
    sunion = pairStatesUnionLeft
    ssing = stateSingleton
    der = transformsDerived
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    nonleaves1 zz = treesElements zz `Set.difference` treesLeaves zz
    nonleaves2 zz = treesElements zz `Set.difference` treesLeaves zz
    roots = treesRoots
    dom = relationsDomain 
    ran = Set.fromList . Map.elems
    union = Set.union
    sing = Set.singleton        
    qqll = Set.toList
    mmll = Map.toList      
    llmm = Map.fromList
    
systemsDecompsNullable :: System -> Decomp -> Maybe Fud
systemsDecompsNullable uu dd
  | zz' /= Nothing = ff'
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsNullable uu zz
              
systemsDecompsTransform :: System -> Decomp -> Maybe Transform
systemsDecompsTransform uu dd 
  | ff' /= Nothing = Just $ fftt $ Fud $ ffqq ff `union` (ffqq (ddff dd))
  | otherwise = Nothing
  where
    ff' = systemsDecompsNullable uu dd
    ff = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt gg = Transform (eff (apply (fund gg) (fder gg `Set.union` fund gg) (fudsSetHistogram gg) (scalar 1)), fder gg)
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    scalar q = fromJust $ histogramScalar q
    fder = fudsDerived
    fund = fudsUnderlying
    eff = histogramsEffective
    union = Set.union
     
systemsDecompsTransform_1 :: System -> Decomp -> Maybe Transform
systemsDecompsTransform_1 uu dd 
  | zz' /= Nothing && ff' /= Nothing = Just $ fftt $ Fud $ ffqq ff `union` (ffqq (ddff dd))
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsNullable uu zz
    ff = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt = fudsTransform
    union = Set.union
      
systemsDecompsListVariablesNullable :: System -> Decomp -> [Variable] -> Maybe (Fud,(System,[Variable]))
systemsDecompsListVariablesNullable uu dd ll
  | zz' /= Nothing = ff'
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsListVariablesNullable uu zz ll
      
systemsDecompsListVariablesTransform :: System -> Decomp -> [Variable] -> Maybe (Transform,(System,[Variable]))
systemsDecompsListVariablesTransform uu dd ll
  | ff' /= Nothing = Just ((fftt $ Fud $ ffqq ff `Set.union` (ffqq (ddff dd))),(uu1,ll1))
  | otherwise = Nothing
  where
    ff' = systemsDecompsListVariablesNullable uu dd ll
    (ff,(uu1,ll1)) = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt gg = Transform (eff (apply (fund gg) (fder gg `Set.union` fund gg) (fudsSetHistogram gg) (scalar 1)), fder gg)
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    scalar q = fromJust $ histogramScalar q
    fder = fudsDerived
    fund = fudsUnderlying
    eff = histogramsEffective
      
systemsDecompsListVariablesTransform_1 :: System -> Decomp -> [Variable] -> Maybe (Transform,(System,[Variable]))
systemsDecompsListVariablesTransform_1 uu dd ll
  | zz' /= Nothing && ff' /= Nothing = Just ((fftt $ Fud $ ffqq ff `Set.union` (ffqq (ddff dd))),(uu1,ll1))
  | otherwise = Nothing
  where
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsListVariablesNullable uu zz ll
    (ff,(uu1,ll1)) = fromJust ff'
    ddff = decompsFud
    ffqq = fudsSetTransform
    fftt gg = Transform (eff (apply (fund gg) (fder gg `Set.union` fund gg) (fudsSetHistogram gg) (scalar 1)), fder gg)
    apply = setVarsSetVarsSetHistogramsHistogramsApply
    scalar q = fromJust $ histogramScalar q
    fder = fudsDerived
    fund = fudsUnderlying
    eff = histogramsEffective
      
systemsDecompsOriginals :: System -> Decomp -> Maybe (Map.Map Variable Variable)
systemsDecompsOriginals uu dd 
  | ok = Just $ llmm $ qqll $ rtorig zz `union` nlorig nl
  | otherwise = Nothing
  where
    ok = zz' /= Nothing && slt' /= Nothing && slc' /= Nothing && vsla' /= Nothing && nl' /= Nothing
    zz' = systemsDecompsApplication uu dd
    zz = fromJust zz'
    slt' = systemsApplicationsTreeSliceTransform uu zz
    slt = fromJust slt'
    slc' = systemsApplicationsTreeSliceContingent uu zz
    slc = fromJust slc'
    vsla' = systemsApplicationsMapVariablesSliceAlternate uu zz
    vsla = fromJust vsla'
    nl' = systemsApplicationsSetNullable uu zz
    nl = fromJust nl'
    rtorig zz = llqq [(full w,w) | (tt,_) <- qqll (roots zz), w <- qqll (der tt)]
    full w = VarPartition $ qqpp $ Set.map sing (cart uu (sing w))
    nlorig nl = llqq [(VarPartition (ppppp ppp),w) | ppp <- qqll nl, w <- qqll (pppvars ppp), w `mem` fder (ddff dd)]
    pppvars = partitionPointedsVars
    ppppp = partitionPointedsPartition 
    fder = fudsDerived
    ddff = decompsFud
    der = transformsDerived
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    roots = treesRoots
    union = Set.union
    qqll = Set.toList
    llqq = Set.fromList
    sing = Set.singleton            
    mem = Set.member            
    llmm = Map.fromList   
    
decompsNullablesOriginals :: Decomp -> Fud -> Map.Map Variable Variable
decompsNullablesOriginals dd ff = 
    llmm [(w,u) | w <- qqll (fder ff), tt <- qqll (ffqq ff), w `mem` der tt, u <- qqll (und tt), u `mem` fder (ddff dd)]
  where
    ffqq = fudsSetTransform
    fder = fudsDerived
    ddff = decompsFud
    der = transformsDerived
    und = transformsUnderlying
    qqll = Set.toList
    mem = Set.member            
    llmm = Map.fromList       
    
histogramsDecompsSetReduction :: Histogram -> Decomp -> Maybe (Set.Set Decomp)
histogramsDecompsSetReduction aa dd 
  | dund dd `subset` vars aa = Just $ distinct $ Decomp $ reds aa fudEmpty stateEmpty (ddzz dd)
  | otherwise = Nothing
  where 
    reds :: Histogram -> Fud -> State -> Tree (State,Transform) -> Tree (State,Transform)
    reds aa ff rr dd = Tree $ llmm $ 
      [((qq,xx),gg) | ((ss,tt),ee) <- zzll dd, let qq = ss `sred` fder ff, 
        let bb = apply (vars aa) (vars aa) (fhis ff `ins` single (rr `sunion` qq) 1) aa, 
        xx <- qqll (treds bb tt), let gg = reds aa (ff `fins` xx) (rr `sunion` qq) ee]
    treds aa tt = fromJust $ histogramsTransformsReductions aa tt         
    apply = setVarsSetVarsSetHistogramsHistogramsApply    
    dund = decompsUnderlying 
    distinct = decompsSetDecompDistinct
    ddzz = decompsTreePairStateTransform
    fhis = fudsSetHistogram
    fder = fudsDerived
    fins ff tt = Fud (Set.insert tt (ffqq ff))
    ffqq = fudsSetTransform
    single ss c = fromJust $ histogramSingleton ss c          
    vars = histogramsVars
    sred ss vv  = varsStatesFilter vv ss
    sunion = pairStatesUnionLeft
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    subset = Set.isSubsetOf
    ins xx x = Set.insert x xx
    qqll = Set.toList
        
instance Represent DecompFud where
  represent (DecompFud zz) = represent zz

decompFudsSetFud :: DecompFud -> Set.Set Fud 
decompFudsSetFud (DecompFud zz) = ran $ elem zz
  where
    ran = relationsRange
    elem = treesElements

decompFudsFuds = decompFudsSetFud

decompFudsFud :: DecompFud -> Fud 
decompFudsFud dd = Fud $ bigcup $ Set.map (\(Fud qq) -> qq) $ fuds dd
  where
    fuds = decompFudsFuds
    bigcup = setSetsUnion

decompFudsUnderlying :: DecompFud -> Set.Set Variable
decompFudsUnderlying dd = fund $ ddff dd
  where
    ddff = decompFudsFud
    fund = fudsUnderlying

decompFudEmpty :: DecompFud
decompFudEmpty = DecompFud (Tree (Map.singleton (stateEmpty,fudEmpty) emptyTree))

treePairStateFudsDecompFud :: Tree (State,Fud) -> Maybe DecompFud
treePairStateFudsDecompFud zz
    | ok zz = Just $ DecompFud zz
    | otherwise = Nothing
  where
    ok zz = okVars zz && okRoots zz && okStates zz
    okVars zz = and [(fvars ff1 `minus` fund ff1) `cap` fund ff2  == empty && okFuds ff1 ff2 | 
      ff1 <- qqll (ran (elem zz)), ff2 <- qqll (ran (elem zz)), ff1 /= ff2]
    okRoots zz = dom (roots zz) == sing stateEmpty
    okStates zz = and [ss `mem` std ff | ((_,ff),(ss,_)) <- qqll (steps zz)]
    okFuds ff1 ff2 = and [ww `cap` xx == empty | 
      Transform (aa,ww) <- qqll (ffqq ff1), Transform (bb,xx) <- qqll (ffqq ff2), (aa,ww) /= (bb,xx)]
    fvars = fudsVars
    fder = fudsDerived
    fund = fudsUnderlying
    ffqq = fudsSetTransform
    std ff = cart (fsys ff) (fder ff)
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    fsys = fudsSystemImplied
    dom = relationsDomain
    ran = relationsRange
    elem = treesElements
    roots = treesRoots
    steps = treesSteps
    qqll = Set.toList
    cap = Set.intersection
    minus = Set.difference
    empty = Set.empty
    sing = Set.singleton
    mem = Set.member
    
decompFudsTreePairStateFud :: DecompFud -> Tree (State,Fud)
decompFudsTreePairStateFud (DecompFud zz) = zz

decompFudsDecomp :: DecompFud -> Maybe Decomp 
decompFudsDecomp (DecompFud zz)
  | ok zz = Just $  Decomp $ funcsTreesMap (\(ss,ff) -> (ss,fftt ff)) zz
  | otherwise = Nothing
  where
    ok zz = and [fder ff1 `cap` fder ff2 == empty | 
      ff1 <- qqll (ran (elem zz)), ff2 <- qqll (ran (elem zz)), ff1 /= ff2]
    fftt = fudsTransform
    fder = fudsDerived
    ran = relationsRange
    elem = treesElements    
    qqll = Set.toList
    cap = Set.intersection
    empty = Set.empty

decompsDecompFud :: Decomp -> DecompFud 
decompsDecompFud (Decomp zz) = 
    DecompFud $ funcsTreesMap (\(ss,tt) -> (ss,ttff tt)) zz
  where
    ttff = Fud . Set.singleton

decompFudsSetVariablesRemove :: DecompFud -> Set.Set Variable -> DecompFud 
decompFudsSetVariablesRemove df kk = DecompFud $ funcsTreesMap remove zz
  where
    DecompFud zz = df
    gg = decompFudsFud df 
    xx = Set.filter (\w -> Set.null (fvars (depends gg w) `cap` kk)) $ fder gg
    remove (ss,ff) = (ss `sred` xx, qqff (bigcup (Set.map (\w -> ffqq (depends gg w)) (fder ff `cap` xx))))
    depends gg w = fudsVarsDepends gg (Set.singleton w)
    fder = fudsDerived
    fvars = fudsVars
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    sred ss xx = llss $ filter (\(v,u) -> v `Set.member` xx) $ ssll ss
    ssll = statesList
    llss = listsState
    bigcup = setSetsUnion
    cap = Set.intersection

decompFudsHistogramsApply :: DecompFud -> Histogram -> Tree (State,Histogram)
decompFudsHistogramsApply (DecompFud zz) aa = apply zz (vars aa) aa
  where
    apply :: Tree (State,Fud) -> Set.Set Variable -> Histogram -> Tree (State,Histogram)
    apply zz vv aa = Tree $ llmm $ [((ss, bb `red` ww), apply yy vv bb) | 
      ((ss,ff),yy) <- zzll zz, let aa' = aa `mul` (single ss 1), let ww = fder ff, let uu = fund ff,
      let bb = if size aa' > 0 then (if uu `subset` vv then applyFud (vv `cup` ww) ff aa' else applyHis (vv `cup` uu) (vv `cup` ww) (ffqq ff) aa') else empty]
    fder = fudsDerived
    fund = fudsUnderlying      
    ffqq = fudsSetHistogram
    single ss c = fromJust $ histogramSingleton ss c
    mul = pairHistogramsMultiply
    applyFud ww ff aa = fromJust $ setVarsFudHistogramsApply ww ff aa
    applyHis = setVarsSetVarsSetHistogramsHistogramsApply
    red = flip setVarsHistogramsReduce
    vars = histogramsVars
    size = histogramsSize
    empty = histogramEmpty
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union
    subset = Set.isSubsetOf

decompFudsHistogramsMultiply :: DecompFud -> Histogram -> Tree (State,Histogram)
decompFudsHistogramsMultiply (DecompFud zz) aa = apply zz (vars aa) aa
  where
    apply :: Tree (State,Fud) -> Set.Set Variable -> Histogram -> Tree (State,Histogram)
    apply zz vv aa = Tree $ llmm $ [((ss, bb), apply yy vv bb) | 
      ((ss,ff),yy) <- zzll zz, let aa' = aa `mul` (single ss 1), let ww = fder ff, let uu = fund ff,
      let bb = if size aa' > 0 then (if uu `subset` vv then applyFud (vv `cup` ww) ff aa' else applyHis (vv `cup` uu) (vv `cup` ww) (ffqq ff) aa') else empty]
    fder = fudsDerived
    fund = fudsUnderlying      
    ffqq = fudsSetHistogram
    single ss c = fromJust $ histogramSingleton ss c
    mul = pairHistogramsMultiply
    applyFud ww ff aa = fromJust $ setVarsFudHistogramsApply ww ff aa
    applyHis = setVarsSetVarsSetHistogramsHistogramsApply
    red = flip setVarsHistogramsReduce
    vars = histogramsVars
    size = histogramsSize
    empty = histogramEmpty
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union
    subset = Set.isSubsetOf

decompFudsHistogramsHistogramsQuery :: DecompFud -> Histogram -> Histogram -> Tree (State,Histogram)
decompFudsHistogramsHistogramsQuery (DecompFud zz) aa qq = query zz aa qq
  where
    vv = vars aa
    kk = vars qq
    query :: Tree (State,Fud) -> Histogram -> Histogram -> Tree (State,Histogram)
    query zz aa qq = Tree $ llmm $ [((ss, bb), query yy bb rr) | 
      ((ss,ff),yy) <- zzll zz, let ww = fder ff, let uu = fund ff, 
      let qq' = qq `mul` (single ss 1), size qq' > 0,
      let rr = if uu `subset` kk then applyFud (vv `cup` ww) ff qq' else applyHis (vv `cup` kk) (vv `cup` ww) (ffqq ff) qq',
      let aa' = aa `mul` (single ss 1),
      let bb = if size aa' > 0 then (if uu `subset` vv then applyFud (vv `cup` ww) ff aa' else applyHis (vv `cup` uu) (vv `cup` ww) (ffqq ff) aa') `mul` (rr `red` ww) else empty]
    fder = fudsDerived
    fund = fudsUnderlying      
    ffqq = fudsSetHistogram
    single ss c = fromJust $ histogramSingleton ss c
    mul = pairHistogramsMultiply
    applyFud ww ff aa = fromJust $ setVarsFudHistogramsApply ww ff aa
    applyHis = setVarsSetVarsSetHistogramsHistogramsApply
    red = flip setVarsHistogramsReduce
    vars = histogramsVars
    size = histogramsSize
    empty = histogramEmpty
    zzll (Tree mm) = mmll mm
    llmm = Map.fromList
    mmll = Map.toList
    cup = Set.union
    subset = Set.isSubsetOf

decompFudsIsDistinct :: DecompFud -> Bool
decompFudsIsDistinct (DecompFud zz) = treesIsDistinct zz
    
systemsDecompFudsIs :: System -> DecompFud -> Bool
systemsDecompFudsIs uu dd =
    and [isState uu ss | ss <- states dd] && and [isOneFunc uu tt | tt <- ddqq dd]
  where 
    ddqq = Set.toList . fudsSetTransform . decompFudsFud
    isOneFunc = systemsTransformsIsOneFunc
    isState = systemsStatesIs
    states = Set.toList . relationsDomain . treesElements . ddzz
    ddzz = decompFudsTreePairStateFud
    
systemsDecompFudsApplication :: System -> DecompFud -> Maybe (Tree (Fud,State))
systemsDecompFudsApplication uu dd 
  | issys uu dd && isdistinct dd = Just $ app zz stateEmpty stateEmpty fudEmpty
  | otherwise = Nothing
  where 
    DecompFud zz = dd
    issys = systemsDecompFudsIs
    isdistinct = decompFudsIsDistinct
    app :: Tree (State,Fud) -> State -> State -> Fud -> Tree (Fud,State)
    app dd qq rr ff = Tree $ llmm $ 
       [((tt,ss), app ee ss (rr `scup` ss) (ff `fins` tt)) | 
         ((pp,tt),ee) <- zzll dd, pp==qq, ss <- qqll (dom (roots ee)), isjoin rr ss,
         (rr `scup` ss) `mem` states (scalar 1 `fmul` (ff `fins` tt `tins` tdis (single rr 1)))] ++
       [((tt,ss), emptyTree) | 
         ((pp,tt),ee) <- zzll dd, pp==qq, ss <- qqll (cart (der tt) Set.\\ dom (roots ee)), isjoin rr ss,
         (rr `scup` ss) `mem` states (scalar 1 `fmul` (ff `fins` tt `tins` tdis (single rr 1)))]
    isjoin = pairStatesIsJoin
    scup = pairStatesUnionLeft         
    fins ff tt = Fud (Set.union (ffqq tt) (ffqq ff))
    tins ff tt = Fud (Set.insert tt (ffqq ff))
    std ff = states (ttaa (fftt ff) `red` fder ff)
    tdis =  histogramsTransformDisjoint
    single ss c = fromJust $ histogramSingleton ss c
    fmul aa ff = fudsHistogramsApply ff aa
    scalar q = fromJust $ histogramScalar q
    states = histogramsStates
    ttaa = transformsHistogram
    red aa vv = setVarsHistogramsReduce vv aa
    fder = fudsDerived
    fftt = fudsTransform
    zzll (Tree mm) = mmll mm
    roots = treesRoots
    ffqq = fudsSetTransform
    cart vv = fromJust $ systemsVarsCartesian uu vv
    der = fudsDerived
    dom = relationsDomain
    llmm = Map.fromList
    mmll = Map.toList
    qqll = Set.toList
    mem = Set.member

systemsDecompFudsListVariablesNullable :: System -> DecompFud -> [Variable] -> Maybe (Fud,(System,[Variable]))
systemsDecompFudsListVariablesNullable uu dd ll
  | zz' /= Nothing = ff'
  | otherwise = Nothing
  where
    zz' = systemsDecompFudsApplication uu dd
    zz = fromJust zz'
    ff' = systemsApplicationsListVariablesNullable uu zz ll

systemsDecompFudsListVariablesFud :: System -> DecompFud -> [Variable] -> Maybe (Fud,(System,[Variable]))
systemsDecompFudsListVariablesFud uu dd ll
  | ff' /= Nothing = Just ((Fud $ ffqq ff `Set.union` (ffqq (ddff dd))),(uu1,ll1))
  | otherwise = Nothing
  where
    ff' = systemsDecompFudsListVariablesNullable uu dd ll
    (ff,(uu1,ll1)) = fromJust ff'
    ddff = decompFudsFud
    ffqq = fudsSetTransform
    
histogramIntegralsMult :: Histogram -> Maybe Integer
histogramIntegralsMult aa
  | isInt aa = Just $ mult (size aa) (counts aa)
  | otherwise = Nothing
  where 
    mult = combinationMultinomial
    isInt = histogramsIsIntegral
    size = numerator. histogramsSize
    aall = histogramsList
    counts aa = [numerator c | (ss,c) <- aall aa]

histogramsHistogramIntegralsPermutorial :: Histogram -> Histogram -> Maybe Rational
histogramsHistogramIntegralsPermutorial ee aa
  | (vars ee == vars aa) && (size ee > 0) && isInt aa = 
      Just $ product [(index ee ss / size ee) ^ numerator (index aa ss) | ss <- states aa]
  | otherwise = Nothing
  where 
    isInt = histogramsIsIntegral
    index aa ss = fromMaybe 0 (histogramsStatesCount aa ss)
    states = Set.toList . histogramsSetState . histogramsTrim
    size = histogramsSize
    vars = histogramsVars

systemsIdentifiersHistorySet :: System -> Set.Set Id -> Set.Set History
systemsIdentifiersHistorySet uu xx =
    Set.fromList [hh | vv <- vars uu, hh <- map qqhh $ filtfunc $ powset $ cross xx (cart uu vv)]
  where
    vars uu = powset $ systemsSetVar uu
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqhh qq = fromJust $ listsHistory $ Set.toList qq
    filtfunc qq = filter relationsIsFunc qq
    powset qq = Set.toList $ setsPowerset qq
    cross aa bb = setsSetsCross aa bb

systemsIdentifiersClassificationSet :: System -> Set.Set Id -> Set.Set Classification
systemsIdentifiersClassificationSet uu xx = Set.fromList $ 
    [gg | vv <- vars uu, pp <- part xx, gg <- map qqgg $ filt $ powset $ cross (cart uu vv) pp] ++ [empty]
  where
    vars uu = powset $ systemsSetVar uu
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqgg qq = fromJust $ listsClassification $ Set.toList qq
    filt qq = filter ispart $ filter isfunc qq
    isfunc = relationsIsFunc
    ispart rr = Set.size (relationsRange rr) == Set.size rr && setsIsPartition (relationsRange rr)
    powset qq = Set.toList $ setsPowerset qq
    cross aa bb = setsSetsCross aa bb
    part xx = Set.toList $ setsPartitionSet xx
    empty = classificationEmpty

systemsIdentifiersClassificationSet_1 :: System -> Set.Set Id -> Set.Set Classification
systemsIdentifiersClassificationSet_1 uu xx = Set.fromList $ 
    [gg | vv <- vars uu, pp <- part xx, gg <- map qqgg $ filt $ powset $ cross (cart uu vv) pp] ++ [empty]
  where
    vars uu = powset $ systemsSetVar uu
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqgg qq = fromJust $ listsClassification $ Set.toList qq
    filt qq = filter isfunc qq
    isfunc rr = relationsIsFunc rr && relationsIsFunc (relationsFlip rr)
    powset qq = Set.toList $ setsPowerset qq
    cross aa bb = setsSetsCross aa bb
    part xx = Set.toList $ setsPartitionSet xx
    empty = classificationEmpty

systemsIdentifiersHistoryVariateSet :: System -> Set.Set Id -> Set.Set History
systemsIdentifiersHistoryVariateSet uu xx = 
    Set.filter isvar $ systemsIdentifiersHistorySet uu xx
  where
    isvar hh = historiesSetVar hh /= Set.empty

systemsIdentifierCardinalitysHistogramSet :: System -> Integer -> Set.Set Histogram
systemsIdentifierCardinalitysHistogramSet uu y =
    Set.fromList $ concat [foldl accum [empty] (cart uu vv) | vv <- vars uu]
  where
    powset qq = Set.toList $ setsPowerset qq
    vars uu = powset $ systemsSetVar uu
    cart uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv
    accum qq ss = qq ++ [aa `add` sing ss (fromIntegral c) | aa <- qq, c <- [1 .. (y - size aa)]]
    size = numerator . histogramsSize
    empty = histogramEmpty
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sing ss c = fromJust $ histogramSingleton ss c

systemsIdentifierCardinalitysTransformSet :: System -> Integer -> Set.Set Transform
systemsIdentifierCardinalitysTransformSet uu y =
    Set.fromList $ [trans aa ww | aa <- histos uu y, ww <- vars aa]
  where
    histos uu y = Set.toList $ systemsIdentifierCardinalitysHistogramSet uu y
    powset qq = Set.toList $ setsPowerset qq
    vars uu = powset $ histogramsSetVar uu
    trans aa ww = fromJust $ histogramsSetVarsTransform aa ww

systemsTransformUnitSet :: System -> Set.Set Transform
systemsTransformUnitSet uu =
    Set.fromList $ [trans aa ww | aa <- histos uu, ww <- pavars aa]
  where
    histos uu = concat [foldl accum [empty] (cart uu vv) | vv <- puvars uu]
    accum qq ss = qq ++ [aa `add` sing ss 1 | aa <- qq]
    powset qq = Set.toList $ setsPowerset qq
    pavars uu = powset $ histogramsSetVar uu
    puvars uu = powset $ systemsSetVar uu
    trans aa ww = fromJust $ histogramsSetVarsTransform aa ww
    empty = histogramEmpty
    cart uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv
    add xx yy = fromJust $ pairHistogramsAdd xx yy
    sing ss c = fromJust $ histogramSingleton ss c

systemsPartitionSet :: System -> Set.Set Partition
systemsPartitionSet uu =
    setSetsUnion $ Set.map (\vv -> Set.map qqpp (part (cart uu vv))) (vars uu)
  where
    powset = setsPowerset
    part = setsPartitionSet
    vars uu = powset $ systemsSetVar uu
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq
    
instance Represent Roll where
  represent (Roll mm) = represent mm
  
rollEmpty :: Roll
rollEmpty = Roll $ Map.empty
  
listsRoll :: [(State, State)] -> Maybe Roll
listsRoll ll
  | and [svars ss == rvars rr && svars tt == rvars rr | (ss, tt) <- ll] = Just rr
  | otherwise = Nothing
  where 
    rr = Roll $ Map.fromList ll
    svars = statesVars
    rvars = rollsSetVar

rollsList :: Roll -> [(State, State)]
rollsList (Roll mm) = Map.toList mm

rollsDomain :: Roll -> Set.Set State
rollsDomain (Roll mm) = Map.keysSet mm

rollsRange :: Roll -> Set.Set State
rollsRange (Roll mm) = Set.fromList $ Map.elems mm

rollsSetState :: Roll -> Set.Set State
rollsSetState rr = rollsDomain rr `Set.union` rollsRange rr

rollsSetVar :: Roll -> Set.Set Variable
rollsSetVar (Roll mm) 
  | Map.size mm == 0 = Set.empty
  | otherwise = statesVars $ head $ Map.keys mm
  
rollsHistogramsRoll :: Roll -> Histogram -> Histogram
rollsHistogramsRoll (Roll mm) aa = Histogram $ llmm (+) [(tt,q) | (ss,q) <- aall aa, let tt = find ss ss mm]
  where
    aall = histogramsList
    find = Map.findWithDefault
    llmm = Map.fromListWith
    
systemsSetVarsRollIdentity :: System -> Set.Set Variable -> Maybe Roll
systemsSetVarsRollIdentity uu vv
  | vv `subset` uvars uu = Just $ Roll $ llmm $ zip (cartll uu vv) (cartll uu vv)
  | otherwise = Nothing  
  where
    cartll uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv  
    uvars = systemsVars
    subset = Set.isSubsetOf
    llmm = Map.fromList
    
rollsIsCircular :: Roll -> Bool
rollsIsCircular rr = rdom rr `cap` rran rr /= empty
  where
    rdom = rollsDomain
    rran = rollsRange
    cap = Set.intersection
    empty = Set.empty
  
rollsIsCircular_1 :: Roll -> Bool
rollsIsCircular_1 rr = iscirc (rrrl rr)
  where
    iscirc = relationsIsCircular
    rrrl (Roll mm) = functionsRelation mm
    
listRollsIsUnique :: [Roll] -> Bool
listRollsIsUnique ll = 
    Set.size (bigcup (llqq (map rrrl ll))) == sum [Set.size (rrrl rr) | rr <- ll]
  where
    rrrl (Roll mm) = functionsRelation mm
    bigcup = setSetsUnion
    llqq = Set.fromList
  
listRollsIsFunc :: [Roll] -> Bool
listRollsIsFunc ll = isfunc $ bigcup $ llqq $ map rrrl ll
  where
    rrrl (Roll mm) = functionsRelation mm
    isfunc = relationsIsFunc
    bigcup = setSetsUnion
    llqq = Set.fromList
  
listRollsIsCircular :: [Roll] -> Bool
listRollsIsCircular ll = 
    or [rdom (ll !! (i-1)) `cap` llbigcup [rran rr | rr <- drop (i-1) ll] /= empty | i <- [1 .. length ll]]
  where
    rdom = rollsDomain
    rran = rollsRange
    llbigcup = setSetsUnion . Set.fromList 
    cap = Set.intersection
    empty = Set.empty
  
listRollsHistogramsRoll :: [Roll] -> Histogram -> Histogram
listRollsHistogramsRoll ll aa = foldl rmul aa ll
  where
    rmul aa rr = rollsHistogramsRoll rr aa

pairRollsCompose :: Roll -> Roll -> Roll
pairRollsCompose rr1 rr2 = Roll $ rlmm $ join (rrrl rr1) (rrrl rr2)
  where
    join = pairRelationsJoinOuter
    rrrl (Roll mm) = functionsRelation mm
    rlmm = Map.fromList . Set.toList

listRollsCompose :: [Roll] -> Roll
listRollsCompose ll = foldl pairRollsCompose rollEmpty ll

systemsRollsPartitionState :: System -> Roll -> Maybe Partition
systemsRollsPartitionState uu rr
  | rvars rr `subset` uvars uu = qqpp $ ran $ inv $ rrmm ( rr `compose` id uu (rvars rr))
  | otherwise = Nothing  
  where
    qqpp = setComponentsPartition  
    id uu vv = fromJust $ systemsSetVarsRollIdentity uu vv
    compose rr2 rr1 = pairRollsCompose rr1 rr2
    rrmm (Roll mm) = mm
    rvars = rollsSetVar
    uvars = systemsVars
    ran = functionsRange
    inv = functionsInverse
    subset = Set.isSubsetOf

systemsSetVariablesRollsSetPartitionVariable :: System -> Set.Set Variable -> Roll -> Maybe SetPartition
systemsSetVariablesRollsSetPartitionVariable uu vv rr
  | vv `subset` uvars uu && (rr == rollEmpty || rvars rr == vv) = Just $
    llqq [(qqpp . ran . inv . llmm) [(ss, tt `red` v) | (ss,tt) <- rrmmll (rr `compose` id uu vv)] | v <- qqll vv]
  | otherwise = Nothing  
  where
    id uu vv = fromJust $ systemsSetVarsRollIdentity uu vv
    compose rr2 rr1 = pairRollsCompose rr1 rr2
    rrmmll (Roll mm) = Map.toList mm
    rvars = rollsSetVar
    qqpp qq = fromJust $ setComponentsPartition qq
    red ss v = setVarsStatesStateFiltered (Set.singleton v) ss
    uvars = systemsVars
    ran = functionsRange
    inv = functionsInverse
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llmm = Map.fromList
       
systemsSetVariablesRollsTransform :: System -> Set.Set Variable -> Roll -> Maybe Transform
systemsSetVariablesRollsTransform uu vv rr
  | vv `subset` uvars uu && (rr == rollEmpty || rvars rr == vv) = Just $ nntt (rrnnv uu vv rr)
  | otherwise = Nothing  
  where
    rrnnv uu vv rr = fromJust $ systemsSetVariablesRollsSetPartitionVariable uu vv rr
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    rvars = rollsSetVar
    uvars = systemsVars
    subset = Set.isSubsetOf       

instance Represent RollValue where
  represent (RollValue rr) = represent rr
  
setVariablesVariablesValuesValuesRollValue :: (Set.Set Variable, Variable, Value, Value) -> Maybe RollValue
setVariablesVariablesValuesValuesRollValue (vv,v,s,t) 
  | v `mem` vv = Just $ RollValue (vv,v,s,t)
  | otherwise = Nothing
  where
    mem = Set.member 

rollValuesSetVariableVariableValueValue :: RollValue -> (Set.Set Variable, Variable, Value, Value)
rollValuesSetVariableVariableValueValue (RollValue (vv,v,s,t)) = (vv,v,s,t)

systemsSetVariablesVariablesValuesValuesRollValue :: 
  System -> (Set.Set Variable, Variable, Value, Value) -> Maybe RollValue
systemsSetVariablesVariablesValuesValuesRollValue uu (vv,v,s,t) 
  | vv `subset` uvars uu && v `mem` vv && 
    and [s `mem2` uvals uu v && t `mem2` uvals uu v | v <- qqll vv] = Just $ RollValue (vv,v,s,t)
  | otherwise = Nothing
  where
    uvals uu v = fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf
    mem = Set.member 
    mem2 = Set.member 

systemsRollValuesRoll :: System -> RollValue -> Roll
systemsRollValuesRoll uu (RollValue (vv,v,s,t))
  | s == t = rollEmpty
  | vv `subset` uvars uu = 
    llrr [(ss,tt) | ss <- cartll uu vv, let (State mm) = ss, mm Map.! v == s, let tt = State (Map.insert v t mm)]
  | otherwise = rollEmpty
  where
    llrr ll = fromJust $ listsRoll ll
    cartll uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf

systemsListRollValuesListRoll :: System -> [RollValue] -> [Roll]
systemsListRollValuesListRoll uu ll = map (vrrr uu) ll
  where
    vrrr = systemsRollValuesRoll

systemsListRollValuesRoll :: System -> [RollValue] -> Roll
systemsListRollValuesRoll uu ll = compose $ jjrrll uu ll
  where
    jjrrll = systemsListRollValuesListRoll
    compose = listRollsCompose

variablesListRollValuesFilter :: Variable -> [RollValue] -> [RollValue]
variablesListRollValuesFilter v ll = filter (\(RollValue (_,w,_,_)) -> w==v) ll

systemsVariablesListRollValuesPartitionValue :: System -> Variable -> [RollValue] -> Maybe (Set.Set (Set.Set Value))
systemsVariablesListRollValuesPartitionValue uu v ll 
  | v `mem` uvars uu = Just $ ran $ inv $ foldl join (id uu v) [sgl (s,t) | RollValue (_,_,s,t) <- filtv v ll]
  | otherwise = Nothing
  where
    id uu v = Set.fromList $ zip (uvalsll uu v) (uvalsll uu v)
    filtv v ll = variablesListRollValuesFilter v ll
    uvalsll uu v = Set.toList $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    join = pairRelationsJoinOuter
    ran = functionsRange
    inv = relationsInverse
    mem = Set.member 
    sgl = Set.singleton 

systemsSetVariablesListRollValuesSetPartitionVariable :: System -> Set.Set Variable -> [RollValue] -> Maybe SetPartition
systemsSetVariablesListRollValuesSetPartitionVariable uu vv jj
  | vv `subset` uvars uu = Just $
    llqq [llpp [llqq2 [single v u | u <- qqll cc] | cc <- partvll uu v jj] | v <- qqll2 vv]
  | otherwise = Nothing  
  where
    partvll uu v jj = Set.toList $ fromJust $ systemsVariablesListRollValuesPartitionValue uu v jj
    llpp ll = fromJust $ setComponentsPartition $ Set.fromList ll
    single = stateSingleton
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList

systemsSetVariablesListRollValuesTransform :: System -> Set.Set Variable -> [RollValue] -> Maybe Transform
systemsSetVariablesListRollValuesTransform uu vv rr
  | vv `subset` uvars uu = Just $ expand uu vv $ nntt $ jjnnv uu vv rr
  | otherwise = Nothing  
  where
    jjnnv uu vv jj = fromJust $ systemsSetVariablesListRollValuesSetPartitionVariable uu vv jj
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    expand uu vv tt = fromJust $ systemsSetVarsTransformVarPartitionsExpand uu vv tt
    uvars = systemsVars
    subset = Set.isSubsetOf       

histogramsHistogramsProductTransformed :: Histogram -> Histogram -> Maybe Histogram
histogramsHistogramsProductTransformed aa qq 
  | size aa > 0 = Just $ qq `mul` pp `red` yy
  | otherwise = Nothing
  where 
    pp = resize 1 aa
    vv = vars aa
    xx = vars qq
    yy = vv `Set.difference` xx
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa
    resize z aa = fromJust $ histogramsResize z aa
    size = histogramsSize
    vars = histogramsVars

histogramsHistogramsProductConditionalTransformed :: Histogram -> Histogram -> Maybe Histogram
histogramsHistogramsProductConditionalTransformed aa qq 
  | size aa > 0 = Just $ qq `mul` pp `divide` (pp `red` xx) `red` yy
  | otherwise = Nothing
  where 
    pp = resize 1 aa
    vv = vars aa
    xx = vars qq
    yy = vv `Set.difference` xx
    mul = pairHistogramsMultiply
    divide = pairHistogramsDivide
    red aa vv = setVarsHistogramsReduce vv aa
    resize z aa = fromJust $ histogramsResize z aa
    size = histogramsSize
    vars = histogramsVars

systemsTransformsHistogramsHistogramsProductTransformed :: 
  System -> Transform -> Histogram -> Histogram -> Maybe Histogram
systemsTransformsHistogramsHistogramsProductTransformed uu tt aa qq 
  | size aa > 0 && size qq > 0 && vv `subset` uvars uu = 
    Just $ norm qq `mul` (norm (cart uu jk)) `tmul` tt `mul` (ttaa tt) `mul` (norm aa) `red` vk
  | otherwise = Nothing
  where 
    vv = vars aa
    kk = vars qq
    vk = vv `Set.difference` kk
    jj = und tt
    jk = jj `Set.difference` kk
    tmul aa tt = transformsHistogramsApply tt aa
    ttaa = transformsHistogram
    und = transformsUnderlying
    cart uu vv = fromJust $ setStatesHistogramUnit $ fromJust $ systemsVarsCartesian uu vv
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa
    norm aa = fromJust $ histogramsResize 1 aa
    size = histogramsSize
    vars = histogramsVars
    uvars = systemsVars
    subset = Set.isSubsetOf       

transformsHistogramsHistogramsProductConditionalTransformed :: 
  Transform -> Histogram -> Histogram -> Maybe Histogram
transformsHistogramsHistogramsProductConditionalTransformed tt aa qq 
  | size aa > 0 && size rr > 0 = Just $ norm rr
  | otherwise = Nothing
  where 
    vv = vars aa
    kk = vars qq
    vk = vv `Set.difference` kk
    rr = qq `tmul` tt `mul` (ttaa tt) `mul` aa `red` vk
    tmul aa tt = transformsHistogramsApply tt aa
    ttaa = transformsHistogram
    und = transformsUnderlying
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa
    norm aa = fromJust $ histogramsResize 1 aa
    size = histogramsSize
    vars = histogramsVars

transformsHistogramsHistogramsSetVariablesSetVariablesProductConditionalTransformed :: 
  Transform -> Histogram -> Histogram -> Set.Set Variable -> Set.Set Variable -> Maybe Histogram
transformsHistogramsHistogramsSetVariablesSetVariablesProductConditionalTransformed tt aa bb kk vvr
  | size aa > 0 && size rr > 0 = Just $ norm rr
  | otherwise = Nothing
  where 
    vv = vars aa
    vk = vv `minus` kk
    Transform (mm,ww) = tt
    rr = bb `red` (kk `cup` vvr) `mul` mm `red` (ww `cup` vvr) `mul` mm `mul` aa `red` (vk `cup` vvr)
    mul = pairHistogramsMultiply
    red aa vv = setVarsHistogramsReduce vv aa
    norm aa = fromJust $ histogramsResize 1 aa
    size = histogramsSize
    vars = histogramsVars
    minus = Set.difference
    cup = Set.union

