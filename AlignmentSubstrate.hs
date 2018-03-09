module AlignmentSubstrate (
  systemsSetVarsTransformUnary,
  systemsSetVarsTransformSelf,
  systemsSetVarsTransformFullValue,
  systemsSetVarsFudBase,
  systemsSetVarsFudBaseCardinality,
  systemsSetVarsSizesHistorySubstrate,
  systemsSetVarsSizesHistorySubstrate_1,
  systemsSetVarsSizesHistogramSubstrate,
  systemsSetVarsSetTransformSubstrate, 
  systemsSetVarsSetTransformSubstrate_1, 
  systemsSetVarsSetTransformSubstrate_2,
  systemsSetVarsSetSetPartitionsSetTransform,
  systemsSetVarsSetTransformSubstrate_3,
  systemsSetVarsSetTransformSubstrate_4,
  systemsSetVarsSetTransformSubstrate_5,
  systemsSetVarsSetTransformSubstrate_6,
  systemsSetVarsSetTransformSubstrateCardinality,
  systemsSetVarsSetSetPartitionSubstrate,
  systemsSetVarsSetSetPartitionPointedSubstrate,
  systemsSetVarsSetSetPartitionSubstrateCardinality,
  valenciesDimensionsSetSetPartitionSubstrateCardinalityRegular,
  systemsSetVarsSetSetPartitionSubstrateCardinalityLower,
  systemsSetVarsSetSetPartitionSubstrateCardinalityUpper,
  systemsSetVarsIntegersFudPowerLimitedLayer, 
  systemsSetVarsIntegersFudPowerLimitedLayer_1,
  systemsSetVarsFudPower, 
  systemsSetVarsFudPower_1,
  systemsSetVarsSetFudSubstrate, 
  systemsSetVarsSetFudSubstrate_1,
  systemsSetVarsTreePairStateTransformPower,
  systemsSetVarsSetDecompSubstrate,
  systemsSetVarsTreePairStateFudPower,
  systemsSetVarsSetDecompFudSubstrate,
  systemsSetVarsSetSetPartitionSubstrateNonOverlap,
  systemsSetVarsSetSetPartitionSubstrateNonOverlap_1,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityLower,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinality,
  valenciesDimensionsSetSetPartitionSubstrateNonOverlapCardinalityRegular,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityUpper,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak_1,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak_2,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinalityLower,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinality,
  systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinalityUpper,
  systemsSetVarsSetTransformSubstrateNonOverlap, 
  systemsSetVarsSetTransformSubstrateNonOverlap_1, 
  systemsSetVarsSetTransformSubstrateNonOverlap_2,
  systemsSetVarsSetTransformSubstrateNonOverlap_3,
  systemsSetVarsSetTransformSubstrateNonOverlapCardinalityLower,
  systemsSetVarsSetTransformSubstrateNonOverlapCardinalityUpper,
  systemsSetVarsSetTransformSubstrateNonOverlapCardinalityUpperUpper,
  systemsSetVarsSetTransformSubstrateNonOverlapStrong,
  systemsSetVarsSetTransformSubstrateNonOverlapStrong_1,
  systemsSetVarsSetTransformSubstrateNonOverlapStrong_2,
  systemsSetVarsSetTransformSubstrateNonOverlapStrong_3,
  systemsSetVarsSetTransformSubstrateNonOverlapStrong_4,  
  systemsSetVarsSetTransformSubstrateNonOverlapStrong_5,
  systemsSetVarsSetTransformSubstrateNonOverlapStrong_6,
  systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityLower,
  systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityUpper,
  systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityUpperUpper,
  systemsSetVarsSetTransformSubstrateFixedDimension,
  systemsSetVarsSetTransformSubstrateFixedDimensionCardinality,
  systemsSetVarsSetSetPartitionSubstrateBinary,
  systemsSetVarsSetSetPartitionSubstrateBinary_1,
  systemsSetVarsSetSetPartitionSubstrateBinaryCardinality,
  valenciesDimensionsSetSetPartitionSubstrateBinaryCardinalityRegular,
  systemsSetVarsSetSetPartitionSubstrateBinaryCardinalityUpper,
  systemsSetVarsSetTransformSubstrateBinary,
  systemsSetVarsSetSetPartitionSubstrateSelf,
  systemsSetVarsSetSetPartitionSubstrateSelf_1,
  systemsSetVarsSetSetPartitionSubstrateSelfCardinality,
  valenciesDimensionsSetSetPartitionSubstrateSelfCardinalityRegular,
  systemsSetVarsSetTransformSubstrateSelf,
  systemsSetVarsSetTransformSubstrateSelf_1,
  systemsSetVarsSetTransformSubstrateSelf_2,
  systemsSetVarsSetTransformSubstrateSelf_3,
  systemsSetVarsSetTransformSubstrateSelf_4,
  systemsSetVarsSetSetPartitionSubstrateOverlappingSelf,
  systemsSetVarsSetSetPartitionSubstrateOverlappingSelfCardinality,
  valenciesDimensionsSetSetPartitionSubstrateOverlappingSelfCardinalityRegular,
  systemsSetVarsSetTransformSubstrateOverlappingSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrate,
  systemsSetVarsSetSetPartitionCartesianSubstrate_1,
  systemsSetVarsSetSetPartitionCartesianSubstrate_2,
  setVarsSetSetPartitionCartesianSubstrateCardinality,
  systemsSetVarsSetTransformCartesianSubstrate,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak_1,
  setVarsSetSetPartitionCartesianSubstrateNonOverlapWeakCardinality,
  systemsSetVarsSetTransformCartesianSubstrateNonOverlapWeak,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlap,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlap_1,
  setVarsSetSetPartitionCartesianSubstrateNonOverlapCardinality,
  systemsSetVarsSetSetPartitionCartesianSubstrateBinary,
  systemsSetVarsSetSetPartitionCartesianSubstrateBinary_1,
  setVarsSetSetPartitionCartesianSubstrateBinaryCardinality,
  systemsSetVarsSetTransformCartesianSubstrateBinary,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelf_1,
  setVarsSetSetPartitionCartesianSubstrateSelfCardinality,
  systemsSetVarsSetTransformCartesianSubstrateSelf,
  systemsSetVarsSetSetPartitionTwoSubstrate,
  systemsSetVarsSetSetPartitionTwoSubstrate_1,
  systemsSetVarsSetSetPartitionTwoSubstrate_2,
  systemsSetVarsSetSetPartitionTwoSubstrateCardinalityLower,
  systemsSetVarsSetSetPartitionTwoSubstrateCardinality,
  valenciesDimensionsSetSetPartitionTwoSubstrateCardinalityRegular,
  systemsSetVarsSetSetPartitionTwoSubstrateCardinalityUpper,
  systemsSetVarsSetTransformTwoSubstrate,
  systemsSetVarsSetSetPartitionTwoSubstrateNonOverlap,
  systemsSetVarsSetSetPartitionTwoSubstrateNonOverlap_1,
  systemsSetVarsSetSetPartitionTwoSubstrateNonOverlapCardinality,
  valenciesDimensionsSetSetPartitionTwoSubstrateNonOverlapCardinalityRegular,
  systemsSetVarsSetSetPartitionTwoSubstrateNonOverlapCardinalityUpper,
  systemsSetVarsSetSetPartitionOneSubstrate,
  systemsSetVarsSetSetPartitionOneSubstrate_1,
  setVarsSetSetPartitionOneSubstrate_Cardinality,
  systemsSetVarsSetTransformOneSubstrate,
  systemsSetVarsSetSetPartitionDecrementedSubstrate,
  systemsSetVarsSetSetPartitionDecrementedSubstrate_1,
  systemsSetVarsSetSetPartitionDecrementedSubstrate_2,
  systemsSetVarsSetSetPartitionDecrementedSubstrate_3,
  systemsSetVarsSetSetPartitionDecrementedSubstrateCardinality,
  valenciesDimensionsSetSetPartitionDecrementedSubstrateCardinalityRegular,
  systemsSetVarsSetSetPartitionDecrementedSubstrateCardinalityUpper,
  systemsSetVarsSetTransformDecrementedSubstrate,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeak,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeak_1,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeakCardinality,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeakCardinalityUpper,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlap,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlap_1,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapCardinality,
  valenciesDimensionsSetSetPartitionDecrementedSubstrateNonOverlapCardinalityRegular,
  systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapCardinalityUpper,
  systemsSetVarsSetSetPartitionDecrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionDecrementedSubstrateSelf_1,
  systemsSetVarsSetSetPartitionDecrementedSubstrateSelf_2,
  systemsSetVarsSetSetPartitionDecrementedSubstrateSelfCardinality,
  systemsSetVarsSetTransformDecrementedSubstrateSelf,
  systemsSetVarsSetTransformDecrementedSubstrateSelf_1,
  valenciesDimensionsSetSetPartitionDecrementedSubstrateSelfCardinalityRegular,
  systemsSetVarsSetSetPartitionIncrementedSubstrate,
  systemsSetVarsSetSetPartitionIncrementedSubstrate_1,
  systemsSetVarsSetSetPartitionIncrementedSubstrateCardinality,
  systemsSetVarsSetSetPartitionIncrementedSubstrateCardinalityUpper,
  systemsSetVarsSetTransformIncrementedSubstrate,
  systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelfCardinalityUpper,
  systemsSetVarsSetTransformCartesianSubstrateNonOverlapDerivedSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelfCardinality,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelfCardinalityUpper,
  setPartitionsTreeSetPartitionDecrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf_1,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfCardinality,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceTree,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceCardinalityExp,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceCardinalityMax,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfWidth,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfDepth,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfDepth_1,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfCardinality,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfCardinalityUpper,
  setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf_1,
  systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreePartitionDecrementedSubstrateSelf,
  setPartitionsTreeSetPartitionPointedIncrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality,
  systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfCardinality,
  systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfWidth,
  systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfDepth,
  systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfDepth_1,
  systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelf,
  systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelfCardinality,
  systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelfCardinalityUpper,
  systemsSetVarsSetRollSubstrate,
  systemsSetVarsSetRollSubstrateCardinalityUpper,
  systemsSetVarsSetRollCompleteSubstrate,
  systemsSetVarsSetRollCompleteSubstrateCardinality,
  systemsSetVarsSetTransformSubstrateRollVariable,
  systemsSetVarsSetTransformSubstrateRollVariableCardinalityUpper,
  systemsSetVarsSetRollValueSubstrate,
  systemsSetVarsSetRollValueSubstrateCardinality,
  systemsSetVarsIntegersSetListFixedRollValueSubstrate,
  systemsSetVarsIntegersSetListFixedRollValueSubstrateCardinality,
  systemsSetVarsTreeRollValueDecrementedSubstrate,
  systemsSetVarsIntegersSetListRollValueDecrementedSubstrate,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimension,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegularLower,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegular,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegularUpper,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolume,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolumeCardinality,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolumeCardinalityUpper,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedValency,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedValencyCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedValencyCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedComponent,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedComponentCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateIntersecting, 
  systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinalityLower, 
  systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinality, 
  systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinalityUpper, 
  valenciesDimensionsLimitsSetSetPartitionSubstrateIntersectingCardinalityRegular,
  valenciesDimensionsLimitsSetSetPartitionSubstrateIntersectingCardinalityRegular_1,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth, 
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth_1, 
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthCardinality, 
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthCardinalityUpper, 
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedBreadthCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthRange,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthRangeCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedBreadthRangeCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth_1,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth_2,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadthCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadthCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValent,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardReg,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardExp,
  systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardMax,
  valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardExpReg,
  systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValent,
  systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValentCardinality,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadth,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadth_1,
  setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadthCardinality,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadth,
  setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinality,
  dimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValency,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinality,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinalityReg,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth_1,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth_2, 
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValent,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValentCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValentCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadth,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthCardinalityRegular,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValent,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValentCardinality,
  valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValentCardReg,
  setPartitionsTreeSetPartitionDecrementedSubstrateSelfPluriValent,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfPluriLimBreadthPluriVal,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfPluriLimBrPluriValCard,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubsSelfPluriLimBrPluriValCardReg,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCard,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCardReg,
  systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateTreeDecSubsSelfPluriLimBrPluriValChoiceMaxCard,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriValSrchStat,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriValSrchStat_1,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValScrhStatReg,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValScrhStatReg_1,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCard,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCardReg,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchStat,
  valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriLimValScrhStatReg,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMinListCard,
  systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchMinStat,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedDerivedVolume,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthDerivedVolume,
  systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthDerivedVolumePluriValent,
  systemsSetVarsLimitsSetFudPartitionLimitedPathModels
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import AlignmentUtil
import Alignment
import AlignmentDistribution(systemsDrawCartesiansSupport)
import GHC.Real

systemsSetVarsTransformUnary :: System -> Set.Set Variable -> Maybe Transform
systemsSetVarsTransformUnary uu vv
  | vv `subset` uvars uu = Just $ pptt (unary uu vv)
  | otherwise = Nothing
  where
    unary uu vv = fromJust $ systemsSetVarsPartitionUnary uu vv
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf

systemsSetVarsTransformSelf :: System -> Set.Set Variable -> Maybe Transform
systemsSetVarsTransformSelf uu vv
  | vv `subset` uvars uu = Just $ pptt (self uu vv)
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf

systemsSetVarsTransformFullValue :: System -> Set.Set Variable -> Maybe Transform
systemsSetVarsTransformFullValue uu vv
  | vv `subset` uvars uu = Just $ fftt $ llqqff [pptt (expand uu vv (self uu v)) | v <- qqll vv]
  | otherwise = Nothing
  where
    fftt = fudsTransform
    self uu v = fromJust $ systemsSetVarsPartitionSelf uu (Set.singleton v)
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList

systemsSetVarsFudBase :: System -> Set.Set Variable -> Maybe Fud
systemsSetVarsFudBase uu vv
  | vv `subset` uvars uu = llqqff [pptt (qqpp pp) | pp <- partsll (cart uu vv)]
  | otherwise = Nothing
  where
    llqqff = setTransformsFud . llqq
    pptt = partitionsTransformVarPartition 
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsFudBaseCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsFudBaseCardinality uu vv
  | vv `subset` uvars uu = Just $ bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf          

systemsSetVarsSizesHistorySubstrate :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set History)
systemsSetVarsSizesHistorySubstrate uu vv z 
  | (vv `Set.isSubsetOf` (uvars uu)) && z > 0 = 
      Just $ Set.fromList $ map llhh $ foldl inc [[]] [1..z]
  | otherwise = Nothing 
  where
    qq = Set.toList (vvqq uu vv)
    inc ll i = [(IdInt i, ss) : mm | mm <- ll, ss <- qq]
    vvqq uu vv = fromJust $ systemsSetVarsSetStateCartesian uu vv
    llhh = historyFromList_u
    uvars = systemsVars

systemsSetVarsSizesHistorySubstrate_1 :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set History)
systemsSetVarsSizesHistorySubstrate_1 uu vv z 
  | (vv `Set.isSubsetOf` (uvars uu)) && z > 0 = Just $ hisinc empty z
  | otherwise = Nothing 
  where
    hisinc hh 0 = Set.singleton hh
    hisinc hh z = setSetsUnion $ Set.map (\ss -> hisinc (llhh (hhll hh ++ [(IdInt z, ss)])) (z-1)) (vvqq uu vv)
    vvqq uu vv = fromJust $ systemsSetVarsSetStateCartesian uu vv
    llhh = historyFromList_u
    hhll = historyToList
    empty = historyEmpty
    uvars = systemsVars

systemsSetVarsSizesHistogramSubstrate :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set Histogram)
systemsSetVarsSizesHistogramSubstrate = systemsDrawCartesiansSupport

systemsSetVarsSetTransformSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate uu vv
  | vv `subset` uvars uu = Just $ llqq [fftt (qqff qq) | qq <- llpowerll [pptt (qqpp pp) | pp <- partsll (cart uu vv)]]
  | otherwise = Nothing
  where
    qqff = fromJust . setTransformsFud
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpowerll = qqll . setsPowerset . llqq
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate_1 uu vv
  | vv `subset` uvars uu = 
    Just $ llqq [fftt (qqff qq) | qq <- llpowerll [pptt (expand uu vv (qqpp pp)) | kk <- powerll vv, pp <- partsll (cart uu kk)]]
  | otherwise = Nothing
  where
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    qqff = fromJust . setTransformsFud
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpowerll = qqll . setsPowerset . llqq
    powerll = qqll . setsPowerset
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
    
systemsSetVarsSetTransformSubstrate_2 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate_2 uu vv
  | vv `subset` uvars uu = Just $ llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- nnvvll uu vv]
  | otherwise = Nothing
  where
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionsSetTransform :: System -> Set.Set Variable -> Set.Set SetPartition -> Maybe (Set.Set Transform)
systemsSetVarsSetSetPartitionsSetTransform uu vv nnvv
  | vv `subset` uvars uu = Just $ llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- qqll2 nnvv]
  | otherwise = Nothing
  where
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    llqq = Set.fromList
      
systemsSetVarsSetTransformSubstrate_3 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate_3 uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf
    
systemsSetVarsSetTransformSubstrate_4 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate_4 uu vv
  | vv `subset` uvars uu = Just $ llqq [expand uu vv (nntt nn) | nn <- qqll (nnvv uu vv)]
  | otherwise = Nothing
  where
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    expand uu vv tt = fromJust $ systemsSetVarsTransformVarPartitionsExpand uu vv tt
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList       
       
systemsSetVarsSetTransformSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (bell (vol uu vv))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf         
    
systemsSetVarsSetSetPartitionSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrate uu vv
  | vv `subset` uvars uu = 
    Just $ llpower [qqpp pp | kk <- powerll vv, pp <- partsll (cart uu kk)]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsSetSetPartitionSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (sum [bell (vol uu kk) | kk <- powerll vv])
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf         

valenciesDimensionsSetSetPartitionSubstrateCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionSubstrateCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ 2 ^ (sum [binom n k * bell (d^k) | k <- [0..n]])
  | otherwise = Nothing
  where
    binom = combination
    
systemsSetVarsSetSetPartitionSubstrateCardinalityLower :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateCardinalityLower uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (bell (vol uu vv))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf      

systemsSetVarsSetSetPartitionSubstrateCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (2 ^ (dim vv) * bell (vol uu vv))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf      

systemsSetVarsSetSetPartitionPointedSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartitionPointed)
systemsSetVarsSetSetPartitionPointedSubstrate uu vv
  | vv `subset` uvars uu = 
    Just $ llpower [ppqqppp (qqpp pp) cc | kk <- powerll vv, pp <- partsll (cart uu kk), cc <- qqll pp]
  | otherwise = Nothing
  where
    ppqqppp pp cc = fromJust $ partitionsComponentsPartitionPointed pp cc 
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsIntegersFudPowerLimitedLayer :: System -> Set.Set Variable -> Integer -> Maybe Fud
systemsSetVarsIntegersFudPowerLimitedLayer uu vv l
  | vv `subset` uvars uu = Just $ powlayer l fudEmpty
  | otherwise = Nothing
  where
    powlayer l ff = 
      if l > 0 
        then powlayer (l-1) (qqff (ffqq ff `union` llqq [tt | kk <- tuplesll ff, tt <- ffqqll (base (uu `uunion` fsys ff) kk)]))
        else ff
    base uu vv = fromJust $ systemsSetVarsFudBase uu vv
    tuplesll ff = Set.toList $ setVariablesFudsSetTuple vv ff
    fsys = fudsSystemImplied
    ffqqll = Set.toList . fudsSetTransform
    ffqq = fudsSetTransform
    qqff = fromJust . setTransformsFud
    uvars = systemsVars
    uunion = pairSystemsUnion
    union = Set.union
    subset = Set.isSubsetOf
    llqq = Set.fromList

systemsSetVarsIntegersFudPowerLimitedLayer_1 :: System -> Set.Set Variable -> Integer -> Maybe Fud
systemsSetVarsIntegersFudPowerLimitedLayer_1 uu vv l
  | vv `subset` uvars uu = Just $ powlayer l fudEmpty
  | otherwise = Nothing
  where
    powlayer l ff = 
      if l > 0 
        then powlayer (l-1) (llqqff [tt | kk <- powerll (fvars ff `union` vv), tt <- ffqqll (base (uu `uunion` fsys ff) kk)])
        else ff
    base uu vv = fromJust $ systemsSetVarsFudBase uu vv
    fsys = fudsSystemImplied
    fvars = fudsVars
    ffqqll = Set.toList . fudsSetTransform
    llqqff = fromJust . setTransformsFud . Set.fromList
    uvars = systemsVars
    uunion = pairSystemsUnion
    powerll = Set.toList . setsPowerset
    union = Set.union
    subset = Set.isSubsetOf
    
systemsSetVarsFudPower :: System -> Set.Set Variable -> Maybe Fud
systemsSetVarsFudPower uu vv
  | vv `subset` uvars uu = Just $ pow fudEmpty
  | otherwise = Nothing
  where
    pow ff = if gg == Set.empty then ff else pow (qqff (ffqq ff `Set.union` gg))
      where
        gg = llqq [tt |  kk <- powerll (fvars ff `union` vv), let hh = depends ff kk, 
          tt <- ffqqll (base (uu `uunion` fsys ff) kk), tt `notmem` ffqq ff,
          pptt (ttpp (fftt (hh `fins` tt))) `notmem` ffqq (flatten hh)]
    base uu vv = fromJust $ systemsSetVarsFudBase uu vv
    depends = fudsVarsDepends
    flatten = fudsFlatten
    fftt = fudsTransform
    fins ff tt = qqff (Set.insert tt (ffqq ff))
    fvars = fudsVars
    fsys = fudsSystemImplied
    ffqqll = Set.toList . fudsSetTransform
    ffqq = fudsSetTransform
    qqff = fromJust . setTransformsFud
    ttpp = transformsPartition
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    uunion = pairSystemsUnion
    powerll = Set.toList . setsPowerset
    union = Set.union
    subset = Set.isSubsetOf
    llqq = Set.fromList
    notmem = Set.notMember
    
systemsSetVarsFudPower_1 :: System -> Set.Set Variable -> Maybe Fud
systemsSetVarsFudPower_1 uu vv
  | vv `subset` uvars uu = Just $ pow fudEmpty
  | otherwise = Nothing
  where
    pow ff = if gg == Set.empty then ff else pow (qqff (ffqq ff `Set.union` gg))
      where
        gg = llqq [tt |  kk <- powerll (fvars ff `union` vv), let hh = depends ff kk, 
          tt <- ffqqll (base (uu `uunion` fsys ff) kk), tt `notmem` ffqq ff,
          pptt (expand uu (fund hh) (ttpp (fftt (hh `fins` tt)))) `notmem` ffqq (flatten hh)]
    base uu vv = fromJust $ systemsSetVarsFudBase uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    depends = fudsVarsDepends
    flatten = fudsFlatten
    fftt = fudsTransform
    fins ff tt = qqff (Set.insert tt (ffqq ff))
    fvars = fudsVars
    fund = fudsUnderlying
    fsys = fudsSystemImplied
    ffqqll = Set.toList . fudsSetTransform
    ffqq = fudsSetTransform
    qqff = fromJust . setTransformsFud
    ttpp = transformsPartition
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    uunion = pairSystemsUnion
    powerll = Set.toList . setsPowerset
    union = Set.union
    subset = Set.isSubsetOf
    llqq = Set.fromList
    notmem = Set.notMember    
    
systemsSetVarsSetFudSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Fud)
systemsSetVarsSetFudSubstrate uu vv
  | vv `subset` uvars uu = Just $ llqq [qqff qq | qq <- powerll (ffqq (power uu vv)), fund (qqff qq) `subset` vv]
  | otherwise = Nothing
  where
    power uu vv = fromJust $ systemsSetVarsFudPower uu vv
    fund = fudsUnderlying
    ffqq = fudsSetTransform
    qqff = fromJust . setTransformsFud
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    llqq = Set.fromList
    
systemsSetVarsSetFudSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set Fud)
systemsSetVarsSetFudSubstrate_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [depends nn xx | xx <- powerll (fvars nn)]
  | otherwise = Nothing
  where
    nn = fromJust $ systemsSetVarsFudPower uu vv
    depends = fudsVarsDepends
    fvars = fudsVars
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    llqq = Set.fromList
       
systemsSetVarsTreePairStateTransformPower :: System -> Set.Set Variable -> Maybe (Tree (State,Transform))
systemsSetVarsTreePairStateTransformPower uu vv
  | vv `subset` uvars uu = Just $ Tree $ llmm $ [((stateEmpty,tt), pow tt (single tt)) | tt <- ttuuvvll]
  | otherwise = Nothing
  where
    pow tt ff = Tree $ llmm $ concat [[((ss,rr), pow rr (ff `ins` rr)), ((ss,rr), emptyTree)] | 
      ss <- stdll tt, rr <- ttuuvvll, rr `notmem` ff]          
    ttuuvvll = Set.toList $ fromJust $ systemsSetVarsSetTransformSubstrate uu vv          
    stdll = Set.toList . transformsStateDeriveds
    uvars = systemsVars
    llmm = Map.fromList
    single = Set.singleton
    notmem = Set.notMember
    ins qq x = Set.insert x qq
    subset = Set.isSubsetOf
 
systemsSetVarsSetDecompSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Decomp)
systemsSetVarsSetDecompSubstrate uu vv
  | vv `subset` uvars uu = Just $ llqq [decomp zz | zz <- qqll (distinct (power uu vv)), okVars zz]
  | otherwise = Nothing
  where
    power uu vv = fromJust $ systemsSetVarsTreePairStateTransformPower uu vv
    okVars zz = and [der tt1 `cap` der tt2 == empty | tt1 <- qqll (ran (elem zz)), tt2 <- qqll (ran (elem zz)), tt1 /= tt2]
    decomp zz = fromJust $ treePairStateTransformsDecomp zz
    der = transformsDerived
    uvars = systemsVars
    elem = treesElements    
    distinct = treesDistinct
    ran = relationsRange
    llqq = Set.fromList
    qqll = Set.toList
    subset = Set.isSubsetOf
    cap = Set.intersection
    empty = Set.empty
    
systemsSetVarsTreePairStateFudPower :: System -> Set.Set Variable -> Maybe (Tree (State,Fud))
systemsSetVarsTreePairStateFudPower uu vv
  | vv `subset` uvars uu = Just $ Tree $ llmm $ [((stateEmpty,ff), pow ff (single ff)) | ff <- ffuuvvll]
  | otherwise = Nothing
  where
    pow ff qq = Tree $ llmm $ concat [[((ss,gg), pow gg (qq `ins` gg)), ((ss,gg), emptyTree)] | 
      ss <- stdll ff, gg <- ffuuvvll, gg `notmem` qq]          
    ffuuvvll = Set.toList $ fromJust $ systemsSetVarsSetFudSubstrate uu vv          
    stdll ff = Set.toList $ cart (fsys ff) (fder ff)
    fsys = fudsSystemImplied
    fder = fudsDerived    
    cart uu vv = fromJust $ systemsVarsCartesian uu vv    
    uvars = systemsVars
    llmm = Map.fromList
    single = Set.singleton
    notmem = Set.notMember
    ins qq x = Set.insert x qq
    subset = Set.isSubsetOf
 
systemsSetVarsSetDecompFudSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set DecompFud)
systemsSetVarsSetDecompFudSubstrate uu vv
  | vv `subset` uvars uu = Just $ llqq [decomp zz | zz <- qqll (distinct (power uu vv))]
  | otherwise = Nothing
  where
    power uu vv = fromJust $ systemsSetVarsTreePairStateFudPower uu vv
    decomp zz = fromJust $ treePairStateFudsDecompFud zz
    uvars = systemsVars
    distinct = treesDistinct
    llqq = Set.fromList
    qqll = Set.toList
    subset = Set.isSubsetOf
    
systemsSetVarsSetSetPartitionSubstrateNonOverlap :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp qq | qq <- nn] | yy <- partsll vv, nn <- qqll (prod [parts (cart uu kk) | kk <- qqll yy])]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    partsll = Set.toList . setsPartitionSet
    parts = setsPartitionSet
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionSubstrateNonOverlap_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateNonOverlap_1 uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [nn | nn <- nnvvll uu vv, ispart vv nn]
  | otherwise = Nothing
  where
    ispart vv nn = let mm = Set.map ppvars nn in card mm == card nn && setSetsUnion mm == vv && setsIsPartition mm
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppvars = partitionsVars
    uvars = systemsVars
    card = Set.size
    subset = Set.isSubsetOf
    empty = Set.empty      
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityLower :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityLower uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf  
    empty = Set.empty  
   
systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ 
      sum [product [bell (vol uu kk) | kk <- qqll yy ] | yy <- partsll vv]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    partsll = Set.toList . setsPartitionSet
    subset = Set.isSubsetOf  
    empty = Set.empty  
    qqll = Set.toList
       
valenciesDimensionsSetSetPartitionSubstrateNonOverlapCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionSubstrateNonOverlapCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ sum [c * product [(bell (d^k))^m | (k,m) <- zip [1..] ll] | (ll,c) <- bcdll n]
  | otherwise = Nothing
  where
    bcdll = Map.toList . bellcd
    
systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityUpper uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf  
    empty = Set.empty  
    
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak uu vv
  | vv `subset` uvars uu = Just $ 
    llqq ([llqq2 [qqpp qq | qq <- nn] | yy <- wpartsll vv, nn <- qqll (prod [parts (cart uu kk) | kk <- qqll yy])] ++ [empty])
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    wpartsll = Set.toList . setsSetPartitionWeak
    parts = setsPartitionSet
    empty = Set.empty  
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak_1 uu vv
  | vv `subset` uvars uu = Just $ 
      llqq ([nn | nn <- nnvvll uu vv, ispart vv nn] ++ [empty])
  | otherwise = Nothing
  where
    ispart vv nn = let mm = Set.map ppvars nn in card mm == card nn && setSetsUnion mm == vv && setsIsPartitionWeak mm
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppvars = partitionsVars
    uvars = systemsVars
    card = Set.size
    subset = Set.isSubsetOf
    empty = Set.empty      
    qqll = Set.toList
    llqq = Set.fromList        
    
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak_2 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak_2 uu vv
  | vv == empty = Just $ llqq [single (unary uu),empty]
  | vv `subset` uvars uu = Just $ 
      llqq (partsll uu vv ++ [unary uu `ins` nn | nn <- partsll uu vv] ++ [empty])
  | otherwise = Nothing
  where
    partsll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    unary uu = fromJust $ systemsSetVarsPartitionUnary uu empty
    uvars = systemsVars
    ins = Set.insert
    subset = Set.isSubsetOf
    empty = Set.empty      
    single = Set.singleton      
    llqq = Set.fromList        
    
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinalityLower :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinalityLower uu vv
  | vv == empty = Just $ 2
  | vv `subset` uvars uu = Just $ 2 * bell (vol uu vv) + 1
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf  
    empty = Set.empty  
   
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinality uu vv
  | vv == empty = Just $ 2
  | vv `subset` uvars uu = Just $ 
      sum [product [bell (vol uu kk) | kk <- qqll yy ] | yy <- wpartsll vv] + 1
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    wpartsll = Set.toList . setsSetPartitionWeak
    subset = Set.isSubsetOf  
    empty = Set.empty  
    qqll = Set.toList
       
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinalityUpper uu vv
  | vv == empty = Just $ 2
  | vv `subset` uvars uu = Just $ 2 * bell (dim vv) * bell (vol uu vv) + 1
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf  
    empty = Set.empty      
    
systemsSetVarsSetTransformSubstrateNonOverlap :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlap uu vv
  | vv `subset` uvars uu = Just $ 
    llqq ([fftt (llff [pptt (expand uu vv (qqpp pp)) | pp <- nn]) | yy <- wpartsll vv, 
      nn <- qqll (prod [parts (cart uu kk) | kk <- qqll yy])] ++ [transformEmpty])
  | otherwise = Nothing
  where
    llff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    wpartsll = Set.toList . setsSetPartitionWeak
    parts = setsPartitionSet
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateNonOverlap_1 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlap_1 uu vv
  | vv `subset` uvars uu = Just $ Set.filter isnonover (ttvv uu vv)
  | otherwise = Nothing
  where
    ttvv uu vv = fromJust $ systemsSetVarsSetTransformSubstrate uu vv
    isnonover = not .transformsIsOverlap
    uvars = systemsVars
    subset = Set.isSubsetOf
    
systemsSetVarsSetTransformSubstrateNonOverlap_2 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlap_2 uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- nnvvnwll uu vv]
  | otherwise = Nothing
  where
    nnvvnwll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    ppvars = partitionsVars
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty      
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateNonOverlapCardinalityLower :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateNonOverlapCardinalityLower uu vv
  | vv == empty = Just $ 2
  | vv `subset` uvars uu = Just $ 2 * bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf  
    empty = Set.empty  
      
systemsSetVarsSetTransformSubstrateNonOverlapCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateNonOverlapCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ nnvvnwcd uu vv
  | otherwise = Nothing
  where
    nnvvnwcd uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlapWeakCardinality uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf  
           
systemsSetVarsSetTransformSubstrateNonOverlapCardinalityUpperUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateNonOverlapCardinalityUpperUpper uu vv
  | vv `subset` uvars uu = Just $ 2 * bell (dim vv) * bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf  
    empty = Set.empty    
    
systemsSetVarsSetTransformSubstrateNonOverlapStrong :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [fftt (llff [pptt (expand uu vv (qqpp pp)) | pp <- nn]) | yy <- partsll vv, 
      nn <- qqll (prod [parts (cart uu kk) | kk <- qqll yy])]
  | otherwise = Nothing
  where
    llff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    partsll = Set.toList . setsSetPartition
    parts = setsPartitionSet
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateNonOverlapStrong_1 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong_1 uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- nnvvnll uu vv]
  | otherwise = Nothing
  where
    nnvvnll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    ppvars = partitionsVars
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty      
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityLower :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityLower uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf  
    empty = Set.empty  
      
systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityUpper uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ nnvvncd uu vv
  | otherwise = Nothing
  where
    nnvvncd uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinality uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
    empty = Set.empty  
    
systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityUpperUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateNonOverlapStrongCardinalityUpperUpper uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf  
    empty = Set.empty    
       
systemsSetVarsSetTransformSubstrateFixedDimension :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateFixedDimension uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [fftt (qqff qq) | qq <- llpowerll [pptt (qqpp pp) | pp <- partsll (cart uu vv)], Set.size qq <= Set.size vv]
  | otherwise = Nothing
  where
    qqff = fromJust . setTransformsFud
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpowerll = qqll . setsPowerset . llqq
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateFixedDimensionCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetTransformSubstrateFixedDimensionCardinality uu vv
  | vv `subset` uvars uu = Just $ sum [combination (bell (vol uu vv)) k | k <- [0 .. dim vv]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf    
      
systemsSetVarsSetSetPartitionSubstrateBinary :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateBinary uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp pp, qqpp qq] |
      kk <- powerll vv, kk /= empty, kk /= vv, 
      pp <- partsll (cart uu kk), qq <- partsll (cart uu (vv `minus` kk))]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = Set.toList . setsPartitionSet
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf    
    empty = Set.empty  
    qqll = Set.toList
    minus = Set.difference    
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionSubstrateBinary_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateBinary_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvnll uu vv, card nn == 2]
  | otherwise = Nothing
  where
    nnvvnll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    uvars = systemsVars
    card = Set.size
    subset = Set.isSubsetOf
    llqq = Set.fromList
       
systemsSetVarsSetSetPartitionSubstrateBinaryCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateBinaryCardinality uu vv
  | vv `subset` uvars uu = Just $ sum [bell (vol uu kk) * bell (vol uu (vv `minus` kk)) | kk <- powerll vv, kk /= empty, kk /= vv] `div` 2
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf  
    empty = Set.empty  
    minus = Set.difference

valenciesDimensionsSetSetPartitionSubstrateBinaryCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionSubstrateBinaryCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ sum [binom n k * bell (d^k) * bell (d^(n-k)) | k <- [1..n-1]] `div` 2
  | otherwise = Nothing
  where
    binom = combination
           
systemsSetVarsSetSetPartitionSubstrateBinaryCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateBinaryCardinalityUpper uu vv
  | dim vv < 2 = Just $ 0
  | vv `subset` uvars uu = Just $ 2 ^ (dim vv - 2) * bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf  
    empty = Set.empty       
    
systemsSetVarsSetTransformSubstrateBinary :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateBinary uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- nnvvnbll uu vv]
  | otherwise = Nothing
  where
    nnvvnbll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateBinary uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp qq | qq <- nn] | nn <- qqll (prod [parts (cart uu w) | w <- qqll vv])]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu w = fromJust $ systemsVarsCartesian uu (Set.singleton w)
    uvars = systemsVars
    prod = listSetsProduct
    parts = setsPartitionSet
    subset = Set.isSubsetOf   
    empty = Set.empty        
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateSelf_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvnll uu vv, card nn == card vv]
  | otherwise = Nothing
  where
    nnvvnll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    uvars = systemsVars
    card = Set.size
    subset = Set.isSubsetOf
    llqq = Set.fromList
       
systemsSetVarsSetSetPartitionSubstrateSelfCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateSelfCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ product [bell (vol uu w) | w <- qqll vv]
  | otherwise = Nothing
  where
    vol uu w = fromJust $ systemsVarsVolume uu (Set.singleton w)
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf  
    empty = Set.empty  
       
valenciesDimensionsSetSetPartitionSubstrateSelfCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionSubstrateSelfCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ (bell d) ^ n
  | otherwise = Nothing
    
systemsSetVarsSetTransformSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateSelf uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- nnvvnsll uu vv]
  | otherwise = Nothing
  where
    nnvvnsll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateSelf uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
    qqll = Set.toList
     
systemsSetVarsSetSetPartitionSubstrateOverlappingSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionSubstrateOverlappingSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp qq | nn <- xx, qq <- qqll2 nn] | xx <- qqll (prod [power (parts (cart uu w)) | w <- qqll vv])]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu w = fromJust $ systemsVarsCartesian uu (Set.singleton w)
    uvars = systemsVars
    prod = listSetsProduct
    parts = setsPartitionSet
    power = setsPowerset    
    subset = Set.isSubsetOf   
    empty = Set.empty        
    qqll = Set.toList
    qqll2 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
           
systemsSetVarsSetSetPartitionSubstrateOverlappingSelfCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionSubstrateOverlappingSelfCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ product [2 ^ (bell (vol uu w)) | w <- qqll vv]
  | otherwise = Nothing
  where
    vol uu w = fromJust $ systemsVarsVolume uu (Set.singleton w)
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf  
    empty = Set.empty  

valenciesDimensionsSetSetPartitionSubstrateOverlappingSelfCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionSubstrateOverlappingSelfCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ 2 ^ (n * bell d)
  | otherwise = Nothing
       
systemsSetVarsSetTransformSubstrateOverlappingSelf :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateOverlappingSelf uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateOverlappingSelf uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
          
systemsSetVarsSetSetPartitionCartesianSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
  | vv `subset` uvars uu = Just $ llqq [llqq2 [self uu kk | kk <- qqll xx] | xx <- powerll (power vv)]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    power = setsPowerset
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList

systemsSetVarsSetSetPartitionCartesianSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrate_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, isself nn]
  | otherwise = Nothing
  where
    isself nn = and [pp == self uu (ppvars pp) | pp <- qqll nn]
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    ppvars = partitionsVars
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrate_2 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrate_2 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, isself nn]
  | otherwise = Nothing
  where
    isself nn = and [card (ppqq pp) == card (bigcup (ppqq pp)) | pp <- qqll nn]
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppqq = partitionsSetComponent
    uvars = systemsVars
    subset = Set.isSubsetOf
    bigcup = setSetsUnion
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
        
setVarsSetSetPartitionCartesianSubstrateCardinality :: Set.Set Variable -> Integer
setVarsSetSetPartitionCartesianSubstrateCardinality vv = 2 ^ (2 ^ dim vv)
  where
    dim = toInteger . Set.size
   
systemsSetVarsSetTransformCartesianSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformCartesianSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [fftt (llqqff [pptt (expand uu vv pp) | pp <- qqll nn]) | nn <- nnvvcll uu vv]
  | otherwise = Nothing
  where
    nnvvcll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    expand uu vv pp = fromJust $ systemsSetVarsPartitionsExpand uu vv pp
    llqqff = fromJust . setTransformsFud . Set.fromList
    fftt = fudsTransform
    pptt = partitionsTransformVarPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
    qqll = Set.toList 
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak uu vv
  | vv `subset` uvars uu = Just $ 
    llqq ([llqq2 [self uu kk | kk <- qqll yy] | yy <- wpartsll vv] ++ [empty])
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    wpartsll = Set.toList . setsSetPartitionWeak
    empty = Set.empty  
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvnw uu vv `cap` nnvvc uu vv
  | otherwise = Nothing
  where
    nnvvnw uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak uu vv
    nnvvc uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
    
setVarsSetSetPartitionCartesianSubstrateNonOverlapWeakCardinality :: Set.Set Variable -> Integer
setVarsSetSetPartitionCartesianSubstrateNonOverlapWeakCardinality vv 
  | dim vv == 0 = 2
  | otherwise = 2 * bell (dim vv) + 1
  where
    dim = toInteger . Set.size
   
systemsSetVarsSetTransformCartesianSubstrateNonOverlapWeak :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformCartesianSubstrateNonOverlapWeak uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapWeak uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlap :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlap uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [self uu kk | kk <- qqll yy] | yy <- partsll vv]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition
    empty = Set.empty  
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlap_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlap_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvn uu vv `cap` nnvvc uu vv
  | otherwise = Nothing
  where
    nnvvn uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    nnvvc uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
    
setVarsSetSetPartitionCartesianSubstrateNonOverlapCardinality :: Set.Set Variable -> Integer
setVarsSetSetPartitionCartesianSubstrateNonOverlapCardinality vv 
  | dim vv == 0 = 0
  | otherwise = bell (dim vv)
  where
    dim = toInteger . Set.size    
    
systemsSetVarsSetSetPartitionCartesianSubstrateBinary :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateBinary uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [self uu kk, self uu (vv `minus` kk)] | kk <- powerll vv, kk /= empty, kk /= vv]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    empty = Set.empty  
    subset = Set.isSubsetOf
    minus = Set.difference    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateBinary_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateBinary_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvnb uu vv `cap` nnvvc uu vv
  | otherwise = Nothing
  where
    nnvvnb uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateBinary uu vv
    nnvvc uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
    
setVarsSetSetPartitionCartesianSubstrateBinaryCardinality :: Set.Set Variable -> Integer
setVarsSetSetPartitionCartesianSubstrateBinaryCardinality vv
  | dim vv < 2 = 0
  | otherwise = 2 ^ (dim vv - 1) - 1
  where
    dim = toInteger . Set.size
   
systemsSetVarsSetTransformCartesianSubstrateBinary :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformCartesianSubstrateBinary uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateBinary uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf    
  
systemsSetVarsSetSetPartitionCartesianSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ sgl $ llqq [self uu v | v <- qqll vv]
  | otherwise = Nothing
  where
    self uu v = fromJust $ systemsSetVarsPartitionSelf uu (sgl v)
    uvars = systemsVars
    sgl = Set.singleton
    subset = Set.isSubsetOf
    empty = Set.empty  
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateSelf_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvns uu vv `cap` nnvvc uu vv
  | otherwise = Nothing
  where
    nnvvns uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateSelf uu vv
    nnvvc uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
    
setVarsSetSetPartitionCartesianSubstrateSelfCardinality :: Set.Set Variable -> Integer
setVarsSetSetPartitionCartesianSubstrateSelfCardinality vv
  | dim vv == 0 = 0
  | otherwise = 1
  where
    dim = toInteger . Set.size
   
systemsSetVarsSetTransformCartesianSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformCartesianSubstrateSelf uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelf uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      

systemsSetVarsSetSetPartitionTwoSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionTwoSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
    power $ llqq [llpp [cc, qq `minus` cc] | kk <- powerll vv, let qq = cart uu kk, cc <- powerll2 qq, cc /= empty, cc /= qq]
  | otherwise = Nothing
  where
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    llpp ll = fromJust $ setComponentsPartition $ Set.fromList ll
    uvars = systemsVars
    power = setsPowerset
    powerll = Set.toList . setsPowerset
    powerll2 = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    empty = Set.empty  
    minus = Set.difference
    llqq = Set.fromList

systemsSetVarsSetSetPartitionTwoSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionTwoSubstrate_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, isbinary nn]
  | otherwise = Nothing
  where
    isbinary nn = and [card (ppqq pp) == 2| pp <- qqll nn]
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppqq = partitionsSetComponent
    uvars = systemsVars
    subset = Set.isSubsetOf
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionTwoSubstrate_2 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionTwoSubstrate_2 uu vv
  | vv `subset` uvars uu = Just $ 
    power $ llqq [qqpp pp | kk <- powerll vv, pp <- partsll (cart uu kk), card pp == 2]
  | otherwise = Nothing
  where
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq
    uvars = systemsVars
    power = setsPowerset
    powerll = Set.toList . setsPowerset
    partsll = Set.toList . setsPartitionSet
    subset = Set.isSubsetOf
    empty = Set.empty  
    card = Set.size
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionTwoSubstrateCardinalityLower :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionTwoSubstrateCardinalityLower uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (2^(vol uu vv -1) - 1)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf               
    
systemsSetVarsSetSetPartitionTwoSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionTwoSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ 2 ^ sum [2^(x-1)-1 | kk <- powerll vv, let x = vol uu kk, x >= 2]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset    
    subset = Set.isSubsetOf           
     
valenciesDimensionsSetSetPartitionTwoSubstrateCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionTwoSubstrateCardinalityRegular d n 
  | n >= 0 && d >= 2 = Just $ 2 ^ sum [binom n k * (2 ^ (d^k-1) - 1) | k <- [1..n]]
  | otherwise = Nothing
  where
    binom = combination
           
systemsSetVarsSetSetPartitionTwoSubstrateCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionTwoSubstrateCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (2^(dim vv + vol uu vv -1))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf           
     
systemsSetVarsSetTransformTwoSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformTwoSubstrate uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionTwoSubstrate uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
    
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlap :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlap uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 nn | yy <- partsll vv, 
      nn <- qqll (prod [llqq3 [llpp [cc, qq `minus` cc] | let qq = cart uu kk, cc <- powerll qq, cc /= empty, cc /= qq] | kk <- qqll2 yy])]
  | otherwise = Nothing
  where
    llpp ll = fromJust $ setComponentsPartition $ Set.fromList ll    
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition
    powerll = Set.toList . setsPowerset    
    prod = listSetsProduct    
    empty = Set.empty  
    minus = Set.difference
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList
    
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlap_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlap_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvn uu vv `cap` nnvv2 uu vv
  | otherwise = Nothing
  where
    nnvvn uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    nnvv2 uu vv = fromJust $ systemsSetVarsSetSetPartitionTwoSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
        
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlapCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlapCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ 
      sum [product [2^(vol uu kk - 1)-1 | kk <- qqll yy] | yy <- partsll vv, and [vol uu kk >= 2 | kk <- qqll yy]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    partsll = Set.toList . setsPartitionSet
    subset = Set.isSubsetOf           
    empty = Set.empty  
    qqll = Set.toList
     
valenciesDimensionsSetSetPartitionTwoSubstrateNonOverlapCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionTwoSubstrateNonOverlapCardinalityRegular d n 
  | n >= 1 && d >= 2 = Just $ 
      sum [c * product [(2^(d^k-1) - 1)^m | (k,m) <- zip [1..] ll] | (ll,c) <- bcdll n]
  | otherwise = Nothing
  where
    bcdll = Map.toList . bellcd
           
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlapCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionTwoSubstrateNonOverlapCardinalityUpper uu vv 
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * (2 ^ vol uu vv - 1)
  | otherwise = Nothing
  where
    dim = toInteger . Set.size        
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars    
    subset = Set.isSubsetOf
    
systemsSetVarsSetSetPartitionOneSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionOneSubstrate uu vv
  | vv `subset` uvars uu = Just $ power $ llqq [unary uu kk | kk <- powerll vv]
  | otherwise = Nothing
  where
    unary uu vv = fromJust $ systemsSetVarsPartitionUnary uu vv
    uvars = systemsVars
    power = setsPowerset
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    llqq = Set.fromList

systemsSetVarsSetSetPartitionOneSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionOneSubstrate_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, isunary nn]
  | otherwise = Nothing
  where
    isunary nn = and [card (ppqq pp) == 1| pp <- qqll nn]
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppqq = partitionsSetComponent
    uvars = systemsVars
    subset = Set.isSubsetOf
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList    

setVarsSetSetPartitionOneSubstrate_Cardinality :: Set.Set Variable -> Integer
setVarsSetSetPartitionOneSubstrate_Cardinality vv
  | otherwise = 2 ^ (2 ^ dim vv)
  where
    dim = toInteger . Set.size
    
systemsSetVarsSetTransformOneSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformOneSubstrate uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionOneSubstrate uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
 
systemsSetVarsSetSetPartitionDecrementedSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 ([qq] ++ [self uu kk | kk <- qqll xx]) | jj <- powerll vv, qq <- decsll uu jj, xx <- powerll2 (power vv)]
  | otherwise = Nothing
  where
    decsll uu vv = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map single (cart uu vv)
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv  
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq
    uvars = systemsVars
    power = setsPowerset
    powerll = Set.toList . setsPowerset
    powerll2 = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    single = Set.singleton
    empty = Set.empty  
    llqq = Set.fromList
    llqq2 = Set.fromList
    qqll = Set.toList

systemsSetVarsSetSetPartitionDecrementedSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrate_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, isdec nn]
  | otherwise = Nothing
  where
    isdec nn = or [card (ppqq qq) == card (bigcup (ppqq qq)) - 1 && 
      and [card (ppqq pp) == card (bigcup (ppqq pp)) | pp <- qqll nn, pp /= qq] | qq <- qqll nn]
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppqq = partitionsSetComponent
    uvars = systemsVars
    subset = Set.isSubsetOf
    bigcup = setSetsUnion
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionDecrementedSubstrate_2 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrate_2 uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [nn `ins` qq | jj <- powerll vv, qq <- decsll uu jj, nn <- nnvvcll uu vv]
  | otherwise = Nothing
  where
    nnvvcll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate  uu vv  
    decsll uu vv = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map single (cart uu vv)
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    single = Set.singleton
    empty = Set.empty  
    ins qq x = Set.insert x qq
    llqq = Set.fromList    
    
systemsSetVarsSetSetPartitionDecrementedSubstrate_3 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrate_3 uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 ([qqpp (cartself uu jj `del` sgl ss1 `del` sgl ss2 `ins` llqq3 [ss1, ss2])] ++ [self uu kk | kk <- qqll xx]) | 
      jj <- powerll vv, ss1 <- cartll uu jj, ss2 <- cartll uu jj, ss1 /= ss2, xx <- powerll2 (power vv)]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv  
    cartll uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv
    cartself uu vv = Set.map sgl $ fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq
    uvars = systemsVars
    power = setsPowerset
    powerll = Set.toList . setsPowerset
    powerll2 = Set.toList . setsPowerset
    subset = Set.isSubsetOf
    sgl = Set.singleton
    del qq x = Set.delete x qq
    ins qq x = Set.insert x qq
    empty = Set.empty  
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionDecrementedSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (2 ^ dim vv - 1) * sum [x * (x-1) | jj <- powerll vv, jj /= empty, let x = vol uu jj]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset    
    empty = Set.empty      
    dim = toInteger . Set.size    
    subset = Set.isSubsetOf           

valenciesDimensionsSetSetPartitionDecrementedSubstrateCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionDecrementedSubstrateCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ 2 ^ 2 ^ n * sum [binom n k * d^k * (d^k-1) `div` 2 | k <- [1..n]]
  | otherwise = Nothing
  where
    binom = combination
         
systemsSetVarsSetSetPartitionDecrementedSubstrateCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (2 ^ dim vv + dim vv - 1) * (vol uu vv) ^ 2
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf           
     
systemsSetVarsSetTransformDecrementedSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformDecrementedSubstrate uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionDecrementedSubstrate uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
    
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeak :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeak uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 ([qq] ++ [self uu kk | kk <- qqll yy, kk /= jj]) | yy <- wpartsll vv, jj <- qqll2 yy, qq <- decsll uu jj]
  | otherwise = Nothing
  where
    decsll uu vv = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map single (cart uu vv)
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv  
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq  
    uvars = systemsVars
    wpartsll = Set.toList . setsSetPartitionWeak
    single = Set.singleton
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeak_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeak_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvnw uu vv `cap` nnvvd uu vv
  | otherwise = Nothing
  where
    nnvvnw uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlapWeak uu vv
    nnvvd uu vv = fromJust $ systemsSetVarsSetSetPartitionDecrementedSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
     
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeakCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeakCardinality uu vv 
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ sum [(vol uu jj) * (vol uu jj - 1) | yy <- wpartsll vv, jj <- qqll yy, jj /= empty] `div` 2
  | otherwise = Nothing
  where
    dim = toInteger . Set.size        
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    wpartsll = Set.toList . setsSetPartitionWeak    
    uvars = systemsVars    
    empty = Set.empty  
    qqll = Set.toList    
    subset = Set.isSubsetOf    
        
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeakCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapWeakCardinalityUpper uu vv 
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * (dim vv) * (vol uu vv) ^ 2
  | otherwise = Nothing
  where
    dim = toInteger . Set.size        
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars    
    subset = Set.isSubsetOf    
    
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlap :: 
  System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlap uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 ([qq] ++ [self uu kk | kk <- qqll yy, kk /= jj]) | yy <- partsll vv, jj <- qqll2 yy, qq <- decsll uu jj]
  | otherwise = Nothing
  where
    decsll uu vv = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map single (cart uu vv)
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv  
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq  
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition
    single = Set.singleton
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlap_1 :: 
  System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlap_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvn uu vv `cap` nnvvd uu vv
  | otherwise = Nothing
  where
    nnvvn uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    nnvvd uu vv = fromJust $ systemsSetVarsSetSetPartitionDecrementedSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
     
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapCardinality uu vv 
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ 
      sum [(vol uu jj) * (vol uu jj - 1) | yy <- partsll vv, jj <- qqll yy, jj /= empty] `div` 2
  | otherwise = Nothing
  where
    dim = toInteger . Set.size        
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    partsll = Set.toList . setsSetPartition    
    uvars = systemsVars    
    empty = Set.empty  
    qqll = Set.toList    
    subset = Set.isSubsetOf    
        
valenciesDimensionsSetSetPartitionDecrementedSubstrateNonOverlapCardinalityRegular :: 
  Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionDecrementedSubstrateNonOverlapCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ sum [c * sum [m * d^k * (d^k-1) `div` 2 | (k,m) <- zip [1..] ll] | (ll,c) <- bcdll n]
  | otherwise = Nothing
  where
    bcdll = Map.toList . bellcd
    
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapCardinalityUpper :: 
  System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateNonOverlapCardinalityUpper uu vv 
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * (dim vv) * (vol uu vv) ^ 2 `div` 2
  | otherwise = Nothing
  where
    dim = toInteger . Set.size        
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars    
    subset = Set.isSubsetOf    
    
systemsSetVarsSetSetPartitionDecrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateSelf uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 ([qq] ++ [self uu (single u) | u <- qqll vv, u /= w]) | w <- qqll vv, qq <- decsll uu (single w)]
  | otherwise = Nothing
  where
    decsll uu vv = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map single (cart uu vv)
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv 
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq  
    uvars = systemsVars
    single = Set.singleton
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionDecrementedSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateSelf_1 uu vv
  | vv `subset` uvars uu = Just $ nnvvns uu vv `cap` nnvvd uu vv
  | otherwise = Nothing
  where
    nnvvns uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateSelf uu vv
    nnvvd uu vv = fromJust $ systemsSetVarsSetSetPartitionDecrementedSubstrate uu vv
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf
    
systemsSetVarsSetSetPartitionDecrementedSubstrateSelf_2 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionDecrementedSubstrateSelf_2 uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 ([qqpp (cartself uu w `del` sgl (single w s) `del` sgl (single w t) `ins` llqq3 [single w s, single w t])] ++ 
      [self uu (sgl u) | u <- qqll vv, u /= w]) | w <- qqll vv, s <- uvalsll uu w, t <- uvalsll uu w, s /= t]
  | otherwise = Nothing
  where
    uvalsll uu v = Set.toList $ fromJust $ systemsVarsSetValue uu v
    cartself uu v = Set.map sgl $ fromJust $ systemsVarsCartesian uu (sgl v)
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv 
    qqpp qq = fromJust $ setComponentsPartition qq 
    single = stateSingleton 
    uvars = systemsVars
    sgl = Set.singleton
    subset = Set.isSubsetOf
    del qq x = Set.delete x qq
    ins qq x = Set.insert x qq    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList
    
systemsSetVarsSetSetPartitionDecrementedSubstrateSelfCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionDecrementedSubstrateSelfCardinality uu vv 
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ sum [(vol uu (single w)) * (vol uu (single w) - 1) | w <- qqll vv] `div` 2
  | otherwise = Nothing
  where
    dim = toInteger . Set.size        
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars    
    single = Set.singleton
    qqll = Set.toList    
    subset = Set.isSubsetOf    

systemsSetVarsSetTransformDecrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformDecrementedSubstrateSelf uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionDecrementedSubstrateSelf uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
  
valenciesDimensionsSetSetPartitionDecrementedSubstrateSelfCardinalityRegular :: Integer -> Integer -> Maybe Integer
valenciesDimensionsSetSetPartitionDecrementedSubstrateSelfCardinalityRegular d n 
  | n >= 0 && d > 0 = Just $ (n * d * (d - 1)) `div` 2
  | otherwise = Nothing
    
systemsSetVarsSetSetPartitionIncrementedSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionIncrementedSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
    llqq [single (qqpp (llqq2 [(single ss), (xx `del` ss)])) | jj <- powerll vv, let xx = cart uu jj, card xx > 1, ss <- qqll xx]
  | otherwise = Nothing
  where
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    qqpp qq = fromJust $ setComponentsPartition qq
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    del qq x = Set.delete x qq
    subset = Set.isSubsetOf
    card = Set.size
    single = Set.singleton
    empty = Set.empty  
    llqq = Set.fromList
    llqq2 = Set.fromList
    qqll = Set.toList

systemsSetVarsSetSetPartitionIncrementedSubstrate_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionIncrementedSubstrate_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, isinc nn]
  | otherwise = Nothing
  where
    isinc nn = card nn == 1 && and [card (ppqq qq) == 2 && or [card cc == 1 | cc <- qqll (ppqq qq)] | qq <- qqll nn]
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    ppqq = partitionsSetComponent
    uvars = systemsVars
    subset = Set.isSubsetOf
    card = Set.size
    qqll = Set.toList
    llqq = Set.fromList
            
systemsSetVarsSetSetPartitionIncrementedSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionIncrementedSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ sum [if x > 2 then x else x `div` 2 | jj <- powerll vv, let x = vol uu jj, x > 1]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset    
    empty = Set.empty      
    dim = toInteger . Set.size    
    subset = Set.isSubsetOf           
     
systemsSetVarsSetSetPartitionIncrementedSubstrateCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionIncrementedSubstrateCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ 2 ^ dim vv * vol uu vv
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf           
     
systemsSetVarsSetTransformIncrementedSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformIncrementedSubstrate uu vv
  | vv `subset` uvars uu = nnvvqq uu vv (nnvv uu vv)
  | otherwise = Nothing
  where
    nnvvqq = systemsSetVarsSetSetPartitionsSetTransform
    nnvv uu vv = fromJust $ systemsSetVarsSetSetPartitionIncrementedSubstrate uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      
    
systemsSetVarsSetTransformSubstrate_5 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate_5 uu vv
  | vv `subset` uvars uu = Just $ llqq ([nntt (ttnn (lltt [tt1,tt2])) | 
      tt1 <- ttvvcll uu vv, tt2 <- ttvvosll (uu `uunion` tsys tt1) (der tt1), tt2 /= tempty] ++ [transformEmpty])
  | otherwise = Nothing
  where
    ttvvcll uu vv = Set.toList $ fromJust $ systemsSetVarsSetTransformCartesianSubstrate uu vv
    ttvvosll uu vv = Set.toList $ fromJust $ systemsSetVarsSetTransformSubstrateOverlappingSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    lltt ll = fudsTransform $ fromJust $ setTransformsFud $ Set.fromList ll
    der = transformsDerived
    tsys = histogramsSystemImplied . transformsHistogram
    tempty = transformEmpty
    uunion = pairSystemsUnion
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList      
    
systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelf uu vv
  | vv `subset` uvars uu = Just $ llqq [ww `union` llqq2 [qqpp qq | nn <- yy, qq <- qqll nn] | xx <- powerll (power vv),
      let ww = llqq3 [self uu kk | kk <- qqll2 xx], yy <- qqll3 (prod [power2 (parts (ppcart w)) | w <- qqll4 ww])]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    ppcart pp = Set.fromList [single (VarPartition pp) (ValComponent cc) | cc <- ppqqll pp]
    ppqqll = Set.toList . partitionsSetComponent
    qqpp qq = fromJust $ setComponentsPartition qq
    single = stateSingleton    
    uvars = systemsVars
    power = setsPowerset
    power2 = setsPowerset
    powerll = Set.toList . setsPowerset
    prod = listSetsProduct
    parts = setsPartitionSet
    union = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    qqll3 = Set.toList
    qqll4 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelfCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelfCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ 2 ^ (2 ^ dim vv * (1 + bell (vol uu vv)))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf           
         
systemsSetVarsSetTransformSubstrate_6 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrate_6 uu vv
  | vv `subset` uvars uu = Just $ 
      llqq ([expand uu vv (nntt (ttnn (nntt nn))) | nn <- nnvvcosll uu vv] ++ [transformEmpty])
  | otherwise = Nothing
  where
    nnvvcosll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateDerivedOverlappingSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    expand uu vv tt = fromJust $ systemsSetVarsTransformVarPartitionsExpand uu vv tt    
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList      
        
systemsSetVarsSetTransformCartesianSubstrateNonOverlapDerivedSelf :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformCartesianSubstrateNonOverlapDerivedSelf uu vv
  | vv `subset` uvars uu = Just $ llqq ([nntt (ttnn (lltt [tt1,tt2])) | 
      tt1 <- qqll (ttvvc uu vv `cap` ttvvn uu vv), 
      tt2 <- qqll (ttvvns (uu `uunion` tsys tt1) (der tt1))] ++ [transformEmpty])
  | otherwise = Nothing
  where
    ttvvc uu vv = fromJust $ systemsSetVarsSetTransformCartesianSubstrate uu vv
    ttvvn uu vv = fromJust $ systemsSetVarsSetTransformSubstrateNonOverlap uu vv
    ttvvns uu vv = fromJust $ systemsSetVarsSetTransformSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    lltt ll = fudsTransform $ fromJust $ setTransformsFud $ Set.fromList ll
    der = transformsDerived
    tsys = histogramsSystemImplied . transformsHistogram
    uunion = pairSystemsUnion
    uvars = systemsVars
    subset = Set.isSubsetOf
    cap = Set.intersection
    llqq = Set.fromList      
    qqll = Set.toList

systemsSetVarsSetTransformSubstrateNonOverlap_3 = systemsSetVarsSetTransformCartesianSubstrateNonOverlapDerivedSelf
    
systemsSetVarsSetTransformSubstrateNonOverlapStrong_2 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong_2 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt (mm `union` nn))) | 
      mm <- qqll (nnvvc uu vv `cap` nnvvn uu vv), nn <- qqll (nnvvns (uu `uunion` tsys (nntt mm)) (der (nntt mm)))]
  | otherwise = Nothing
  where
    nnvvc uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    nnvvn uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    nnvvns uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    der = transformsDerived
    tsys = histogramsSystemImplied . transformsHistogram
    uunion = pairSystemsUnion
    uvars = systemsVars
    subset = Set.isSubsetOf
    union = Set.union
    cap = Set.intersection
    llqq = Set.fromList      
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelf uu vv
  | vv `subset` uvars uu = Just $ llqq [mm `union` llqq2 [qqpp qq | qq <- nn] | yy <- partsll vv,
      let mm = llqq3 [self uu kk | kk <- qqll yy], nn <- qqll2 (prod [parts (ppcart w) | w <- qqll3 mm])]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    ppcart pp = Set.fromList [single (VarPartition pp) (ValComponent cc) | cc <- ppqqll pp]
    ppqqll = Set.toList . partitionsSetComponent
    qqpp qq = fromJust $ setComponentsPartition qq
    single = stateSingleton    
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition
    prod = listSetsProduct
    parts = setsPartitionSet
    union = Set.union
    subset = Set.isSubsetOf
    qqll = Set.toList
    qqll2 = Set.toList
    qqll3 = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelfCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelfCardinality uu vv
  | vv `subset` uvars uu = Just $ sum [product [bell (vol uu kk) | kk <- qqll yy] | yy <- partsll vv]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv  
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition
    subset = Set.isSubsetOf
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelfCardinalityUpper = systemsSetVarsSetSetPartitionSubstrateNonOverlapCardinalityUpper
             
systemsSetVarsSetTransformSubstrateNonOverlapStrong_3 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong_3 uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [nntt (ttnn (nntt nn)) | nn <- nnvvncnsll uu vv]
  | otherwise = Nothing
  where
    nnvvncnsll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapDerivedSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList          
   
setPartitionsTreeSetPartitionDecrementedSubstrateSelf :: SetPartition -> Tree SetPartition
setPartitionsTreeSetPartitionDecrementedSubstrateSelf mm = 
    Tree $ llmm $ [(nn, tdec nn) | 
      nn <- [llqq ([qq] ++ [ppself rr | rr <- qqll mm, rr /= pp]) | pp <- qqll mm, qq <- decsll pp]]
  where  
    tdec = setPartitionsTreeSetPartitionDecrementedSubstrateSelf
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map sgl $ ppcart pp
    ppself pp = qqpp $ Set.map sgl $ ppcart pp
    ppcart pp = Set.fromList [single (VarPartition pp) (ValComponent cc) | cc <- ppqqll pp]
    ppqqll = Set.toList . partitionsSetComponent
    qqpp qq = fromJust $ setComponentsPartition qq  
    single = stateSingleton    
    llmm = Map.fromList    
    sgl = Set.singleton
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [mm `union` bigcup (llqq2 ll) | let mm = llqq3 [self uu (sgl v) | v <- qqll vv], ll <- (subpathsll (tdec mm) ++ [[]])]
  | otherwise = Nothing
  where
    tdec = setPartitionsTreeSetPartitionDecrementedSubstrateSelf  
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    subpathsll = Set.toList . treesSubPaths
    bigcup = setSetsUnion
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList    
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf_1 uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [bigcup (llqq2 ll) | let mm = llqq3 [self uu (sgl v) | v <- qqll vv], ll <- subpathsll (zsgl mm (tdec mm))]
  | otherwise = Nothing
  where
    tdec = setPartitionsTreeSetPartitionDecrementedSubstrateSelf  
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    subpathsll = Set.toList . treesSubPaths
    bigcup = setSetsUnion
    empty = Set.empty
    subset = Set.isSubsetOf
    zsgl x zz = Tree $ Map.singleton x zz   
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList       
    
systemsSetVarsSetTransformSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateSelf_1 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt nn)) | nn <- nnvvnsctnsdll uu vv]
  | otherwise = Nothing
  where
    nnvvnsctnsdll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality :: System -> Set.Set Variable -> Maybe (Tree (Integer,[Integer]))
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality uu vv
  | vv == empty = Just $ emptyTree
  | vv `subset` uvars uu = Just $ let ll = [ucard uu v | v <- qqll vv] in Tree $ Map.singleton (1,ll) (tdeccd 1 ll)
  | otherwise = Nothing
  where
    tdeccd :: Integer -> [Integer] -> Tree (Integer,[Integer])
    tdeccd k ll =  Tree $ llmm $ [((m,mm), tdeccd m mm) | i <- [1..length ll], let d = ll !! (i-1), d > 1, 
                                                     let m = k * d * (d-1) `div` 2, let mm = take (i-1) ll ++ [d-1] ++ drop i ll]
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    llmm = Map.fromList        
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfCardinality :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | ll <- subpathsll (tdeccd uu vv), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tdeccd uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality uu vv
    subpathsll = Set.toList . treesSubPaths
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfWidth :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfWidth uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | ll <- pathsll (tdeccd uu vv), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tdeccd uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality uu vv
    pathsll = Set.toList . treesPaths
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty
  
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfDepth :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfDepth uu vv
  | vv `subset` uvars uu = Just $ sum [ucard uu v - 1 | v <- qqll vv]
  | otherwise = Nothing
  where
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfDepth_1 :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfDepth_1 uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ treesDepth (tdeccd uu vv) - 1
  | otherwise = Nothing
  where
    tdeccd uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [mm `union` bigcup (llqq2 ll) | yy <- partsll vv, let mm = llqq3 [self uu kk | kk <- qqll yy], ll <- (subpathsll (tdec mm) ++ [[]])]
  | otherwise = Nothing
  where
    tdec = setPartitionsTreeSetPartitionDecrementedSubstrateSelf  
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    subpathsll = Set.toList . treesSubPaths
    partsll = Set.toList . setsSetPartition    
    bigcup = setSetsUnion
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList    
    
systemsSetVarsSetTransformSubstrateNonOverlapStrong_4 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong_4 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt nn)) | nn <- nnvvnctnsdll uu vv]
  | otherwise = Nothing
  where
    nnvvnctnsdll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
        
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfCardinality :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | yy <- partsll vv, let mm = llqq [self uu kk | kk <- qqll yy],
      ll <- subpathsll (tdeccd (uu `uunion` tsys (nntt mm)) (der (nntt mm))), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tdeccd uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfTreeCardinality uu vv
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    der = transformsDerived
    tsys = histogramsSystemImplied . transformsHistogram
    subpathsll = Set.toList . treesSubPaths
    uunion = pairSystemsUnion    
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition    
    llqq = Set.fromList
    qqll = Set.toList    
    subset = Set.isSubsetOf
    empty = Set.empty   
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfCardinalityUpper uu vv
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * (vol uu vv) ^ (2 * vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf           
    
setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf :: SetPartition -> Tree SetPartition
setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf mm = 
    Tree $ llmm $ [(nn, tpdec nn) | pp <- qqll mm, qq <- decsll pp, let nn = mm `del` pp `ins` qq]
  where  
    tpdec = setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ ppqq pp
    ppqq = partitionsSetComponent
    qqpp qq = fromJust $ setComponentsPartition qq  
    del xx x = Set.delete x xx
    ins xx x = Set.insert x xx
    llmm = Map.fromList    
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [nn | let mm = llqq2 [self uu (sgl v) | v <- qqll vv], nn <- (elementsll (tpdec mm) ++ [mm])]
  | otherwise = Nothing
  where
    tpdec = setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf  
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    elementsll = Set.toList . treesElements
    empty = Set.empty
    subset = Set.isSubsetOf
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf_1 uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [nn | nn <- (elementsll (tpdec (full uu vv)) ++ [full uu vv])]
  | otherwise = Nothing
  where
    tpdec = setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf  
    full uu vv = fromJust $ systemsSetVarsSetPartitionFull uu vv
    uvars = systemsVars
    elementsll = Set.toList . treesElements
    empty = Set.empty
    subset = Set.isSubsetOf
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateSelf_2 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateSelf_2 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt nn)) | nn <- nnvvnsctpnsdll uu vv]
  | otherwise = Nothing
  where
    nnvvnsctpnsdll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreePartitionDecrementedSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreePartitionDecrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreePartitionDecrementedSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [nn | yy <- partsll vv, let mm = llqq2 [self uu kk | kk <- qqll yy], nn <- (elementsll (tpdec mm) ++ [mm])]
  | otherwise = Nothing
  where
    tpdec = setPartitionsTreePartitionSetPartitionDecrementedSubstrateSelf  
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    elementsll = Set.toList . treesElements
    partsll = Set.toList . setsSetPartition    
    empty = Set.empty
    subset = Set.isSubsetOf
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetTransformSubstrateNonOverlapStrong_5 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong_5 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt nn)) | nn <- nnvvnctpnsdll uu vv]
  | otherwise = Nothing
  where
    nnvvnctpnsdll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrateNonOverlapTreePartitionDecrementedSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList            
    
setPartitionsTreeSetPartitionPointedIncrementedSubstrateSelf :: SetPartitionPointed -> Tree SetPartitionPointed
setPartitionsTreeSetPartitionPointedIncrementedSubstrateSelf mm = 
    Tree $ llmm $ [(nn, tinc nn) | pp <- qqll mm, qq <- incsll pp, let nn = mm `del` pp `ins` qq]
  where  
    tinc = setPartitionsTreeSetPartitionPointedIncrementedSubstrateSelf
    incsll pp = Set.toList $ Set.map qqpp $ partitionPointedSetsIncrements $ ppqq pp
    ppqq pp = (partitionsSetComponent (partitionPointedsPartition pp), partitionPointedsPoint pp)
    qqpp (pp,cc) = fromJust $ partitionsComponentsPartitionPointed (fromJust (setComponentsPartition pp)) cc
    del xx x = Set.delete x xx
    ins xx x = Set.insert x xx
    llmm = Map.fromList    
    qqll = Set.toList
        
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartitionPointed)
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [nn | nn <- (elementsll (tinc (one uu vv)) ++ [one uu vv])]
  | otherwise = Nothing
  where
    tinc = setPartitionsTreeSetPartitionPointedIncrementedSubstrateSelf  
    one uu vv = fromJust $ systemsSetVarsSetPartitionPointedOne uu vv
    uvars = systemsVars
    elementsll = Set.toList . treesElements
    empty = Set.empty
    subset = Set.isSubsetOf
    llqq = Set.fromList
    
systemsSetVarsSetTransformSubstrateSelf_3 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateSelf_3 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt (Set.map ppppp nn))) | nn <- nnvvnsctnsill uu vv]
  | otherwise = Nothing
  where
    nnvvnsctnsill uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    ppppp = partitionPointedsPartition 
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList
    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality :: 
  System -> Set.Set Variable -> Maybe (Tree (Integer,[(Integer,Integer)]))
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality uu vv
  | vv == empty = Just $ emptyTree
  | vv `subset` uvars uu = Just $ let ll = [(ucard uu v,0) | v <- qqll vv] in zsgl (1,ll) (tinccd 1 ll)
  | otherwise = Nothing
  where
    tinccd :: Integer -> [(Integer,Integer)] -> Tree (Integer,[(Integer,Integer)])
    tinccd k ll =  llzz $ [((m,mm), tinccd m mm) | i <- [1..length ll], let (d,c) = ll !! (i-1), d > 1, 
                                                     let m = k * d, let mm = take (i-1) ll ++ [(d-1,c+1)] ++ drop i ll] ++
                          [((m,mm), tinccd m mm) | i <- [1..length ll], let (d,c) = ll !! (i-1), d > 1, 
                                                     let m = k * d * c, let mm = take (i-1) ll ++ [(d-1,c)] ++ drop i ll]
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    llzz ll = Tree $ Map.fromList ll       
    zsgl x zz = Tree $ Map.singleton x zz   
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfCardinality :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | ll <- subpathsll (tinccd uu vv), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tinccd uu vv = fromJust $ systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality uu vv
    subpathsll = Set.toList . treesSubPaths
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty
    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfWidth :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfWidth uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | ll <- pathsll (tinccd uu vv), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tinccd uu vv = fromJust $ systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality uu vv
    pathsll = Set.toList . treesPaths
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty
  
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfDepth :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfDepth uu vv
  | vv `subset` uvars uu = Just $ sum [ucard uu v - 1 | v <- qqll vv]
  | otherwise = Nothing
  where
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf
    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfDepth_1 :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfDepth_1 uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ treesDepth (tinccd uu vv) - 1
  | otherwise = Nothing
  where
    tinccd uu vv = fromJust $ systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf
    empty = Set.empty       

systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelf :: System -> Set.Set Variable -> Maybe (Set.Set SetPartitionPointed)
systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelf uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [nn | yy <- partsll vv, let mm = llqq2 [unaryp uu kk | kk <- qqll yy], nn <- (elementsll (tinc mm) ++ [mm])]
  | otherwise = Nothing
  where
    tinc = setPartitionsTreeSetPartitionPointedIncrementedSubstrateSelf  
    unaryp uu vv = fromJust $ systemsSetVarsPartitionPointedUnary uu vv  
    uvars = systemsVars
    elementsll = Set.toList . treesElements
    partsll = Set.toList . setsSetPartition    
    empty = Set.empty
    subset = Set.isSubsetOf
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    
systemsSetVarsSetTransformSubstrateNonOverlapStrong_6 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateNonOverlapStrong_6 uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (ttnn (nntt (Set.map ppppp nn))) | nn <- nnvvn1tnsill uu vv]
  | otherwise = Nothing
  where
    nnvvn1tnsill uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelf uu vv
    ttnn = transformsSetPartition
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    ppppp = partitionPointedsPartition     
    uvars = systemsVars
    subset = Set.isSubsetOf
    llqq = Set.fromList            
       
systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelfCardinality :: System -> Set.Set Variable -> Maybe Integer    
systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelfCardinality uu vv
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | yy <- partsll vv, let mm = llqq [self uu kk | kk <- qqll yy],
      ll <- subpathsll (tinccd (uu `uunion` tsys (nntt mm)) (der (nntt mm))), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tinccd uu vv = fromJust $ systemsSetVarsSetSetPartitionPointedOneSubstrateSelfTreeIncrementedSubstrateSelfTreeCardinality uu vv
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    der = transformsDerived
    tsys = histogramsSystemImplied . transformsHistogram
    subpathsll = Set.toList . treesSubPaths
    uunion = pairSystemsUnion    
    uvars = systemsVars
    partsll = Set.toList . setsSetPartition    
    llqq = Set.fromList
    qqll = Set.toList    
    subset = Set.isSubsetOf
    empty = Set.empty   
    
systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelfCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionPointedOneSubstrateNonOverlapTreeIncrementedSubstrateSelfCardinalityUpper uu vv
  | dim vv == 0 = Just $ 0
  | vv `subset` uvars uu = Just $ bell (dim vv) * (2 * vol uu vv) ^ (2 * vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf           
    
systemsSetVarsSetRollSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Roll)
systemsSetVarsSetRollSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
      Set.map qqrr $ bigcup $ Set.map (power . llqq) $ prod [llqq [(ss,tt) | tt <- cartll uu vv] | ss <- cartll uu vv]
  | otherwise = Nothing
  where
    qqrr qq = fromJust $ listsRoll $ Set.toList qq
    cartll uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct  
    bigcup = setSetsUnion
    power = setsPowerset
    subset = Set.isSubsetOf
    llqq = Set.fromList

systemsSetVarsSetRollSubstrateCardinalityUpper :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetRollSubstrateCardinalityUpper uu vv
  | vv `subset` uvars uu = Just $ (vol uu vv) ^ (vol uu vv) * 2 ^ (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf          
   
systemsSetVarsSetRollCompleteSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set Roll)
systemsSetVarsSetRollCompleteSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
      Set.map (qqrr . llqq) $ prod [llqq [(ss,tt) | tt <- cartll uu vv] | ss <- cartll uu vv]
  | otherwise = Nothing
  where
    qqrr qq = fromJust $ listsRoll $ Set.toList qq
    cartll uu vv = Set.toList $ fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    subset = Set.isSubsetOf
    llqq = Set.fromList

systemsSetVarsSetRollCompleteSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetRollCompleteSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ (vol uu vv) ^ (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf          
   
systemsSetVarsSetTransformSubstrateRollVariable :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateRollVariable uu vv
  | vv `subset` uvars uu = Just $ llqq [nntt (rrppv uu vv rr) | rr <- qqll (rrvv uu vv)] 
  | otherwise = Nothing
  where
    rrvv uu vv = fromJust $ systemsSetVarsSetRollSubstrate uu vv
    rrppv uu vv rr = fromJust $ systemsSetVariablesRollsSetPartitionVariable uu vv rr
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList            
    llqq = Set.fromList            
       
systemsSetVarsSetTransformSubstrateRollVariableCardinalityUpper = systemsSetVarsSetRollCompleteSubstrateCardinality
           
systemsSetVarsSetRollValueSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set RollValue)
systemsSetVarsSetRollValueSubstrate uu vv
  | vv `subset` uvars uu = Just $ 
      llqq [vvvstvr (vv,v,s,t) | v <- qqll vv, s <- uvalsll uu v, t <- uvalsll uu v]
  | otherwise = Nothing
  where
    vvvstvr xx = fromJust $ setVariablesVariablesValuesValuesRollValue xx
    uvalsll uu v = Set.toList $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    subset = Set.isSubsetOf
    qqll = Set.toList            
    llqq = Set.fromList

systemsSetVarsSetRollValueSubstrateCardinality :: System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetRollValueSubstrateCardinality uu vv
  | vv `subset` uvars uu = Just $ sum [(ucard uu v) ^ 2 | v <- qqll vv]
  | otherwise = Nothing
  where
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    qqll = Set.toList            
    subset = Set.isSubsetOf          
   
systemsSetVarsIntegersSetListFixedRollValueSubstrate :: System -> Set.Set Variable -> Integer -> Maybe (Set.Set [RollValue])
systemsSetVarsIntegersSetListFixedRollValueSubstrate uu vv l
  | l < 0 = Nothing
  | vv `subset` uvars uu = Just $ (prod . take (fromInteger l) . repeat) (rvvv uu vv)
  | otherwise = Nothing
  where
    rvvv uu vv = fromJust $ systemsSetVarsSetRollValueSubstrate uu vv
    prod = listSetsProduct
    uvars = systemsVars
    subset = Set.isSubsetOf

systemsSetVarsIntegersSetListFixedRollValueSubstrateCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsIntegersSetListFixedRollValueSubstrateCardinality uu vv l
  | vv `subset` uvars uu = Just $ (rvvvcd uu vv) ^ l
  | otherwise = Nothing
  where
    rvvvcd uu vv = fromJust $ systemsSetVarsSetRollValueSubstrateCardinality uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf          

systemsSetVarsSetTransformSubstrateSelf_4 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformSubstrateSelf_4 uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ llqq [jjtt uu vv jj | jj <- jjvvll uu vv (sum [ucard uu v - 1 | v <- qqll vv])]
  | otherwise = Nothing
  where  
    jjvvll uu vv l = Set.toList $ fromJust $ systemsSetVarsIntegersSetListFixedRollValueSubstrate uu vv l
    jjtt uu vv jj = fromJust $ systemsSetVariablesListRollValuesTransform uu vv jj
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    empty = Set.empty
    subset = Set.isSubsetOf
    llqq = Set.fromList
    qqll = Set.toList    

systemsSetVarsSetTransformDecrementedSubstrateSelf_1 :: System -> Set.Set Variable -> Maybe (Set.Set Transform)
systemsSetVarsSetTransformDecrementedSubstrateSelf_1 uu vv
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = 
      Just $ llqq [jjtt uu vv jj | jj <- jjvvll uu vv 1, and [s /= t | rv <- jj, let (s,t) = rvst rv]]
  | otherwise = Nothing
  where
    jjvvll uu vv l = Set.toList $ fromJust $ systemsSetVarsIntegersSetListFixedRollValueSubstrate uu vv l
    jjtt uu vv jj = fromJust $ systemsSetVariablesListRollValuesTransform uu vv jj
    rvst rv = let (_,_,s,t) = rollValuesSetVariableVariableValueValue rv in (s,t)
    uvars = systemsVars
    empty = Set.empty
    subset = Set.isSubsetOf
    llqq = Set.fromList
    qqll = Set.toList    
    
systemsSetVarsTreeRollValueDecrementedSubstrate :: System -> Set.Set Variable -> Maybe (Tree RollValue)
systemsSetVarsTreeRollValueDecrementedSubstrate uu vv
  | vv `subset` uvars uu = Just $ tdecrv uu vv []
  | otherwise = Nothing
  where
    tdecrv :: System -> Set.Set Variable -> [RollValue] -> Tree RollValue
    tdecrv uu vv jj = Tree $ llmm $ [(xxrv (vv,v,s,t), tdecrv uu vv (jj ++ [xxrv (vv,v,s,t)])) | 
                        v <- qqll vv, s <- uvalsll uu v, t <- uvalsll uu v, s > t,
                        let xx = llqq [x | (w,x) <- map rvvs jj, w==v], s `notmem` xx, t `notmem` xx]
    uvalsll uu v = Set.toList $ fromJust $ systemsVarsSetValue uu v
    rvvs rv = let (_,v,s,_) = rollValuesSetVariableVariableValueValue rv in (v,s)
    xxrv xx = fromJust $ setVariablesVariablesValuesValuesRollValue xx
    llmm = Map.fromList    
    uvars = systemsVars
    notmem = Set.notMember
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList

systemsSetVarsIntegersSetListRollValueDecrementedSubstrate :: System -> Set.Set Variable -> Maybe (Set.Set [RollValue])
systemsSetVarsIntegersSetListRollValueDecrementedSubstrate uu vv
  | vv `subset` uvars uu = Just $ treesSubPaths (tdecrv uu vv)
  | otherwise = Nothing
  where
    tdecrv uu vv = fromJust $ systemsSetVarsTreeRollValueDecrementedSubstrate uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimension :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimension uu vv kmax
  | kmax < 0 = Nothing
  | vv `subset` uvars uu = 
    Just $ llpower [qqpp pp | kk <- powerll vv, dim kk <= kmax, pp <- partsll (cart uu kk)]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinality uu vv kmax
  | vv `subset` uvars uu = Just $ 2 ^ (sum [bell (vol uu kk) | kk <- powerll vv, dim kk <= kmax])
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegularLower :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegularLower d n kmax
  | n >= 0 && d > 0 && kmax >= 0 = Just $ 2 ^ (bell (d^(min kmax n)))
  | otherwise = Nothing
    
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegular :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegular d n kmax
  | n >= 0 && d > 0 && kmax >= 0 = Just $ 2 ^ (sum [binom n k * bell (d^k) | k <- [0..(min kmax n)]])
  | otherwise = Nothing
  where
    binom = combination
    
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegularUpper :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionCardinalityRegularUpper d n kmax
  | n >= 0 && d > 0 && kmax >= 0 = Just $ 2 ^ (2^n * bell (d^(min kmax n)))
  | otherwise = Nothing
    
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolume :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolume uu vv xmax
  | xmax < 1 = Nothing
  | vv `subset` uvars uu = 
    Just $ llpower [qqpp pp | kk <- powerll vv, vol uu kk <= xmax, pp <- partsll (cart uu kk)]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolumeCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolumeCardinality uu vv xmax
  | vv `subset` uvars uu = Just $ 2 ^ (sum [bell (vol uu kk) | kk <- powerll vv, vol uu kk <= xmax])
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolumeCardinalityUpper :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingVolumeCardinalityUpper uu vv xmax
  | xmax < 1 = Nothing
  | vv `subset` uvars uu = Just $ 2 ^ (2 ^ (dim vv) * bell (min xmax (vol uu vv)))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf      

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedValency :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedValency uu vv umax
  | umax < 1 = Nothing
  | vv `subset` uvars uu = 
    Just $ llpower [qqpp pp | kk <- powerll vv, pp <- partsll (cart uu kk), cd pp <= umax]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    cd = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedValencyCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedValencyCardinality uu vv umax
  | vv `subset` uvars uu = Just $ 
      2 ^ sum [stirlingSecond (vol uu kk) u | kk <- powerll vv, u <- [1..umax], u <= vol uu kk]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedValencyCardinalityRegular :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedValencyCardinalityRegular d n umax
  | n >= 0 && d > 0 && umax > 0 = Just $ 
      2 ^ sum [binom n k * stirlingSecond (d^k) u | k <- [0..n], u <- [1..umax], u <= d^k]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedComponent :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedComponent uu vv cmin
  | cmin < 1 = Nothing
  | vv `subset` uvars uu = 
    Just $ llpower [qqpp pp | kk <- powerll vv, pp <- partsll (cart uu kk), and [cd cc >= cmin | cc <- qqll pp]]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    cd = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedComponentCardinalityRegular :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedComponentCardinalityRegular d n cmin
  | n >= 0 && d > 0 && cmin > 0 = Just $ 
      2 ^ sum [binom n k * b | k <- [0..n], (ll,b) <- bcdll (d^k), and [m == 0 | m <- take (fromInteger (cmin-1)) ll]]
  | otherwise = Nothing
  where
    binom = combination
    bcdll = Map.toList . bellcd

systemsSetVarsLimitsSetSetPartitionSubstrateIntersecting :: 
  System -> Set.Set Variable -> Set.Set Variable -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateIntersecting uu vv xx
  | vv `cap` xx == empty = Nothing
  | vv `subset` uvars uu = 
    Just $ llpower [qqpp pp | kk <- powerll vv, kk `cap` xx /= empty, pp <- partsll (cart uu kk)]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llpower = setsPowerset . Set.fromList
    powerll = qqll . setsPowerset
    cap = Set.intersection
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinalityLower :: 
  System -> Set.Set Variable -> Set.Set Variable -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinalityLower uu vv xx
  | vv `subset` uvars uu = Just $ 2 ^ bell (vol uu vv)
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    subset = Set.isSubsetOf      

systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinality :: 
  System -> Set.Set Variable -> Set.Set Variable -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinality uu vv xx
  | vv `cap` xx == empty = Nothing
  | vv `subset` uvars uu = Just $ 2 ^ (sum [bell (vol uu kk) | kk <- powerll vv, kk `cap` xx /= empty])
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    cap = Set.intersection
    empty = Set.empty
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinalityUpper :: 
  System -> Set.Set Variable -> Set.Set Variable -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateIntersectingCardinalityUpper uu vv xx
  | vv `cap` xx == empty = Nothing
  | vv `subset` uvars uu = Just $ 2 ^ (dim (vv `cap` xx) * 2 ^ (dim vv - 1) * bell (vol uu vv))
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    cap = Set.intersection
    empty = Set.empty
    dim = toInteger . Set.size
    subset = Set.isSubsetOf      

valenciesDimensionsLimitsSetSetPartitionSubstrateIntersectingCardinalityRegular :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateIntersectingCardinalityRegular d n x
  | n > 0 && d > 0 && x >= 0  && x <= n = Just $ 
    2 ^ (sum [(binom n k - binom (n-x) k) * bell (d^k) | k <- [1..n-x]] + 
         sum [binom n k * bell (d^k) | k <- [n-x+1..n]])
  | otherwise = Nothing
  where
    binom = combination
    
valenciesDimensionsLimitsSetSetPartitionSubstrateIntersectingCardinalityRegular_1 :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateIntersectingCardinalityRegular_1 d n x
  | n > 0 && d > 0 && x >= 0  && x <= n = Just $ 
    2 ^ sum [(binom n k - binom (n-x) k) * bell (d^k) | k <- [1..n]]
  | otherwise = Nothing
  where
    binom = combination
    
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = 
    Just $ llcombs [qqpp pp | kk <- powerll vv, pp <- partsll (cart uu kk)] bmax
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombs ll bmax = setsPowersetLimited (Set.fromList ll) bmax
    powerll = qqll . setsPowerset
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth_1 :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth_1 uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, dim nn <= bmax]
  | otherwise = Nothing
  where
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    llqq = Set.fromList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthCardinality uu vv bmax
  | vv `subset` uvars uu = Just $ 
      let c = sum [bell (vol uu kk) | kk <- powerll vv] in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    binom = combination
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthCardinalityUpper :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthCardinalityUpper uu vv bmax
  | bmax < 1 = Nothing
  | vv `subset` uvars uu = Just $ 
      let c = 2 ^ (dim vv) * bell (vol uu vv) in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    binom = combination
    dim = toInteger . Set.size
    subset = Set.isSubsetOf      
    
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedBreadthCardinalityRegular :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedBreadthCardinalityRegular d n bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      let c = sum [binom n k * bell (d^k) | k <- [0..n]] in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth uu vv kmax bmax
  | bmax < 0 = Nothing
  | kmax < 0 = Nothing
  | vv `subset` uvars uu = 
    Just $ llcombs  bmax [qqpp pp | kk <- combsll kmax vv, pp <- partsll (cart uu kk)]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombs bmax ll = setsPowersetLimited (Set.fromList ll) bmax
    combsll kmax qq = Set.toList $ setsPowersetLimited qq kmax
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth_1 :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth_1 uu vv kmax bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvkmaxll uu vv kmax, dim nn <= bmax]
  | otherwise = Nothing
  where
    nnvvkmaxll uu vv kmax = Set.toList $ fromJust $ systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimension uu vv kmax
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    llqq = Set.fromList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth_2 :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadth_2 uu vv kmax bmax
  | bmax < 0 = Nothing
  | kmax < 0 = Nothing
  | vv `subset` uvars uu = 
    Just $ llcombs [qqpp pp | kk <- powerll vv, dim kk <= kmax, pp <- partsll (cart uu kk)] bmax
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombs ll bmax = setsPowersetLimited (Set.fromList ll) bmax
    powerll = qqll . setsPowerset
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    


systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthCardinality :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthCardinality uu vv kmax bmax
  | vv `subset` uvars uu = Just $ 
      let c = sum [bell (vol uu kk) | kk <- powerll vv, dim kk <= kmax] in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    binom = combination
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthCardinalityRegular :: 
  Integer -> Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthCardinalityRegular d n kmax bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      let c = sum [binom n k * bell (d^k) | k <- [0..(min n kmax)]] in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValent :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValent uu vv kmax bmax
  | bmax < 0 = Nothing
  | kmax < 0 = Nothing
  | vv `subset` uvars uu = 
    Just $ llcombs  bmax [qqpp pp | kk <- combsll kmax vv, pp <- partsll (cart uu kk), dim pp >= 2]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombs bmax ll = setsPowersetLimited (Set.fromList ll) bmax
    combsll kmax qq = Set.toList $ setsPowersetLimited qq kmax
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValentCardinality :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValentCardinality uu vv kmax bmax
  | vv `subset` uvars uu = Just $ 
      let c = sum [bell (vol uu kk) - 1 | kk <- powerll vv, dim kk <= kmax] in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    binom = combination
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValentCardinalityRegular :: 
  Integer -> Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthPluriValentCardinalityRegular d n kmax bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      let c = sum [binom n k * (bell (d^k) - 1) | k <- [0..(min n kmax)]] in sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadth :: 
  System -> Set.Set Variable -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadth uu vv xx kmax bmax
  | vv `cap` xx == empty = Nothing
  | vv `subset` uvars uu = 
    Just $ llcombs [qqpp pp | kk <- powerll vv, kk `cap` xx /= empty, dim kk <= kmax, pp <- partsll (cart uu kk)] bmax
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombs ll bmax = setsPowersetLimited (Set.fromList ll) bmax
    powerll = qqll . setsPowerset
    cap = Set.intersection
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList    
    dim = toInteger . Set.size

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthCardinality :: 
  System -> Set.Set Variable -> Set.Set Variable -> Integer -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthCardinality uu vv xx kmax bmax
  | vv `subset` uvars uu = Just $ 
      let c = sum [bell (vol uu kk) | kk <- powerll vv, kk `cap` xx /= empty, dim kk <= kmax] in 
      sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    binom = combination
    cap = Set.intersection
    empty = Set.empty
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthCardinalityRegular :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthCardinalityRegular d n x kmax bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      let c = sum [(binom n k - binom (n-x) k) * bell (d^k) | k <- [0..(min n kmax)]] in 
      sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValent :: 
  System -> Set.Set Variable -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValent uu vv xx kmax bmax
  | vv `cap` xx == empty = Nothing
  | vv `subset` uvars uu = 
    Just $ llcombs bmax [qqpp pp | kk <- powerll vv, kk `cap` xx /= empty, dim kk <= kmax, 
                              pp <- partsll (cart uu kk), dim pp >= 2]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombs bmax ll = setsPowersetLimited (Set.fromList ll) bmax
    powerll = qqll . setsPowerset
    cap = Set.intersection
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList    
    dim = toInteger . Set.size

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValentCardinality :: 
  System -> Set.Set Variable -> Set.Set Variable -> Integer -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValentCardinality uu vv xx kmax bmax
  | vv `subset` uvars uu = Just $ 
      let c = sum [bell (vol uu kk) - 1 | kk <- powerll vv, kk `cap` xx /= empty, dim kk <= kmax] in 
      sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    binom = combination
    cap = Set.intersection
    empty = Set.empty
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValentCardReg :: 
  Integer -> Integer -> Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedIntersectingUnderlyingDimensionBreadthPluriValentCardReg d n x kmax bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      let c = sum [(binom n k - binom (n-x) k) * (bell (d^k) - 1) | k <- [0..(min n kmax)]] in 
      sum [binom c b | b <- [0..(min c bmax)]]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthRange :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthRange uu vv bmin bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = 
    Just $ llqq [nn | nn <- llcombsll [qqpp pp | kk <- powerll vv, pp <- partsll (cart uu kk)] bmax, dim nn >= bmin]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombsll ll bmax = Set.toList $ setsPowersetLimited (Set.fromList ll) bmax
    powerll = qqll . setsPowerset
    subset = Set.isSubsetOf
    dim = toInteger . Set.size
    qqll = Set.toList    
    llqq = Set.fromList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthRangeCardinality :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadthRangeCardinality uu vv bmin bmax
  | vv `subset` uvars uu = Just $ 
      let c = sum [bell (vol uu kk) | kk <- powerll vv] in sum [binom c b | b <- [(max 0 bmin)..(min c bmax)]]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    powerll = Set.toList . setsPowerset
    binom = combination
    dim = toInteger . Set.size
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedBreadthRangeCardinalityRegular :: 
  Integer -> Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateLimitedBreadthRangeCardinalityRegular d n bmin bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      let c = sum [binom n k * bell (d^k) | k <- [0..n]] in sum [binom c b | b <- [(max 0 bmin)..(min c bmax)]]
  | otherwise = Nothing
  where
    binom = combination

systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp qq | qq <- nn] | yy <- stirsll vv bmax, nn <- qqll (prod [parts (cart uu kk) | kk <- qqll yy])]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    parts = setsPartitionSet
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
      
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth_1 :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth_1 uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvnll uu vv, dim nn <= bmax]
  | otherwise = Nothing
  where
    nnvvnll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    uvars = systemsVars
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    llqq = Set.fromList    

systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth_2 :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth_2 uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ nnvvn uu vv `cap` nnvvbmax uu vv bmax
  | otherwise = Nothing
  where
    nnvvn uu vv = fromJust $ systemsSetVarsSetSetPartitionSubstrateNonOverlap uu vv
    nnvvbmax uu vv bmax = fromJust $ systemsSetVarsLimitsSetSetPartitionSubstrateLimitedBreadth uu vv bmax
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf

systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadthCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadthCardinality uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ sum [product [bell (vol uu kk) | kk <- qqll yy] | yy <- stirsll vv bmax]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    qqll = Set.toList
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadthCardinalityRegular :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadthCardinalityRegular d n bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      sum [c * product [(bell (d^k))^m | (k,m) <- zip [1..] ll] | (ll,c) <- sscdll n (min n bmax)]
  | otherwise = Nothing
  where
    sscdll n k = Map.toList $ Map.filterWithKey (\kk _ -> sum kk <= k) (bellcd n)

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadth :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadth uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ llqq [llqq2 [self uu kk | kk <- qqll yy] | yy <- stirsll vv bmax]
  | otherwise = Nothing
  where
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    empty = Set.empty  
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadth_1 :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadth_1 uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ nnvvc uu vv `cap` nnvvnbmax uu vv bmax
  | otherwise = Nothing
  where
    nnvvc uu vv = fromJust $ systemsSetVarsSetSetPartitionCartesianSubstrate uu vv
    nnvvnbmax uu vv bmax = fromJust $ systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapLimitedBreadth uu vv bmax
    uvars = systemsVars
    cap = Set.intersection
    subset = Set.isSubsetOf

setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadthCardinality :: 
  Set.Set Variable -> Integer -> Integer
setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapLimitedBreadthCardinality vv bmax =
  sum [stirlingSecond (dim vv) b | b <- [1 .. (min (dim vv) bmax)]]
  where
    dim = toInteger . Set.size    
    uvars = systemsVars

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedDerivedVolume :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedDerivedVolume uu vv wmax
  | wmax <= 0 = Nothing
  | vv `subset` uvars uu = Just $ llqq [nn | nn <- nnvvll uu vv, vol nn <= wmax]
  | otherwise = Nothing
  where
    nnvvll uu vv = Set.toList $ fromJust $ systemsSetVarsSetSetPartitionSubstrate uu vv
    uvars = systemsVars
    ppcd = toInteger . Set.size . partitionsSetComponent
    vol nn = product [ppcd pp | pp <- Set.toList nn]
    subset = Set.isSubsetOf
    llqq = Set.fromList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthDerivedVolume :: 
  System -> Set.Set Variable -> Integer -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthDerivedVolume uu vv kmax bmax wmax
  | wmax <= 0 = Nothing
  | bmax < 0 = Nothing
  | kmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
      llqq [nn | nn <- llcombsll bmax [qqpp pp | kk <- combsll kmax vv, pp <- partsll (cart uu kk)], vol nn <= wmax]
  | otherwise = Nothing
  where
    ppcd = toInteger . Set.size . partitionsSetComponent
    vol nn = product [ppcd pp | pp <- Set.toList nn]
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombsll bmax ll = Set.toList $ setsPowersetLimited (Set.fromList ll) bmax
    combsll kmax qq = Set.toList $ setsPowersetLimited qq kmax
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    
    llqq = Set.fromList    

systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthDerivedVolumePluriValent :: 
  System -> Set.Set Variable -> Integer -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateLimitedUnderlyingDimensionBreadthDerivedVolumePluriValent uu vv kmax bmax wmax
  | wmax <= 0 = Nothing
  | bmax < 0 = Nothing
  | kmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
      llqq [nn | nn <- llcombsll bmax [qqpp pp | kk <- combsll kmax vv, pp <- partsll (cart uu kk), dim pp >= 2], 
                 vol nn <= wmax]
  | otherwise = Nothing
  where
    ppcd = toInteger . Set.size . partitionsSetComponent
    vol nn = product [ppcd pp | pp <- Set.toList nn]
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    partsll = qqll . setsPartitionSet
    llcombsll bmax ll = Set.toList $ setsPowersetLimited (Set.fromList ll) bmax
    combsll kmax qq = Set.toList $ setsPowersetLimited qq kmax
    dim = toInteger . Set.size
    subset = Set.isSubsetOf
    qqll = Set.toList    
    llqq = Set.fromList    

systemsSetVarsLimitsSetFudPartitionLimitedPathModels :: 
  System -> Set.Set Variable -> Integer -> Integer -> Integer -> Integer -> Maybe (Set.Set Fud)
systemsSetVarsLimitsSetFudPartitionLimitedPathModels uu vv xmax bmax wmax lmax
  | lmax <= 0 = Nothing
  | wmax <= 0 = Nothing
  | bmax < 0 = Nothing
  | xmax <= 0 = Nothing
  | vv `subset` uvars uu = Just $ treesElements (tfiubhd uu vv fudEmpty 1)
  | otherwise = Nothing
  where
    tfiubhd uu vv ff h = Tree $ llmm $ [(hh, tfiubhd (uu `uunion` fsys hh) vv hh (h+1)) | h <= lmax, 
        gg <- llfudsll bmax [qqtt pp | kk <- tuplesll vv ff, vol uu kk <= xmax, pp <- partsll (cart uu kk)], 
        let hh = ff `funion` gg, let ww = fder hh, vol (uu `uunion` fsys hh) ww <= wmax]
    tuplesll vv ff = Set.toList $ setVariablesFudsSetTuple vv ff
    llfudsll bmax ll = Set.toList $ Set.map qqff $ setsPowersetLimited (Set.fromList ll) bmax
    qqff = fromJust . setTransformsFud
    ffqq = fudsSetTransform
    fder = fudsDerived
    funion ff gg = qqff (ffqq ff `Set.union` ffqq gg)
    fvars = fudsVars
    fsys = fudsSystemImplied
    vars = histogramsVars
    qqtt = pptt . qqpp
    pptt = partitionsTransformVarPartition 
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    uunion = pairSystemsUnion
    partsll = Set.toList . setsPartitionSet
    llmm = Map.fromList
    subset = Set.isSubsetOf
   
setPartitionsTreeSetPartitionDecrementedSubstrateSelfPluriValent :: SetPartition -> Tree SetPartition
setPartitionsTreeSetPartitionDecrementedSubstrateSelfPluriValent mm = 
    Tree $ llmm $ [(nn, tdecp nn) | 
      nn <- [llqq ([qq] ++ [ppself rr | rr <- qqll mm, rr /= pp]) | pp <- qqll mm, ppcd pp > 2, qq <- decsll pp]]
  where  
    tdecp = setPartitionsTreeSetPartitionDecrementedSubstrateSelfPluriValent
    decsll pp = Set.toList $ Set.map qqpp $ partitionSetsDecrements $ Set.map sgl $ ppcart pp
    ppcd = toInteger . Set.size . partitionsSetComponent
    ppself pp = qqpp $ Set.map sgl $ ppcart pp
    ppcart pp = Set.fromList [single (VarPartition pp) (ValComponent cc) | cc <- ppqqll pp]
    ppqqll = Set.toList . partitionsSetComponent
    qqpp qq = fromJust $ setComponentsPartition qq  
    single = stateSingleton    
    llmm = Map.fromList    
    sgl = Set.singleton
    qqll = Set.toList
    llqq = Set.fromList
    
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfPluriLimBreadthPluriVal :: 
    System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfPluriLimBreadthPluriVal 
    uu vv bmax
  | bmax < 0 = Nothing
  | vv == empty = Just $ empty
  | vv `subset` uvars uu = Just $ 
      llqq [mm `union` bigcup (llqq2 ll) | yy <- stirsll vv bmax, dim yy >= 2,
             let mm = llqq3 [self uu kk | kk <- qqll yy], ll <- (subpathsll (tdecp mm) ++ [[]])]
  | otherwise = Nothing
  where
    tdecp = setPartitionsTreeSetPartitionDecrementedSubstrateSelfPluriValent
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    uvars = systemsVars
    subpathsll = Set.toList . treesSubPaths
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    bigcup = setSetsUnion
    dim = toInteger . Set.size
    union = Set.union
    empty = Set.empty
    subset = Set.isSubsetOf
    sgl = Set.singleton    
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
    llqq3 = Set.fromList    

systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfPluriValentTreeCardinality :: 
  System -> Set.Set Variable -> Maybe (Tree (Integer,[Integer]))
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfPluriValentTreeCardinality uu vv
  | vv == empty = Just $ emptyTree
  | vv `subset` uvars uu = Just $ let ll = [ucard uu v | v <- qqll vv] in Tree $ Map.singleton (1,ll) (tdecpcd 1 ll)
  | otherwise = Nothing
  where
    tdecpcd :: Integer -> [Integer] -> Tree (Integer,[Integer])
    tdecpcd k ll =  Tree $ llmm $ [((m,mm), tdecpcd m mm) | i <- [1..length ll], let d = ll !! (i-1), d > 2, 
                                    let m = k * d * (d-1) `div` 2, let mm = take (i-1) ll ++ [d-1] ++ drop i ll]
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    llmm = Map.fromList        
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfPluriLimBrPluriValCard ::
  System -> Set.Set Variable -> Integer -> Maybe Integer    
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubstrateSelfPluriLimBrPluriValCard uu vv bmax
  | vv == empty = Just $ 0
  | vv `subset` uvars uu = Just $ sum [m | yy <- stirsll vv bmax, dim yy >= 2,
      let mm = llqq [self uu kk | kk <- qqll yy],
      ll <- subpathsll (tdecpcd (uu `uunion` tsys (nntt mm)) (der (nntt mm))), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tdecpcd uu vv = fromJust $
      systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfPluriValentTreeCardinality uu vv
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    nntt nn = fromJust $ setPartitionsTransformVarPartition nn
    der = transformsDerived
    tsys = histogramsSystemImplied . transformsHistogram
    subpathsll = Set.toList . treesSubPaths
    uunion = pairSystemsUnion    
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    llqq = Set.fromList
    qqll = Set.toList    
    subset = Set.isSubsetOf
    empty = Set.empty   

valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubsSelfPluriLimBrPluriValCardReg :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrementedSubsSelfPluriLimBrPluriValCardReg d n bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      sum [c * m | (mm,c) <- sscdlimpvll n (min n bmax), 
                   let rr = concat [replicate (fromInteger q) (d^j) | (j,q) <- zip [1..] mm], 
                   ll <- subpathsll (Tree $ Map.singleton (1,rr) (tdecpcd 1 rr)), let (m,_) = last ll]
  | otherwise = Nothing
  where
    tdecpcd :: Integer -> [Integer] -> Tree (Integer,[Integer])
    tdecpcd k ll =  Tree $ llmm $ [((m,mm), tdecpcd m mm) | i <- [1..length ll], let d = ll !! (i-1), d > 2, 
                                    let m = k * d * (d-1) `div` 2, let mm = take (i-1) ll ++ [d-1] ++ drop i ll]
    sscdlimpvll n k = Map.toList $ Map.filterWithKey (\kk _ -> sum kk > 1 && sum kk <= k) (bellcd n)
    sysreg d n = fromJust $ systemRegular d n
    uvars = systemsVars
    subpathsll = Set.toList . treesSubPaths
    llmm = Map.fromList        

systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValent :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValent uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp qq | qq <- nn] | yy <- stirsll vv bmax, dim yy >= 2,
            nn <- qqll (prod [partspl (cart uu kk) | kk <- qqll yy])]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    partspl xx = Set.singleton xx `Set.delete` setsPartitionSet xx
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
      
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardinality :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardinality uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
      sum [product [bell (vol uu kk) - 1 | kk <- qqll yy] | yy <- stirsll vv bmax, dim yy >= 2]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardReg :: 
  Integer -> Integer -> Integer -> Maybe Integer
valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardReg d n bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      sum [c * product [(bell (d^k) - 1)^m | (k,m) <- zip [1..] ll] | (ll,c) <- sscdlimpvll n (min n bmax)]
  | otherwise = Nothing
  where
    sscdlimpvll n k = Map.toList $ Map.filterWithKey (\kk _ -> sum kk > 1 && sum kk <= k) (bellcd n)

systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValent :: 
  System -> Set.Set Variable -> Set.Set (Set.Set Variable) -> Maybe (Set.Set SetPartition)
systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValent uu vv yy
  | bigcup yy /= vv = Nothing
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [qqpp qq | qq <- nn] | nn <- qqll (prod [partspl (cart uu kk) | kk <- qqll yy])]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    prod = listSetsProduct
    partspl xx = Set.singleton xx `Set.delete` setsPartitionSet xx
    bigcup = setSetsUnion
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList
      
systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValentCardinality :: 
  System -> Set.Set Variable -> Set.Set (Set.Set Variable) -> Maybe Integer
systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValentCardinality uu vv yy
  | bigcup yy /= vv = Nothing
  | vv `subset` uvars uu = Just $ product [bell (vol uu kk) - 1 | kk <- qqll yy]
  | otherwise = Nothing
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    bigcup = setSetsUnion
    qqll = Set.toList
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadth :: 
  System -> Set.Set Variable -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadth uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [self uu kk | kk <- qqll yy] | yy <- stirsll vv bmax, dim yy >= 2]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    cart uu vv = fromJust $ systemsVarsCartesian uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList

setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinality :: 
  Set.Set Variable -> Integer -> Integer
setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinality vv bmax =
  sum [stirlingSecond (dim vv) b | b <- [2 .. (min (dim vv) bmax)]]
  where
    dim = toInteger . Set.size    
    uvars = systemsVars

dimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinalityRegular :: 
  Integer -> Integer -> Integer
dimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinalityRegular n bmax =
  sum [stirlingSecond n b | b <- [2 .. (min n bmax)]]

systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardExp :: 
  System -> Set.Set Variable -> Integer -> Maybe Rational
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardExp uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu && nnvvncpbmaxcd vv bmax > 0 = Just $ 
      fromInteger (sum [nnvvnpvyycd uu vv yy | yy <- stirsll vv bmax, dim yy >= 2]) / nnvvncpbmaxcd vv bmax
  | otherwise = Nothing
  where
    nnvvnpvyycd uu vv yy = fromInteger $ fromJust $
      systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValentCardinality uu vv yy
    nnvvncpbmaxcd vv bmax = fromInteger $  
      setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinality vv bmax
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardMax :: 
  System -> Set.Set Variable -> Integer -> Maybe Integer
systemsSetVarsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardMax uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu = Just $ maximum [nnvvnpvyycd uu vv yy | yy <- stirsll vv bmax, dim yy >= 2]
  | otherwise = Nothing
  where
    nnvvnpvyycd uu vv yy = fromInteger $ fromJust $
      systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateNonOverlapPluriValentCardinality uu vv yy
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardExpReg :: 
  Integer -> Integer -> Integer -> Maybe Rational
valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentSearchedCardExpReg d n bmax
  | nnvvncpbmaxcdr n bmax > 0 = Just $ nnvvnpbmaxpvcdr d n bmax / nnvvncpbmaxcdr n bmax
  | otherwise = Nothing
  where
    nnvvnpbmaxpvcdr d n bmax = fromInteger $ fromJust $
      valenciesDimensionsLimitsSetSetPartitionSubstrateNonOverlapPluriLimitedBreadthPluriValentCardReg d n bmax
    nnvvncpbmaxcdr n bmax = fromInteger $ 
      dimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinalityRegular n bmax

systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceTree :: 
  System -> Set.Set Variable -> Maybe (Tree [Integer])
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceTree uu vv
  | vv == empty = Just $ emptyTree
  | vv `subset` uvars uu = Just $ let ll = [ucard uu v | v <- qqll vv] in Tree $ Map.singleton ll (tdecch ll)
  | otherwise = Nothing
  where
    tdecch ll =  Tree $ llmm $ [(mm, tdecch mm) | i <- [1..length ll], let d = ll !! (i-1), d > 1, 
                                  let mm = take (i-1) ll ++ [d-1] ++ drop i ll]
    ucard uu v = toInteger $ Set.size $ fromJust $ systemsVarsSetValue uu v
    uvars = systemsVars
    llmm = Map.fromList        
    empty = Set.empty
    subset = Set.isSubsetOf
    qqll = Set.toList
    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceCardinalityExp :: 
  System -> Set.Set Variable -> Maybe Rational    
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceCardinalityExp uu vv
  | vv `subset` uvars uu = Just $ 
    let pp = paths (tdecch uu vv) in 
    fromInteger (sum [schd mm | ll <- qqll pp, mm <- ll]) / fromInteger (toInteger (Set.size pp))
  | otherwise = Nothing
  where
    tdecch uu vv = fromJust $ 
      systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceTree uu vv
    paths = treesPaths
    schd = foldl (\n c -> n + c * (c-1) `div` 2) 0
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf

systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceCardinalityMax :: 
  System -> Set.Set Variable -> Maybe Integer
systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceCardinalityMax uu vv
  | vv `subset` uvars uu = Just $ 
    let pp = paths (tdecch uu vv) in maximum [sum [schd mm | mm <- ll] | ll <- qqll pp]
  | otherwise = Nothing
  where
    tdecch uu vv = fromJust $ 
      systemsSetVarsSetSetPartitionCartesianSubstrateSelfTreeDecrementedSubstrateSelfChoiceTree uu vv
    paths = treesPaths
    schd = foldl (\n c -> n + c * (c-1) `div` 2) 0
    uvars = systemsVars
    qqll = Set.toList
    subset = Set.isSubsetOf

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCard ::
  System -> Set.Set Variable -> Integer -> Maybe [Integer]    
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCard
  uu vv bmax
  | vv == empty = Just $ []
  | vv `subset` uvars uu = Just $ 
    [mx 0 ll | yy <- stirsll vv bmax, dim yy >= 2, let ll = sort [v | jj <- qqll yy, let v = vol uu jj, v > 2]]
  | otherwise = Nothing
  where
    mx m [] = m
    mx m ll = mx (m + schd ll) (dec ll)
    dec [] = []
    dec (x:xx) = if x > 2 then ((x-1):xx) else (dec xx)
    schd = foldl (\n c -> if c > 2 then n + c * (c-1) `div` 2 else n) 0
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList    
    subset = Set.isSubsetOf
    empty = Set.empty   

valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCardReg :: 
  Integer -> Integer -> Integer -> Maybe [Integer]
valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCardReg 
  d n bmax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      concat [replicate (fromInteger a) (mx 0 mm) | (ll,a) <- sscdlimpvll n (min n bmax), 
            let mm = sort (concat [replicate (fromInteger p) (d^j) | (j,p) <- zip [1..] ll, d^j > 2])]
  | otherwise = Nothing
  where
    mx m [] = m
    mx m ll = mx (m + schd ll) (dec ll)
    dec [] = []
    dec (x:xx) = if x > 2 then ((x-1):xx) else (dec xx)
    schd = foldl (\n c -> if c > 2 then n + c * (c-1) `div` 2 else n) 0
    sscdlimpvll n k = Map.toList $ Map.filterWithKey (\kk _ -> sum kk > 1 && sum kk <= k) (bellcd n)

systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateTreeDecSubsSelfPluriLimBrPluriValChoiceMaxCard :: 
  System -> Set.Set Variable -> Set.Set (Set.Set Variable) -> Maybe Integer
systemsSetVarsSubstratePartitionsSetSetPartitionSubstrateTreeDecSubsSelfPluriLimBrPluriValChoiceMaxCard uu vv yy
  | bigcup yy /= vv = Nothing
  | vv `subset` uvars uu = Just $ mx 0 $ sort [v | jj <- qqll yy, let v = vol uu jj, v > 2]
  | otherwise = Nothing
  where
    mx m [] = m
    mx m ll = mx (m + schd ll) (dec ll)
    dec [] = []
    dec (x:xx) = if x > 2 then ((x-1):xx) else (dec xx)
    schd = foldl (\n c -> if c > 2 then n + c * (c-1) `div` 2 else n) 0
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    bigcup = setSetsUnion
    qqll = Set.toList
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriValSrchStat :: 
  System -> Set.Set Variable -> Integer -> Maybe (Rational,Integer)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriValSrchStat uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu && t > 0 = Just $ (fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCard 
        uu vv bmax
    t = fromInteger $ toInteger $ length mm
    uvars = systemsVars
    subset = Set.isSubsetOf         

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriValSrchStat_1 :: 
  System -> Set.Set Variable -> Integer -> Maybe (Rational,Integer)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriValSrchStat_1 uu vv bmax
  | bmax < 0 = Nothing
  | vv `subset` uvars uu && t > 0 = Just $ (fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCard 
        uu vv bmax
    t = fromInteger $  
      setVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinality vv bmax
    uvars = systemsVars
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValScrhStatReg :: 
  Integer -> Integer -> Integer -> Maybe (Rational,Integer)
valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValScrhStatReg 
  d n bmax
  | n >= 0 && d > 0 && bmax >= 0 && t > 0 = Just $ (fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCardReg 
        d n bmax
    t = fromInteger $ toInteger $ length mm

valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValScrhStatReg_1 :: 
  Integer -> Integer -> Integer -> Maybe (Rational,Integer)
valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriValScrhStatReg_1 
  d n bmax
  | n >= 0 && d > 0 && bmax >= 0 && t > 0 = Just $ (fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriValChoiceMaxListCardReg 
        d n bmax
    t = fromInteger $  
      dimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthCardinalityRegular n bmax

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValency :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Set.Set SetPartition)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValency uu vv bmax umax
  | bmax < 0 = Nothing
  | umax < 0 = Nothing
  | vv `subset` uvars uu = Just $ 
    llqq [llqq2 [self uu kk | kk <- qqll yy] | yy <- stirsll vv bmax, dim yy >= 2, 
                                               and [vol uu jj <= umax | jj <- qqll yy]]
  | otherwise = Nothing
  where
    qqpp qq = fromJust $ setComponentsPartition qq
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    self uu vv = fromJust $ systemsSetVarsPartitionSelf uu vv
    subset = Set.isSubsetOf
    qqll = Set.toList
    llqq = Set.fromList
    llqq2 = Set.fromList

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinality :: 
  System -> Set.Set Variable -> Integer -> Integer -> Integer
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinality 
  uu vv bmax umax = 
    toInteger $ length [yy | yy <- stirsll vv bmax, dim yy >= 2, and [vol uu jj <= umax | jj <- qqll yy]]
  where
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size    
    qqll = Set.toList
    uvars = systemsVars

valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinalityReg :: 
  Integer -> Integer -> Integer -> Integer -> Integer
valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapPluriLimitedBreadthLimitedValencyCardinalityReg 
  d n bmax umax = 
    sum [c | (mm,c) <- sscdlimpvll n (min n bmax), and [d^j <= umax | (j,q) <- zip [1..] mm, q>0]]
  where
    sscdlimpvll n k = Map.toList $ Map.filterWithKey (\kk _ -> sum kk > 1 && sum kk <= k) (bellcd n)

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCard ::
  System -> Set.Set Variable -> Integer -> Integer -> Maybe [Integer]    
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCard
  uu vv bmax umax
  | vv == empty = Just $ []
  | vv `subset` uvars uu = Just $ 
    [mx 0 ll | yy <- stirsll vv bmax, dim yy >= 2, and [vol uu jj <= umax | jj <- qqll yy], 
               let ll = sort [v | jj <- qqll yy, let v = vol uu jj, v > 2]]
  | otherwise = Nothing
  where
    mx m [] = m
    mx m ll = mx (m + schd ll) (dec ll)
    dec [] = []
    dec (x:xx) = if x > 2 then ((x-1):xx) else (dec xx)
    schd = foldl (\n c -> if c > 2 then n + c * (c-1) `div` 2 else n) 0
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList    
    subset = Set.isSubsetOf
    empty = Set.empty   

valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCardReg :: 
  Integer -> Integer -> Integer -> Integer -> Maybe [Integer]
valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCardReg 
  d n bmax umax
  | n >= 0 && d > 0 && bmax >= 0 = Just $ 
      concat [replicate (fromInteger a) (mx 0 mm) | (ll,a) <- sscdlimpvll n (min n bmax), 
            and [d^j <= umax | (j,q) <- zip [1..] ll, q>0],      
            let mm = sort (concat [replicate (fromInteger p) (d^j) | (j,p) <- zip [1..] ll, d^j > 2])]
  | otherwise = Nothing
  where
    mx m [] = m
    mx m ll = mx (m + schd ll) (dec ll)
    dec [] = []
    dec (x:xx) = if x > 2 then ((x-1):xx) else (dec xx)
    schd = foldl (\n c -> if c > 2 then n + c * (c-1) `div` 2 else n) 0
    sscdlimpvll n k = Map.toList $ Map.filterWithKey (\kk _ -> sum kk > 1 && sum kk <= k) (bellcd n)

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchStat :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Rational,Integer)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchStat 
  uu vv bmax umax
  | umax < 0 = Nothing
  | bmax < 0 = Nothing
  | vv `subset` uvars uu && t > 0 = Just $ (fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCard 
        uu vv bmax umax
    t = fromInteger $ toInteger $ length mm
    uvars = systemsVars
    subset = Set.isSubsetOf         

valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriLimValScrhStatReg :: 
  Integer -> Integer -> Integer -> Integer -> Maybe (Rational,Integer)
valenciesDimensionsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecSubsSelfPluriLimBrPluriLimValScrhStatReg 
  d n bmax umax
  | n >= 0 && d > 0 && bmax >= 0 && umax >= 0 && t > 0 = Just $ (fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      valenciesDimensionsLimitsSetSetPartitionCartesianSubsNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMaxListCardReg 
        d n bmax umax
    t = fromInteger $ toInteger $ length mm

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMinListCard ::
  System -> Set.Set Variable -> Integer -> Integer -> Maybe [Integer]    
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMinListCard
  uu vv bmax umax
  | vv == empty = Just $ []
  | vv `subset` uvars uu = Just $ 
    [mx 0 ll | yy <- stirsll vv bmax, dim yy >= 2, and [vol uu jj <= umax | jj <- qqll yy], 
               let ll = reverse $ sort [v | jj <- qqll yy, let v = vol uu jj, v > 2]]
  | otherwise = Nothing
  where
    mx m [] = m
    mx m ll = mx (m + schd ll) (dec ll)
    dec [] = []
    dec (x:xx) = if x > 2 then ((x-1):xx) else (dec xx)
    schd = foldl (\n c -> if c > 2 then n + c * (c-1) `div` 2 else n) 0
    vol uu vv = fromJust $ systemsVarsVolume uu vv
    uvars = systemsVars
    stirsll vv bmax = Set.toList $ setsSetPartitionLimited vv bmax
    dim = toInteger . Set.size
    qqll = Set.toList    
    subset = Set.isSubsetOf
    empty = Set.empty   

systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchMinStat :: 
  System -> Set.Set Variable -> Integer -> Integer -> Maybe (Integer,Rational,Integer)
systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOverlapTreeDecrSubsSelfPluriLimBrPluriLimValSrchMinStat 
  uu vv bmax umax
  | umax < 0 = Nothing
  | bmax < 0 = Nothing
  | vv `subset` uvars uu && t > 0 = Just $ (minimum mm, fromInteger (sum mm) / t, maximum mm)
  | otherwise = Nothing
  where
    mm = fromJust $ 
      systemsSetVarsLimitsSetSetPartitionCartesianSubstrateNonOvTreeDecSubsSelfPluriLimBrPluriLimValChMinListCard 
        uu vv bmax umax
    t = fromInteger $ toInteger $ length mm
    uvars = systemsVars
    subset = Set.isSubsetOf         

