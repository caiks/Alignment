{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ConstrainedClassMethods #-}

module AlignmentUtil (
  Tree(..), 
  treesMap,
  treesRelation,
  emptyTree, 
  treesNodes, 
  treesSteps,
  treesDescendents,
  treesElements,
  treesRoots, 
  treesLeaves, 
  pairTreesUnion, 
  treesPaths, 
  pathsTree,
  treesSubPaths,
  funcsTreesMap,
  funcsTreesMapNode,
  funcsTreesMapAccum,
  funcsTreesMapNodeAccum,
  treesPlaces,
  treesElementsLocations,
  treesDepth,
  pairTreesDot,
  treesDistinct,
  treesIsDistinct,
  treeRegular,
  treesTreeInteger,
  treesTreePositioned,
  funcsListsTreesTraversePreOrder,
  funcsListsTreesTraverseInOrder,
  funcsListsTreesTraversePostOrder,
  setsPowerset,
  setsPowersetLimited,
  setsPowersetFixed,
  setsPartitionSet, setsSetPartition,
  setsSetPartitionLimited,
  setsSetPartitionFixed,
  setsSetPartitionWeak,
  setsIsPartition,
  setsIsPartitionWeak,
  setsIsPartitionUnary,
  setsIsPartitionSelf,
  pairPartitionsIsParent,
  pairPartitionsIsDecrement,
  partitionSetsDecrements,
  partitionPointedSetsIncrements,
  setsAll,
  setSetsUnion,
  setsSetsCross,
  setsSetsDot,
  listSetsProduct,
  relationsIsFunc,
  relationsIsBijective,
  relationsIsCircular,
  relationsCount,
  relationsDomain,
  relationsRange,
  relationsFlip,
  relationsInverse,
  pairRelationsJoin,
  pairRelationsJoinOuter,
  relationsClosure,
  functionsRelation,
  functionsDomain,
  functionsRange,
  functionsIsBijective,
  functionsInverse,
  functionsIsProbability,
  relationsMaximum,
  relationsMinimum,
  relationsSum,
  relationsNormalise,
  fromJust,
  fromMaybe,
  fromJustNum,
  Represent,
  represent,
  factorial,
  factorialFalling,
  factorialRising,
  combination, combination_1, combination_2,
  combinationMultinomial, combinationMultinomial_1,
  compositionWeak, compositionWeak_1,
  compositionStrong,
  stirlingSecond, stirlingSecond_1,
  bell,
  bellcd, bellcd_1,
  stircd,
  entropy
)
where
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import GHC.Real

fromJust :: Maybe a -> a
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe d (Just x) = x
fromMaybe d Nothing = d

fromJustNum :: (Num a) => Maybe a -> a
fromJustNum (Just x) = x
fromJustNum Nothing = 0

data Tree a = Tree (Map.Map a (Tree a))
               deriving (Eq, Ord, Read, Show)

treesMap :: Tree a -> Map.Map a (Tree a)
treesMap (Tree mm) = mm

emptyTree :: Tree a
emptyTree = Tree (Map.empty)

treesNodes :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set (a, Tree a)
treesNodes tt = 
    rel tt `Set.union` (Map.foldr Set.union Set.empty (Map.map (\rr -> nodes rr) mm))
  where
    (Tree mm) = tt
    rel = treesRelation
    nodes = treesNodes

treesSteps :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set (a, a)
treesSteps tt = 
    Set.fromList [(x,y) | (x,rr) <- Set.toList (nodes tt), (y,_) <- Set.toList (rel rr)]
  where
    rel = treesRelation
    nodes = treesNodes

treesDescendents :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set (a, a)
treesDescendents = relationsClosure . treesSteps

treesElements :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set a
treesElements = relationsDomain . treesNodes

treesRelation :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set (a, Tree a)
treesRelation (Tree mm) = Set.fromList (Map.toList mm)

treesRoots :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set a
treesRoots = relationsDomain . treesRelation

treesLeaves :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set a
treesLeaves tt = 
    Set.fromList [x | (x,rr) <- Set.toList (nodes tt), rr == empty]
  where
    nodes = treesNodes
    empty = emptyTree

pairTreesUnion :: (Ord a, Ord (Tree a)) => Tree a -> Tree a -> Tree a
pairTreesUnion ss tt 
  | tt == empty = ss
  | ss == empty = tt
  | otherwise = Tree $ Map.unionWith union (ttmm ss) (ttmm tt) 
  where
    empty = emptyTree
    union = pairTreesUnion
    ttmm (Tree mm) = mm

pairTreesDot :: (Ord a, Ord (Tree a), Ord b, Ord (Tree b)) => Tree a -> Tree b -> Tree (a,b)
pairTreesDot (Tree mm) (Tree nn) = 
  Tree $ Map.fromList [((a,b), pairTreesDot xx yy) | ((a,xx),(b,yy)) <- zip (Map.toList mm) (Map.toList nn)]
    
treesDistinct :: (Ord a, Ord b, Ord (Tree (a,b))) => Tree (a,b) -> Set.Set (Tree (a,b))
treesDistinct (Tree mm) = 
    Set.map totree (Set.filter istotalfunc (setsPowerset (Set.fromList [((x,y),rr) | 
      ((x,y),ss) <- Map.toList mm, rr <- Set.toList (treesDistinct ss)])))
  where 
    istotalfunc nn = Set.size nn == Set.size (relationsDomain (Map.keysSet mm)) && relationsIsFunc (relationsDomain nn)
    totree nn = Tree (Map.fromList (Set.toList nn))
      
treesIsDistinct :: (Ord a, Ord b, Ord (Tree (a,b))) => Tree (a,b) -> Bool
treesIsDistinct (Tree mm) = 
    isfunc mm && and [isdistinct ss | (_,ss) <- Map.toList mm]
  where 
    isfunc mm = relationsIsFunc (Map.keysSet mm)
    isdistinct = treesIsDistinct   
        
treesPaths :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set [a]
treesPaths tt = 
    listsTreesPaths [] tt
  where 
    listsTreesPaths :: (Ord a, Ord (Tree a)) => [a] -> Tree a -> Set.Set [a]
    listsTreesPaths ll tt 
      | tt == empty = Set.singleton ll
      | otherwise = bigcup $ Set.map (\(x,rr) -> paths (ll ++ [x]) rr) (rel tt)
      where
        rel = treesRelation
        empty = emptyTree
        paths = listsTreesPaths
        bigcup ss = Set.fold Set.union Set.empty ss

pathsTree :: (Ord a, Ord (Tree a)) => Set.Set [a] -> Tree a
pathsTree qq = 
    Set.fold union empty $ Set.map lltt qq
  where
    lltt [] = empty
    lltt [x] = Tree $ Map.singleton x empty
    lltt (x:xx) = Tree $ Map.singleton x (tree (Set.singleton xx))
    union = pairTreesUnion
    empty = emptyTree
    tree = pathsTree
    
funcsTreesMap :: (Ord a, Ord (Tree a), Ord b, Ord (Tree b)) => (a -> b) -> Tree a -> Tree b
funcsTreesMap ff tt = funcsTreesMapNode gg tt where gg x _ = ff x
  
funcsTreesMapNode :: (Ord a, Ord (Tree a), Ord b, Ord (Tree b)) => (a -> Tree a -> b) -> Tree a -> Tree b
funcsTreesMapNode ff (Tree mm) = Tree $ Map.fromList [(ff k xx, funcsTreesMapNode ff xx) | (k,xx) <- Map.toList mm]
    
funcsTreesMapAccum :: (Ord a, Ord (Tree a), Ord b, Ord (Tree b)) => ([a] -> b) -> Tree a -> Tree b
funcsTreesMapAccum ff tt = funcsTreesMapNodeAccum gg tt where gg x _ = ff x
 
funcsTreesMapNodeAccum :: (Ord a, Ord (Tree a), Ord b, Ord (Tree b)) => ([a] -> Tree a -> b) -> Tree a -> Tree b
funcsTreesMapNodeAccum ff tt = accum ff [] tt
  where
    accum ff ll (Tree zz) = Tree $ Map.fromList [(ff mm rr, accum ff mm rr) | (x,rr) <- Map.toList zz, let mm = ll ++ [x]]
   
treesPlaces :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set ([a], Tree a)
treesPlaces tt = treesElements $ funcsTreesMapNodeAccum (\ll rr -> (ll,rr)) tt

treesSubPaths :: (Ord a, Ord (Tree a)) => Tree a -> Set.Set [a]
treesSubPaths  = relationsDomain . treesPlaces

treesElementsLocations :: (Ord a, Ord (Tree a)) => Tree a -> Map.Map a (Set.Set [a])
treesElementsLocations = functionsInverse . Map.fromList . Set.toList . treesElements . funcsTreesMapAccum (\ll -> (ll, last ll))

treesDepth :: (Ord a, Ord (Tree a)) => Tree a -> Integer
treesDepth tt = if tt==emptyTree then 0 else (toInteger . Set.findMax . Set.map length . treesPaths) tt

treeRegular :: Integer -> Integer -> Tree [Integer]
treeRegular k h 
  | k > 0 && h > 0 = reg k h []
  | otherwise = empty
  where
    reg _ 0 _ = empty
    reg k h ll = Tree $ Map.fromList [(ll ++ [i], reg k (h-1) (ll ++ [i])) | i <- [1..k]] 
    empty = emptyTree    
    
treesTreeInteger :: (Ord a, Ord (Tree a)) => Tree a -> Tree Integer
treesTreeInteger tt = 
  let mm = Map.fromList (zip (Set.toList (treesElements tt)) [1..]) in funcsTreesMap (\x -> mm Map.! x) tt

treesTreePositioned :: (Ord a, Ord (Tree a)) => Tree a -> Tree (a,[Int])
treesTreePositioned tt = ttlltt tt []
  where
    ttlltt (Tree mm) ll = 
      Tree $ Map.fromList [((k, jj), ttlltt xx jj) | ((k,xx),i) <- zip (Map.toList mm) [0..], let jj = ll ++ [i]]
 
funcsListsTreesTraversePreOrder :: (Ord a, Ord (Tree a), Ord c, Ord (Tree c)) => (a -> b -> c) -> [b] -> Tree a -> (Tree c,[b])
funcsListsTreesTraversePreOrder ff ll (Tree mm)
  | mm /= Map.empty = next (zip (Map.toList mm) ll) (drop (Map.size mm) ll) []
  | otherwise = (emptyTree,ll)
  where
--    next :: [((a,Tree a),b)] -> [b] -> [(c,Tree c)] -> (Tree c,[b])
    next [] jj kk = (Tree (Map.fromList kk),jj)
    next (((a,xx),b):yy) jj kk = let (tt,ii) = traverse ff jj xx in next yy ii ((ff a b, tt):kk) 
    traverse = funcsListsTreesTraversePreOrder
  
funcsListsTreesTraverseInOrder :: (Ord a, Ord (Tree a), Eq b, Ord c, Ord (Tree c)) => (a -> b -> c) -> [b] -> Tree a -> (Tree c,[b])
funcsListsTreesTraverseInOrder ff ll (Tree mm)
  | mm /= Map.empty = next (Map.toList mm) ll []
  | otherwise = (emptyTree,ll)
  where
--    next :: [(a,Tree a)] -> [b] -> [(c,Tree c)] -> (Tree c,[b])
    next [] jj kk = (Tree (Map.fromList kk),jj)
    next ((a,xx):yy) jj kk = 
      let (tt,ii) = traverse ff jj xx in if ii /= [] then next yy (tail ii) ((ff a (head ii), tt):kk) else next yy [] kk
    traverse = funcsListsTreesTraverseInOrder
    
funcsListsTreesTraversePostOrder :: (Ord a, Ord (Tree a), Ord c, Ord (Tree c)) => (a -> b -> c) -> [b] -> Tree a -> (Tree c,[b])
funcsListsTreesTraversePostOrder ff ll (Tree mm)
  | mm /= Map.empty = next (Map.toList mm) ll []
  | otherwise = (emptyTree,ll)
  where
--    next :: [(a,Tree a)] -> [b] -> [(a,Tree c)] -> (Tree c,[b])
    next [] jj kk = (Tree (Map.fromList [(ff a b, tt) | ((a,tt),b) <- zip (reverse kk) jj]),drop (length kk) jj)
    next ((a,xx):yy) jj kk = let (tt,ii) = traverse ff jj xx in next yy ii ((a, tt):kk) 
    traverse = funcsListsTreesTraversePostOrder
      
setsPowerset :: Ord a => Set.Set a -> Set.Set (Set.Set a)
setsPowerset ss = 
    Set.fold insert (Set.singleton Set.empty) ss
  where
    insert x rr = rr `Set.union` Set.map (Set.insert x) rr

setsPowersetLimited :: Ord a => Set.Set a -> Integer -> Set.Set (Set.Set a)
setsPowersetLimited ss k
  | k < 0 = Set.empty
  | fromInteger k >= Set.size ss = setsPowerset ss
  | otherwise = Set.fold insert (Set.singleton Set.empty) ss
  where
    insert x rr = rr `Set.union` Set.map (Set.insert x) (Set.filter (\xx -> Set.size xx < fromInteger k) rr)

setsPowersetFixed :: Ord a => Set.Set a -> Integer -> Set.Set (Set.Set a)
setsPowersetFixed ss k 
  | k < 0 = Set.empty
  | fromInteger k > Set.size ss = Set.empty
  | otherwise = setsPowersetLimited ss k `Set.difference` setsPowersetLimited ss (k-1)
    
setsPartitionSet :: Ord a => Set.Set a -> Set.Set (Set.Set (Set.Set a))
setsPartitionSet ss 
  | ss == Set.empty = Set.empty
  | otherwise = Set.fold xqq (sgl (sgl (sgl (min ss)))) (Set.delete (min ss) ss)
  where
    xqq x qq = bigcup (Set.map (xpp x) qq)
    xpp x pp = Set.insert (Set.insert (sgl x) pp) (Set.map (\cc -> Set.insert (Set.insert x cc) (Set.delete cc pp)) pp)
    sgl = Set.singleton
    min = Set.findMin
    bigcup = Set.fold Set.union Set.empty
    
setsSetPartition :: Ord a => Set.Set a -> Set.Set (Set.Set (Set.Set a))
setsSetPartition =  setsPartitionSet  

setsSetPartitionLimited :: Ord a => Set.Set a -> Integer -> Set.Set (Set.Set (Set.Set a))
setsSetPartitionLimited ss k 
  | ss == Set.empty || k <= 0 = Set.empty
  | fromInteger k >= Set.size ss = setsSetPartition ss
  | otherwise = Set.fold xqq (sgl (sgl (sgl (min ss)))) (Set.delete (min ss) ss)
  where
    xqq x qq = bigcup (Set.map (xpp x) qq)
    xpp x pp = if Set.size pp < fromInteger k then Set.insert (Set.insert (sgl x) pp) (xrr x pp) else (xrr x pp)
    xrr x pp = Set.map (\cc -> Set.insert (Set.insert x cc) (Set.delete cc pp)) pp
    sgl = Set.singleton
    min = Set.findMin
    bigcup = Set.fold Set.union Set.empty
    
setsSetPartitionFixed :: Ord a => Set.Set a -> Integer -> Set.Set (Set.Set (Set.Set a))
setsSetPartitionFixed ss k 
  | ss == Set.empty || k <= 0 = Set.empty
  | fromInteger k > Set.size ss = Set.empty
  | otherwise = setsSetPartitionLimited ss k `Set.difference` setsSetPartitionLimited ss (k-1)
    
setsSetPartitionWeak :: Ord a => Set.Set a -> Set.Set (Set.Set (Set.Set a))
setsSetPartitionWeak ss 
  | ss == Set.empty = Set.singleton (Set.singleton Set.empty)
  | otherwise = setsSetPartition ss `Set.union` Set.map (Set.insert Set.empty) (setsSetPartition ss)

setsIsPartition :: Ord a => Set.Set (Set.Set a) -> Bool
setsIsPartition pp =
    (pp /= Set.empty) && (not (Set.empty `Set.member` pp)) && 
      ((Set.size $ bigcup pp) == (foldl (+) 0 $ map Set.size $ Set.toList pp))
  where
    bigcup = Set.fold Set.union Set.empty

setsIsPartitionWeak :: Ord a => Set.Set (Set.Set a) -> Bool
setsIsPartitionWeak pp =
      ((Set.size $ bigcup pp) == (foldl (+) 0 $ map Set.size $ Set.toList pp))
  where
    bigcup = Set.fold Set.union Set.empty

setsIsPartitionUnary :: Ord a => Set.Set (Set.Set a) -> Bool
setsIsPartitionUnary pp = setsIsPartition pp && (Set.size pp == 1)

setsIsPartitionSelf :: Ord a => Set.Set (Set.Set a) -> Bool
setsIsPartitionSelf pp = setsIsPartition pp && setsAll (\cc -> Set.size cc == 1) pp

pairPartitionsIsParent :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set a) -> Bool
pairPartitionsIsParent pp qq = 
    isPart pp &&  isPart qq && bigcup pp == bigcup qq && 
      isFunc [(dd,cc) | dd <- Set.toList qq, cc <- Set.toList pp, dd `Set.intersection` cc /= Set.empty]
  where
    isPart = setsIsPartition
    bigcup = Set.fold Set.union Set.empty
    isFunc = relationsIsFunc . Set.fromList 

pairPartitionsIsDecrement :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set a) -> Bool
pairPartitionsIsDecrement pp qq = pairPartitionsIsParent pp qq && Set.size pp == Set.size qq - 1

partitionSetsDecrements :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set (Set.Set a))
partitionSetsDecrements pp = 
    Set.fromList [pp Set.\\ Set.fromList [cc,dd] `Set.union` Set.singleton (cc `Set.union` dd) | 
      cc <- Set.toList pp, dd <- Set.toList pp, dd /= cc]
    
{-
partitionSetsDecrements :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set (Set.Set a))
partitionSetsDecrements pp = 
    llqq [pp Set.\\ llqq [cc,dd] `cup` sing (cc `cup` dd) | cc <- qqll pp, dd <- qqll pp, dd /= cc]
  where
    llqq = Set.fromList
    qqll = Set.toList
    cup = Set.union
    sing = Set.singleton   
-} 

partitionPointedSetsIncrements :: Ord a => (Set.Set (Set.Set a), Set.Set a) -> Set.Set ((Set.Set (Set.Set a), Set.Set a))
partitionPointedSetsIncrements (pp,ccp) 
  | Set.size ccp > 1 = 
    Set.fromList [(ccp `Set.delete` pp `Set.union` Set.fromList [ddp,dd], ddp) | 
      ss <- Set.toList ccp, let ddp = ss `Set.delete` ccp, let dd = Set.singleton ss] `Set.union`
      Set.fromList [(pp Set.\\ Set.fromList [ccp,cc] `Set.union` Set.fromList [ddp,dd], ddp) | 
        ss <- Set.toList ccp, cc <- Set.toList pp, cc /= ccp, let ddp = ss `Set.delete` ccp, let dd = ss `Set.insert` cc]
  | otherwise = Set.empty

setsAll :: Ord a => (a -> Bool) -> Set.Set a -> Bool
setsAll f xx = Set.fold (\x b -> b && f x) True xx

setSetsUnion :: Ord a => Set.Set (Set.Set a) -> Set.Set a
setSetsUnion  =  Set.fold Set.union Set.empty

setsSetsCross :: (Ord a, Ord b) => Set.Set a -> Set.Set b -> Set.Set (a,b)
setsSetsCross xx yy = setSetsUnion $ Set.map (\x -> Set.map (\y -> (x,y)) yy) xx

listSetsProduct :: Ord a => [Set.Set a] -> Set.Set [a]
listSetsProduct ll = foldl mul (Set.singleton []) ll
  where
    mul rr qq = Set.fromList [jj ++ [x] | jj <- Set.toList rr, x <- Set.toList qq]

-- From base-4.3.1.0
-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
{-
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
-}

setsSetsDot :: (Ord a, Ord b) => Set.Set a -> Set.Set b -> Set.Set (Set.Set (a,b))
setsSetsDot xx yy 
  | Set.size xx <= Set.size yy = dot xx yy
  | otherwise = Set.empty
  where 
    dot aa bb = Set.fromList [Set.fromList (zip (Set.toList xx) pp) | pp <- permutations (Set.toList yy)]  

relationsIsFunc :: (Ord a) => Set.Set (a,b) -> Bool
relationsIsFunc ss = Map.size (Map.fromList (Set.toList ss)) == Set.size ss

relationsIsBijective :: (Ord a, Ord b) => Set.Set (a,b) -> Bool
relationsIsBijective ss = relationsIsFunc ss && Set.size (relationsRange ss) == Set.size (relationsDomain ss)

relationsIsCircular :: (Ord a) => Set.Set (a,a) -> Bool
relationsIsCircular ss = relationsDomain ss `Set.intersection` relationsRange ss /= Set.empty

relationsCount :: (Ord a, Ord b) => Set.Set (a,b) -> Map.Map a Integer
relationsCount ss = 
    Map.fromListWith (+) $ map (\(a,b) -> (a,1)) $ Set.toList ss

relationsDomain :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set a
relationsDomain ss = Set.map (\(x,y) -> x) ss

relationsRange :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set b
relationsRange ss = Set.map (\(x,y) -> y) ss

relationsFlip :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (b,a)
relationsFlip ss = Set.map (\(x,y) -> (y,x)) ss

relationsInverse :: (Ord a, Ord b) => Set.Set (a,b) -> Map.Map b (Set.Set a)
relationsInverse qq = llmm [(y, sing x) | (x,y) <- qqll qq] 
  where
    qqll = Set.toList
    sing = Set.singleton
    llmm = Map.fromListWith Set.union

pairRelationsJoin :: (Ord a, Ord b, Ord c) => Set.Set (a,b) -> Set.Set (b,c) -> Set.Set (a,c)
pairRelationsJoin xx yy = Set.fromList [(a,c) | (a,b) <- Set.toList xx, (b',c) <- Set.toList yy, b'==b]

pairRelationsJoinOuter :: (Ord a) => Set.Set (a,a) -> Set.Set (a,a) -> Set.Set (a,a)
pairRelationsJoinOuter xx yy = Set.fromList (
  [(s1,t2) | (s1,t1) <- Set.toList xx, (s2,t2) <- Set.toList yy, s2==t1] ++
  [(s1,t1) | (s1,t1) <- Set.toList xx, t1 `Set.notMember` (relationsDomain yy)] ++
  [(s2,t2) | (s2,t2) <- Set.toList yy, s2 `Set.notMember` (relationsDomain xx)])

relationsClosure :: (Ord a) => Set.Set (a,a) -> Set.Set (a,a)
relationsClosure ss = let ss' = ss `Set.union` (pairRelationsJoin ss ss) in if ss' == ss then ss else relationsClosure ss'

functionsRelation :: (Ord a, Ord b) => Map.Map a b -> Set.Set (a,b)
functionsRelation = llqq . mmll 
  where
    mmll = Map.toList
    llqq = Set.fromList
    
functionsIsBijective :: (Ord a, Ord b) => Map.Map a b -> Bool
functionsIsBijective = relationsIsBijective . functionsRelation
    
functionsDomain :: (Ord a, Ord b) => Map.Map a b -> Set.Set a
functionsDomain = Map.keysSet
    
functionsRange :: (Ord a, Ord b) => Map.Map a b -> Set.Set b
functionsRange = relationsRange . functionsRelation
    
functionsInverse :: (Ord a, Ord b) => Map.Map a b -> Map.Map b (Set.Set a)
functionsInverse mm = llmm [(y, sing x) | (x,y) <- mmll mm] 
  where
    mmll = Map.toList
    sing = Set.singleton
    llmm = Map.fromListWith Set.union

functionsIsProbability :: Ord a => Map.Map a Rational -> Bool
functionsIsProbability mm = 
  mm /= Map.empty && maximum ee <= 1 && minimum ee >= 0 && sum ee == 1
  where 
    ee = Map.elems mm

relationsMaximum :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (a,b)
relationsMaximum ss 
  | ss == Set.empty =  Set.empty
  | otherwise = Set.filter (\(x,y) -> y==m) ss
  where
    m = Set.findMax (relationsRange ss)

relationsMinimum :: (Ord a, Ord b) => Set.Set (a,b) -> Set.Set (a,b)
relationsMinimum ss 
  | ss == Set.empty =  Set.empty
  | otherwise = Set.filter (\(x,y) -> y==m) ss
  where
    m = Set.findMin (relationsRange ss)

relationsSum :: (Ord a, Ord b, Num b) => Set.Set (a,b) -> b
relationsSum qq = sum [y | (_,y) <- Set.toList qq] 

relationsNormalise :: (Ord a, Ord b, Fractional b) => Set.Set (a,b) -> Set.Set (a,b)
relationsNormalise qq = Set.map (\(x,y) -> (x,y/s)) qq
  where
    s = relationsSum qq

class Represent a where
  represent :: Show a => a -> String
  represent x = show x

instance Represent Integer where
  represent x = show x

instance Represent Rational where
  represent x = show x

instance Represent Double where
  represent x = show x

instance Represent Int where
  represent x = show x

instance Represent Char where
  represent x = show x

instance Represent Bool where
  represent x = show x
  
llrep :: (Represent a, Show a) => [a] ->  String
llrep [] = ""
llrep [x] = represent x
llrep (x:rr) = represent x ++ "," ++ llrep rr
 
instance (Represent a, Show a) => Represent [a] where
  represent ll = "[" ++ llrep ll ++ "]"
  
instance (Represent a, Show a, Represent b, Show b) => Represent (a,b) where
  represent (x,y) = "(" ++ represent x ++ "," ++ represent y ++ ")"

instance (Represent a, Show a, Represent b, Show b, Represent c, Show c) => Represent (a,b,c) where
  represent (x,y,z) = "(" ++ represent x ++ "," ++ represent y ++ "," ++ represent z ++ ")"

instance (Represent a, Show a, Represent b, Show b, Represent c, Show c, Represent d, Show d) => Represent (a,b,c,d) where
  represent (x1,x2,x3,x4) = "(" ++ represent x1 ++ "," ++ represent x2 ++ "," ++ represent x3 ++ "," ++ represent x4 ++ ")"

instance (Represent a, Show a, Represent b, Show b, Represent c, Show c, Represent d, Show d, Represent e, Show e) => Represent (a,b,c,d,e) where
  represent (x1,x2,x3,x4,x5) = "(" ++ represent x1 ++ "," ++ represent x2 ++ "," ++ represent x3 ++ "," ++ represent x4 ++ "," ++ represent x5 ++ ")"

instance (Represent a, Show a) => Represent (Set.Set a) where
  represent xx = "{" ++ llrep (Set.toList xx) ++ "}"

instance (Represent a, Show a, Represent b, Show b) => Represent (Map.Map a b) where
  represent xx = "{" ++ llrep (Map.assocs xx) ++ "}"

instance (Represent a, Show a) => Represent (Tree a) where
  represent (Tree xx) = represent xx

factorial :: Integer -> Integer
factorial n 
  | n <= 1 = 1
  | otherwise = n * factorial (n-1)

factorialFalling :: Integer -> Integer -> Integer
factorialFalling n k
  | k <= 0 = 1
  | k == 1 = n
  | k > n = factorialFalling n n
  | otherwise = n * factorialFalling (n-1) (k-1)

factorialRising :: Integer -> Integer -> Integer
factorialRising n k
  | k <= 0 = 1
  | k == 1 = n
  | otherwise = n * factorialRising (n+1) (k-1)

combination :: Integer -> Integer -> Integer
combination n k 
  | n < 0 || k < 0 || k > n = 0
  | otherwise = factorialFalling n k `div` factorial k

combination_2 :: Integer -> Integer -> Integer
combination_2 n k 
  | n < 0 || k < 0 || k > n = 0
  | otherwise = numerator $ fromInteger (factorialFalling n k) / fac k
  where
    fac = toRational . factorial

combination_1 :: Integer -> Integer -> Integer
combination_1 n k 
  | n < 0 || k < 0 || k > n = 0
  | otherwise = numerator $ fac n / fac k / fac (n-k)
  where
    fac = toRational . factorial

combinationMultinomial :: Integer -> [Integer] -> Integer
combinationMultinomial n kk = factorial n `div` product [factorial k | k <- kk]

combinationMultinomial_1 :: Integer -> [Integer] -> Integer
combinationMultinomial_1 n kk = numerator $ fac n / product [fac k | k <- kk]
  where
    fac = toRational . factorial

compositionWeak :: Integer -> Integer -> Integer
compositionWeak z v = factorial ( z + v -1) `div` factorial z `div` factorial (v-1)

compositionWeak_1 :: Integer -> Integer -> Integer
compositionWeak_1 z v = numerator $ fac ( z + v -1) / fac z / fac (v-1)
  where
    fac = toRational . factorial

compositionStrong :: Integer -> Integer -> Integer
compositionStrong z v = combination (z-1) (v-1)

stirlingSecond_1 :: Integer -> Integer -> Integer
stirlingSecond_1 n k
  | n < 1 || k < 1 || k > n = 0
  | n == 1 || k == 1 || k == n = 1
  | otherwise = sum [(-1)^(k-j) * j^(n-1) * fac k `div` fac (j-1) `div` fac (k-j) | j <- [1..k]] `div` fac k
  where
    fac = factorial

stirlingSecond :: Integer -> Integer -> Integer
stirlingSecond n k
  | n < 1 || k < 1 || k > n = 0
  | n == 1 || k == 1 || k == n = 1
  | otherwise = numerator $ sum [toRational ((-1)^(k-j) * j^(n-1)) / fac (j-1) / fac (k-j) | j <- [1..k]]
  where
    fac = toRational . factorial

bell :: Integer -> Integer
bell n
  | n < 1 = 0
  | n == 1 = 1
  | otherwise = sum [stirlingSecond n k | k <- [1..n]]

bellcd :: Integer -> Map.Map [Integer] Integer 
bellcd n
  | n < 1 = Map.empty
  | n == 1 = Map.singleton [1] 1
  | otherwise = Map.fromList [(ll, fac n `div` product [(fac k)^r * fac r | (k,r) <- zip [1..] ll]) | 
                               (_,ll) <- Set.toList (foldl accum (Set.fromList [(0,[0]),(n,[1])]) [n-1,n-2 .. 1])]
  where 
    accum qq i = Set.fromList [(t+i*m, m:ll) | (t,ll) <- Set.toList qq, m <- [0 .. n `div` i], 
                                        if i==1 then t+i*m == n else t+i*m <= n]
    fac = factorial

bellcd_1 :: Integer -> Map.Map [Integer] Integer 
bellcd_1 n
  | n < 1 = Map.empty
  | n == 1 = Map.singleton [1] 1
  | otherwise = Map.fromList [(ll, fac n `div` product [(fac k)^r * fac r | (k,r) <- zip [1..] ll]) | 
                               ll <- Set.toList (foldl accum (Set.fromList [[0],[1]]) [n-1,n-2 .. 1])]
  where 
    accum qq i = Set.fromList [m : ll | ll <- Set.toList qq, m <- [0 .. n `div` i], 
                                        let t = sum [k*r | (k,r) <- zip [i+1..] ll] + i*m, 
                                        if i==1 then t==n else t <= n]
    fac = factorial

stircd :: Integer -> Integer -> Map.Map [Integer] Integer 
stircd n k = Map.filterWithKey (\kk _ -> sum kk == k) (bellcd n)

entropy :: [Rational] -> Double
entropy [] = 0
entropy ll = - sum [r' * log r' | r <- ll, let r' = fromRational (r/s)]
  where
    s = sum ll

