-- | Implements a basic fuzzy logic rules system
module Fuzzy (
   MF,
   RuleTree,
   FuzzySet,
   Predicate,
   alternatives,
   val,
   (~=>),
   weight,
   fuzzifyBool,
   fuzzify,
   runRuleTree,
   iterSet,
   (~==),
   (~/=),
   (~||),
   (~&&),
   union,
   not,
   up,
   down,
   triangle,
   gauss,
   trapezoid
) where

import Prelude hiding (not)
import Data.List (groupBy, foldl1', concatMap, sort)

-- | Membership function type, should return a value between 0 and 1.0
type MF a = (a -> Double)

-- | List of items x, and their membership in the set
type FuzzySet a = [FuzzyVar a]

-- | An item in a fuzzy set along with its membership in the set
type FuzzyVar a = (a, Double)

-- | Describes a tree of fuzzy rules 
data RuleTree a = RuleSet [Rule a] | Consequent a deriving (Eq, Show)

-- | A predicate that returns the degree to which this rule applies
type Predicate = Double

-- | A  single rule within a RuleTree
type Rule a = (Predicate, RuleTree a)

-- | Constructor for a set of alternative rules
alternatives :: [Rule a] -> RuleTree a
alternatives = RuleSet

-- | Constructor for a simple value
val :: a -> RuleTree a
val = Consequent

-- | Logical implication
(~=>) :: Predicate -> RuleTree a -> Rule a
(~=>) pred consequent = (pred, consequent)

infix 1 ~=>

-- | Make a rule weight into a predicate
weight :: Double -> Predicate
weight = id

fuzzifyBool :: Bool -> Predicate
fuzzifyBool True = 1.0
fuzzifyBool False = 0.0

-- | Build a function that uses a mapping from values to membership
-- functions to convert a value to a fuzzy set
fuzzify :: [(x, MF a)] -> a -> FuzzySet x
fuzzify membershipFuncs input = map applyInput membershipFuncs
   where
      applyInput (x, mf) = (x, mf input)

-- TODO: Maybe make this tail call recursive?
-- | Applies a rule tree to generate a consequent fuzzy set
runRuleTree :: (Ord a) => RuleTree a -> FuzzySet a
runRuleTree (Consequent a) = [(a, 1.0)]
runRuleTree (RuleSet rules) = combineConsequents $ concatMap runRule rules
   where
      combineConsequents allVars = map applyOr $ groupBy equalVars $ sort allVars
      equalVars (a, _) (b, _) = a == b
      applyOr group = foldl1' combineVars group
      combineVars (a, p1) (a', p2) = (a, p1 ~|| p2)

      runRule (pred, tree) = map (applyPred pred) $ runRuleTree tree
      applyPred pred (a, pred') = (a, pred ~&& pred')

iterSet :: FuzzySet a -> [(a, Double)]
iterSet = id

-- Logical operators

-- | The 'is' operator in fuzzy logic, e.g. waterTemp ~== Hot
(~==) :: (Eq x) => FuzzySet x -> x -> Predicate
(~==) set matcher = setLookup matcher set

-- | The negation of the ~/= operator
(~/=) :: (Eq x) => FuzzySet x -> x -> Predicate
(~/=) set matcher = not (setLookup matcher set)

-- | Fuzzy logic OR operator (PROBOR)
(~||) :: Predicate -> Predicate -> Predicate
(~||) a b = (a + b) - (a * b)

-- | Fuzzy logic AND operator (PROD)
(~&&) :: Predicate -> Predicate -> Predicate
(~&&) a b = a * b

-- Fuzzy logic negation
not :: Predicate -> Predicate
not val = 1 - val

infixr 2 ~||
infixr 3 ~&&
infix 4 ~/=
infix 4 ~==

setLookup :: (Eq x) => x -> FuzzySet x -> Double
setLookup k s = case Prelude.lookup k s of
                  Nothing -> 0.0
                  Just val -> val

-- Set combination functions

union :: (Ord x) => FuzzySet x -> FuzzySet x -> FuzzySet x
union s1 s2 = map combine $ groupBy valuesEqual $ sort $ s1 ++ s2
   where
      valuesEqual (a1, _) (a2, _) = a1 == a2
      combine items@((val, m):_) = (val, foldl1' (~||) $ map snd items)

-- Common membership functions

-- | Goes from 0.0 to 1.0 membership in a straight line
-- between a and b
up :: Double -> Double -> MF Double
up a b x
  | x < a = 0
  | x < b = (x - a) / (b - a)
  | otherwise = 1

-- | Goes from 1.0 to 0.0 membership in a straight line
-- between a and b
down :: Double -> Double -> MF Double
down a b x
    | x < a = 1.0
    | x < b = (x-b)/(a-b)
    | otherwise = 0.0


-- | Goes from 0.0 to 1.0 membership back to 0.0 in a triangle
-- shape from a to b to c
triangle :: Double -> Double -> Double -> MF Double
triangle a b c x | x <= a = 0
             | a <= x && x <= b = (x-a)/(b-a)
             | b <= x && x <= c = (c-x)/(c-b)
             | c <= x = 0

-- | Makes a gauss distribution with amplitude 1.0 centered at c
-- with standard deviation sig
gauss :: Double -> Double -> MF Double
gauss sig c x = let e = exp 1 in e**(top/bottom)
                where
                  top = negate $ (x-c)**2
                  bottom = 2*(sig**2)

-- | Goes from 0.0 to 1.0 to 1.0 and back to 0.0 in a trapezoid
-- shape from a to b to c to d
trapezoid :: Double -> Double -> Double -> Double -> MF Double
trapezoid a b c d x | x <= a || d <= x = 0
                | a <= x && x <= b = (x-a)/(b-a)
                | b <= x && x <= c = 1
                | c <= x && x <= d = (d-x)/(d-c)
                | otherwise = 0
