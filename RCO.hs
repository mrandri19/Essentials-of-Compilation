module RCO (R0co (..), removeComplexOperations, pprint, Exp (..), Cplx (..), Arg (..)) where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId)
import qualified R0

-- ****************************************************************************

data R0co = Program Info Exp deriving (Show)

data Exp = Arg Arg | Let Var Cplx Exp deriving (Show)

data Cplx = Read | Minus Arg | Plus Arg Arg deriving (Show)

data Arg = Int Int | Var Var deriving (Show)

type Info = ()

type Var = String

-- ****************************************************************************

removeComplexOperations :: R0.R0 -> R0co
removeComplexOperations (R0.Program i e) =
  let initialCounter = 1
      subs = Map.empty
   in let ((s, a), _) = runState (rcoE subs e) initialCounter
       in let r = foldl (\acc (key, value) -> (Let key value acc)) (Arg a) (reverse (Map.assocs s))
           in Program i r

newVar :: State Int Var
newVar = do
  n <- get
  put (n + 1)
  return ("x." ++ (show n))

type Subs = Map.Map Var Arg

-- Remove Complex Operations from an Exp
rcoE :: Subs -> R0.Exp -> State Int (Map.Map Var Cplx, Arg)
rcoE _ (R0.Int n) = return (Map.empty, Int n)
rcoE s (R0.Var x) =
  return
    ( Map.empty,
      case (Map.lookup x s) of
        Just x' -> x'
        Nothing -> (Var x)
    )
rcoE _ R0.Read = do
  x <- newVar
  return (Map.singleton x Read, Var x)
rcoE s (R0.Plus e1 e2) = do
  (nv1, e1') <- rcoE s e1
  (nv2, e2') <- rcoE s e2
  x <- newVar
  return
    ( (Map.union (Map.union nv1 nv2) (Map.singleton x (Plus e1' e2'))),
      (Var x)
    )
rcoE s (R0.Minus e) = do
  (nv, e') <- rcoE s e
  x <- newVar
  return
    ( (Map.union nv (Map.singleton x (Minus e'))),
      Var x
    )
rcoE s (R0.Let x xe be) = do
  (nvx, xe') <- rcoE s xe
  let s' = Map.insert x xe' s
  (nvb, be') <- rcoE s' be
  x <- newVar
  return
    ( (Map.union nvx nvb),
      be'
    )

-- ****************************************************************************

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

indent :: Int -> String
indent l = duplicate " " l

pprint :: R0co -> String
pprint p = pprintR0co p 0
  where
    pprintR0co (Program i e) l =
      "Program " ++ show i ++ "\n" ++ indent (l + 2) ++ (pprintExp e (l + 2))
    pprintExp (Let x xe be) l =
      "Let " ++ x ++ " = " ++ show xe ++ " in\n"
        ++ (indent (l + 2))
        ++ (pprintExp be (l + 2))
    pprintExp e l = show e
