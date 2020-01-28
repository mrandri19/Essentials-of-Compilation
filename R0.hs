module R0 (uniquify, R0 (..), Exp (..), interpret, pprint) where

import Control.Monad.State
import qualified Data.Map.Strict as Map

-- ****************************************************************************

type Var = String

type Info = ()

data Exp
  = Int Int
  | Read
  | Minus Exp
  | Plus Exp Exp
  | Var Var
  | Let String Exp Exp
  deriving (Show)

data R0 = Program Info Exp deriving (Show)

-- ****************************************************************************

interpret :: R0 -> Int
interpret (Program _ e) = interpretE Map.empty e

type Env = Map.Map Var Int

interpretE :: Env -> Exp -> Int
interpretE _ (Int i) = i
interpretE _ (Read) = 32
interpretE env (Minus e) = - (interpretE env e)
interpretE env (Plus e1 e2) = (interpretE env e1) + (interpretE env e2)
interpretE env (Var x) = env Map.! x
interpretE env (Let x xe be) =
  let env' = Map.insert x (interpretE env xe) env
   in interpretE env' be

-- ****************************************************************************

type Subs = Map.Map Var Var

-- Removes scopedness from vars
uniquify :: R0 -> R0
uniquify (Program i e) =
  let initialCounter = 1
      subs = Map.empty
   in let (e', _) = runState (uniquifyE subs e) initialCounter
       in (Program i e')

newVar :: State Int Var
newVar = do
  n <- get
  put (n + 1)
  return ("x." ++ (show n))

uniquifyE :: Subs -> Exp -> State Int Exp
uniquifyE s e@(Int _) = return e
uniquifyE s e@(Read) = return e
uniquifyE s (Minus e) = do
  e' <- uniquifyE s e
  return (Minus e')
uniquifyE s (Plus e1 e2) = do
  e1' <- uniquifyE s e1
  e2' <- uniquifyE s e2
  return (Plus e1' e2')
uniquifyE s e@(Var x) = do
  case (Map.lookup x s) of
    Just x' -> return (Var x')
    Nothing -> return e
uniquifyE s (Let x xe be) = do
  x' <- newVar
  let s' = Map.insert x x' s
  xe' <- uniquifyE s xe
  be' <- uniquifyE s' be
  return (Let x' xe' be')

-- ****************************************************************************

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

indent :: Int -> String
indent l = duplicate " " l

pprint :: R0 -> String
pprint p = pprintR0 p 0
  where
    pprintR0 (Program i e) l =
      "Program "
        ++ show i
        ++ "\n"
        ++ indent (l + 2)
        ++ (pprintExp e (l + 2))
    pprintExp (Let x xe be) l =
      "Let " ++ x ++ " = " ++ show xe ++ " in\n"
        ++ (indent (l + 2))
        ++ (pprintExp be (l + 2))
    pprintExp e l = show e
