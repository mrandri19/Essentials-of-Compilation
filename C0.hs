module C0
  ( explicateControl,
    pprint,
    uncoverLocals,
    C0 (..),
    Tail (..),
    Stmt (..),
    Expr (..),
    Arg (..),
  )
where

import qualified Data.Set as Set
import qualified RCO

-- ****************************************************************************

type Label = String

type Var = String

type Info = Set.Set Var

data C0 = Program Info [(Label, Tail)] deriving (Show)

data Tail = Return Arg | Seq Stmt Tail deriving (Show)

data Stmt = Assign Var Expr deriving (Show)

data Expr = Arg Arg | Read | Minus Arg | Plus Arg Arg deriving (Show)

data Arg = Int Int | Var Var deriving (Show)

-- ****************************************************************************

explicateControl :: RCO.R0co -> C0
explicateControl (RCO.Program () e) = Program Set.empty [("body", econE e)]

econE :: RCO.Exp -> Tail
econE (RCO.Arg a) = Return (econA a)
econE (RCO.Let x xe be) = Seq (Assign x (econC xe)) (econE be)

econC :: RCO.Cplx -> Expr
econC (RCO.Read) = Read
econC (RCO.Minus a) = Minus (econA a)
econC (RCO.Plus a1 a2) = Plus (econA a1) (econA a2)

econA :: RCO.Arg -> Arg
econA (RCO.Int i) = Int i
econA (RCO.Var v) = Var v

-- ****************************************************************************

uncoverLocals :: C0 -> C0
uncoverLocals (Program vars sections) =
  let vars' = Set.unions (map (\(_, tail) -> localsT tail) sections)
   in Program vars' sections

localsT :: Tail -> Set.Set Var
localsT (Return a) = localsA a
localsT (Seq s t) = (localsS s) `Set.union` localsT t

localsS :: Stmt -> Set.Set Var
localsS (Assign v e) = (Set.singleton v) `Set.union` localsE e

localsE :: Expr -> Set.Set Var
localsE (Arg a) = localsA a
localsE (Read) = Set.empty
localsE (Minus a) = localsA a
localsE (Plus a1 a2) = localsA a1 `Set.union` localsA a2

localsA :: Arg -> Set.Set Var
localsA (Int _) = Set.empty
localsA (Var v) = Set.singleton v

-- ****************************************************************************

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

indent :: Int -> String
indent l = duplicate " " l

pprint :: C0 -> String
pprint p = pprintC0 p 0
  where
    pprintC0 (Program i sections) l =
      "Program " ++ show i ++ "\n"
        ++ indent (l + 2)
        ++ ( concat
               ( map
                   ( \(label, t) ->
                       label ++ "\n" ++ indent (l + 2 + 2) ++ (pprintTail t (l + 2 + 2))
                   )
                   sections
               )
           )
    pprintTail (Return a) l = "Return " ++ show a
    pprintTail (Seq stmt t) l = show stmt ++ "\n" ++ indent l ++ pprintTail t l
