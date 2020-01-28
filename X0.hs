module X0
  ( selectInstructions,
    pprint,
    assignHomes,
    patch,
    printX86,
  )
where

import qualified C0
import Data.Char (toLower)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ****************************************************************************

type Label = String

type Var = String

type ProgramInfo = Set.Set Var

type BlockInfo = ()

data Register
  = RSP
  | RBP
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Show)

data Arg = Int Int | Reg Register | Deref Register Int | Var Var deriving (Show)

data Instr
  = Addq Arg Arg
  | Subq Arg Arg
  | Movq Arg Arg
  | Retq
  | Negq Arg
  | Callq Label
  | Pushq Arg
  | Popq Arg
  | Jmp Label
  deriving (Show)

data Block = Block BlockInfo [Instr] deriving (Show)

data X0 = Program ProgramInfo [(Label, Block)] deriving (Show)

-- ****************************************************************************

selectInstructions :: C0.C0 -> X0
selectInstructions (C0.Program i sections) =
  Program i (map (\(label, t) -> (label, Block () (selectT t))) sections)

selectT :: C0.Tail -> [Instr]
-- TODO(andrea): where should we jump
selectT (C0.Return a) = [Movq (selectA a) (Reg RAX), Jmp "end"]
selectT (C0.Seq s t) = (selectS s) ++ (selectT t)

selectS :: C0.Stmt -> [Instr]
selectS (C0.Assign x e) = selectE (selectA (C0.Var x)) e

selectE :: Arg -> C0.Expr -> [Instr]
selectE dst (C0.Arg a) = [Movq (selectA a) dst]
selectE dst (C0.Read) = [Callq "_read_int", Movq (Reg RAX) dst]
selectE dst (C0.Minus a) = [Movq (selectA a) dst, Negq dst]
selectE dst (C0.Plus a1 a2) = [Movq (selectA a1) dst, Addq (selectA a2) dst]

selectA :: C0.Arg -> Arg
selectA (C0.Int i) = Int i
selectA (C0.Var v) = Var v

-- ****************************************************************************

assignHomes :: X0 -> X0
assignHomes (Program vars blocks) =
  let s =
        zip (Set.toList vars) [1 ..]
          & map (\(v, i) -> (v, Deref RSP (i * 8)))
          & Map.fromList
      numVars = length vars
      stackSize =
        if ((numVars) * 8) `mod` 16 == 0
          then numVars * 8
          else (numVars + 1) * 8
   in Program
        vars
        ( ( "main",
            Block
              ()
              [ Pushq (Reg RBP),
                Movq (Reg RSP) (Reg RBP),
                Subq (Int stackSize) (Reg RSP),
                Jmp "body"
              ]
          )
            : (map (\(l, b) -> (l, assignB s b)) blocks)
            ++ [ ( "end",
                   Block
                     ()
                     [ Addq (Int stackSize) (Reg RSP),
                       Popq (Reg RBP),
                       Retq
                     ]
                 )
               ]
        )

type Subs = Map.Map Var Arg

assignB :: Subs -> Block -> Block
assignB s (Block i is) = Block i (map (assignI s) is)

assignI :: Subs -> Instr -> Instr
assignI s (Addq a1 a2) = Addq (assignA s a1) (assignA s a2)
assignI s (Subq a1 a2) = Subq (assignA s a1) (assignA s a2)
assignI s (Movq a1 a2) = Movq (assignA s a1) (assignA s a2)
assignI s (Retq) = Retq
assignI s (Negq a) = Negq (assignA s a)
assignI s (Callq l) = Callq l
assignI s (Jmp l) = Jmp l
assignI s (Pushq a) = Pushq (assignA s a)
assignI s (Popq a) = Popq (assignA s a)

assignA :: Subs -> Arg -> Arg
assignA _ (Int i) = Int i
assignA _ (Reg r) = Reg r
assignA _ (Deref r i) = Deref r i
assignA s (Var v) = s Map.! v

-- ****************************************************************************

-- The purpose of this pass is to make sure that each instruction adheres to
-- the restrictions regarding which arguments can be memory references.
-- For most instructions, the rule is that at most one argument may be a memory
-- reference
patch :: X0 -> X0
patch (Program vars blocks) = Program vars (map (\(l, b) -> (l, patchB b)) blocks)

patchB :: Block -> Block
patchB (Block i is) = Block i (concat (map patchI is))

patchI :: Instr -> [Instr]
patchI (Addq (Deref r1 i1) (Deref r2 i2)) = [Movq (Deref r1 i1) (Reg RAX), Addq (Reg RAX) (Deref r2 i2)]
patchI (Subq (Deref r1 i1) (Deref r2 i2)) = [Movq (Deref r1 i1) (Reg RAX), Subq (Reg RAX) (Deref r2 i2)]
patchI (Movq (Deref r1 i1) (Deref r2 i2)) = [Movq (Deref r1 i1) (Reg RAX), Movq (Reg RAX) (Deref r2 i2)]
patchI i = [i]

-- ****************************************************************************

printX86 :: X0 -> String
printX86 p = printX0 p 0
  where
    printX0 (Program i sections) l =
      ".globl main\n"
        ++ ( concat
               ( map
                   ( \(label, b) ->
                       label ++ ":" ++ (printBlock b (l + 2)) ++ "\n"
                   )
                   sections
               )
           )
    printBlock (Block i instrs) l =
      indent l ++ foldl (\acc instr -> acc ++ "\n" ++ indent l ++ printInstr instr) [] instrs
    printInstr (Retq) = "retq"
    printInstr (Callq l) = "callq " ++ l
    printInstr (Jmp l) = "jmp " ++ l
    printInstr (Negq a) = "negq " ++ (printA a)
    printInstr (Pushq a) = "pushq " ++ (printA a)
    printInstr (Popq a) = "popq " ++ (printA a)
    printInstr (Addq a1 a2) = "addq " ++ (printA a1) ++ ", " ++ (printA a2)
    printInstr (Subq a1 a2) = "subq " ++ (printA a1) ++ ", " ++ (printA a2)
    printInstr (Movq a1 a2) = "movq " ++ (printA a1) ++ ", " ++ (printA a2)
    printA (Int i) = "$" ++ show i
    printA (Reg r) = "%" ++ map toLower (show r)
    printA (Deref r i) = show i ++ "(%" ++ show r ++ ")"
    printA (Var _) = undefined

-- ****************************************************************************

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

indent :: Int -> String
indent l = duplicate " " l

pprint :: X0 -> String
pprint p = pprintC0 p 0
  where
    pprintC0 (Program i sections) l =
      "Program " ++ show i ++ "\n"
        ++ ( concat
               ( map
                   ( \(label, b) ->
                       indent (l + 2) ++ label ++ (pprintBlock b (l + 3 + 2)) ++ "\n"
                   )
                   sections
               )
           )
    pprintBlock (Block i instrs) l = indent l ++ foldl (\acc instr -> acc ++ "\n" ++ indent l ++ (show instr)) [] instrs
