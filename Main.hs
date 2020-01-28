module Main where

import qualified C0
import Data.Function ((&))
import qualified R0
import qualified RCO
import System.Exit (ExitCode (..))
import System.IO (IOMode (WriteMode), hClose, hPutStr, openFile)
import System.Process (callCommand, createProcess, shell, waitForProcess)
import qualified X0

main :: IO ()
main = do
  let p =
        R0.Program
          ()
          ( R0.Plus
              (R0.Plus (R0.Int 2) (R0.Int 3))
              (R0.Let "x" R0.Read (R0.Plus (R0.Var "x") (R0.Var "x")))
          )
  putStrLn ""
  putStrLn "***************** uniquify *****************"
  p
    & R0.uniquify
    & R0.pprint
    & putStrLn
  putStrLn ""
  putStrLn "***************** removeComplexOperations *****************"
  p
    & R0.uniquify
    & RCO.removeComplexOperations
    & RCO.pprint
    & putStrLn
  putStrLn ""
  putStrLn "***************** explicateControl *****************"
  p
    & R0.uniquify
    & RCO.removeComplexOperations
    & C0.explicateControl
    & C0.pprint
    & putStrLn
  putStrLn ""
  putStrLn "***************** uncoverLocals *****************"
  p
    & R0.uniquify
    & RCO.removeComplexOperations
    & C0.explicateControl
    & C0.uncoverLocals
    & C0.pprint
    & putStrLn
  putStrLn ""
  putStrLn "***************** selectInstructions *****************"
  p
    & R0.uniquify
    & RCO.removeComplexOperations
    & C0.explicateControl
    & C0.uncoverLocals
    & X0.selectInstructions
    & X0.pprint
    & putStrLn
  putStrLn ""
  putStrLn "***************** assignHomes *****************"
  p
    & R0.uniquify
    & RCO.removeComplexOperations
    & C0.explicateControl
    & C0.uncoverLocals
    & X0.selectInstructions
    & X0.assignHomes
    & X0.pprint
    & putStrLn
  putStrLn "***************** patch *****************"
  p
    & R0.uniquify
    & RCO.removeComplexOperations
    & C0.explicateControl
    & C0.uncoverLocals
    & X0.selectInstructions
    & X0.assignHomes
    & X0.patch
    & X0.pprint
    & putStrLn
  putStrLn "***************** printX86 *****************"
  let x86 =
        p
          & R0.uniquify
          & RCO.removeComplexOperations
          & C0.explicateControl
          & C0.uncoverLocals
          & X0.selectInstructions
          & X0.assignHomes
          & X0.patch
          & X0.printX86
  putStr x86
  putStrLn "***************** writing x86 *****************"
  h <- openFile "generated.s" WriteMode
  hPutStr h x86
  hClose h
  putStrLn "***************** compiling with runtime *****************"
  callCommand "clang runtime.c generated.s"
  putStrLn "***************** executing generate program *****************"
  (_, _, _, ph) <- createProcess (shell "./a.out")
  exitCode <- waitForProcess ph
  putStrLn
    ( "Compiled program output: "
        ++ show
          ( case exitCode of
              ExitSuccess -> 1
              ExitFailure i -> i
          )
    )
  putStrLn ("Interpreted program output: " ++ show (R0.interpret p))
  return ()
