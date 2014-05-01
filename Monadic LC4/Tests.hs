{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}
module Tests where 

import Prelude
import Test.HUnit hiding (Label, State)
import Data.Vector ((//), replicate, fromList)
import Data.Map as Map hiding ((!))

import DataModel
import LC4


-- | A Simple Machine for Testing
simpMachine :: MachineState
simpMachine = MachineState {
                 pc = 0,
                 nzp = (False, False, False),
                 regs = Data.Vector.fromList [0..7],
                 priv = True,
                 memory = Data.Vector.replicate 65535 (DataVal 0),
                 labels = Map.fromList [("lab", 10)]
              }

tNOP :: Test
tNOP = execOnce (Single NOP) simpMachine ~?= (Right $ simpMachine { pc = 1 })

tRTI :: Test
tRTI = execOnce (Single RTI) m ~?= (Right $ m { pc = 7, priv = False })
       where m = simpMachine { priv = True }

tJSRR :: Test
tJSRR = execOnce (Unary JSRR (R 1)) simpMachine ~?= 
        (Right $ simpMachine { regs = (regs simpMachine) // [(7,1)], pc = 1, nzp = (False, False, True) })

tJMP :: Test
tJMP = execOnce (Unary JMP (LABEL "lab")) simpMachine ~?= (Right $ simpMachine { pc = 10 })

tJMP2 :: Test
tJMP2 = execOnce (Unary JMP (IMM 10)) simpMachine ~?= (Right $ simpMachine { pc = 10 })

tJMPR :: Test
tJMPR = execOnce (Unary JMPR (R 5)) simpMachine ~?= (Right $ simpMachine { pc = 5 })

tTRAP :: Test
tTRAP = execOnce (Unary TRAP (IMM 10)) simpMachine ~?=
        (Right $ simpMachine { priv = True, regs = (regs simpMachine) // [(7, 1)], pc = 32778, nzp = (False, False, True) })

tBR :: Test
tBR = execOnce (Unary BRn (IMM 10)) simpMachine ~?= (Right $ simpMachine { pc = 1 })

tBR2 :: Test
tBR2 = execOnce (Unary BRn (IMM 10)) m ~?= (Right $ m { pc = 10 })
       where m = simpMachine { nzp = (True, False, False) }

tBR3 :: Test
tBR3 = execOnce (Unary BRn (LABEL "lab")) m ~?= (Right $ m { pc = 10 })
       where m = simpMachine { nzp = (True, False, False) }

tBR4 :: Test
tBR4 = execOnce (Unary BRnzp (IMM 10)) m ~?= (Right $ m { pc = 10 })      
       where m = emptyMachine { nzp = (True, False, False) }

tCMP :: Test
tCMP = execOnce (Binary CMP (R 1) (R 2)) m ~?= (Right $ m { pc = 1, nzp = (False, False, True) })
       where m = simpMachine { regs = (regs simpMachine) // [(2, -1)] }

tCMPU :: Test
tCMPU = execOnce (Binary CMPU (R 1) (R 2)) m ~?= (Right $ m { pc = 1, nzp = (True, False, False) })
        where m = simpMachine { regs = (regs simpMachine) // [(2, -1)] }

tCMPIU :: Test
tCMPIU = execOnce (Binary CMPIU (R 1) (IMM (-1))) simpMachine ~?=
         (Right $ simpMachine { pc = 1, nzp = (True, False, False) })

tCMPI :: Test
tCMPI = execOnce (Binary CMPI (R 1) (IMM (-1))) simpMachine ~?= (Right $ 
        simpMachine { pc = 1, nzp = (False, False, True) })

tADD :: Test
tADD = execOnce (Ternary ADD (R 1) (R 2) (R 3)) simpMachine ~?= (Right $  
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 5)], nzp = (False, False, True) })

tADDI :: Test
tADDI = execOnce (Ternary ADD (R 1) (R 2) (IMM (-7))) simpMachine ~?= (Right $
        simpMachine {pc = 1, regs = (regs simpMachine) // [(1, -5)], nzp = (True, False, False)})

tSUB :: Test
tSUB = execOnce (Ternary SUB (R 1) (R 2) (R 3)) m ~?= (Right $ 
       m { pc = 1, regs = (regs m) // [(1, 3)], nzp = (False, False, True) })
       where m = simpMachine { regs = (regs simpMachine) // [(3, -1)] }


-- Why did i fail here?
tSUB2 :: Test
tSUB2 = execOnce (Ternary SUB (R 1) (R 2) (R 3)) simpMachine ~?= (Right $ 
        simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 3)], nzp = (True, False, False) })

tADD2 :: Test
tADD2 = execOnce (Ternary ADD (R 1) (R 2) (IMM 3)) simpMachine ~?= (Right $  
        simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 5)], nzp = (False, False, True) })

tMOD :: Test
tMOD = execOnce (Ternary MOD (R 1) (R 5) (R 3)) simpMachine ~?= (Right $ 
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 2)], nzp = (False, False, True) })

tAND :: Test
tAND = execOnce (Ternary AND (R 1) (R 2) (R 6)) simpMachine ~?= (Right $ 
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 2)], nzp = (False, False, True) })

tAND2 :: Test
tAND2 = execOnce (Ternary AND (R 1) (R 2) (IMM 6)) simpMachine ~?= (Right $ 
        simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 2)], nzp = (False, False, True) })

tSLL :: Test
tSLL = execOnce (Ternary SLL (R 1) (R 2) (IMM 2)) simpMachine ~?= (Right $ 
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 8)], nzp = (False, False, True) })

tSRA :: Test
tSRA = execOnce (Ternary SRL (R 1) (R 2) (IMM 2)) m ~?= (Right $ 
       m { pc = 1, regs = (regs m) // [(1, 16383)], nzp = (False, False, True) })
       where m = simpMachine { regs = (regs simpMachine) // [(2, -1)]}

tSRL :: Test
tSRL = execOnce (Ternary SRA (R 1) (R 2) (IMM 2)) m ~?= (Right $ 
       m { pc = 1, regs = (regs m) // [(1, -1)], nzp = (True, False, False)})
       where m = simpMachine { regs = (regs simpMachine) // [(2, -1)]}

tLDR :: Test
tLDR = execOnce (Ternary LDR (R 1) (R 2) (IMM 3)) m ~?= (Right $ 
       m { pc = 1, regs = (regs m) // [(1, 10)], nzp = (False, False, True) })
       where m = simpMachine { memory = (memory simpMachine) // [(5, DataVal 10)] }

tSTR :: Test
tSTR = execOnce (Ternary STR (R 1) (R 2) (IMM 3)) simpMachine ~?= (Right $ 
       simpMachine { pc = 1, memory = (memory simpMachine) // [(5, DataVal 1)] })

tCONST :: Test
tCONST = execOnce (Binary CONST (R 1) (IMM 10)) simpMachine ~?= (Right $ 
         simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 10)], nzp = (False, False, True) })

tLEA :: Test
tLEA = execOnce (Binary LEA (R 1) (LABEL "lab")) simpMachine ~?= (Right $ 
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 10)], nzp = (False, False, True) })

tLC :: Test
tLC = execOnce (Binary LC (R 1) (LABEL "lab")) m ~?= (Right $ 
      m { pc = 1, regs = (regs m) // [(1, 3)] })
      where m = simpMachine { memory = (memory simpMachine) // [(10, DataVal 3)], nzp = (False, False, True) }

runTests :: IO ()
runTests = do _ <- runTestTT (
                TestList [ tNOP, tRTI, tJSRR, tJMP, tJMP2, tJMPR, tTRAP,
                           tBR, tBR2, tBR3, tCMP, tCMPU, tCMPIU, tCMPI,
                           tADD, tSUB, tADD2, tMOD, tAND, tAND2, tSLL,
                           tSRL, tSRA, tLDR, tSTR, tCONST, tLEA, tLC ])
              return ()