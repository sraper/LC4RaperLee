{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Runner where
import Data.Vector ((//), (!), filter)
import Data.Map as Map hiding ((!))
import MachineStateWrapper
import LC4Parser
import Execute
import ParserCombinators
import Test.HUnit hiding (Label)
import DataModel

execS :: Insn -> MachineState -> MachineState
execS insn = execState (execute insn)

populateMemory :: LC4 -> MachineState -> MachineState
populateMemory [] ms = ms
populateMemory xs ms = Prelude.foldl pop (ms { pc = 0 }) xs where
    pop acc i = case i of
        Comment     -> acc
        Directive _ -> acc
        Memory t    -> acc { memory = (memory acc) // [(fromIntegral (pc acc), t)],
                         pc = (pc acc) + 1 }
        Label s     -> acc { labels = Map.insert s (pc acc) (labels acc),
                             pc = (pc acc) + 1 }

runLC4 :: LC4 -> IO ()
runLC4 insns = let ms = populateMemory insns simpMachine
                   ms' = execProg (ms { pc = 0 }) in
               printMS ms'

execProg :: MachineState -> MachineState
execProg ms = let insn = (memory ms) ! (fromIntegral (pc ms)) in
              case insn of
                DataVal _ -> ms -- MIGHT NOT BE NOP THAT WE WANT AS FINAL
                InsnVal i -> execProg $ execState (execute i) ms

main :: IO ()
main = do s <- parseFromFile lc4P "sample.asm"
          case s of
            (Left _) -> print "f up"
            (Right x) -> runLC4 $ reverse x
          return ()

testPopulateMemory :: IO ()
testPopulateMemory = do s <- parseFromFile lc4P "sample.asm"
                        case s of
                          (Left _) -> print "f up"
                          (Right x) -> printMS $ populateMemory x simpMachine
                        return ()

testExecuteJSRR :: IO ()
testExecuteJSRR = let a = Unary JSRR (R 5)
                  in runOneInsn a

runOneInsn :: Insn -> IO ()
runOneInsn insn = let ms = execS insn simpMachine in
           printMS ms

printMS :: MachineState -> IO ()
printMS ms = do putStr "pc: "
                print $ pc ms
                putStr "priv: "
                print $ priv ms
                let (n, z, p) = nzp ms
                putStr "nzp: "
                if n then (print 1) else (print 0)
                if z then (print 1) else (print 0)
                if p then (print 1) else (print 0)
                putStr "regs: "
                print $ regs ms
                putStr "labels: "
                print $ labels ms
                putStr "memory: "
                printPopulatedMemory ms

printPopulatedMemory :: MachineState -> IO()
printPopulatedMemory ms = do let mem = memory ms
                                 filt = Data.Vector.filter (\x -> if x == DataVal 0 then False else True) mem
                             print filt

-- TESTS
tNOP :: Test
tNOP = execS (Single NOP) simpMachine ~?= simpMachine { pc = 1 }

tRTI :: Test
tRTI = execS (Single RTI) m ~?= m { pc = 7, priv = False }
       where m = simpMachine { priv = True }

tJSRR :: Test
tJSRR = execS (Unary JSRR (R 1)) simpMachine ~?= 
        simpMachine { regs = (regs simpMachine) // [(7,1)], pc = 1, nzp = (False, False, True) }

tJMP :: Test
tJMP = execS (Unary JMP (LABEL "lab")) simpMachine ~?= simpMachine { pc = 11 }

tJMP2 :: Test
tJMP2 = execS (Unary JMP (IMM 10)) simpMachine ~?= simpMachine { pc = 11 }

tJMPR :: Test
tJMPR = execS (Unary JMPR (R 5)) simpMachine ~?= simpMachine { pc = 5 }

tTRAP :: Test
tTRAP = execS (Unary TRAP (IMM 10)) simpMachine ~?=
        simpMachine { priv = True, regs = (regs simpMachine) // [(7, 1)], pc = 32778, nzp = (False, False, True) }

tBR :: Test
tBR = execS (Unary BRn (IMM 10)) simpMachine ~?= simpMachine { pc = 1 }

tBR2 :: Test
tBR2 = execS (Unary BRn (IMM 10)) m ~?= m { pc = 11 }
       where m = simpMachine { nzp = (True, False, False) }

tBR3 :: Test
tBR3 = execS (Unary BRn (LABEL "lab")) m ~?= m { pc = 11 }
       where m = simpMachine { nzp = (True, False, False) }

tCMP :: Test
tCMP = execS (Binary CMP (R 1) (R 2)) m ~?= m { pc = 1, nzp = (False, False, True) }
       where m = simpMachine { regs = (regs simpMachine) // [(2, -1)] }

tCMPU :: Test
tCMPU = execS (Binary CMPU (R 1) (R 2)) m ~?= m { pc = 1, nzp = (True, False, False) }
        where m = simpMachine { regs = (regs simpMachine) // [(2, -1)] }

tCMPIU :: Test
tCMPIU = execS (Binary CMPIU (R 1) (IMM (-1))) simpMachine ~?=
         simpMachine { pc = 1, nzp = (True, False, False) }

tCMPI :: Test
tCMPI = execS (Binary CMPI (R 1) (IMM (-1))) simpMachine ~?=
        simpMachine { pc = 1, nzp = (False, False, True) }

tADD :: Test
tADD = execS (Ternary ADD (R 1) (R 2) (R 3)) simpMachine ~?= 
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 5)], nzp = (False, False, True) }

tSUB :: Test
tSUB = execS (Ternary SUB (R 1) (R 2) (R 3)) m ~?=
       m { pc = 1, regs = (regs m) // [(1, 3)], nzp = (False, False, True) }
       where m = simpMachine { regs = (regs simpMachine) // [(3, -1)] }

tADD2 :: Test
tADD2 = execS (Ternary ADD (R 1) (R 2) (IMM 3)) simpMachine ~?= 
        simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 5)], nzp = (False, False, True) }

tMOD :: Test
tMOD = execS (Ternary MOD (R 1) (R 5) (R 3)) simpMachine ~?=
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 2)], nzp = (False, False, True) }

tAND :: Test
tAND = execS (Ternary AND (R 1) (R 2) (R 6)) simpMachine ~?=
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 2)], nzp = (False, False, True) }

tAND2 :: Test
tAND2 = execS (Ternary AND (R 1) (R 2) (IMM 6)) simpMachine ~?=
        simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 2)], nzp = (False, False, True) }

tSLL :: Test
tSLL = execS (Ternary SLL (R 1) (R 2) (IMM 2)) simpMachine ~?=
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 8)], nzp = (False, False, True) }

tSRA :: Test
tSRA = execS (Ternary SRL (R 1) (R 2) (IMM 2)) m ~?=
       m { pc = 1, regs = (regs m) // [(1, 16383)], nzp = (False, False, True) }
       where m = simpMachine { regs = (regs simpMachine) // [(2, -1)]}

tSRL :: Test
tSRL = execS (Ternary SRA (R 1) (R 2) (IMM 2)) m ~?=
       m { pc = 1, regs = (regs m) // [(1, -1)], nzp = (True, False, False)}
       where m = simpMachine { regs = (regs simpMachine) // [(2, -1)]}

tLDR :: Test
tLDR = execS (Ternary LDR (R 1) (R 2) (IMM 3)) m ~?=
       m { pc = 1, regs = (regs m) // [(1, 10)] }
       where m = simpMachine { memory = (memory simpMachine) // [(5, DataVal 10)] }

tSTR :: Test
tSTR = execS (Ternary STR (R 1) (R 2) (IMM 3)) simpMachine ~?=
       simpMachine { pc = 1, memory = (memory simpMachine) // [(5, DataVal 1)] }

tCONST :: Test
tCONST = execS (Binary CONST (R 1) (IMM 10)) simpMachine ~?=
         simpMachine { pc = 1, memory = (memory simpMachine) // [(1, DataVal 10)], nzp = (False, False, True) }

tLEA :: Test
tLEA = execS (Binary LEA (R 1) (LABEL "lab")) simpMachine ~?=
       simpMachine { pc = 1, regs = (regs simpMachine) // [(1, 10)], nzp = (False, False, True) }

tLC :: Test
tLC = execS (Binary LC (R 1) (LABEL "lab")) m ~?=
      m { pc = 1, regs = (regs m) // [(1, 3)] }
      where m = simpMachine { memory = (memory simpMachine) // [(10, DataVal 3)], nzp = (False, False, True) }
      
allT :: IO ()
allT = do _ <- runTestTT (TestList [ tNOP, tRTI, tJSRR, tJMP, tJMP2, tJMPR, tTRAP,
                                     tBR, tBR2, tBR3, tCMP, tCMPU, tCMPIU, tCMPI,
                                     tADD, tSUB, tADD2, tMOD, tAND, tAND2, tSLL,
                                     tSRL, tSRA, tLDR, tSTR, tCONST, tLEA, tLC ])
          return ()