{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module LC4 where

import Prelude
import Data.Vector (Vector, (!), (//), update, singleton, replicate, fromList, filter)
import Data.Map as Map hiding ((!))
import Data.Word (Word16)
import Data.Int (Int16)
import Data.Bits
import Control.Monad.State
import Control.Monad.Error
import DataModel
import LC4Parser hiding (main)
import ParserCombinators
import Test.HUnit hiding (Label, State)

type MLC4 = ErrorT LC4Error (State MachineState)

data MachineState = 
     MachineState { pc :: Word16,
                    nzp :: (Bool, Bool, Bool),
                    regs :: Vector Word16,
                    priv :: Bool,
                    memory :: Vector MemVal,
                    labels :: Map String Word16 }
                    deriving (Eq)

type Delta = [Change]

data Change = SetPC Word16
            | IncPC
            | SetNZP (Bool, Bool, Bool)
            | SetReg Int Word16
            | SetPriv Bool
            | SetMem Int MemVal
            | SetLabel String Word16

data LC4Error = SomeError String
              | NoSuchInstruction
              | DivisionByZero
              | IllegalMemAccess
              | IllegalInsnAccess
              deriving (Eq)

instance Error LC4Error where
  noMsg = SomeError "Unknown error"
  strMsg s = SomeError s

instance Show LC4Error where
  show (NoSuchInstruction)   = "Instruction does not exist"
  show (DivisionByZero)      = "Cannot divide by zero"
  show (IllegalMemAccess)    = "Cannot access memory" --NEED TO IMPLEMENT
  show (IllegalInsnAccess)   = "Cannot access Insn"
  show (SomeError msg)       = msg

instance Show MachineState where
  show (MachineState _pc _nzp _regs _priv _memory _labels) = 
        "\npc: " ++ show _pc ++
        "\npriv: " ++ show _priv ++
        "\nnzp: " ++ (if n then "1" else "0") ++
        (if z then "1" else "0") ++
        (if p then "1" else "0") ++
        "\nregs: " ++ show _regs ++
        "\nlabels: " ++ show _labels ++
        "\nmemory: " ++ showPopulatedMemory _memory ++
        "\n--------------------------------------"
    where (n, z, p) = _nzp

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
                print $ memory ms

-- | Convert populated portion of memory into a string
showPopulatedMemory :: Vector MemVal -> String
showPopulatedMemory mem = show ( Data.Vector.filter (/= DataVal 0) mem )

-- | Initial MachineState
emptyMachine :: MachineState
emptyMachine = MachineState {
                 pc = 0,
                 nzp = (False, True, False),
                 regs = Data.Vector.replicate 8 0,
                 priv = False,
                 memory = Data.Vector.replicate 65535 (DataVal 0),
                 labels = Map.empty
               }

-- | Fetches the next insn
fetch :: (MonadState MachineState m, MonadError LC4Error m) => m Insn
fetch = do ms <- get
           let insn = (memory ms) ! (fromIntegral (pc ms))
           case insn of
             InsnVal i -> return i
             DataVal _ -> throwError $ SomeError "impossible" --FIX??

-- | Helper function that handles arithmetic or logical operations
arithOrLogic :: (Word16, Word16, Int) -> (Word16 -> Word16 -> Word16) -> Delta
arithOrLogic (rsv, rtv, rd) f = let res = f rsv rtv in
  [SetReg rd res, SetNZP $ calcNZP $ word16ToInt16 res, IncPC ]


-- | Helper function that returns value of the register with given index
getRegVal :: MachineState -> Int -> Word16
getRegVal ms i = (regs ms) ! i

-- | Helper function that determines NZP bits based on input number
calcNZP :: (Num a, Eq a, Ord a) => a -> (Bool, Bool, Bool)
calcNZP x | x < 0  = (True, False, False) -- N
calcNZP x | x == 0 = (False, True, False) -- Z
calcNZP _          = (False, False, True) -- P


-- | Helper function that returns True if any of the NZP bits match the input
matchNZP :: (Bool, Bool, Bool) -> (Bool, Bool, Bool) -> Bool
matchNZP (n, z, p) (n', z', p') = ( n == True && n' == True ) ||
                                  ( z == True && z' == True ) ||
                                  ( p == True && p' == True )

-- | Helper function to handle branching
branchLogic :: (MonadState MachineState m, MonadError LC4Error m) => 
               Tok -> Bool -> m Delta
branchLogic tok p = 
  do ms <- get
     case p of
       True -> case tok of
                 LABEL l -> return [SetPC $ (Map.findWithDefault 0 l $ labels ms)]
                 IMM i   -> return [SetPC ((pc ms ) + (fromIntegral i))]
                 _       -> throwError $ SomeError "Branch Error" --NEED ERROR
       False -> return [IncPC]

-- | Decoder
decodeInsn :: (MonadState MachineState m, MonadError LC4Error m) => Insn -> m Delta
decodeInsn i = 
  do ms <- get
     case i of
       (Single NOP)         -> return [IncPC]
       (Single RTI)         -> let r7_val = (regs ms) ! 7 in
                               return [ SetPriv False, SetPC r7_val ]
       (Unary JSRR (R rs))  -> return 
                               [ SetReg 7 $ 1 + (pc ms),
                                 SetNZP $ calcNZP $ 1 + (pc ms),
                                 SetPC $ (regs ms) ! rs ]
       (Unary JSR t)        -> ---------FIX cases not covered???
         case t of
              LABEL l -> let add = Map.findWithDefault 0 l $ labels ms in
                         return [ SetPC add ]
              _       -> throwError $ SomeError "JSR: not label"
       (Unary JMP t)        -> 
         case t of
              LABEL l -> let add = Map.findWithDefault 0 l $ labels ms in
                         return [ SetPC add ]
              IMM i   -> let add = intToWord16 i in
                         return [ SetPC $ (pc ms) + add ]
              _       -> throwError $ SomeError "JMP: cannot take regVal"
       (Unary JMPR (R rs))  -> return [ SetPC $ (regs ms) ! rs ]
       (Single RET)         -> decodeInsn (Unary JMPR (R 7))
       (Unary TRAP (IMM i)) -> let newpcv = i + 0x8000 in
                               return [ SetReg 7 $ (pc ms) + 1,
                                        SetPriv True,
                                        SetNZP $ calcNZP $ (pc ms) + 1,
                                        SetPC $ fromIntegral newpcv ]

       -------------------------------------------------------------------------------
       ---------------------------------- BRANCHES -----------------------------------
       -------------------------------------------------------------------------------
       (Unary BRn l)      -> branchLogic l (matchNZP (nzp ms) (True, False, False))
       (Unary BRnz l)     -> branchLogic l (matchNZP (nzp ms) (True, True, False))
       (Unary BRz l)      -> branchLogic l (matchNZP (nzp ms) (False, True, False))
       (Unary BRzp l)     -> branchLogic l (matchNZP (nzp ms) (False, True, True))
       (Unary BRnp l)     -> branchLogic l (matchNZP (nzp ms) (True, False, True))
       (Unary BRp l)      -> branchLogic l (matchNZP (nzp ms) (False, False, True))
       (Unary BRnzp l)    -> branchLogic l (matchNZP (nzp ms) (True, True, True))
       -------------------------------------------------------------------------------
       --------------------------------- COMPARES ------------------------------------
       -------------------------------------------------------------------------------
       (Binary CMP (R rs) (R rt)) -> 
               let rsv = word16ToInt16 $ (regs ms) ! rs
                   rtv = word16ToInt16 $ (regs ms) ! rt in
               return [ SetNZP $ calcNZP $ rsv - rtv, IncPC ]
       (Binary CMPU (R rs) (R rt)) -> 
               let rsv = word16ToInt $ (regs ms) ! rs
                   rtv = word16ToInt $ (regs ms) ! rt in
               return [ SetNZP $ calcNZP $ rsv - rtv, IncPC ]
       (Binary CMPI (R rs) (IMM i)) -> 
               let rsv = word16ToInt16 $ (regs ms) ! rs
                   iv = (fromIntegral i) :: Int16 in
               return [ SetNZP $ calcNZP $ rsv - iv, IncPC ]
       (Binary CMPIU (R rs) (IMM i)) ->
               let rsv = word16ToInt $ (regs ms) ! rs
                   unsignedi = (fromIntegral i) :: Word16
                   expandi = (fromIntegral unsignedi) :: Int in
               return [ SetNZP $ calcNZP $ rsv - expandi, IncPC ]
       -------------------------------------------------------------------------------
       ------------------------------ ARITHMETIC OPS ---------------------------------
       -------------------------------------------------------------------------------
       (Ternary ADD (R rd) (R rs) (R rt)) ->
               return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (+)
       (Ternary MUL (R rd) (R rs) (R rt)) ->
               return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (*)
       (Ternary SUB (R rd) (R rs) (R rt)) ->
               return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (-)
       (Ternary DIV (R rd) (R rs) (R rt)) ->
               let rsv = (regs ms) ! rs
                   rtv = (regs ms) ! rt in
               if(rtv == 0) then throwError $ DivisionByZero
               else return [ SetReg rd (rsv `div` rtv),
                    SetNZP $ calcNZP $ word16ToInt16 $ rsv `div` rtv, IncPC ]
       (Ternary ADD (R rd) (R rs) (IMM i)) ->
               return $ arithOrLogic (getRegVal ms rs, intToWord16 i, rd) (+)
       (Ternary MOD (R rd) (R rs) (R rt)) ->
               return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (mod)
       -------------------------------------------------------------------------------
       ------------------------------- LOGICAL OPS -----------------------------------
       -------------------------------------------------------------------------------
       (Ternary AND (R rd) (R rs) (R rt)) -> 
               return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (.&.)
       (Binary NOT (R rd) (R rs)) ->
               let rsv = (regs ms) ! rs in
               return [ SetReg rd $ complement rsv,
                        SetNZP $ calcNZP $ word16ToInt16 $ complement rsv, IncPC ]
       (Ternary OR (R rd) (R rs) (R rt)) ->
               return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (.|.)
       (Ternary XOR (R rd) (R rs) (R rt)) ->
                return $ arithOrLogic (getRegVal ms rs, getRegVal ms rt, rd) (xor)
       (Ternary AND (R rd) (R rs) (IMM i)) ->
               return $ arithOrLogic (getRegVal ms rs, intToWord16 i, rd) (.&.)
       -------------------------------------------------------------------------------
       ---------------------------------- SHIFTS -------------------------------------
       -------------------------------------------------------------------------------
       (Ternary SLL (R rd) (R rs) (IMM i)) -> 
               let rsv = (regs ms) ! rs in
               return [ SetReg rd $ shiftL rsv i,
                        SetNZP $ calcNZP $ shiftL rsv i, IncPC ]
       (Ternary SRL (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs in
               return [ SetReg rd $ shiftR rsv i,
                        SetNZP $ calcNZP $ shiftR rsv i, IncPC ]
       (Ternary SRA (R rd) (R rs) (IMM i)) -> 
               let rsv = fromIntegral ((regs ms) ! rs) :: Int16
                   shifted = fromIntegral $ shiftR rsv i :: Word16 in
               return [ SetReg rd shifted,
                        SetNZP $ calcNZP $ shiftR rsv i, IncPC ]
       -------------------------------------------------------------------------------
       ------------------------------ MEMORY ACCESS ----------------------------------
       -------------------------------------------------------------------------------
       (Ternary LDR (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs
                   addr = (word16ToInt rsv) + i
                   val = (memory ms) ! addr in
               case val of
                    DataVal d -> return [ SetReg rd d,
                                          SetNZP $ calcNZP d, IncPC]
                    _ -> throwError $ SomeError "LDR" -- NEED ERROR
       (Ternary STR (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs
                   addr = (word16ToInt rsv) + i
                   val = (regs ms) ! rd in
               return [ SetMem addr $ DataVal val, IncPC ]
       (Binary CONST (R rd) (IMM i)) ->
               return [ SetReg rd $ intToWord16 i,
                        SetNZP $ calcNZP i, IncPC ]
       (Binary LEA (R r1) (LABEL l)) ->
               let addr = Map.findWithDefault 0 l $ labels ms in
               return [ SetReg r1 addr, 
                        SetNZP $ calcNZP addr, IncPC ]
       (Binary LC (R r1) (LABEL l)) ->
               let addr = Map.findWithDefault 0 l $ labels ms
                   val = (memory ms) ! word16ToInt addr in
               case val of
                 DataVal d -> return [ SetReg r1 d, 
                                       SetNZP $ calcNZP d, IncPC]
                 _         -> throwError $ SomeError "LC" -- NEED ERROR
       _  -> throwError $ NoSuchInstruction

-- | Updates MachineState using input list of changes
updateMachineState :: Delta -> MachineState -> MachineState
updateMachineState [] ms     = ms
updateMachineState (x:xs) ms = 
      case x of
       (SetPC v)      -> update xs (ms { pc = v })
       (IncPC)        -> update xs (ms { pc = (pc ms) + 1 })
       (SetNZP v)     -> update xs (ms { nzp = v })
       (SetReg r v)   -> update xs (ms { regs = (regs ms) // [(r, v)] })
       (SetPriv v)    -> update xs (ms { priv = v })
       (SetMem i v)   -> update xs (ms { memory = (memory ms) // [(i, v)] })
       (SetLabel l v) -> update xs (ms { labels = (Map.insert l v (labels ms)) })
       where update = updateMachineState

-- | Preprocessing method that handles directives & loads instructions into memory
populateMemory :: LC4 -> MachineState -> MachineState
populateMemory [] ms = ms
populateMemory xs ms = 
    Prelude.foldl pop (ms { pc = 0 }) xs where
      pop acc i = case i of
        Comment            -> acc
        Directive (FALIGN) -> case (pc acc) `mod` 16 of
                                0 -> acc
                                x -> acc { pc = (pc acc) + (16 - x) }
        Directive (ADDR a) -> acc { pc = a }
        Directive (FILL v) -> acc { memory = (memory acc) // [(fromIntegral (pc acc), DataVal v)], pc = (pc acc) + 1 }
        Directive (BLKW v) -> acc { pc = (pc acc) + v }
        Directive _        -> acc
        Memory t           -> acc { memory = (memory acc) // [(fromIntegral (pc acc), t)],
                              pc = (pc acc) + 1 }
        Label s            -> acc { labels = Map.insert s (pc acc) (labels acc) }

           
-- | Helper function to check whether or not program should terminate        
isTerminate :: (MonadState MachineState m) => m Bool
isTerminate = do ms <- get
                 let insn = (memory ms) ! (fromIntegral (pc ms))
                 case insn of
                   DataVal _ -> return True
                   InsnVal _ -> return False

-- | execution loop - fetch, decode, and update state
execute :: (MonadState MachineState m, MonadError LC4Error m) => m ()
execute = do p <- isTerminate
             case p of
               False  -> do i <- fetch 
                            d <- decodeInsn i
                            _ <- modify (updateMachineState d)
                            execute
               True   -> return ()

-- | Runs LC4 using some initial state
runLC4 :: MachineState -> IO()
runLC4 ms =
  let (err, ms') = runState (runErrorT execute) ms in
  case err of
    Left e  -> print $ show e -- print out exception
    Right _ -> print ms' -- otherwise print out final machine state

main :: String -> IO ()
main file = do s <- parseFromFile lc4P file
               case s of
                 (Left _) -> print "Error while parsing through file"
                 (Right insns) -> let ms = populateMemory insns emptyMachine 
                                      ms'= ms {pc = 0} in
                                      runLC4 ms'
               return ()


-- | Simulate the execution of one instruction
execOneInsn :: (MonadState MachineState m, MonadError LC4Error m) => Insn -> m ()
execOneInsn insn = do d <- decodeInsn insn
                      _ <- modify (updateMachineState d)
                      return ()

-- | Runs the machine using one instruction; for debugging purposes
execOnce :: Insn -> MachineState -> Either String MachineState
execOnce insn ms =
  let (err, ms') = runState (runErrorT (execOneInsn insn)) ms in
  case err of
    Left e  -> Left (show e) -- output the error
    Right _ -> Right ms' -- otherwise print out final machine state

--Let's Test Away

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


--Jasmine's questions:
--Why does PC end at 18 for multiply.asm?
--tSUB2 result is incorrect?
--Tests run rather slowly
--Note: NZP is 010 in the initial state
--Note: Changed calcNZP function because it was convoluted
--Note: new arithOrLogic function for arithmetic and logical operations
--Note: changed printPopulatedMemory to showPopulatedMemory


--To Do:
--Add more types of errors
--JSRR
--Perhaps bring pretty printing back..
--