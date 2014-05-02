{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module LC4 where

import Prelude

import Control.Monad.State
import Control.Monad.Error
import Data.Vector (Vector, (!), (//), replicate, filter)
import Data.Map as Map hiding ((!), update)
import Data.Word (Word16)
import Data.Int (Int16)
import Data.Bits

import DataModel
import LC4Parser hiding (main)
import ParserCombinators

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

data LC4Error = OtherError String
              | NoSuchInstruction
              | NoSuchLabel String
              | DivisionByZero
              | IllegalMemAccess
              | IllegalInsnAccess
              deriving (Eq)

instance Error LC4Error where
  noMsg = OtherError "Unknown error"
  strMsg s = OtherError s

instance Show LC4Error where
  show (NoSuchInstruction)   = "Instruction does not exist"
  show (NoSuchLabel s)       = "The label " ++ s ++ " does not exist"
  show (DivisionByZero)      = "Cannot divide by zero"
  show (IllegalMemAccess)    = "Cannot access memory" --NEED TO IMPLEMENT
  show (IllegalInsnAccess)   = "Cannot access Insn"
  show (OtherError msg)       = msg

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
             DataVal _ -> throwError $ OtherError "wrong fetch, got a data value"

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
         LABEL l -> let v = Map.lookup l (labels ms) in
                    case v of
                      (Just i) -> return [SetPC i]
                      Nothing   -> throwError $ NoSuchLabel l
         IMM i   -> return [SetPC ((pc ms ) + (fromIntegral i))]
         _       -> throwError $ OtherError "Cannot Branch to a register"
       False -> return [IncPC]

-- | Decoder
decodeInsn :: (MonadState MachineState m, MonadError LC4Error m) => Insn -> m Delta
decodeInsn insn = 
  do ms <- get
     case insn of
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
              _       -> throwError $ OtherError "JSR: not label"
       (Unary JMP t)        -> 
         case t of
              LABEL l -> let add = Map.findWithDefault 0 l $ labels ms in
                         return [ SetPC add ]
              IMM i   -> let add = intToWord16 i in
                         return [ SetPC $ (pc ms) + add ]
              _       -> throwError $ OtherError "JMP: cannot take regVal"
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
                    _ -> throwError $ OtherError "Load Error" -- NEED ERROR
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
                 _         -> throwError $ OtherError "Cannot load constant" -- NEED ERROR
       _  -> throwError $ NoSuchInstruction

-- | Updates MachineState using input list of changes
updateMachineState :: Delta -> MachineState -> MachineState
updateMachineState [] ms     = ms
updateMachineState (x:xs) ms = let update = updateMachineState in
      case x of
       (SetPC v)      -> update xs (ms { pc = v })
       (IncPC)        -> update xs (ms { pc = (pc ms) + 1 })
       (SetNZP v)     -> update xs (ms { nzp = v })
       (SetReg r v)   -> update xs (ms { regs = (regs ms) // [(r, v)] })
       (SetPriv v)    -> update xs (ms { priv = v })
       (SetMem i v)   -> update xs (ms { memory = (memory ms) // [(i, v)] })
       (SetLabel l v) -> update xs (ms { labels = (Map.insert l v (labels ms)) })

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
execute = do halt <- isTerminate 
             when ( not halt ) $ do
               i <- fetch 
               d <- decodeInsn i
               modify (updateMachineState d)
               execute

-- | Runs LC4 using some initial state
runLC4 :: MachineState -> IO()
runLC4 ms =
  let (err, ms') = runState (runErrorT execute) ms in
  case err of
    Left e  -> print $ show e -- print out exception
    Right _ -> print ms' -- otherwise print out final machine state

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
    Left e  -> Left (show e) -- output error
    Right _ -> Right ms' -- otherwise print out final machine state

main :: String -> IO ()
main file = do s <- parseFromFile lc4P file
               case s of
                 (Left _) -> print "Error while parsing through file"
                 (Right insns) -> let ms = populateMemory insns emptyMachine 
                                      ms'= ms {pc = 0} in
                                      runLC4 ms'
               return ()