{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module LC4 where

import Control.Monad.State
import Control.Monad.Error
import Data.Vector (Vector, (!), (//), replicate, filter)
import Data.Map as Map hiding ((!), update)
import Data.Word (Word16)
import Data.Int (Int16)
import Data.Bits

import Optimizations
import DataModel
import LC4Parser
import ParserCombinators

type MLC4 = ErrorT LC4Error (State MachineState)

data MachineState = MachineState { 
                      pc :: Word16,
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
              | TokenMismatch String
              deriving (Eq)

instance Error LC4Error where
  noMsg = OtherError "Unknown error"
  strMsg s = OtherError s

instance Show LC4Error where
  show (NoSuchInstruction)   = "Instruction does not exist"
  show (TokenMismatch s)     = "Expecting either LABEL or IMM but got R in " ++ s
  show (NoSuchLabel s)       = "The label " ++ s ++ " does not exist"
  show (DivisionByZero)      = "Cannot divide by zero"
  show (IllegalMemAccess)    = "Cannot access memory"
  show (IllegalInsnAccess)   = "Cannot access Insn"
  show (OtherError msg)      = msg

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

-- | Convert populated portion of memory into a string
showPopulatedMemory :: Vector MemVal -> String
showPopulatedMemory mem = show ( Data.Vector.filter (/= DataVal 0) mem )

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

int16ToWord16 :: Int16 -> Word16
int16ToWord16 = fromIntegral

word16ToInt16 :: Word16 -> Int16
word16ToInt16 = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

-- | Initial MachineState
emptyMachine :: MachineState
emptyMachine = MachineState {
                 pc = 0,
                 nzp = (False, True, False),
                 regs = Data.Vector.replicate 8 0,
                 priv = True,
                 memory = Data.Vector.replicate 65535 (DataVal 0),
                 labels = Map.empty
               }

-- | Helper function that returns value of the register with given index
getRegVal :: MachineState -> Int -> Word16
getRegVal ms i = (regs ms) ! i

-- | Helper function that handles arithmetic or logical operations
arithOrLogic :: (Word16, Word16, Int) -> (Word16 -> Word16 -> Word16) -> Delta
arithOrLogic (rsv, rtv, rd) f = let res = f rsv rtv in
  [SetReg rd res, SetNZP $ calcNZP $ word16ToInt16 res, IncPC ]

-- | Helper function that determines NZP bits based on input number
calcNZP :: (Num a, Eq a, Ord a) => a -> (Bool, Bool, Bool)
calcNZP x | x < 0     = (True, False, False) -- N
          | x == 0    = (False, True, False) -- Z
          | otherwise = (False, False, True) -- P


-- | Helper function that returns True if any of the NZP bits match the input
matchNZP :: (Bool, Bool, Bool) -> (Bool, Bool, Bool) -> Bool
matchNZP (n, z, p) (n', z', p') = ( n == True && n' == True ) ||
                                  ( z == True && z' == True ) ||
                                  ( p == True && p' == True )

-- | Helper function to handle branching
branchLogic :: (MonadError LC4Error m) => MachineState -> Tok -> Bool -> m Delta
branchLogic ms tok p = do 
  if p
  then case tok of
         LABEL l -> let v = Map.lookup l (labels ms) in
                    case v of
                      (Just i) -> return [SetPC i]
                      Nothing   -> throwError $ NoSuchLabel l
         IMM i   -> return [SetPC ((pc ms ) + (fromIntegral i))]
         _       -> throwError $ TokenMismatch "BRANCH"
  else return [IncPC]

------------------------------------------------------------------------------------------------
--------------------------------------- Execution Functions ------------------------------------
------------------------------------------------------------------------------------------------
-- | Runs the machine using the list of instructions parsed from assembly file
main :: String -> IO ()
main file = do 
  s <- parseFromFile lc4P file
  case s of
    (Left _) -> print "Error while parsing through file"
    (Right insns) -> let ms = populateMemory (optimize insns) emptyMachine 
                         ms'= ms {pc = 0}
                         (err, ms'') = runLC4 ms' in
                     do print $ show err
                        print ms''
  return ()

-- | Runs LC4 using some initial state
runLC4 :: MachineState -> (Maybe LC4Error, MachineState)
runLC4 ms =
  let (err, ms') = runState (runErrorT execute) ms in
  case err of
    Left e  -> (Just e, ms')--print $ show e -- print out exception
    Right _ -> (Nothing, ms')--print ms' -- otherwise print out final machine state

-- | execution loop - fetch, decode, and update state
execute :: (MonadState MachineState m, MonadError LC4Error m) => m ()
execute = do 
  halt <- isTerminate 
  when ( not halt ) $ do
    ms <- get
    i  <- fetch ms
    d  <- decodeInsn ms i
    modify $ updateMachineState d
    execute

-- | Helper function to check whether or not program should terminate        
isTerminate :: (MonadState MachineState m) => m Bool
isTerminate = do ms <- get
                 let insn = (memory ms) ! (fromIntegral (pc ms))
                 case insn of
                   InsnVal (Single EOF) -> return True
                   _                    -> return False

-- | Preprocessing method that handles directives & loads instructions into memory
populateMemory :: LC4 -> MachineState -> MachineState
populateMemory [] ms = ms
populateMemory xs ms = 
    let ms' = Prelude.foldl pop (ms { pc = 0 }) xs in
    ms' { memory = (memory ms') // [(fromIntegral (pc ms'), InsnVal (Single EOF))] }
    where
      pop acc i = case i of
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

isValidPC :: Word16 -> Bool -> Bool
isValidPC pcv privv = not ((pcv >= 0x2000 && pcv < 0x8000) || pcv >= 0xA000) -- in data section
                   && not ((pcv >= 0x8000 && pcv < 0xA000) && not privv)     -- in OS data w/o

-- | Fetches the next insn
fetch :: (MonadError LC4Error m) => MachineState -> m Insn
fetch ms = do
  if isValidPC (pc ms) (priv ms)
  then
    let insn = (memory ms) ! (fromIntegral (pc ms)) in
    case insn of
      InsnVal i -> return i
      DataVal _ -> throwError $ OtherError "wrong fetch, got a data value"
  else
    throwError $ OtherError $ "PC value of " ++ show (pc ms) ++ " is illegal" 

-- | Decoder that returns a list of changes that need to be made to MachineState
decodeInsn :: (MonadError LC4Error m) => MachineState -> Insn -> m Delta
decodeInsn ms insn = do
     case insn of
       (Single NOP)         -> return [IncPC]
       (Single RTI)         -> let r7_val = (regs ms) ! 7 in
                               return [ SetPriv False, SetPC r7_val ]
       (Unary JSRR (R rs))  -> return 
                               [ SetReg 7 $ 1 + (pc ms),
                                 SetNZP $ calcNZP $ 1 + (pc ms),
                                 SetPC $ (regs ms) ! rs ]
       (Unary JSR (LABEL l)) ->
              let v = Map.lookup l (labels ms) in
                case v of
                  (Just i) -> return [ SetPC $ ((pc ms) .&. 0x8000) .|. (shiftL i 4) ]
                  Nothing  -> throwError $ NoSuchLabel l
       (Unary JSR (IMM i))   ->
              return [ SetPC $ ((pc ms) .&. 0x8000) .|. (shiftL (fromIntegral i) 4) ]
       (Unary JMP t)        -> branchLogic ms t True
       (Unary JMPR (R rs))  -> return [ SetPC $ (regs ms) ! rs ]
       (Single RET)         -> decodeInsn ms (Unary JMPR (R 7))
       (Unary TRAP (IMM i)) -> let newpcv = i + 0x8000 in
                               return [ SetReg 7 $ (pc ms) + 1,
                                        SetPriv True,
                                        SetNZP $ calcNZP $ (pc ms) + 1,
                                        SetPC $ fromIntegral newpcv ]

       -------------------------------------------------------------------------------
       ---------------------------------- BRANCHES -----------------------------------
       -------------------------------------------------------------------------------
       (Unary BRn l)      -> branchLogic ms l (matchNZP (nzp ms) (True, False, False))
       (Unary BRnz l)     -> branchLogic ms l (matchNZP (nzp ms) (True, True, False))
       (Unary BRz l)      -> branchLogic ms l (matchNZP (nzp ms) (False, True, False))
       (Unary BRzp l)     -> branchLogic ms l (matchNZP (nzp ms) (False, True, True))
       (Unary BRnp l)     -> branchLogic ms l (matchNZP (nzp ms) (True, False, True))
       (Unary BRp l)      -> branchLogic ms l (matchNZP (nzp ms) (False, False, True))
       (Unary BRnzp l)    -> branchLogic ms l (matchNZP (nzp ms) (True, True, True))
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
                    DataVal d -> if addr < 0x2000 || (addr >= 0x8000 && addr < 0xA000)
                                    || addr > 0xFFFF
                                 then throwError IllegalMemAccess
                                 else return [ SetReg rd d,
                                               SetNZP $ calcNZP d, IncPC]
                    _ -> throwError $ IllegalMemAccess -- NEED ERROR
       (Ternary STR (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs
                   addr = (word16ToInt rsv) + i
                   val = (regs ms) ! rd in
               if (addr < 0x2000 || (addr >= 0x8000 && addr < 0xA000) || addr > 0xFFFF) ||
                  (not (priv ms) && (addr >= 0xA000 && addr <= 0x0FFFF))
               then throwError IllegalMemAccess
               else return [ SetMem addr $ DataVal val, IncPC ]
       (Binary CONST (R rd) (IMM i)) ->
               return [ SetReg rd $ intToWord16 i,
                        SetNZP $ calcNZP i, IncPC ]
       (Binary LEA (R r1) (LABEL l)) ->
               let v = Map.lookup l (labels ms) in
                       case v of
                         (Just i) -> return [ SetReg r1 i, 
                                              SetNZP $ calcNZP i, IncPC ]
                         Nothing   -> throwError $ NoSuchLabel l
       (Binary LC (R r1) (LABEL l)) ->
               let addr = Map.lookup l (labels ms) in
                 case addr of
                   (Just i) -> let val = (memory ms) ! word16ToInt i in
                                 case val of
                                   DataVal d -> return [ SetReg r1 d, 
                                                         SetNZP $ calcNZP d, IncPC]
                                   _         -> throwError IllegalMemAccess
                   Nothing   -> throwError $ NoSuchLabel l
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

------------------------------------------------------------------------------------------------
--------------------------------------- For Testing Purposes -----------------------------------
------------------------------------------------------------------------------------------------

-- | Simulate the execution of one insn
execOneInsn :: (MonadState MachineState m, MonadError LC4Error m) => Insn -> m ()
execOneInsn insn = do 
  ms <- get
  d  <- decodeInsn ms insn
  _  <- modify $ updateMachineState d
  return ()

-- | Runs the machine using one insn; for testing single insns
runOnce :: Insn -> MachineState -> Either String MachineState
runOnce insn ms =
  let (err, ms') = runState (runErrorT (execOneInsn insn)) ms in
  case err of
    Left e  -> Left (show e) -- output error
    Right _ -> Right ms' -- otherwise print out final machine state
