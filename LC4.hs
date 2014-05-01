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
--import Debug.Trace
import Control.Monad.State
import Control.Monad.Error
import DataModel
import LC4Parser
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

type ErrExec = Either LC4Error

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
  show (IllegalMemAccess)    = "Cannot access memory"
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
        --"\nmemory: " ++ printPopulatedMemory memory ++
        "\n--------------------------------------"
    where (n, z, p) = _nzp

-- | Initial MachineState
emptyMachine :: MachineState
emptyMachine = MachineState {
                 pc = 0,
                 nzp = (False, False, False),
                 regs = Data.Vector.replicate 8 0,
                 priv = False,
                 memory = Data.Vector.replicate 65535 (DataVal 0),
                 labels = Map.empty
               }

-- | fetch the next insn
fetch :: (MonadState MachineState m, MonadError LC4Error m) => m Insn
fetch = do ms <- get
           let insn = (memory ms) ! (fromIntegral (pc ms))
           case insn of
             InsnVal i -> return i
             DataVal _ -> throwError $ SomeError "impossible" --FIX??

-- | Helper function that determines NZP bits based on input number
calcNZPVal :: (Num a, Eq a, Ord a) => a -> (Bool, Bool, Bool)
calcNZPVal x | x > 0  = (True, False, False) -- N
calcNZPVal x | x == 0 = (False, True, False) -- Z
calcNZPVal _          = (False, False, True) -- P


-- | Helper function that returns True if any of the NZP bits match the input
matchNZP :: (Bool, Bool, Bool) -> String -> Bool
matchNZP (n, z, p) s = Prelude.foldr (\x y -> y || (matchSingleNZP x)) False s
    where matchSingleNZP 'N' = n
          matchSingleNZP 'Z' = z
          matchSingleNZP 'P' = p
          matchSingleNZP _   = False

-- | Helper function that handles arithmetic or logical operations
arithOrLogic :: (Word16, Word16, Int) -> (Word16 -> Word16 -> Word16) -> Delta
arithOrLogic (rsv, rtv, rd) f = let res = f rsv rtv in
                                    [SetReg rd res, SetNZP $ calcNZPVal res, IncPC ]


-- | Helper function that returns value of the register with given index
getRegVal :: MachineState -> Int -> Word16
getRegVal ms i = (regs ms) ! i


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

decode :: (MonadState MachineState m, MonadError LC4Error m) => Insn -> m Delta
decode i = 
  do ms <- get
     case i of
       (Single NOP)         -> return [IncPC]
       (Single RTI)         -> let r7_val = (regs ms) ! 7 in
                               return [ SetPriv False, SetPC r7_val ]
       (Unary JSRR (R rs))  -> return 
                               [ SetReg 7 $ 1 + (pc ms),
                                 SetNZP $ calcNZPVal $ 1 + (pc ms),
                                 SetPC $ (regs ms) ! rs ]
       (Unary JSR t)        -> ---------FIX??
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
       (Single RET)         -> decode (Unary JMPR (R 7))
       (Unary TRAP (IMM i)) -> let newpcv = i + 0x8000 in
                               return [ SetReg 7 $ (pc ms) + 1,
                                        SetPriv True,
                                        SetNZP $ calcNZPVal $ (pc ms) + 1,
                                        SetPC $ fromIntegral newpcv ]

       -------------------------------------------------------------------------------
       ---------------------------------- BRANCHES -----------------------------------
       -------------------------------------------------------------------------------
       (Unary BRn l)      -> branchLogic l (matchNZP (nzp ms) "N")
       (Unary BRnz l)     -> branchLogic l (matchNZP (nzp ms) "NZ")
       (Unary BRz l)      -> branchLogic l (matchNZP (nzp ms) "Z")
       (Unary BRzp l)     -> branchLogic l (matchNZP (nzp ms) "ZP")
       (Unary BRnp l)     -> branchLogic l (matchNZP (nzp ms) "NP")
       (Unary BRp l)      -> branchLogic l (matchNZP (nzp ms) "P")
       (Unary BRnzp l)    -> branchLogic l (matchNZP (nzp ms) "NZP")
       -------------------------------------------------------------------------------
       --------------------------------- COMPARES ------------------------------------
       -------------------------------------------------------------------------------
       (Binary CMP (R rs) (R rt)) -> 
               let rsv = word16ToInt16 $ (regs ms) ! rs
                   rtv = word16ToInt16 $ (regs ms) ! rt in
               return [ SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
       (Binary CMPU (R rs) (R rt)) -> 
               let rsv = word16ToInt $ (regs ms) ! rs
                   rtv = word16ToInt $ (regs ms) ! rt in
               return [ SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
       (Binary CMPI (R rs) (IMM i)) -> 
               let rsv = word16ToInt16 $ (regs ms) ! rs
                   iv = (fromIntegral i) :: Int16 in
               return [ SetNZP $ calcNZPVal $ rsv - iv, IncPC ]
       (Binary CMPIU (R rs) (IMM i)) ->
               let rsv = word16ToInt $ (regs ms) ! rs
                   unsignedi = (fromIntegral i) :: Word16
                   expandi = (fromIntegral unsignedi) :: Int in
               return [ SetNZP $ calcNZPVal $ rsv - expandi, IncPC ]
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
                    SetNZP $ calcNZPVal $ word16ToInt16 $ rsv `div` rtv, IncPC ]
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
                        SetNZP $ calcNZPVal $ word16ToInt16 $ complement rsv, IncPC ]
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
                        SetNZP $ calcNZPVal $ shiftL rsv i, IncPC ]
       (Ternary SRL (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs in
               return [ SetReg rd $ shiftR rsv i,
                        SetNZP $ calcNZPVal $ shiftR rsv i, IncPC ]
       (Ternary SRA (R rd) (R rs) (IMM i)) -> 
               let rsv = fromIntegral ((regs ms) ! rs) :: Int16
                   shifted = fromIntegral $ shiftR rsv i :: Word16 in
               return [ SetReg rd shifted,
                        SetNZP $ calcNZPVal $ shiftR rsv i, IncPC ]
       -------------------------------------------------------------------------------
       ------------------------------ MEMORY ACCESS ----------------------------------
       -------------------------------------------------------------------------------
       (Ternary LDR (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs
                   addr = (word16ToInt rsv) + i
                   val = (memory ms) ! addr in
               case val of
                    DataVal d -> return [ SetReg rd d,
                                          SetNZP $ calcNZPVal d, IncPC]
                    _ -> throwError $ SomeError "LDR" -- NEED ERROR
       (Ternary STR (R rd) (R rs) (IMM i)) ->
               let rsv = (regs ms) ! rs
                   addr = (word16ToInt rsv) + i
                   val = (regs ms) ! rd in
               return [ SetMem addr $ DataVal val, IncPC ]
       (Binary CONST (R rd) (IMM i)) ->
               return [ SetReg rd $ intToWord16 i,
                        SetNZP $ calcNZPVal i, IncPC ]
       (Binary LEA (R r1) (LABEL l)) ->
               let addr = Map.findWithDefault 0 l $ labels ms in
               return [ SetReg r1 addr, 
                        SetNZP $ calcNZPVal addr, IncPC ]
       (Binary LC (R r1) (LABEL l)) ->
               let addr = Map.findWithDefault 0 l $ labels ms
                   val = (memory ms) ! word16ToInt addr in
               case val of
                 DataVal d -> return [ SetReg r1 d, 
                                       SetNZP $ calcNZPVal d, IncPC]
                 _         -> throwError $ SomeError "LC" -- NEED ERROR
       _  -> throwError $ NoSuchInstruction


apply :: Delta -> MachineState -> MachineState
apply [] ms     = ms
apply (x:xs) ms = 
      case x of
       (SetPC v)      -> apply xs (ms { pc = v })
       (IncPC)        -> apply xs (ms { pc = (pc ms) + 1 })
       (SetNZP v)     -> apply xs (ms { nzp = v })
       (SetReg r v)   -> apply xs (ms { regs = (regs ms) // [(r, v)] })
       (SetPriv v)    -> apply xs (ms { priv = v })
       (SetMem i v)   -> apply xs (ms { memory = (memory ms) // [(i, v)] })
       (SetLabel l v) -> apply xs (ms { labels = (Map.insert l v (labels ms)) })
                   
-- runLC4 :: LC4 -> IO ()
-- runLC4 insns = let ms = populateMemory insns emptyMachine
--                    ms' = execProg (ms { pc = 0 }) in
--                case ms' of
--                  Left x     -> print $ show x
--                  Right ms'' -> print ms''


isTerminate :: (MonadState MachineState m) => m Bool
isTerminate = do ms <- get
                 let insn = (memory ms) ! (fromIntegral (pc ms))
                 case insn of
                   DataVal _ -> return True
                   InsnVal _ -> return False

-- | fetch, decode, execute loop
runLC4 :: (MonadState MachineState m, MonadError LC4Error m) => m ()
runLC4 = do p <- isTerminate
            case p of
              True  -> do i <- fetch 
                          d <- decode i
                          _ <- modify (apply d)
                          runLC4
              False -> return ()

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

execute :: MachineState -> LC4 -> IO()
execute st insns =
  let (exn, st') = runState (runErrorT runLC4) st in
  case exn of
    Left e  -> print $ show e -- print out exception
    Right _ -> print st' -- otherwise print out final machine state

main :: String -> IO ()
main file = do s <- parseFromFile lc4P file
               case s of
                 (Left _) -> print "Error while parsing through file"
                 (Right insns) -> let ms = populateMemory insns emptyMachine 
                                      ms'= ms {pc = 0} in
                                  execute ms' insns
               return ()