{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where

import LC4Parser
import MachineStateWrapper

import Numeric

import Data.Word (Word16)
import Data.Int (Int16)
import Data.Bits
import Data.Vector ((!))
import qualified Data.Map as Map
import Debug.Trace
import MachineStateWrapper()

-- | Helper that converts binary string to integer
binToInt :: String -> Int
binToInt = fst . head . readInt 2 predicate digToInt
           where predicate = \x -> x == '0' || x == '1'
                 digToInt = \y -> if y == '0' then 0 else 1

-- | Helper that checks if NZP bits match input
matchSingleNZP :: MachineState -> Char -> Bool
matchSingleNZP ms 'N' = n where (n, _, _) = nzp ms
matchSingleNZP ms 'Z' = z where (_, z, _) = nzp ms
matchSingleNZP ms 'P' = p where (_, _, p) = nzp ms
matchSingleNZP _  _   = False

matchNZPs :: MachineState -> String -> Bool
matchNZPs ms (x:xs) = matchSingleNZP ms x || matchNZPs ms xs
matchNZPs _ []      = False

wordToInt :: Word16 -> Int
wordToInt = fromIntegral

word16ToInt16 :: Word16 -> Int16
word16ToInt16 = fromIntegral

intToWord :: Int -> Word16
intToWord = fromIntegral

traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

findNZP :: (Num a, Eq a, Ord a) => a -> (Bool, Bool, Bool)
findNZP 0 = (False, True, False)      -- Z
findNZP x = if x < 0
            then (True, False, False) -- N
            else (False, False, True) -- P

branchLogic :: MachineState -> Tok -> Bool -> StateM MachineState ()
branchLogic ms t b = if b
                     then case t of
                          LABEL l -> aPut [SetPC ((Map.findWithDefault 0 l $ labels ms) + 1)]
                          IMM i   -> aPut [SetPC ((pc ms ) + 1 + (fromIntegral i))]
                          _       -> aPut [IncPC] --NEED ERROR
                     else aPut [IncPC]

execute :: Insn -> StateM MachineState ()
execute (Single NOP) = aPut [IncPC]
-------------------------------------------------------------------------------
--------------------------------- PC UPDATES ----------------------------------
-------------------------------------------------------------------------------
execute (Single RTI) = do ms <- get
                          let r7v = (regs ms) ! 7
                          aPut [ SetPriv False, SetPC r7v ]
execute (Unary JSRR (R rs)) 
                     = do ms <- get
                          aPut [SetReg 7 (1 + (pc ms)), SetPC ((regs ms) ! rs)]
execute (Unary JMP t)
                     = do ms <- get
                          let pcv = pc ms
                              add = case t of
                                 LABEL l -> Map.findWithDefault 0 l $ labels ms
                                 IMM i   -> intToWord i
                                 _       -> 0 --NEED ERROR
                          aPut [ SetPC (pcv + 1 + add) ]
execute (Unary JMPR (R rs)) 
                     = do ms <- get
                          aPut [SetPC ((regs ms) ! rs)]
execute (Single RET)
                     = execute (Unary JMPR (R 7))
execute (Unary TRAP (IMM i)) 
                     = do ms <- get
                          let pcv = pc ms
                              newpcv = i + 0x8000
                          aPut [ SetReg 7 (pcv + 1), 
                                 SetPC (fromIntegral newpcv), SetPriv True ]
-------------------------------------------------------------------------------
---------------------------------- BRANCHES -----------------------------------
-------------------------------------------------------------------------------
execute (Unary BRn l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "N")
execute (Unary BRnz l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "NZ")
execute (Unary BRz l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "Z")
execute (Unary BRzp l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "ZP")
execute (Unary BRnp l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "NP")
execute (Unary BRp l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "P")
execute (Unary BRnzp l)
                     = do ms <- get
                          branchLogic ms l (matchNZPs ms "NZP")
-------------------------------------------------------------------------------
--------------------------------- COMPARES ------------------------------------
-------------------------------------------------------------------------------
execute (Binary CMP (R rs) (R rt))
                     = do ms <- get
                          let rsv = word16ToInt16 $ (regs ms) ! rs
                              rtv = word16ToInt16 $ (regs ms) ! rt
                          aPut [ IncPC, SetNZP (findNZP (rsv - rtv)) ]
execute (Binary CMPU (R rs) (R rt))
                     = do ms <- get
                          let rsv = wordToInt $ (regs ms) ! rs
                              rtv = wordToInt $ (regs ms) ! rt
                          aPut [ IncPC, SetNZP (findNZP (rsv - rtv)) ]
execute (Binary CMPI (R rs) (IMM i))
                     = do ms <- get
                          let rsv = word16ToInt16 $ (regs ms) ! rs
                              iv = (fromIntegral i) :: Int16
                          aPut [ IncPC, SetNZP (findNZP (rsv - iv)) ]
execute (Binary CMPIU (R rs) (IMM i))
                     = do ms <- get
                          let rsv = wordToInt $ (regs ms) ! rs
                              unsignedi = (fromIntegral i) :: Word16
                              expandi = (fromIntegral unsignedi) :: Int
                          aPut [ IncPC, SetNZP (findNZP (rsv - expandi)) ]
-------------------------------------------------------------------------------
------------------------------ ARITHMETIC OPS ---------------------------------
-------------------------------------------------------------------------------
execute (Ternary ADD (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv + rtv), IncPC]
execute (Ternary MUL (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv * rtv), IncPC]
execute (Ternary SUB (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv - rtv), IncPC]
execute (Ternary DIV (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv `div` rtv), IncPC]
execute (Ternary ADD (R rd) (R rs) (IMM imm))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [SetReg rd (rsv + (intToWord imm)), IncPC]
execute (Ternary MOD (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv `mod` rtv), IncPC]
-------------------------------------------------------------------------------
------------------------------- LOGICAL OPS -----------------------------------
-------------------------------------------------------------------------------
execute (Ternary AND (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv .&. rtv), IncPC]
execute (Binary NOT (R rd) (R rs)) -- i don't think this works complement 8 = -9???
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [SetReg rd (complement rsv), IncPC]
execute (Ternary OR (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv .|. rtv), IncPC]
execute (Ternary XOR (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [SetReg rd (rsv `xor` rtv), IncPC]
execute (Ternary AND (R rd) (R rs) (IMM i)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [SetReg rd (rsv .&. (intToWord i)), IncPC]
-------------------------------------------------------------------------------
---------------------------------- SHIFTS -------------------------------------
-------------------------------------------------------------------------------
execute (Ternary SLL (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd (shiftL rsv i), IncPC ]
execute (Ternary SRL (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd (shiftR rsv i), IncPC ]
execute (Ternary SRA (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = fromIntegral ((regs ms) ! rs) :: Int16
                              shifted = fromIntegral $ shiftR rsv i :: Word16
                          aPut [ SetReg rd shifted, IncPC ]
-------------------------------------------------------------------------------
------------------------------ MEMORY ACCESS ----------------------------------
-------------------------------------------------------------------------------
execute (Ternary LDR (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              addr = (wordToInt rsv) + i
                              val = (memory ms) ! addr
                          case val of
                             DataVal d -> aPut [SetReg rd d, IncPC]
                             _ -> return () -- NEED ERROR
execute (Ternary STR (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              addr = (wordToInt rsv) + i
                              val = (regs ms) ! rd
                          aPut [SetMem addr (DataVal val), IncPC]
execute (Binary CONST (R rd) (IMM i))
                     = aPut [ IncPC, SetMem rd (DataVal (intToWord i)) ]
execute (Binary LEA (R r1) (LABEL l))
                     = do ms <- get
                          let addr = Map.findWithDefault 0 l $ labels ms
                          aPut [ SetReg r1 addr, IncPC ]
execute (Binary LC (R r1) (LABEL l))
                     = do ms <- get
                          let addr = Map.findWithDefault 0 l $ labels ms
                              val = (memory ms) ! wordToInt addr
                          case val of
                             DataVal d -> aPut [SetReg r1 d, IncPC]
                             _ -> return () -- NEED ERROR
execute _ = do traceM "failed"