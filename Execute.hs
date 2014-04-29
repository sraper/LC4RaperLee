{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where

import MachineStateWrapper
import Numeric
import Data.Word (Word16)
import Data.Int (Int16)
import Data.Bits
import Data.Vector ((!))
import qualified Data.Map as Map
import Debug.Trace
import MachineStateWrapper()
import DataModel

--NOT NEEDED
-- | Helper that converts binary string to integer
binToInt :: String -> Int
binToInt = fst . head . readInt 2 predicate digToInt
           where predicate = \x -> x == '0' || x == '1'
                 digToInt = \y -> if y == '0' then 0 else 1

matchNZPs :: MachineState -> String -> Bool
matchNZPs ms xs = foldr (\x y -> y || (matchSingleNZP x)) False xs
    where (n, z, p) = nzp ms
          matchSingleNZP 'N' = n
          matchSingleNZP 'Z' = z
          matchSingleNZP 'P' = p
          matchSingleNZP _   = False

traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

calcNZPVal :: (Num a, Eq a, Ord a) => a -> (Bool, Bool, Bool)
calcNZPVal 0 = (False, True, False)      -- Z
calcNZPVal x = if x < 0
               then (True, False, False) -- N
               else (False, False, True) -- P

branchLogic :: MachineState -> Tok -> Bool -> StateM MachineState ()
branchLogic ms t b = if b
                     then case t of
                          LABEL l -> aPut [SetPC $ (Map.findWithDefault 0 l $ labels ms) + 1]
                          IMM i   -> aPut [SetPC ((pc ms ) + 1 + (fromIntegral i))]
                          _       -> aPut [IncPC] --NEED ERROR
                     else aPut [IncPC]
{-}
arithLogic :: (Num a, Integral a) => MachineState -> (a -> a -> a) -> Tok -> Tok -> Tok -> StateM MachineState ()
arithLogic ms f (R rd) (R rs) (R rt) = let rsv = (regs ms) ! rs
                                           rtv = (regs ms) ! rt in
                                       aPut [ SetReg rd $ rsv `f` rtv,
                                              SetNZP $ calcNZPVal $ rsv `f` rtv,
                                              IncPC ]
-}

data ErrMachine a = ErrorT String (StateM MachineState a)

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
                          aPut [ SetReg 7 $ 1 + (pc ms),
                                 SetNZP $ calcNZPVal $ 1 + (pc ms),
                                 SetPC $ (regs ms) ! rs ]
-- NO JSR RIGHT NOW
execute (Unary JMP t)
                     = do ms <- get
                          let add = case t of
                                 LABEL l -> Map.findWithDefault 0 l $ labels ms
                                 IMM i   -> intToWord16 i
                                 _       -> 0 --NEED ERROR
                          aPut [ SetPC $ (pc ms) + 1 + add ]
execute (Unary JMPR (R rs)) 
                     = do ms <- get
                          aPut [ SetPC $ (regs ms) ! rs ]
execute (Single RET)
                     = execute (Unary JMPR (R 7))
execute (Unary TRAP (IMM i)) 
                     = do ms <- get
                          let newpcv = i + 0x8000
                          aPut [ SetReg 7 $ (pc ms) + 1,
                                 SetPriv True,
                                 SetNZP $ calcNZPVal $ (pc ms) + 1,
                                 SetPC $ fromIntegral newpcv ]
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
                          aPut [ SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
execute (Binary CMPU (R rs) (R rt))
                     = do ms <- get
                          let rsv = word16ToInt $ (regs ms) ! rs
                              rtv = word16ToInt $ (regs ms) ! rt
                          aPut [ SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
execute (Binary CMPI (R rs) (IMM i))
                     = do ms <- get
                          let rsv = word16ToInt16 $ (regs ms) ! rs
                              iv = (fromIntegral i) :: Int16
                          aPut [ SetNZP $ calcNZPVal $ rsv - iv, IncPC ]
execute (Binary CMPIU (R rs) (IMM i))
                     = do ms <- get
                          let rsv = word16ToInt $ (regs ms) ! rs
                              unsignedi = (fromIntegral i) :: Word16
                              expandi = (fromIntegral unsignedi) :: Int
                          aPut [ SetNZP $ calcNZPVal $ rsv - expandi, IncPC ]
-------------------------------------------------------------------------------
------------------------------ ARITHMETIC OPS ---------------------------------
-------------------------------------------------------------------------------
execute (Ternary ADD (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd (rsv + rtv),
                                 SetNZP $ calcNZPVal $ rsv + rtv, IncPC ]
execute (Ternary MUL (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd (rsv * rtv),
                                 SetNZP $ calcNZPVal $ rsv * rtv, IncPC ]
execute (Ternary SUB (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd (rsv - rtv),
                                 SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
execute (Ternary DIV (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd (rsv `div` rtv),
                                 SetNZP $ calcNZPVal $ rsv `div` rtv, IncPC ]
execute (Ternary ADD (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd $ rsv + (intToWord16 i),
                                 SetNZP $ calcNZPVal $ rsv + (intToWord16 i), IncPC ]
execute (Ternary MOD (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd (rsv `mod` rtv),
                                 SetNZP $ calcNZPVal $ rsv `mod` rtv, IncPC ]
-------------------------------------------------------------------------------
------------------------------- LOGICAL OPS -----------------------------------
-------------------------------------------------------------------------------
execute (Ternary AND (R rd) (R rs) (R rt))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd $ rsv .&. rtv,
                                 SetNZP $ calcNZPVal $ rsv .&. rtv, IncPC ]
execute (Binary NOT (R rd) (R rs))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd $ complement rsv,
                                 SetNZP $ calcNZPVal $ complement rsv, IncPC ]
execute (Ternary OR (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd $ rsv .|. rtv,
                                 SetNZP $ calcNZPVal $ rsv .|. rtv, IncPC ]
execute (Ternary XOR (R rd) (R rs) (R rt)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              rtv = (regs ms) ! rt
                          aPut [ SetReg rd $ rsv `xor` rtv,
                                 SetNZP $ calcNZPVal $ rsv `xor` rtv, IncPC ]
execute (Ternary AND (R rd) (R rs) (IMM i)) 
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd $ rsv .&. (intToWord16 i),
                                 SetNZP $ calcNZPVal $ rsv .&. (intToWord16 i), IncPC ]
-------------------------------------------------------------------------------
---------------------------------- SHIFTS -------------------------------------
-------------------------------------------------------------------------------
execute (Ternary SLL (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd $ shiftL rsv i,
                                 SetNZP $ calcNZPVal $ shiftL rsv i, IncPC ]
execute (Ternary SRL (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [ SetReg rd $ shiftR rsv i,
                                 SetNZP $ calcNZPVal $ shiftR rsv i, IncPC ]
execute (Ternary SRA (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = fromIntegral ((regs ms) ! rs) :: Int16
                              shifted = fromIntegral $ shiftR rsv i :: Word16
                          aPut [ SetReg rd shifted,
                                 SetNZP $ calcNZPVal $ shiftR rsv i, IncPC ]
-------------------------------------------------------------------------------
------------------------------ MEMORY ACCESS ----------------------------------
-------------------------------------------------------------------------------
execute (Ternary LDR (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              addr = (word16ToInt rsv) + i
                              val = (memory ms) ! addr
                          case val of
                             DataVal d -> aPut [SetReg rd d, IncPC]
                             _ -> return () -- NEED ERROR
execute (Ternary STR (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                              addr = (word16ToInt rsv) + i
                              val = (regs ms) ! rd
                          aPut [ SetMem addr $ DataVal val, IncPC ]
execute (Binary CONST (R rd) (IMM i))
                     = aPut [ SetMem rd $ DataVal $ intToWord16 i,
                              SetNZP $ calcNZPVal i, IncPC ]
execute (Binary LEA (R r1) (LABEL l))
                     = do ms <- get
                          let addr = Map.findWithDefault 0 l $ labels ms
                          aPut [ SetReg r1 addr, 
                                 SetNZP $ calcNZPVal addr, IncPC ]
execute (Binary LC (R r1) (LABEL l))
                     = do ms <- get
                          let addr = Map.findWithDefault 0 l $ labels ms
                              val = (memory ms) ! word16ToInt addr
                          case val of
                             DataVal d -> aPut [SetReg r1 d, 
                                                SetNZP $ calcNZPVal d, IncPC]
                             _ -> return () -- NEED ERROR
execute _ = do traceM "failed"