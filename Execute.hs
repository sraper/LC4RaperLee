{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where

import LC4Parser
import ParserCombinators
import MachineStateWrapper

import Numeric

import Data.Word (Word16)
import Data.Int (Int16)
import Data.Bits
import Data.Vector (Vector, (!), (//), update, singleton, replicate)
import qualified Data.Map as Map
import Debug.Trace
import MachineStateWrapper

-- | Helper that converts binary string to integer
binToInt :: String -> Int
binToInt = fst . head . readInt 2 pred digToInt
           where pred = \x -> x == '0' || x == '1'
                 digToInt = \y -> if y == '0' then 0 else 1

-- | Helper that checks if NZP bits match input
matchNZP :: MachineState -> Char -> Bool
matchNZP ms 'N' = testBit (nzp ms) 2
matchNZP ms 'Z' = testBit (nzp ms) 1
matchNZP ms 'P' = testBit (nzp ms) 0
matchNZP _  _   = False

wordToInt :: Word16 -> Int16
wordToInt = fromIntegral

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()


execute :: Insn -> StateM MachineState ()
execute (Single NOP) = aPut [IncPC]
execute (Single RTI) = aPut [SetPriv False]
execute (Unary JSRR (R rs)) = do ms <- get
                                 aPut [SetReg 7 (1 + (pc ms)), SetPC ((regs ms) ! (word16ToInt rs))]
execute (Unary JMPR (R rs)) = do ms <- get
                                 aPut [SetPC ((regs ms) ! (word16ToInt rs))]
execute (Unary TRAP (IMM i)) = do ms <- get
                                  let pcv = pc ms
                                      newpcv = i + 0x8000
                                  aPut [SetReg 7 (pcv + 1), SetPC newpcv, SetPriv True]
execute (Unary BRn l)
                                = do ms <- get
                                     if matchNZP ms 'N'
                                     then case l of
                                              Label l -> aPut [SetPC (Map.findWithDefault 0 l $ labels ms)]
                                              IMM i   -> aPut [SetPC ((pc ms ) + 1 + i)]
                                     else return ()
execute (Binary CMP (R rs) (R rt))
                                = do ms <- get
                                     return ()
execute (Binary NOT (R r1) (R r2))
                                = do ms <- get
                                     return ()
-- HOW DO YOU BITS
execute (Unary JMP l)
                                = do ms <- get
                                     let pcv = pc ms
                                         add = case l of
                                            LABEL l -> Map.findWithDefault 0 l $ labels ms
                                            IMM i   -> i
                                            -- ERROR HERE
                                     aPut [SetPC (pcv + 1 + add)]
execute (Binary CONST (R rd) (IMM i))
                                = aPut [IncPC, SetReg rd i]
--execute (Binary LEA (R r1) (LABEL l))
  --                              = do ms <- get
    --                                 let addr = Map.findWithDefault 0 l $ labels ms
--                                     setRegVal r1 addr
execute (Binary LC (R r1) (LABEL l))
                                = do ms <- get
                                     let addr = Map.findWithDefault 0 l $ labels ms
                                         val = (memory ms) ! addr
                                     case val of
                                        DataVal d -> aPut [SetReg r1 d]
                                        _ -> return () -- NEED ERROR
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
                                     aPut [SetReg rd (rs + imm), IncPC]
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
                                     aPut [SetReg rd (rsv .&. i), IncPC]
execute (Ternary LDR (R rd) (R rs) (IMM i))
                                = do ms <- get
                                     let rsv = (regs ms) ! rs
                                         addr = rsv + i
                                         val = (memory ms) ! addr
                                     case val of
                                        DataVal d -> aPut [SetReg rd d]
                                        _ -> return () -- NEED ERROR
execute (Ternary STR (R rd) (R rs) (IMM i))
                                = do ms <- get
                                     let rsv = (regs ms) ! rs
                                         addr = rsv + i
                                         val = (regs ms) ! rd
                                     aPut [SetMem addr (DataVal val)]

execute _ = do traceM "failed"