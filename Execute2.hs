{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute2 where
import Data.Array.IO
import qualified Data.Map as Map
import Control.Monad.State
import Main
import Numeric
import Data.Word
import Data.Bits
import ParserCombinators
import Debug.Trace
import Data.Vector (Vector, (!), (//), update, singleton, replicate)

binToInt :: String -> Int
binToInt = fst . head . readInt 2 (\x -> x == '0' || x == '1') (\y -> if y == '0' then 0 else 1)

matchNZP :: MachineState a -> String -> Bool
matchNZP ms v = (binToInt v) == (nzp ms)

wordToInt :: Word -> Int
wordToInt = fromIntegral

traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

type Delta = [Change]

data Change = SetPC Int
            | IncPC
            | SetNZP Int
            | SetReg Int Int
            | SetPriv Bool
            | SetMem Int Insn
            | SetLabel String Int


aPut :: Delta -> State ( MachineState a) ()
aPut []     = do return ()
aPut (x:xs) = do ms <- get
                 case x of
                     SetPC v      -> put $ ms { pc = v }
                     IncPC        -> put $ ms { pc = (pc ms) + 1 }
                     SetNZP v     -> put $ ms { nzp = v }
                     SetReg r v   -> put $ ms { regs = (regs ms) // [(r, v)] }
                     SetPriv v    -> put $ ms { priv = v }
                     SetMem i v   -> put $ ms { memory = (memory ms) // [(i, v)] }
                     SetLabel l v -> put $ ms { labels = (Map.insert l v (labels ms)) }
                 aPut xs

execute :: Insn -> State (MachineState ()) ()
--execute (Single NOP) = incPC
execute (Single RTI) = aPut [SetPriv False]
execute (Unary JSRR (R rs)) = do ms <- get
                                 aPut [SetReg 7 (1 + (pc ms)), SetPC ((regs ms) ! rs)]
execute (Unary JMPR (R rs)) = do ms <- get
                                 aPut [SetPC ((regs ms) ! rs)]
execute (Unary TRAP (IMM i)) = do ms <- get
                                  let pcv = pc ms
                                      newpcv = i + 0x8000
                                  aPut [SetReg 7 (pcv + 1), SetPC newpcv, SetPriv True]
execute (Binary BRn (IMM i) (LABEL l))
                                = do ms <- get
                                     matchNZP ms "100"
                                     return ()
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