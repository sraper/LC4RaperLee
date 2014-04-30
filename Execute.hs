{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}
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
import Control.Monad.State.Class
import Control.Monad.Error.Class

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

branchLogic :: MachineState -> Tok -> Bool -> ErrExec Delta --StateM MachineState ()
branchLogic ms t b = if b
                     then case t of
                          LABEL l -> return [SetPC $ (Map.findWithDefault 0 l $ labels ms)]
                          IMM i   -> return [SetPC ((pc ms ) + (fromIntegral i))]
                          _       -> throwError $ SomeError "BRANCH" --NEED ERROR
                     else return [IncPC]
{-}
arithLogic :: (Num a, Integral a) => MachineState -> (a -> a -> a) -> Tok -> Tok -> Tok -> StateM MachineState ()
arithLogic ms f (R rd) (R rs) (R rt) = let rsv = (regs ms) ! rs
                                           rtv = (regs ms) ! rt in
                                       return [ SetReg rd $ rsv `f` rtv,
                                              SetNZP $ calcNZPVal $ rsv `f` rtv,
                                              IncPC ]
-}

type ErrExec = Either ExecutionError

data ExecutionError = SomeError String
                    | NoSuchInstruction String
                    deriving (Eq)

instance Error ExecutionError where
    noMsg = SomeError "Message"
    strMsg s = SomeError s

instance Show ExecutionError where
    show (SomeError bad) = show bad
    show (NoSuchInstruction insn) = show $ "There is no instruction called " ++ insn

execute :: MachineState -> Insn -> ErrExec Delta
--execute :: Insn -> StateM MachineState ()
--execute :: (MonadState MachineState m, MonadError String m) => Insn -> m ()
execute ms (Single NOP) = return [IncPC]
-------------------------------------------------------------------------------
--------------------------------- PC UPDATES ----------------------------------
-------------------------------------------------------------------------------
execute ms (Single RTI) = let r7v = (regs ms) ! 7 in
                          return [ SetPriv False, SetPC r7v ]
execute ms (Unary JSRR (R rs)) 
                     = return [ SetReg 7 $ 1 + (pc ms),
                                 SetNZP $ calcNZPVal $ 1 + (pc ms),
                                 SetPC $ (regs ms) ! rs ]

execute ms (Unary JSR t)
                     = case t of
                         LABEL l -> let add = Map.findWithDefault 0 l $ labels ms in
                                        return [ SetPC add ]
                         _       -> throwError $ SomeError "JSR"
execute ms (Unary JMP t)
                     = case t of
                         LABEL l -> let add = Map.findWithDefault 0 l $ labels ms in
                                        return [ SetPC $ (pc ms) + 1 + add ]
                         IMM i   -> let add = intToWord16 i in
                                        return [ SetPC $ (pc ms) + 1 + add ]
                         _       -> throwError $ SomeError "JMP"
execute ms (Unary JMPR (R rs)) 
                     = return [ SetPC $ (regs ms) ! rs ]
execute ms (Single RET)
                     = execute ms (Unary JMPR (R 7))
execute ms (Unary TRAP (IMM i)) 
                     = let newpcv = i + 0x8000 in
                       return [ SetReg 7 $ (pc ms) + 1,
                                SetPriv True,
                                SetNZP $ calcNZPVal $ (pc ms) + 1,
                                SetPC $ fromIntegral newpcv ]
-------------------------------------------------------------------------------
---------------------------------- BRANCHES -----------------------------------
-------------------------------------------------------------------------------

execute ms (Unary BRn l)   = branchLogic ms l (matchNZPs ms "N")
execute ms (Unary BRnz l)  = branchLogic ms l (matchNZPs ms "NZ")
execute ms (Unary BRz l)   = branchLogic ms l (matchNZPs ms "Z")
execute ms (Unary BRzp l)  = branchLogic ms l (matchNZPs ms "ZP")
execute ms (Unary BRnp l)  = branchLogic ms l (matchNZPs ms "NP")
execute ms (Unary BRp l)   = branchLogic ms l (matchNZPs ms "P")
execute ms (Unary BRnzp l) = branchLogic ms l (matchNZPs ms "NZP")
-------------------------------------------------------------------------------
--------------------------------- COMPARES ------------------------------------
-------------------------------------------------------------------------------
execute ms (Binary CMP (R rs) (R rt))
                     = let rsv = word16ToInt16 $ (regs ms) ! rs
                           rtv = word16ToInt16 $ (regs ms) ! rt in
                       return [ SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
execute ms (Binary CMPU (R rs) (R rt))
                     = let rsv = word16ToInt $ (regs ms) ! rs
                           rtv = word16ToInt $ (regs ms) ! rt in
                       return [ SetNZP $ calcNZPVal $ rsv - rtv, IncPC ]
execute ms (Binary CMPI (R rs) (IMM i))
                     = let rsv = word16ToInt16 $ (regs ms) ! rs
                           iv = (fromIntegral i) :: Int16 in
                       return [ SetNZP $ calcNZPVal $ rsv - iv, IncPC ]
execute ms (Binary CMPIU (R rs) (IMM i))
                     = let rsv = word16ToInt $ (regs ms) ! rs
                           unsignedi = (fromIntegral i) :: Word16
                           expandi = (fromIntegral unsignedi) :: Int in
                       return [ SetNZP $ calcNZPVal $ rsv - expandi, IncPC ]
-------------------------------------------------------------------------------
------------------------------ ARITHMETIC OPS ---------------------------------
-------------------------------------------------------------------------------
execute ms (Ternary ADD (R rd) (R rs) (R rt))
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd (rsv + rtv),
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv + rtv, IncPC ]
execute ms (Ternary MUL (R rd) (R rs) (R rt))
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd (rsv * rtv),
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv * rtv, IncPC ]
execute ms (Ternary SUB (R rd) (R rs) (R rt)) 
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd (rsv - rtv),
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv - rtv, IncPC ]
execute ms (Ternary DIV (R rd) (R rs) (R rt)) 
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd (rsv `div` rtv),
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv `div` rtv, IncPC ]
execute ms (Ternary ADD (R rd) (R rs) (IMM i))
                     = let rsv = (regs ms) ! rs in
                       return [ SetReg rd $ rsv + (intToWord16 i),
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv + (intToWord16 i), IncPC ]
execute ms (Ternary MOD (R rd) (R rs) (R rt))
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd (rsv `mod` rtv),
                                SetNZP $ calcNZPVal $ rsv `mod` rtv, IncPC ]
-------------------------------------------------------------------------------
------------------------------- LOGICAL OPS -----------------------------------
-------------------------------------------------------------------------------
execute ms (Ternary AND (R rd) (R rs) (R rt))
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd $ rsv .&. rtv,
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv .&. rtv, IncPC ]
execute ms (Binary NOT (R rd) (R rs))
                     = let rsv = (regs ms) ! rs in
                       return [ SetReg rd $ complement rsv,
                                SetNZP $ calcNZPVal $ word16ToInt16 $ complement rsv, IncPC ]
execute ms (Ternary OR (R rd) (R rs) (R rt)) 
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd $ rsv .|. rtv,
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv .|. rtv, IncPC ]
execute ms (Ternary XOR (R rd) (R rs) (R rt)) 
                     = let rsv = (regs ms) ! rs
                           rtv = (regs ms) ! rt in
                       return [ SetReg rd $ rsv `xor` rtv,
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv `xor` rtv, IncPC ]
execute ms (Ternary AND (R rd) (R rs) (IMM i)) 
                     = let rsv = (regs ms) ! rs in
                       return [ SetReg rd $ rsv .&. (intToWord16 i),
                                SetNZP $ calcNZPVal $ word16ToInt16 $ rsv .&. (intToWord16 i), IncPC ]
-------------------------------------------------------------------------------
---------------------------------- SHIFTS -------------------------------------
-------------------------------------------------------------------------------
execute ms (Ternary SLL (R rd) (R rs) (IMM i))
                     = let rsv = (regs ms) ! rs in
                       return [ SetReg rd $ shiftL rsv i,
                                SetNZP $ calcNZPVal $ shiftL rsv i, IncPC ]
execute ms (Ternary SRL (R rd) (R rs) (IMM i))
                     = let rsv = (regs ms) ! rs in
                       return [ SetReg rd $ shiftR rsv i,
                                SetNZP $ calcNZPVal $ shiftR rsv i, IncPC ]
execute ms (Ternary SRA (R rd) (R rs) (IMM i))
                     = let rsv = fromIntegral ((regs ms) ! rs) :: Int16
                           shifted = fromIntegral $ shiftR rsv i :: Word16 in
                       return [ SetReg rd shifted,
                                SetNZP $ calcNZPVal $ shiftR rsv i, IncPC ]
-------------------------------------------------------------------------------
------------------------------ MEMORY ACCESS ----------------------------------
-------------------------------------------------------------------------------
execute ms (Ternary LDR (R rd) (R rs) (IMM i))
                     = let rsv = (regs ms) ! rs
                           addr = (word16ToInt rsv) + i
                           val = (memory ms) ! addr in
                       case val of
                          DataVal d -> return [SetReg rd d,
                                               SetNZP $ calcNZPVal d, IncPC]
                          _ -> throwError $ SomeError "LDR" -- NEED ERROR
execute ms (Ternary STR (R rd) (R rs) (IMM i))
                     = let rsv = (regs ms) ! rs
                           addr = (word16ToInt rsv) + i
                           val = (regs ms) ! rd in
                       return [ SetMem addr $ DataVal val, IncPC ]
execute ms (Binary CONST (R rd) (IMM i))
                     = return [ SetReg rd $ intToWord16 i,
                                SetNZP $ calcNZPVal i, IncPC ]
execute ms (Binary LEA (R r1) (LABEL l))
                     = let addr = Map.findWithDefault 0 l $ labels ms in
                       return [ SetReg r1 addr, 
                                SetNZP $ calcNZPVal addr, IncPC ]
execute ms (Binary LC (R r1) (LABEL l))
                     = let addr = Map.findWithDefault 0 l $ labels ms
                           val = (memory ms) ! word16ToInt addr in
                       case val of
                          DataVal d -> return [SetReg r1 d, 
                                               SetNZP $ calcNZPVal d, IncPC]
                          _ -> throwError $ SomeError "LC" -- NEED ERROR
execute _ _ = throwError $ NoSuchInstruction "sup"

