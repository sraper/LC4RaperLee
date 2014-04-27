{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where

import LC4Parser
import MachineStateWrapper
import Test.HUnit hiding (Label)

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

int16ToWord16 :: Int16 -> Word16
int16ToWord16 = fromIntegral

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


dataToInsn :: MemVal -> MemVal
dataToInsn (DataVal d) = case get_op d of
                              "Branch" ->
                                 case get_branch_op d of
                                      "NOP" -> InsnVal $ Single NOP
                                      "BRn" -> InsnVal $ Unary BRn (IMM _imm9)
                                      "BRnz" -> InsnVal $ Unary BRnz (IMM _imm9)
                                      "BRnp" -> InsnVal $ Unary BRnp (IMM _imm9)
                                      "BRz" -> InsnVal $ Unary BRz (IMM _imm9)
                                      "BRzp" -> InsnVal $ Unary BRzp (IMM _imm9)
                                      "BRp" -> InsnVal $ Unary BRp (IMM _imm9)
                                      "BRnzp" -> InsnVal $ Unary BRnzp (IMM _imm9)
                                      _ -> error "Invalid"
                              "Arith" -> 
                                 case get_arith_op d of
                                      "ADD" -> InsnVal $ Ternary ADD (R rd) (R rs) (R rt)
                                      "SUB" -> InsnVal $ Ternary SUB (R rd) (R rs) (R rt)
                                      "MUL" -> InsnVal $ Ternary MUL (R rd) (R rs) (R rt)
                                      "DIV" -> InsnVal $ Ternary DIV (R rd) (R rs) (R rt)
                                      "ADDI" -> InsnVal $ Ternary ADDI (R rd) (R rs) (IMM _imm5)
                                      _ -> error "Invalid"
                                      where rd = r11_9 d   
                                            rs = r8_6 d
                                            rt = r2_0 d
                              "Compare" -> 
                                 case get_cmp_op d of
                                      "CMP" -> InsnVal $ Binary CMP (R rs) (R rt)
                                      "CMPU" -> InsnVal $ Binary CMPU (R rs) (R rt)
                                      "CMPI" -> InsnVal $ Binary CMPI (R rs) (IMM _imm7) 
                                      "CMPIU" -> InsnVal $ Binary CMPIU (R rs) (IMM _uimm7)
                                      _ -> error "Invalid"
                                      where rs = r11_9 d
                                            rt = r2_0 d
                              "Subroutines" -> undefined
                              "Logical" -> 
                                 case get_logical_op d of
                                      "AND" -> InsnVal $ Ternary AND (R rd) (R rs) (R rt)
                                      "NOT" -> InsnVal $ Binary NOT (R rd) (R rs)
                                      "OR" -> InsnVal $ Ternary OR (R rd) (R rs) (R rt)
                                      "XOR" -> InsnVal $ Ternary XOR (R rd) (R rs) (R rt)
                                      "ADDI" -> InsnVal $ Ternary ANDI (R rd) (R rs) (IMM _imm5)
                                      _ -> error "Invalid"
                                      where rd = r11_9 d
                                            rs = r8_6 d
                                            rt = r2_0 d
                              "Load" -> 
                                InsnVal $ Ternary LDR (R (r11_9 d)) (R (r8_6 d)) (IMM _imm6)
                              "Store" -> 
                                InsnVal $ Ternary LDR (R (r11_9 d)) (R (r8_6 d)) (IMM _imm6)
                              "RTI" -> InsnVal $ Single RTI
                              "Constant" -> InsnVal $ Binary CONST (R (r11_9 d)) (IMM _imm9)
                              "Shift" -> case testBit d 11 of
                                          True ->
                                           InsnVal $ Unary JMP (IMM _imm11)
                                          False ->
                                           InsnVal $ Unary JMPR (R (r8_6 d))
                              "Jump" -> undefined
                              "Hiconst" -> InsnVal $ Binary HICONST (R (r11_9 d)) (IMM _uimm8)
                              "Trap" -> InsnVal $ Unary TRAP (IMM _uimm8)
                              _ -> error "Invalid"
                              where _imm11 = imm11 d
                                    _imm9 = imm9 d
                                    _uimm8 = uimm8 d
                                    _uimm7 = uimm7 d
                                    _imm7 = imm7 d
                                    _imm6 = imm6 d
                                    _imm5 = imm5 d
           
dataToInsn _           = error "wrong type"

get_op :: Word16 -> String
get_op d = case op_code d of
                    0 -> "Branch"
                    1 -> "Arith"
                    2 -> "Compare"
                    4 -> "Subroutines"
                    5 -> "Logical"
                    6 -> "Load"
                    7 -> "Store"
                    8 -> "RTI"
                    9 -> "Constant"
                    10 -> "Shift"
                    12 -> "Jump"
                    13 -> "Hiconst"
                    15 -> "Trap"
                    _ -> error "Invalid Op-code"

get_branch_op :: Word16 -> String
get_branch_op d = case r11_9 d of
                   0 -> "NOP"
                   1 -> "BRn"
                   2 -> "BRnz"
                   3 -> "BRnp"
                   4 -> "BRz"
                   5 -> "BRzp"
                   6 -> "BRp"
                   7 -> "BRnzp"
                   _ -> error "Invalid"

get_arith_op :: Word16 -> String
get_arith_op d = case subop_code d of
                   0 -> "ADD"
                   1 -> "MUL"
                   2 -> "SUB"
                   3 -> "DIV"
                   _ -> "ADDI"

get_logical_op :: Word16 -> String
get_logical_op d = case subop_code d of
                   0 -> "AND"
                   1 -> "NOT"
                   2 -> "OR"
                   3 -> "XOR"
                   _ -> "ANDI"
 
get_cmp_op :: Word16 -> String
get_cmp_op d = case cmp_code d of
                    0 -> "CMP"
                    1 -> "CMPU"
                    2 -> "CMPI"
                    3 -> "CMPIU"
                    _ -> error "Invalid Op_code"

get_shift_op :: Word16 -> String
get_shift_op d = case shift_code d of
                    0 -> "SLL"
                    1 -> "SRA"
                    2 -> "SRL"
                    3 -> "MOD"
                    _ -> error "Invalid Op_code"

imm :: Word16 -> Int -> Int
imm i n = wordToInt $ r_shift $ l_shift i 
     where l_shift x = shiftL (word16ToInt16 x) (16-n)
           r_shift y = int16ToWord16 (shiftR y (16-n))

uimm :: Word16 -> Int -> Int
uimm i n =  wordToInt $ shiftR ( shiftL i (16 - n) ) ( 16-n )

imm11 :: Word16 -> Int
imm11 i = imm i 11

imm9 :: Word16 -> Int
imm9 i = imm i 9

uimm8 :: Word16 -> Int
uimm8 i = uimm i 8

imm7 :: Word16 -> Int
imm7 i = imm i 7

uimm7 :: Word16 -> Int
uimm7 i = uimm i 7

imm6 :: Word16 -> Int
imm6 i = imm i 6

imm5 :: Word16 -> Int
imm5 i = imm i 5

uimm4 :: Word16 -> Int
uimm4 i = uimm i 4

op_code :: Word16 -> Word16
op_code i = shiftR i 12

subop_code :: Word16 -> Word16
subop_code i = 7 .&. (shiftR i 3)

cmp_code :: Word16 -> Word16
cmp_code i = 4 .&. (shiftR i 7)

shift_code :: Word16 -> Word16
shift_code i = 4 .&. (shiftR i 4)

r11_9 :: Word16 -> Int
r11_9 i = wordToInt ( 7 .&. (shiftR i 9) )

r8_6 :: Word16 -> Int
r8_6 i = wordToInt ( 7 .&. (shiftR i 6) )

r2_0 :: Word16 -> Int
r2_0 i = wordToInt ( 7 .&. i )

t_0 :: Test
t_0 = uimm7 (36854 :: Word16) ~?= 118

t_1 :: Test
t_1 = imm7 (36854 :: Word16) ~?= 65526

t_2 :: Test
t_2 = dataToInsn (DataVal 4673) ~?= InsnVal ( Ternary ADD (R 1) (R 1) (R 1))

main' :: IO ()
main' = do _ <- runTestTT $ TestList [t_0, t_1]
           return ()

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
execute (Ternary ADD (R rd) (R rs) (IMM i))
                     = do ms <- get
                          let rsv = (regs ms) ! rs
                          aPut [SetReg rd (rsv + (intToWord i)), IncPC]
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