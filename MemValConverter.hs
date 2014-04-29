{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Execute where

import MachineStateWrapper()
import Test.HUnit hiding (Label)
import LC4Parser
import Data.Bits
import Data.Int (Int16)
import Data.Word (Word16)
import DataModel

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
imm i n = word16ToInt $ r_shift $ l_shift i 
     where l_shift x = shiftL (word16ToInt16 x) (16-n)
           r_shift y = int16ToWord16 (shiftR y (16-n))

uimm :: Word16 -> Int -> Int
uimm i n =  word16ToInt $ shiftR ( shiftL i (16 - n) ) ( 16-n )

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
r11_9 i = word16ToInt ( 7 .&. (shiftR i 9) )

r8_6 :: Word16 -> Int
r8_6 i = word16ToInt ( 7 .&. (shiftR i 6) )

r2_0 :: Word16 -> Int
r2_0 i = word16ToInt ( 7 .&. i )

-------------------------------------------------------------------------------
------------------------------------- TESTS -----------------------------------
-------------------------------------------------------------------------------
t_0 :: Test
t_0 = uimm7 (36854 :: Word16) ~?= 118

t_1 :: Test
t_1 = imm7 (36854 :: Word16) ~?= 65526

t_2 :: Test
t_2 = dataToInsn (DataVal 4673) ~?= InsnVal ( Ternary ADD (R 1) (R 1) (R 1))

main' :: IO ()
main' = do _ <- runTestTT $ TestList [t_0, t_1]
           return ()
