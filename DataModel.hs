{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module DataModel where 
import Data.Word (Word16)
import Data.Int (Int16)

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

int16ToWord16 :: Int16 -> Word16
int16ToWord16 = fromIntegral

word16ToInt16 :: Word16 -> Int16
word16ToInt16 = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

type LC4 = [Line]

data Line = Memory MemVal 
          | Directive Dir 
          | Label String
          deriving (Show, Eq)

data MemVal = InsnVal Insn 
            | DataVal Word16
            deriving (Show, Eq)

data Dir = DATA | CODE | FALIGN 
         | ADDR Word16 | FILL Word16
         | BLKW Word16 | ICONST String Word16 | UCONST String Word16
         deriving (Show, Eq)

data Insn = Single Op
          | Unary UnaryOp Tok 
          | Binary BinaryOp Tok Tok
          | Ternary TernaryOp Tok Tok Tok
          deriving (Show, Eq)

data Op = NOP | RTI | RET | EOF
          deriving (Show, Eq)

data UnaryOp = BRn | BRnz | BRnp | BRz | BRzp | BRp | BRnzp
             | JSRR | JSR | JMPR | TRAP | JMP
               deriving (Show, Eq)

data BinaryOp =  CMP | CMPU | CMPI | CMPIU | NOT
               | CONST | HICONST
               | LEA | LC
               deriving (Show, Eq)

data TernaryOp = ADD | MUL | SUB | DIV
               | AND | OR | XOR
               | LDR | STR               
               | SLL | SRA | SRL | MOD
               deriving (Show, Eq)

data Tok = R Int | IMM Int | LABEL String
         deriving (Show, Eq)