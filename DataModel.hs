{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults 
    -XTypeSynonymInstances -XFlexibleInstances#-}
module DataModel where 

import Prelude
import Data.Word (Word16)
import Data.Int (Int16)
import Control.Monad
import Test.QuickCheck


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

instance Arbitrary Tok where 
  arbitrary = oneof [ liftM R arbitrary, 
                      liftM IMM arbitrary,
                      liftM LABEL arbLabel ]

arbLabel :: Gen String
arbLabel = elements ["A", "B", "C", "D", "E", "F"]

instance Arbitrary Op where
  arbitrary = elements [ NOP, RTI, RET, EOF ]

instance Arbitrary UnaryOp where
  arbitrary = elements [ BRn, BRnz, BRnp, BRz, BRzp, BRp, BRnzp, JSRR, JSR, JMPR, TRAP, JMP ]

instance Arbitrary BinaryOp where
  arbitrary = elements [ CMP, CMPU, CMPI, CMPIU, NOT, CONST, HICONST, LEA, LC ]

instance Arbitrary TernaryOp where
  arbitrary = elements [ ADD, MUL, SUB, DIV, AND, OR, XOR, LDR, STR, SLL, SRA, SRL, MOD ]

instance Arbitrary Insn where
  arbitrary = oneof [ liftM Single arbitrary,
                      liftM2 Unary arbitrary arbitrary,
                      liftM3 Binary arbitrary arbitrary arbitrary,
                      liftM4 Ternary arbitrary arbitrary arbitrary arbitrary ]

arbDir :: Gen Dir
arbDir = elements [ DATA, CODE, FALIGN ]

instance Arbitrary Dir where
  arbitrary = oneof [ arbDir,
                      liftM ADDR arbitrary,
                      liftM FILL arbitrary,
                      liftM BLKW arbitrary,
                      liftM2 ICONST arbLabel arbitrary,
                      liftM2 UCONST arbLabel arbitrary ] 

instance Arbitrary MemVal where
  arbitrary = oneof [ liftM InsnVal arbitrary,
                      liftM DataVal arbitrary ]

instance Arbitrary Line where
  arbitrary = oneof [ liftM Memory arbitrary,
                      liftM Directive arbitrary,
                      liftM Label arbitrary ]

instance Arbitrary LC4 where
  arbitrary = genList

genList ::  (Arbitrary a) => Gen [a]
genList = frequency [ (1, return []),
                      (7, liftM2 (:) arbitrary genList) ]