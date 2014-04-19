-- Advanced Programming

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where 

--import Control.Monad
--import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint as PP

import Parser
import Test.HUnit

type LC4 = [Insn]

data Insn = Single Op
          | Unary UnaryOp Tok 
          | Binary BinaryOp Tok Tok
          | Ternary TernaryOp Tok Tok Tok
          deriving (Show, Eq)


data Op = NOP | RTI
                deriving (Show, Eq)

data UnaryOp = JSRR | JMPR | TRAP
               deriving (Show, Eq)

data BinaryOp =  BRn | BRnz | BRz | BRzp | BRp | BRnzp
               | CMP | CMPU | CMPI | CMPIU
               | NOT | JMP
               | CONST | HICONST
               deriving (Show, Eq)

data TernaryOp = ADD | MUL | SUB | DIV
               | AND | OR | XOR | JSR
               | LDR | STR               
               | SLL | SRA | SRL | MOD
               deriving (Show, Eq)

data Tok = R Int
         | IMM Int
         | UIMM Int
         | LABEL String
         deriving (Show, Eq)

wList :: LC4
wList = [ wADD, wCONST, wCMP ]

wADD :: Insn
wADD = Ternary ADD (R 5) (R 4) (R 3)

wCONST :: Insn
wCONST = Binary CONST (R 1) (IMM 5)

wCMP :: Insn
wCMP = Binary CMP (R 1) (R 3)

wJMP :: Insn
wJMP = Binary JMP (IMM 5) (LABEL "Hello")

---PP stuffs

class PP a where
  pp :: a -> Doc

instance PP Op where
  pp NOP = PP.text "NOP"
  pp RTI = PP.text "RTI"

instance PP UnaryOp where
  pp JSRR = PP.text "JSRR"
  pp JMPR = PP.text "JMPR"
  pp TRAP = PP.text "TRAP"

instance PP BinaryOp where
  pp BRn     = PP.text "BRn"
  pp BRnz    = PP.text "BRnz"
  pp BRz     = PP.text "BRz"
  pp BRzp    = PP.text "BRzp"
  pp BRp     = PP.text "BRp"
  pp BRnzp   = PP.text "BRnzp"
  pp CMP     = PP.text "CMP"
  pp CMPU    = PP.text "CMPU"
  pp CMPI    = PP.text "CMPI"
  pp CMPIU   = PP.text "CMPIU"
  pp NOT     = PP.text "NOT"
  pp JMP     = PP.text "JMP"
  pp CONST   = PP.text "CONST"
  pp HICONST = PP.text "HICONST"

instance PP TernaryOp where
  pp ADD  = PP.text "ADD"
  pp MUL  = PP.text "MUL"
  pp SUB  = PP.text "SUB"
  pp DIV  = PP.text "DIV"
  pp AND  = PP.text "AND"
  pp OR   = PP.text "OR"
  pp XOR  = PP.text "XOR"
  pp JSR  = PP.text "JSR"
  pp LDR  = PP.text "LDR"
  pp STR  = PP.text "STR"
  pp SLL  = PP.text "SLL"
  pp SRA  = PP.text "SRA"
  pp SRL  = PP.text "SRL"
  pp MOD  = PP.text "MOD"

instance PP Insn where
  pp (Single op)        = pp op
  pp (Unary op a)       = pp op <+> pp a
  pp (Binary op a b)    = pp op <+> pp a <+> pp b
  pp (Ternary op a b c) = pp op <+> pp a <+> pp b <+> pp c

instance PP Tok where
  pp (R x)     = PP.char 'R' <> PP.int x
  pp (IMM x)   = PP.char '#' <> PP.int x
  pp (UIMM x)  = PP.char '#' <> PP.int x
  pp (LABEL s) = PP.char '<' <> PP.text s <> PP.char '>'
  
display :: PP a => a -> String
display = show . pp

-- Simple tests 

t0 :: Test
t0 = TestList
     [  display wADD ~?= "ADD R5 R4 R3",
        display wCONST ~?= "CONST R1 #5",
        display wCMP ~?= "CMP R1 R3",
        display wJMP ~?= "JMP #5 <Hello>" ]