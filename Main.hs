-- Advanced Programming

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where 

--import Control.Monad
--import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>),($$),(<>))
import qualified Text.PrettyPrint as PP

import Parser
import Data.Array.IO
import Data.Map as Map
import ParserCombinators
import Test.HUnit
import Data.Array

type LC4 = [Insn]

data Insn = Single Op
          | Unary UnaryOp Tok 
          | Binary BinaryOp Tok Tok
          | Ternary TernaryOp Tok Tok Tok
          deriving (Show, Eq)


data Op = NOP | RTI | RET
          deriving (Show, Eq)

data UnaryOp = JSRR | JMPR | TRAP
               deriving (Show, Eq)

data BinaryOp =  BRn | BRnz | BRz | BRzp | BRp | BRnzp
               | CMP | CMPU | CMPI | CMPIU
               | NOT | JMP
               | CONST | HICONST
               | LEA | LC
               deriving (Show, Eq)

data TernaryOp = ADD | MUL | SUB | DIV
               | AND | OR | XOR | JSR
               | LDR | STR               
               | SLL | SRA | SRL | MOD
               deriving (Show, Eq)

data Tok = R Int
         | IMM Int
         | LABEL String
         deriving (Show, Eq)

data MachineState = MachineState {
                         pc :: Int,
                         nzp :: Int,
                         regs :: Map Int Int,
                         priv :: Bool,
                         memory :: Map Int Int,
                         labels :: Map String Int }


wList :: LC4
wList = [ wADD, wCONST, wCMP ]

wADD :: Insn
wADD = Ternary ADD (R 5) (R 4) (R 3)

wCONST :: Insn
wCONST = Binary CONST (R 1) (IMM 5)

wCMP :: Insn
wCMP = Binary CMP (R 1) (R 3)

wJMP :: Insn
wJMP = Binary JMP (IMM 5) (LABEL "TRAP_PUTC")

---PP stuffs

class PP a where
  pp :: a -> Doc

instance PP Op where
  pp NOP = PP.text "NOP"
  pp RTI = PP.text "RTI"
  pp RET = PP.text "RET"

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
  pp LEA     = PP.text "LEA"
  pp LC      = PP.text "LC"

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

-- Problem 1

constP :: String -> a -> Parser a
constP s x = do s' <- string s
                if s' == s then return x else fail "did not match"

wsP :: Parser a -> Parser a
wsP p = do _ <- many (space <|> char '\n' <|> char '\t')
           a <- p
           return a

tokenP :: Parser Tok
tokenP = regP <|> immP <|> labelP

regP :: Parser Tok
regP = do _ <- wsP $ string "R"
          i <- wsP $ int
          return $ (R i) 

immP :: Parser Tok
immP = do _ <- wsP $ string "#"
          i <- wsP $ int
          return $ IMM i

labelP :: Parser Tok
labelP = do s <- many (wsP get)
            return $ LABEL s

opP :: Parser Insn
opP = constP "NOP" (Single NOP) <|> constP "RTI" (Single RTI)
      <|> constP "RTI" (Single RTI)

unaryP :: Parser UnaryOp
unaryP = constP "JSRR" JSRR <|> constP "JMPR" JMPR
         <|> constP "TRAP" TRAP

binaryP :: Parser BinaryOp
binaryP = constP "BRn" BRn <|> constP "BRnz" BRnz
          <|> constP "BRz" BRz <|> constP "BRzp" BRzp 
          <|> constP "BRp" BRp <|> constP "BRnzp" BRnzp 
          <|> constP "CMP" CMP <|> constP "CMPU" CMPU
          <|> constP "CMPI" CMPI <|> constP "CMPIU" CMPIU
          <|> constP "NOT" NOT <|> constP "JMP" JMP
          <|> constP "CONST" CONST <|> constP "HICONST" HICONST
          <|> constP "LEA" LEA <|> constP "LC" LC

ternaryP :: Parser TernaryOp
ternaryP = constP "ADD" ADD <|> constP "MUL" MUL
          <|> constP "SUB" SUB <|> constP "DIV" DIV 
          <|> constP "AND" AND <|> constP "OR" OR
          <|> constP "XOR" XOR <|> constP "JSR" JSR
          <|> constP "LDR" LDR <|> constP "STR" STR
          <|> constP "SLL" SLL <|> constP "SRA" SRA
          <|> constP "SRL" SRL <|> constP "MOD" MOD

unaryStP :: Parser Insn
unaryStP = do op <- wsP $ unaryP
              tok <- wsP $ tokenP
              return $ Unary op tok

binaryStP :: Parser Insn
binaryStP = do op <- wsP $ binaryP
               tok1 <- wsP $ tokenP
               tok2 <- wsP $ tokenP
               return $ Binary op tok1 tok2

ternaryStP :: Parser Insn
ternaryStP = do op <- wsP $ ternaryP
                tok1 <- wsP $ tokenP
                tok2 <- wsP $ tokenP
                tok3 <- wsP $ tokenP
                return $ Ternary op tok1 tok2 tok3

insnP :: Parser Insn
insnP = opP <|> unaryStP <|> binaryStP <|> ternaryStP

lc4P :: Parser LC4
lc4P = wsP $ many1 insnP

sADD :: String
sADD = "ADD R5 R4 R3"

sCMP :: String
sCMP = "CMP R1 R3"

sCONST :: String
sCONST = "CONST R1 #-5"

sJMP :: String 
sJMP = "JMP #5 TRAP_PUTC"

sProg :: String
sProg = sCMP ++ "\n" ++ sJMP

t1 :: Test
t1 = parse insnP sADD ~?=
     Right ( Ternary ADD (R 5) (R 4) (R 3) )

t2 :: Test
t2 = parse insnP sCONST  ~?=
     Right ( Binary CONST (R 1) (IMM (-5)) )

t3 :: Test
t3 = parse insnP sCMP ~?=
     Right ( Binary CMP (R 1) (R 3) )

t4 :: Test
t4 = parse insnP sJMP ~?=
     Right ( Binary JMP (IMM 5) (LABEL "TRAP_PUTC") )

t5 :: Test
t5 = parse lc4P sProg ~?=
     Right ( [Binary CMP (R 1) (R 3) , Binary JMP (IMM 5) (LABEL "TRAP_PUTC")] )

t6 :: Test
t6 = TestList ["s1" ~: p "sample.asm" ] where
  p s = parseFromFile lc4P s >>= succeed
  succeed (Left _)  = assert False
  succeed (Right _) = assert True

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
                deriving (Eq, Ix, Ord, Show, Enum)

a1 :: Array Register Int
a1 = array (R1, R7) [(x, 0) | x <- enumFrom R1]


main :: IO () 
main = do _ <- runTestTT (TestList [ t1, t2, t3, t4, t5, t6 ])
          return ()
