-- Advanced Programming

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module LC4Parser where 

import Parser
import ParserCombinators
import Test.HUnit hiding (Label)
import Data.Word (Word16)

type LC4 = [Line]

data Line = Memory MemVal 
          | Directive Dir 
          | Label String
          | Comment
          deriving (Show, Eq)

data MemVal = InsnVal Insn 
            | DataVal Word16
            deriving (Show, Eq)

data Dir = DATA | CODE | FALIGN 
         | ADDR Word16 | FILL Word16
         | BLKW Word16 | ICONST Word16 | UCONST Word16
         deriving (Show, Eq)

data Insn = Single Op
          | Unary UnaryOp Tok 
          | Binary BinaryOp Tok Tok
          | Ternary TernaryOp Tok Tok Tok
          deriving (Show, Eq)

data Op = NOP | RTI | RET
          deriving (Show, Eq)

data UnaryOp = BRn | BRnz | BRz | BRzp | BRp | BRnzp
             | JSRR | JMPR | TRAP | JMP
               deriving (Show, Eq)

data BinaryOp =  CMP | CMPU | CMPI | CMPIU | NOT
               | CONST | HICONST
               | LEA | LC
               deriving (Show, Eq)

data TernaryOp = ADD | MUL | SUB | DIV
               | AND | OR | XOR | JSR
               | LDR | STR               
               | SLL | SRA | SRL | MOD
               deriving (Show, Eq)

data Tok = R Word16 | IMM Word16 | LABEL String
         deriving (Show, Eq)

-- | given a parser, try to apply it at most once
once :: Parser a -> Parser [a]
once p = aux p <|> many0
   where many0 = return []
         aux pa = do x <- pa
                     return [x]

-- | parser for word16
word16 :: Parser Word16
word16 = do _ <- sP $ once $ char '#'
            n <- sP $ string "-" <|> return []
            s <- many1 digit  
            return $ (read (n ++ s) :: Word16)

constP :: String -> a -> Parser a
constP s x = do s' <- string s
                if s' == s then return x else fail "did not match"

-- | given a parser, apply it after ignoring all leading white spaces
wsP :: Parser a -> Parser a
wsP p = do _ <- many space
           a <- p
           return a

-- | given a parser, apply it after ignoring all leading spaces
sP :: Parser a -> Parser a
sP p = do _ <- many $ (char ' ' <|> char '\t')
          a <- p
          return a

-- | parser that parses any character except newline
notNewLineP :: Parser Char
notNewLineP = satisfy ('\n' /=)

-- | parser that parses comment
commentP :: Parser [a]
commentP = do _ <- sP $ char ';'
              _ <- many notNewLineP
              return []

regP :: Parser Tok
regP = do _ <- sP $ once $ char ','
          _ <- sP $ string "R"
          i <- word16
          return $ (R i) 

immP :: Parser Tok
immP = do _ <- sP $ once $ char ','
          i <- word16
          return $ IMM i

labelTokP :: Parser Tok
labelTokP = do _ <- sP $ once $ char ','
               s <- sP $ many notNewLineP
               return $ LABEL s

tokenP :: Parser Tok
tokenP = regP <|> immP <|> labelTokP

unaryP :: Parser UnaryOp
unaryP =  constP "BRn" BRn <|> constP "BRnz" BRnz
         <|> constP "BRz" BRz <|> constP "BRzp" BRzp 
         <|> constP "BRp" BRp <|> constP "BRnzp" BRnzp 
         <|> constP "JSRR" JSRR <|> constP "JMPR" JMPR
         <|> constP "TRAP" TRAP <|> constP "JMP" JMP

binaryP :: Parser BinaryOp
binaryP = constP "CMP" CMP <|> constP "CMPU" CMPU
          <|> constP "CMPI" CMPI <|> constP "CMPIU" CMPIU
          <|> constP "NOT" NOT 
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

opP :: Parser Line
opP = (wsP $ constP "NOP" (Memory $ InsnVal $ Single NOP) )
      <|> ( wsP $ constP "RTI" (Memory $ InsnVal $ Single RTI))
      <|> ( wsP $ constP "RET" (Memory $ InsnVal $ Single RET))

unaryStP :: Parser Line
unaryStP = do op <- wsP $ unaryP
              tok <- sP $ tokenP
              _ <- once $ commentP
              return ( Memory $ InsnVal $ Unary op tok )

binaryStP :: Parser Line
binaryStP = do op <- wsP $ binaryP
               tok1 <- tokenP
               tok2 <- tokenP
               _ <- once $ commentP
               return ( Memory $ InsnVal $ Binary op tok1 tok2)

ternaryStP :: Parser Line
ternaryStP = do op <- wsP $ ternaryP
                tok1 <- tokenP
                tok2 <- tokenP
                tok3 <- tokenP
                _ <- once $ commentP
                return ( Memory $ InsnVal $ Ternary op tok1 tok2 tok3 )

dataP :: Parser Line
dataP = do _ <- wsP $ string ".DATA"
           _ <- once $ commentP
           return ( Directive $ DATA )

codeP :: Parser Line
codeP = do _ <- wsP $ string ".CODE"
           _ <- once $ commentP
           return ( Directive $ CODE )

falignP :: Parser Line
falignP = do _ <- wsP $ string ".FALIGN"
             _ <- once $ commentP
             return ( Directive $ FALIGN )

addrP :: Parser Line
addrP = do _ <- wsP $ string ".ADDR"
           i <- word16
           _ <- once $ commentP
           return ( Directive $ ADDR i )

fillP :: Parser Line
fillP = do _ <- wsP $ string ".FILL"
           i <- word16
           _ <- once $ commentP
           return ( Directive $ FILL i )

blkwP :: Parser Line
blkwP = do _ <- wsP $ string ".BLKW"
           i <- word16
           _ <- once $ commentP
           return ( Directive $ BLKW i )

iconstP :: Parser Line
iconstP = do _ <- wsP $ string ".CONST"
             i <- word16
             _ <- once $ commentP
             return ( Directive $ ICONST i )

uconstP :: Parser Line
uconstP = do _ <- wsP $ string ".UCONST"
             i <- word16
             _ <- once $ commentP
             return ( Directive $ UCONST i )

dirP :: Parser Line
dirP = dataP <|> codeP <|> falignP 
       <|> addrP <|> fillP <|> blkwP <|> iconstP <|> uconstP

memValP :: Parser Line
memValP = opP <|> unaryStP 
          <|> binaryStP 
          <|> ternaryStP

commentLineP :: Parser Line
commentLineP = do _ <- wsP $ commentP
                  return Comment

labelP :: Parser Line
labelP = do s <- wsP $ many notNewLineP
            return $ Label s

lineP :: Parser Line
lineP = memValP <|> dirP <|> commentLineP

lc4P :: Parser LC4
lc4P = many lineP

sADD :: String
sADD = "ADD R5 R4 R3"

sCMP :: String
sCMP = "CMP R1 R3   ;   boohoo"

sCONST :: String
sCONST = "CONST R1 -5   ; Hello"

sJMP :: String 
sJMP = "JMP TRAP_PUTC"

sComment :: String
sComment = ";    CIS 552"

sDir :: String
sDir = ".ADDR #5  ; what"

sLabel :: String
sLabel = "BEGIN"

sProg :: String
sProg = sComment ++ "\n " ++ sCMP ++ "\n" ++ sJMP

t0 :: Test
t0 = parse lineP sLabel ~?= Right ( Label "BEGIN" )

t1 :: Test
t1 = parse lineP sADD ~?=
     Right ( Memory $ InsnVal $ Ternary ADD (R 5) (R 4) (R 3) )

t2 :: Test
t2 = parse lineP sCONST  ~?=
     Right ( Memory $ InsnVal $ Binary CONST (R 1) (IMM (-5)) )

t3 :: Test
t3 = parse lineP sCMP ~?=
     Right ( Memory $ InsnVal $ Binary CMP (R 1) (R 3) )

t4 :: Test
t4 = parse lineP sJMP ~?=
     Right ( Memory $ InsnVal $ Unary JMP (LABEL "TRAP_PUTC") )

t5 :: Test
t5 = parse lc4P sProg ~?=
     Right ( [ Comment, Memory $ InsnVal $ Binary CMP (R 1) (R 3) , 
               Memory $ InsnVal $ Unary JMP (LABEL "TRAP_PUTC") ] )

t6 :: Test
t6 = parse lineP sComment ~?=
     Right ( Comment )

t7 :: Test
t7 = parse lineP sDir ~?=
     Right ( Directive $ ADDR 5)

t8 :: Test
t8 = TestList ["s1" ~: p "sample.asm" ] where
   p s = parseFromFile lc4P s >>= succeed
   succeed (Left _)  = assert False
   succeed (Right _) = assert True

t9 :: IO ()
t9 = do p <- parseFromFile lc4P "sample.asm"
        let bool = p ~?= Right [ Comment,
--              Label "BEGIN",
              Memory $ InsnVal $ Binary CONST (R 1) (IMM 1),
              Memory $ InsnVal $ Ternary ADD (R 1) (R 1) (IMM 2),
              Memory $ InsnVal $ Ternary ADD (R 2) (R 1) (IMM 3),
              Memory $ InsnVal $ Ternary SUB (R 1) (R 2) (R 1),
              Comment,
--            Label "END",
              Memory $ InsnVal $ Single NOP ] 
        _ <- runTestTT bool
        return ()

main :: IO () 
main = do _ <- runTestTT (TestList [ t1, t2, t3, t4, t5, t6, t7, t8 ])
          t9
          return ()
