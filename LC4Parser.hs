-- Advanced Programming

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module LC4Parser where 

import Parser
import ParserCombinators
import Test.HUnit hiding (Label)
import Data.Word (Word16)
import DataModel
import Numeric

-- | given a parser, try to apply it at most once
once :: Parser a -> Parser [a]
once p = aux p <|> return []
   where aux pa = do x <- pa
                     return [x]

-- | parser for word16
dirIntP :: Parser Word16
dirIntP = do _ <- sP $ once $ char '#'
             n <- sP $ string "-" <|> return []
             s <- many1 digit  
             return $ (read (n ++ s) :: Word16)

dirHexP :: Parser Word16
dirHexP = do _ <- sP $ (string "x" <|> string "X")
             s <- many1 hexDigit
             return $ fromIntegral $ (fst . head . readHex) s

-- | parser for immediate field of directives
word16 :: Parser Word16
word16 = dirIntP <|> dirHexP


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

listPredicates :: [Char -> Bool]
listPredicates = [('\n' /=), (' ' /=), ('\t' /=), ('\v' /=), ('\r' /=), ('\f' /=)]

notNewLineOrSpaceP :: Parser Char
notNewLineOrSpaceP = satisfyAll listPredicates

-- | parser that parses comment
commentP :: Parser [a]
commentP = do _ <- sP $ char ';'
              _ <- many notNewLineP
              return []

regP :: Parser Tok
regP = do _ <- sP $ once $ char ','
          _ <- sP $ string "R"
          i <- sP int
          return $ (R i) 

immP :: Parser Tok
immP = decimalP <|> hexP

decimalP :: Parser Tok
decimalP = do _ <- sP $ once $ char ','
              _ <- sP $ once $ char '#'
              i <- sP int
              return $ IMM i

hexP :: Parser Tok
hexP = do _ <- sP $ once $ char ','
          _ <- sP $ (string "x" <|> string "X")
          i <- hex
          return $ IMM i

labelTokP :: Parser Tok
labelTokP = do _ <- sP $ once $ char ','
               s <- sP $ many1 notNewLineP
               return $ LABEL s

tokenP :: Parser Tok
tokenP = choice [ regP, immP, labelTokP ]

unaryP :: Parser UnaryOp
unaryP =  choice [ constP "BRnzp" BRnzp,
          constP "BRnz" BRnz,
          constP "BRnp" BRnp,
          constP "BRzp" BRzp,
          constP "BRn" BRn,
          constP "BRz" BRz,
          constP "BRp" BRp,
          constP "JSRR" JSRR,
          constP "JSR" JSR,
          constP "JMPR" JMPR,
          constP "TRAP" TRAP,
          constP "JMP" JMP]

binaryP :: Parser BinaryOp
binaryP = choice [ constP "CMPIU" CMPIU,
          constP "CMPI" CMPI,
          constP "CMPU" CMPU,
          constP "CMP" CMP,
          constP "NOT" NOT,
          constP "CONST" CONST,
          constP "HICONST" HICONST, 
          constP "LEA" LEA, 
          constP "LC" LC ]

ternaryP :: Parser TernaryOp
ternaryP = choice [ constP "ADDI" ADDI, constP "MUL" MUL,
            constP "SUB" SUB, constP "DIV" DIV,
            constP "ADD" ADD, constP "ANDI" ANDI,
            constP "OR" OR, constP "XOR" XOR,
            constP "AND" AND,
            constP "LDR" LDR, constP "STR" STR,
            constP "SLL" SLL, constP "SRA" SRA,
            constP "SRL" SRL, constP "MOD" MOD ]

opP :: Parser Line
opP = choice 
      [ wsP $ constP "NOP" (Memory $ InsnVal $ Single NOP),
       wsP $ constP "RTI" (Memory $ InsnVal $ Single RTI),
       wsP $ constP "RET" (Memory $ InsnVal $ Single RET),
       wsP $ constP "EOF" (Memory $ InsnVal $ Single EOF) ]

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
iconstP = do l <- wsP $ many1 $ notNewLineOrSpaceP
             _ <- sP $ string ".CONST"
             i <- word16
             _ <- once $ commentP
             return ( Directive $ ICONST l i )

uconstP :: Parser Line
uconstP = do l <- wsP $ many1 $ notNewLineOrSpaceP
             _ <- sP $ string ".UCONST"
             i <- word16
             _ <- once $ commentP
             return ( Directive $ UCONST l i )

dirP :: Parser Line
dirP = choice [ dataP, codeP, falignP, addrP, 
         fillP, blkwP, iconstP, uconstP ]

memValP :: Parser Line
memValP = choice [opP, unaryStP, binaryStP, ternaryStP]

commentLineP :: Parser Line
commentLineP = do _ <- wsP $ commentP
                  return Comment

labelP :: Parser Line
labelP = do s <- wsP $ many1 $ notNewLineOrSpaceP
            return $ Label s

lineP :: Parser Line
lineP = choice [ memValP, dirP, commentLineP, labelP ]

otherP :: Parser LC4
otherP = many $ labelP

lc4P :: Parser LC4
lc4P = many lineP

sADDI :: String
sADDI = "ADDI R5 R4 xAB"

sCMP :: String
sCMP = "CMP R1 R3   ;   boohoo"

sCONST :: String
sCONST = "CONST R1 -5   ; Hello"

sJMP :: String 
sJMP = "JMP TRAP_PUTC"

sComment :: String
sComment = ";    CIS 552"

sDir :: String
sDir = ".ADDR #      5  ; what"

sLabel :: String
sLabel = "BEGIN"

sProg :: String
sProg = "\n \t " ++ sLabel ++ "      \n" ++ sComment ++  "\n" ++ sJMP ++ "\n" ++ sADDI

t0 :: Test
t0 = parse lineP sLabel ~?= Right ( Label "BEGIN" )

t1 :: Test
t1 = parse lineP sADDI ~?=
     Right ( Memory $ InsnVal $ Ternary ADDI (R 5) (R 4) (IMM (171)) )

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
     Right ( [Label "BEGIN",Comment,Memory (InsnVal (Unary JMP (LABEL "TRAP_PUTC"))),Memory (InsnVal (Ternary ADDI (R 5) (R 4) (IMM (171))))] )

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
              Label "BEGIN",
              Memory $ InsnVal $ Binary CONST (R 1) (IMM 1),
              Memory $ InsnVal $ Ternary ADDI (R 1) (R 1) (IMM 2),
              Memory $ InsnVal $ Ternary ADDI (R 2) (R 1) (IMM 171),
              Memory $ InsnVal $ Ternary SUB (R 1) (R 2) (R 1),
              Comment,Memory (InsnVal (Single NOP)),Label "END"] 
        _ <- runTestTT bool
        return ()

t10 :: Test
t10 = parse lc4P (sJMP ++ sDir) ~?= Right( [Directive DATA,Directive CODE,Directive (ICONST "WHAT" 132),Memory (InsnVal (Unary BRnzp (IMM 118))),Memory (InsnVal (Unary BRzp (IMM (-4)))),Comment,Label "BEGIN",Memory (InsnVal (Binary CONST (R 1) (IMM 1))),Memory (InsnVal (Ternary ADDI (R 1) (R 1) (IMM 2))),Memory (InsnVal (Ternary ADDI (R 2) (R 1) (IMM 171))),Memory (InsnVal (Ternary SUB (R 1) (R 2) (R 1))),Comment,Memory (InsnVal (Single NOP)),Directive FALIGN,Label "END"] )

main :: IO () 
main = do _ <- runTestTT (TestList [ t1, t2, t3, t4, t5, t6, t7, t8 ])
          t9
          return ()