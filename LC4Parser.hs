-- Advanced Programming

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module LC4Parser where 

import Parser
import ParserCombinators
import Data.Word (Word16)
import DataModel
import Numeric

-- | parser for word16
dirIntP :: Parser Word16
dirIntP = do _ <- sP $ string "#" <|> return []
             n <- sP $ string "-" <|> return []
             s <- many1 digit  
             return $ (read (n ++ s) :: Word16)

dirHexP :: Parser Word16
dirHexP = do _ <- sP $ string "0x" <|> string "0X" <|> 
               string "x" <|> string "X"
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
regP = do _ <- sP $ string "," <|> return []
          _ <- sP $ string "R"
          i <- sP int
          return $ (R i) 

immP :: Parser Tok
immP = hexP <|> decimalP

decimalP :: Parser Tok
decimalP = do _ <- sP $ string "," <|> return []
              _ <- sP $ string "#" <|> return []
              i <- sP int
              return $ IMM i

hexP :: Parser Tok
hexP = do _ <- sP $ string "0x" <|> string "0X" <|> 
               string "x" <|> string "X"
          i <- hex
          return $ IMM i

labelTokP :: Parser Tok
labelTokP = do _ <- sP $ string "," <|> return []
               s <- sP $ many1 notNewLineOrSpaceP
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
ternaryP = choice [ constP "ADDI" ADD, constP "MUL" MUL,
            constP "SUB" SUB, constP "DIV" DIV,
            constP "ADD" ADD, constP "OR" OR, 
            constP "XOR" XOR,
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
              _ <- many $ commentP
              return ( Memory $ InsnVal $ Unary op tok )

binaryStP :: Parser Line
binaryStP = do op <- wsP $ binaryP
               tok1 <- tokenP
               tok2 <- tokenP
               _ <- many commentP
               return ( Memory $ InsnVal $ Binary op tok1 tok2)

ternaryStP :: Parser Line
ternaryStP = do op <- wsP $ ternaryP
                tok1 <- tokenP
                tok2 <- tokenP
                tok3 <- tokenP
                _ <- many commentP
                return ( Memory $ InsnVal $ Ternary op tok1 tok2 tok3 )

dataP :: Parser Line
dataP = do _ <- wsP $ string ".DATA"
           _ <- many $ commentP
           return ( Directive $ DATA )

codeP :: Parser Line
codeP = do _ <- wsP $ string ".CODE"
           _ <- many commentP
           return ( Directive $ CODE )

falignP :: Parser Line
falignP = do _ <- wsP $ string ".FALIGN"
             _ <- many commentP
             return ( Directive $ FALIGN )

addrP :: Parser Line
addrP = do _ <- wsP $ string ".ADDR"
           i <- word16
           _ <- many commentP
           return ( Directive $ ADDR i )

fillP :: Parser Line
fillP = do _ <- wsP $ string ".FILL"
           i <- word16
           _ <- many commentP
           return ( Directive $ FILL i )

blkwP :: Parser Line
blkwP = do _ <- wsP $ string ".BLKW"
           i <- word16
           _ <- many commentP
           return ( Directive $ BLKW i )

iconstP :: Parser Line
iconstP = do l <- wsP $ many1 $ notNewLineOrSpaceP
             _ <- sP $ string ".CONST"
             i <- word16
             _ <- many commentP
             return ( Directive $ ICONST l i )

uconstP :: Parser Line
uconstP = do l <- wsP $ many1 $ notNewLineOrSpaceP
             _ <- sP $ string ".UCONST"
             i <- word16
             _ <- many commentP
             return ( Directive $ UCONST l i )

dirP :: Parser Line
dirP = choice [ dataP, codeP, falignP, addrP, 
         fillP, blkwP, iconstP, uconstP ]

memValP :: Parser Line
memValP = choice [opP, unaryStP, binaryStP, ternaryStP]

labelP :: Parser Line
labelP = do s <- wsP $ many1 $ notNewLineOrSpaceP
            return $ Label s

lineP :: Parser Line
lineP = do _ <- many $ wsP commentP
           choice [ memValP, dirP, labelP ]

lc4P :: Parser LC4
lc4P = many lineP

sADD :: String
sADD = "ADD R5 R4 0x11"

sCMP :: String
sCMP = "CMP R1 R3   ;   boohoo"

sCONST :: String
sCONST = "CONST R1 -5   ; Hello"

sJMP :: String 
sJMP = "JMP TRAP_PUTC"

sComment :: String
sComment = ";    CIS 552"

sDir :: String
sDir = ".ADDR #   5  ; what"

sLabel :: String
sLabel = "BEGIN"

sBRz :: String
sBRz = "BRz ZERO     ; R3 = 0"

sProg :: String
sProg = "\n \t " ++ sLabel ++ "\n" ++ sComment ++  "\n" ++ sJMP ++ "\n" ++ sADD

