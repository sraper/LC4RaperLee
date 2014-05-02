-- Advanced Programming

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module LC4Parser where 

import Parser
import ParserCombinators
import DataModel

-- | parses any characters except newline
notNewLineP :: Parser Char
notNewLineP = satisfy ('\n' /=)

-- | parses any characters except newline or space
notNewLineOrSpaceP :: Parser Char
notNewLineOrSpaceP = satisfyAll listPredicates
  where listPredicates = [('\n' /=), (' ' /=), ('\t' /=), ('\v' /=), ('\r' /=), ('\f' /=)]

-- | parser that parses comment
commentP :: Parser [a]
commentP = do _ <- sP $ char ';'
              _ <- many notNewLineP
              return []

-- | Parses and returns a register Token
regP :: Parser Tok
regP = do _ <- sP $ string "," <|> return []
          _ <- sP $ string "R"
          i <- sP int
          return $ (R i) 

-- | Parses and returns an immediate Token
immP :: Parser Tok
immP = hexP <|> decimalP

-- | Parses a string of decimal value and returns an immediate token
decimalP :: Parser Tok
decimalP = do _ <- sP $ string "," <|> return []
              _ <- sP $ string "#" <|> return []
              i <- sP int
              return $ IMM i

-- | Parses a string of hex value and returns an immediate token
hexP :: Parser Tok
hexP = do _ <- sP $ string "0x" <|> string "0X" <|> 
               string "x" <|> string "X"
          i <- hex
          return $ IMM i

-- | Parses and returns a label token
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

-- | Parser for directives
dirP :: Parser Line
dirP = choice [ dataP, codeP, falignP, addrP, 
         fillP, blkwP, iconstP, uconstP ]

-- | Parser for memory values
memValP :: Parser Line
memValP = choice [opP, unaryStP, binaryStP, ternaryStP]

-- | Parser for labels
labelP :: Parser Line
labelP = do s <- wsP $ many1 $ notNewLineOrSpaceP
            return $ Label s

-- | Parses into lines
lineP :: Parser Line
lineP = do _ <- many $ wsP commentP
           choice [ memValP, dirP, labelP ]

lc4P :: Parser LC4
lc4P = many lineP