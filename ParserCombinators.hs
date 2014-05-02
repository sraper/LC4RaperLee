{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- CIS 552, University of Pennsylvania
-- based on Parsec and ReadP parsing libraries
module ParserCombinators where

import Data.Char
import Data.Word (Word16)
import System.IO
import Numeric
import Parser
            
type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors.
-- However, for compatibility with Parsec, we give this function 
-- the same type.
parse :: Parser a -> String -> Either ParseError a
parse parser str = case (doParse parser str) of 
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"
    
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do 
  handle <- openFile filename ReadMode 
  str <- hGetContents handle
  return $ parse parser str

-- | Parsers for specific sorts of characters 
digit, space, hexDigit :: Parser Char
digit = satisfy isDigit            
space = satisfy isSpace
hexDigit = satisfy isHexDigit

-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = mapM char

-- | Parses and returns a specific object. 
-- Succeeds only if the input is the given string
constP :: String -> a -> Parser a
constP s x = do s' <- string s
                if s' == s then return x else fail "did not match"

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = do n <- string "-" <|> return []
         s <- many1 digit  
         return $ (read (n ++ s) :: Int)

hex :: Parser Int
hex = do s <- many1 hexDigit
         return $ (fst . head . readHex) s

-- | parses a string of int into word16
intToWord16P :: Parser Word16
intToWord16P = do _ <- sP $ string "#" <|> return []
                  n <- sP $ string "-" <|> return []
                  s <- many1 digit
                  return $ (read (n ++ s) :: Word16)

-- | parses a string of hex into word16
hexToWord16P :: Parser Word16
hexToWord16P = do _ <- sP $ string "0x" <|> string "0X" <|> 
                       string "x" <|> string "X"
                  s <- many1 hexDigit
                  return $ fromIntegral $ (fst . head . readHex) s

-- | parses a string of int or hex into word16
word16 :: Parser Word16
word16 = hexToWord16P <|> intToWord16P

-- | given a parser, apply it as many times as possible                       
-- and return the answer in a list
many   :: Parser a -> Parser [a]
many p = many1 p <|> many0 
   where many0 = return []
                    
-- | given a parser, apply it as many times as possible,
-- but at least once.
many1 :: Parser a -> Parser [a]
many1 p = do x  <- p
             xs <- many p
             return (x:xs)

-- | given a parser, apply it after ignoring all leading white spaces and newlines
wsP :: Parser a -> Parser a
wsP p = do _ <- many space
           a <- p
           return a

-- | given a parser, apply it after ignoring all leading spaces
sP :: Parser a -> Parser a
sP p = do _ <- many $ (char ' ' <|> char '\t')
          a <- p
          return a

-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = foldr (<|>) (fail "")
