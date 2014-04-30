{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- The basic definition of the parsing monad as developed in lecture.
-- Operations for building composite parsers are in the module
-- ParserCombinators.

module Parser (Parser,
                   get,
                   choose,
                   (<|>),
                   satisfy,
                   satisfyAll,
                   doParse,  
                   ) where

newtype Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)] 
doParse (P p) s = p s

-- | Return the next character
-- (this was called 'oneChar' in lecture)
get :: Parser Char
get = P (\cs -> case cs of 
                (x:xs) -> [ (x,xs) ]
                []     -> [])

-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- get
               if (p c) then return c else fail "End of input"

satisfyAll :: [(Char -> Bool)] -> Parser Char
satisfyAll l = do c <- get
                  if sat l c then return c
                  else fail "End of input"
   where sat [] _     = True
         sat (x:xs) c = if (x c == False)
                        then False
                        else sat xs c

instance Monad Parser where
   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
                             doParse (fp2 a) cs') 

   return x   = P (\cs -> [ (x, cs) ])

   fail _     = P (\_ ->  [ ] )

instance Functor Parser where
   fmap f p = do x <- p
                 return (f x)

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]
