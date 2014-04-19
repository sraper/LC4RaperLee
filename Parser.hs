{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- The basic definition of the parsing monad as developed in lecture.
-- Operations for building composite parsers are in the module
-- ParserCombinators.

module Parser (Parser,                  
                   get,
                   choose,
                   (<|>),
                   satisfy,
                   doParse,  
                   ) where

newtype Parser c a = P ([c] -> [(a, [c])])

doParse :: Parser c a -> [c] -> [(a, [c])] 
doParse (P p) s = p s

-- | Return the next character (or element of b)
-- (this was called 'oneChar' in lecture)
get :: Parser c c
get = P (\cs -> case cs of 
                (x:xs) -> [ (x,xs) ]
                []     -> [])

-- | Return the next character (element of b) if it satisfies the given 
-- predicate (this was called satP in lecture)
satisfy :: (c -> Bool) -> Parser c c
satisfy p = do c <- get
               if (p c) then return c else fail "End of input"

instance Monad (Parser c) where
   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
                             doParse (fp2 a) cs') 

   return x   = P (\cs -> [ (x, cs) ])

   fail _     = P (\_ ->  [ ] )

instance Functor (Parser c) where
   fmap f p = do x <- p
                 return (f x)

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: Parser c a -> Parser c a -> Parser c a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

-- | Combine two parsers together in parallel, but only use the 
-- first result. This means that the second parser is used only 
-- if the first parser completely fails. 
(<|>) :: Parser c a -> Parser c a -> Parser c a
p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]
