module Text.ParserCombinators.Poly.Plain
  ( -- * The Parser datatype
    Parser(P)	-- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)	-- internal to the Parser Monad.
  , runParser	-- :: Parser t a -> [t] -> (Either String a, [t])
    -- ** basic parsers
  , next	-- :: Parser t t
  , eof		-- :: Parser t ()
  , satisfy	-- :: (t->Bool) -> Parser t t
  , onFail	-- :: Parser t a -> Parser t a -> Parser t a

    -- ** re-parsing
  , reparse	-- :: [t] -> Parser t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where

import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Text.ParserCombinators.Poly.Parser
import Control.Applicative

-- The only differences between a Plain and a Lazy parser are the instance
-- of Applicative, and the type (and implementation) of runParser.

-- | Apply a parser to an input token sequence.
runParser :: Parser t a -> [t] -> (Either String a, [t])
runParser (P p) = resultToEither . p

instance Applicative (Parser t) where
    pure f    = return f
    pf <*> px = do { f <- pf; x <- px; return (f x) }
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative (Parser t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser t)

------------------------------------------------------------------------
