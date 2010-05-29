module Text.ParserCombinators.Poly.Lazy
  ( -- * The Parser datatype
    Parser(P)	-- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)	-- internal to the parser monad
  , runParser	-- :: Parser t a -> [t] -> (Either String a, [t])
    -- ** basic parsers
  , next	-- :: Parser t t
  , eof		-- :: Parser t ()
  , satisfy	-- :: (t->Bool) -> Parser t t
  , onFail      -- :: Parser t a -> Parser t a -> Parser t a

    -- ** re-parsing
  , reparse	-- :: [t] -> Parser t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where

import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Text.ParserCombinators.Poly.Parser
import Control.Applicative

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

-- The only differences between a Plain and a Lazy parser are the instance
-- of Applicative, and the type (and implementation) of runParser.

-- | Apply a parser to an input token sequence.
runParser :: Parser t a -> [t] -> (a, [t])
runParser (P p) = fromResult . p
  where
    fromResult :: Result z a -> (a, z)
    fromResult (Success z a)  =  (a, z)
    fromResult (Failure z e)  =  throwE e
    fromResult (Committed r)  =  fromResult r

instance Applicative (Parser t) where
    pure f    = return f
    --   Apply a parsed function to a parsed value.  This version
    --   is strict in the result of the function parser, but
    --   lazy in the result of the argument parser.  (Argument laziness is
    --   the distinctive feature over other implementations.)
    (P pf) <*> px = P (continue . pf)
      where
        continue (Success z f)  = let (x,z') = runParser px z
                                  in Success z' (f x)
        continue (Committed r)  = Committed (continue r)
        continue (Failure z e)  = Failure z e
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative (Parser t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser t)

------------------------------------------------------------------------
