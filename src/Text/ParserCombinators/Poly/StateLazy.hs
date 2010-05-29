module Text.ParserCombinators.Poly.StateLazy
  ( -- * The Parser datatype
    Parser(P)	-- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)	-- internal to the parser monad
  , runParser	-- :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
    -- ** basic parsers
  , next	-- :: Parser s t t
  , eof		-- :: Parser s t ()
  , satisfy	-- :: (t->Bool) -> Parser s t t
  , onFail      -- :: Parser s t a -> Parser s t a -> Parser s t a
  , manyFinally	-- :: Parser s t a -> Parser s t z -> Parser s t [a]
    -- ** State-handling
  , stUpdate    -- :: (s->s) -> Parser s t ()
  , stQuery     -- :: (s->a) -> Parser s t a
  , stGet       -- :: Parser s t s
    -- ** re-parsing
  , reparse	-- :: [t] -> Parser s t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where


import Text.ParserCombinators.Poly.Base hiding (manyFinally)
import Text.ParserCombinators.Poly.Result
import Text.ParserCombinators.Poly.StateParser
import Control.Applicative

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

-- The only differences between a State and a StateLazy parser are the instance
-- of Applicative, and the type (and implementation) of runParser.

-- | Apply a parser to an input token sequence.
runParser :: Parser s t a -> s -> [t] -> (a, s, [t])
runParser (P p) = \s -> fromResult . p s
  where
    fromResult :: Result (z,s) a -> (a, s, z)
    fromResult (Success (z,s) a)  =  (a, s, z)
    fromResult (Failure   _   e)  =  throwE e
    fromResult (Committed r)      =  fromResult r


instance Applicative (Parser s t) where
    pure f    = return f
    --   Apply a parsed function to a parsed value.  This version
    --   is strict in the result of the function parser, but
    --   lazy in the result of the argument parser.  (Argument laziness is
    --   the distinctive feature over other implementations.)
    (P pf) <*> px = P (\s-> continue . pf s)
      where
        continue (Success (z,s) f) = let (x,s',z') = runParser px s z
                                     in Success (z',s') (f x)
        continue (Failure zs e)    = Failure zs e
        continue (Committed r)     = Committed (continue r)
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative (Parser s t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser s t)

manyFinally :: Parser s t a -> Parser s t z -> Parser s t [a]
{-
manyFinally pp@(P p) pt@(P t) = P (\s ts -> item s ts (p s ts))
    where
      item _ _  (Success ts s x) = success ts s x
      item s ts (Failure _ _ e)  = terminate (t s ts)
      item s ts (Committed r)    = Committed (within r)

      success ts s x =
            let (tail,s',ts') = runParser (manyFinally pp pt) s ts
            in Success ts' s' (x:tail)

      terminate (Success ts s _) = Success ts s []
      terminate (Failure ts s e) = Failure ts s e
      terminate (Committed r)    = Committed (terminate r)

      within (Success ts s x)    = success ts s x
      within (Failure ts s e)    = Failure ts s e
      within (Committed r)       = within r
-}

manyFinally p z =
    (do x <- p; return (x:) `apply` manyFinally p z)
      `onFail`
    (do z; return [])
      `onFail`
    oneOf' [ ("item in sequence",    (do p; return []))
           , ("sequence terminator", (do z; return [])) ]

------------------------------------------------------------------------
