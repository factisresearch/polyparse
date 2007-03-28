module Text.ParserCombinators.Poly.StateLazy
  ( -- * The Parser datatype.
    -- $parser
    Parser(P)	-- datatype, instance of: Functor, Monad
  , runParser	-- :: Parser s t a -> s -> [t] -> (a, s, [t])
    -- * Combinators:
    -- ** Primitives
  , next	-- :: Parser s t t
  , satisfy	-- :: (t->Bool) -> Parser s t t
    -- ** State-handling
  , stUpdate	-- :: (s->s) -> Parser s t ()
  , stQuery	-- :: (s->a) -> Parser s t a
  , stGet	-- :: Parser s t s
    -- ** Re-parsing
  , reparse	-- :: [t] -> Parser s t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where

import Text.ParserCombinators.Poly.Base

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

-- $parser
-- Parsers do not return explicit failure.  An exception is raised
-- instead.  This allows partial results to be returned before a
-- full parse is complete.

-- | The @Parser@ datatype is a fairly generic parsing monad with error
--   reporting and a running state.  It can be used for arbitrary token
--   types, not just String input.
newtype Parser s t a = P (s -> [t] -> (Either String a, s, [t]))

-- | A return type like Either, that distinguishes not only between
--   right and wrong answers, but also had gradations of wrongness.
--   Not used in this library.  !!!!!!!!!!!!!!!!!!!!!!!!!!
type EitherE a b = Either (Bool,a) b

-- | Apply a parser to an initial state and input token sequence.
--   The parser cannot return an error value explicitly, so errors
--   raise an exception.  Thus, results can be partial (lazily constructed,
--   but containing undefined).
runParser :: Parser s t a -> s -> [t] -> (a, s, [t])
runParser (P p) s =
    (\ (e,s,ts)-> (case e of {Left m->throwE m; Right x->x}, s, ts))
    . p s

instance Functor (Parser s t) where
    fmap f (P p) = P (\s ts-> case p s ts of
                                (Left msg, s', ts') -> (Left msg,    s', ts')
                                (Right x,  s', ts') -> (Right (f x), s', ts'))
instance Monad (Parser s t) where
    return x     = P (\s ts-> (Right x, s, ts))
    (P f) >>= g  = P (\s ts-> case f s ts of
                                (Left msg, s', ts') -> (Left msg, s', ts')
                                (Right x,  s', ts') -> let (P g') = g x
                                                       in g' s' ts')
    fail msg     = P (\s ts-> (Left msg, s, ts))

instance PolyParse (Parser s t) where
    failBad msg      = P (\s ts-> (throwE msg, s, ts))

    commit (P p) = P (\s ts-> case p s ts of
                            (Left e, s', ts') -> (throwE e, s', ts')
                            right             -> right )
    (P p) `adjustErr` f =
        P (\s ts-> case p s ts of
                     (Left msg, s', ts') -> (Left (f msg), s, ts')
                     right               -> right )
    (P p) `onFail` (P q) = P (\s ts-> case p s ts of
                                    (Left _, _, _) -> q s ts
                                    right          -> right )
    oneOf' ps = accum [] ps
      where accum errs [] =
              case errs of
                [] -> failBad ("internal failure in parser (oneOf'):\n"
                              ++indent 2 (show (map fst ps)))
                [(_,e)] -> fail e
                es -> fail ("one of the following failures occurred:\n"
                           ++indent 2 (concatMap showErr (reverse es)))
            accum errs ((e,P p):ps) =
                P (\u ts-> case p u ts of
                           (Left err,_,_) -> let (P p) = accum ((e,err):errs) ps
                                             in p u ts
                           right          -> right )
            showErr (name,err) = name++":\n"++indent 2 err

-- Combinators

-- | Yield one token.
next :: Parser s t t
next = P (\s ts-> case ts of
                    []  -> (Left "Ran out of input (EOF)", s, [])
                    (t:ts') -> (Right t, s, ts') )

-- | Yield one token if it satisfies a predicate.
satisfy :: (t->Bool) -> Parser s t t
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }


------------------------------------------------------------------------
-- State handling

-- | Update the internal state.
stUpdate   :: (s->s) -> Parser s t ()
stUpdate f  = P (\s ts-> (Right (), f s, ts))

-- | Query the internal state.
stQuery    :: (s->a) -> Parser s t a
stQuery f   = P (\s ts-> (Right (f s), s, ts))

-- | Deliver the entire internal state.
stGet      :: Parser s t s
stGet       = P (\s ts-> (Right s, s, ts))

------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser s t ()
reparse ts  = P (\s inp-> (Right (), s, ts++inp))

------------------------------------------------------------------------
