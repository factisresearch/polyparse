module Text.ParserCombinators.Poly.Lazy
  ( -- * The Parser datatype.
    -- $parser
    Parser(P)	-- datatype, instance of: Functor, Monad
  , runParser	-- :: Parser t a -> [t] -> (a, [t])
    -- ** basic parsers
  , next	-- :: Parser t t
  , satisfy	-- :: (t->Bool) -> Parser t t
    -- one defn from 'Base' is overridden here, because it depends
    -- on the representation of the Lazy parser monad
  , manyFinally	-- :: Parser t a -> Parser t z -> Parser t [a]
    -- ** Re-parsing
  , reparse	-- :: [t] -> Parser t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where

import Text.ParserCombinators.Poly.Base hiding ( manyFinally )

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

-- $parser
-- When applied, these parsers do not return explicit failure.
-- An exception is
-- raised instead.  This allows partial results to be returned
-- before a full parse is complete.
-- One of the key ways to ensure that your parser is properly lazy,
-- is to parse the initial portion of text returning a function, then
-- use the @apply@ combinator to build the final value.

-- | The @Parser@ datatype is a fairly generic parsing monad with error
--   reporting.  It can be used for arbitrary token types, not just
--   String input.  (If you require a running state, use module PolyStateLazy
--   instead.)
newtype Parser t a = P ([t] -> (Either String a, [t]))

-- | Apply a parser to an input token sequence.  The parser cannot return
--   an error value explicitly, so errors raise an exception.  Thus, results
--   can be partial (lazily constructed, but containing undefined).
runParser :: Parser t a -> [t] -> (a, [t])
runParser (P p) =
    (\ (e,ts)-> (case e of {Left m->throwE m; Right x->x}, ts) )
    . p

instance Functor (Parser t) where
    fmap f (P p) = P (\ts-> case p ts of
                                (Left msg, ts') -> (Left msg,    ts')
                                (Right x,  ts') -> (Right (f x), ts'))
instance Monad (Parser t) where
    return x     = P (\ts-> (Right x, ts))
    (P f) >>= g  = P (\ts-> case f ts of
                                (Left msg, ts') -> (Left msg, ts')
                                (Right x,  ts') -> let (P g') = g x in g' ts')
    fail e       = P (\ts-> (Left e, ts))


instance PolyParse (Parser t) where
    commit (P p)         = P (\ts-> case p ts of
                                      (Left e, ts') -> (throwE e, ts')
                                      right         -> right )
    (P p) `onFail` (P q) = P (\ts-> case p ts of
                                      (Left _, _) -> q ts
                                      right       -> right )
    (P p) `adjustErr` f  = P (\ts-> case p ts of
                                      (Left msg, ts') -> (Left (f msg), ts')
                                      right           -> right )
    oneOf' ps = accum [] ps
      where accum errs [] =
              case errs of
                [] ->  failBad ("internal failure in parser (oneOf'):\n"
                               ++indent 2 (show (map fst ps)))
                [(_,e)] -> fail e
                es -> fail ("one of the following failures occurred:\n"
                           ++indent 2 (concatMap showErr (reverse es)))
            accum errs ((e,P p):ps) =
                P (\ts-> case p ts of
                           (Left err,_) -> let (P p) = accum ((e,err):errs) ps
                                           in p ts
                           right        -> right )
            showErr (name,err) = name++":\n"++indent 2 err
    -- | This version of `apply`
    --   is strict in the result of the function parser, but
    --   lazy in the result of the argument parser.  (Argument laziness is
    --   the distinctive feature over other implementations.)
    (P pf) `apply` (P px) = P (\ts->
        case pf ts of
          (Left msg, ts') -> (Left msg, ts')
          (Right f,  ts') -> let (x',ts'') = px ts'
                                 x = case x' of { Right x -> x
                                                ; Left e -> throwE e }
                             in (Right (f x), ts'') )

-- | Next token
next = P (\ts-> case ts of
                  []  -> (Left "Ran out of input (EOF)", [])
                  (t:ts') -> (Right t, ts') )

-- | One token satifying a predicate
satisfy :: (t->Bool) -> Parser t t
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }

-- | 'manyFinally e t' parses a possibly-empty sequence of e's,
--   terminated by a t.  Any parse failures could be due either to
--   a badly-formed terminator or a badly-formed element, so raise
--   both possible errors.
manyFinally :: Parser t a -> Parser t z -> Parser t [a]
manyFinally pp@(P p) pt@(P t) = P (\ts ->
    case p ts of
        (Left e, _) ->
            case t ts of
                (Right _, ts') -> (Right [], ts')
                (Left e,  ts') -> (Left e,   ts')
        (Right x, ts') ->
            let (tail,ts'') = runParser (manyFinally pp pt) ts'
            in (Right (x:tail), ts'') )


------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser t ()
reparse ts  = P (\inp-> (Right (), ts++inp))

------------------------------------------------------------------------
