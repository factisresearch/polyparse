module Text.ParserCombinators.Poly.State
  ( -- * The Parser datatype
    Parser(P)	-- datatype, instance of: Functor, Monad
  , runParser	-- :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
    -- ** basic parsers
  , next	-- :: Parser s t t
  , satisfy	-- :: (t->Bool) -> Parser s t t
    -- ** State-handling
  , stUpdate	-- :: (s->s) -> Parser s t ()
  , stQuery	-- :: (s->a) -> Parser s t a
  , stGet	-- :: Parser s t s
    -- ** Re-parsing
  , reparse	-- :: [t] -> Parser s t ()
    -- * Re-export all more general combinators.
  , module Text.ParserCombinators.Poly.Base
  ) where

import Text.ParserCombinators.Poly.Base

-- | The @Parser@ datatype is a fairly generic parsing monad with error
--   reporting and a running state.  It can be used for arbitrary token
--   types, not just String input.
newtype Parser s t a = P (s -> [t] -> (EitherE String a, s, [t]))

-- | A return type like Either, that distinguishes not only between
--   right and wrong answers, but also had gradations of wrongness.
type EitherE a b = Either (Bool,a) b

-- | Apply a parser to an initial state and input token sequence.
runParser :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
runParser (P p) s =
    (\ (e,s,ts)-> (case e of Left (_,m)->Left m; Right m->Right m
                  ,s,ts))
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
    fail msg     = P (\s ts-> (Left (False,msg), s, ts))

instance PolyParse (Parser s t) where
    commit (P p) = P (\s ts-> case p s ts of
                          (Left (_,e), s', ts') -> (Left (True,e), s', ts')
                          right                 -> right )
    (P p) `onFail` (P q) = P (\s ts-> case p s ts of
                                        r@(Left (True,_), _, _) -> r
                                        (Left _, _, _) -> q s ts
                                        right          -> right )
    (P p) `adjustErr` f  = P (\s ts-> case p s ts of
                                        (Left (b,msg), s', ts')
                                                  -> (Left (b,(f msg)), s, ts')
                                        right     -> right )
    oneOf' = accum []
      where accum errs [] =
              case filter isBad errs of
                [] -> fail ("failed to parse any of the possible choices:\n"
                           ++indent 2 (concatMap showErr (reverse errs)))
                [(_,(_,e))] -> failBad e
                es -> failBad ("one of the following failures occurred:\n"
                              ++indent 2 (concatMap showErr (reverse es)))
            accum errs ((e,P p):ps) =
                P (\u ts-> case p u ts of
                           (Left err,_,_) -> let (P p) = accum ((e,err):errs) ps
                                             in p u ts
                           right          -> right )
            showErr (name,(_,err)) = name++":\n"++indent 2 err
            isBad (_,(b,_)) = b

------------------------------------------------------------------------
next = P (\s ts-> case ts of
                    []  -> (Left (False,"Ran out of input (EOF)"), s, [])
                    (t:ts') -> (Right t, s, ts') )

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
