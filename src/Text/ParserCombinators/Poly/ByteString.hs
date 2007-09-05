module Text.ParserCombinators.Poly.ByteString
  ( -- * The Parser datatype
    Parser(P)	-- datatype, instance of: Functor, Monad, PolyParse
  , runParser	-- :: Parser a -> ByteString -> (Either String a, ByteString)
    -- ** basic parsers
  , next	-- :: Parser Char
  , satisfy	-- :: (Char->Bool) -> Parser Char

    -- ** re-parsing
  , reparse	-- :: [Char] -> Parser ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where

import Text.ParserCombinators.Poly.Base
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
#if defined(__GLASGOW_HASKELL__)
import GHC.Base (unsafeCoerce#)
unsafeCoerce = unsafeCoerce#
#else
#endif

-- | This @Parser@ datatype is a parsing monad with error reporting.
--   It has fixed token type, namely Char stored in a ByteString, so
--   cannot really be called 'poly' in the sense of polymorphic tokens.
--   But in all other respects, this parser type follows the pattern,
--   e.g. by having two levels of errors.
newtype Parser a = P (ByteString -> (EitherE String a, ByteString))

--   A return type like Either, that distinguishes not only between
--   right and wrong answers, but also has gradations of wrongness.
type EitherE a b = Either (Bool,a) b

-- | Apply a parser to an input token sequence.
runParser :: Parser a -> ByteString -> (Either String a, ByteString)
runParser (P p) =
    (\ (e,ts)-> (case e of {Left (_,m)->Left m; Right m->Right m}, ts) )
    . p

instance Functor Parser where
    fmap f (P p) = P (\ts-> case p ts of
                                (Left msg, ts') -> (Left msg,    ts')
                                (Right x,  ts') -> (Right (f x), ts'))
instance Monad Parser where
    return x     = P (\ts-> (Right x, ts))
    (P f) >>= g  = P (\ts-> case f ts of
                                (Left msg, ts') -> (Left msg, ts')
                                (Right x,  ts') -> let (P g') = g x in g' ts')
    fail e       = P (\ts-> (Left (False,e), ts))

instance PolyParse Parser where
    commit (P p)         = P (\ts-> case p ts of
                                      (Left (_,e), ts') -> (Left (True,e), ts')
                                      right             -> right )
    (P p) `adjustErr` f  = P (\ts-> case p ts of
                                 (Left (b,msg), ts') -> (Left (b,(f msg)), ts')
                                 right               -> right )
    (P p) `onFail` (P q) = P (\ts-> case p ts of
                                      r@(Left (True,_), _) -> r
                                      (Left _, _) -> q ts
                                      right       -> right )
    oneOf' = accum []
      where accum errs [] =
              case filter isBad errs of
                [] ->  fail ("failed to parse any of the possible choices:\n"
                            ++indent 2 (concatMap showErr (reverse errs)))
                [(_,(_,e))] -> failBad e
                es -> failBad ("one of the following failures occurred:\n"
                              ++indent 2 (concatMap showErr (reverse es)))
            accum errs ((e,P p):ps) =
                P (\ts-> case p ts of
                           (Left err,_) -> let (P p) = accum ((e,err):errs) ps
                                           in p ts
                           right        -> right )
            showErr (name,(_,err)) = name++":\n"++indent 2 err
            isBad (_,(b,_)) = b

-- next token
next :: Parser Char
next = P (\ts-> case BS.uncons ts of
                  Nothing  -> (Left (False,"Ran out of input (EOF)"), BS.empty)
                  Just (t,ts') -> (Right (unsafeCoerce t), ts') )

satisfy :: (Char->Bool) -> Parser Char
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }

------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [Char] -> Parser ()
reparse ts  = P (\inp-> (Right (), (BS.pack ts `BS.append` inp)))

------------------------------------------------------------------------
