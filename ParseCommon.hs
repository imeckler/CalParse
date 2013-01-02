{-# LANGUAGE FlexibleContexts #-}

module ParseCommon
    ( module Text.Parsec
    , manyTillE
    , word
    ) where

import Text.Parsec
import Control.Applicative ((<$>))
import Data.Char (isSpace)

-- | @manyTillE p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@
-- with the value returned by @end@ at the list's end.

manyTillE :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m [a]
manyTillE p end = scan
    where scan = (:[]) <$> end
              <|>
                 do { x <- p; xs <- scan; return (x:xs)}

-- | @nonSpace@ parses a single non-whitespace character.

nonSpace :: Stream s m Char => ParsecT s u m Char
nonSpace = satisfy (not . isSpace)


-- | @word@ parses a non-empty sequence of letters.
word :: Stream s m Char => ParsecT s u m String
word = many1 letter


