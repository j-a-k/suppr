{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module JavaParser
    ( Line(..),
      getSuppressions,
      javaParser,
      javaLineParser,
      suppressionParser,
      reasonParser,
      quotedString,
      skipString,
      skipToEOL
    ) where


import Control.Applicative
import Data.Attoparsec.Text
import Data.List
import qualified Data.Text as T
import Data.Data
import Data.Generics

-- |Represents a code line we may or may not be interested in
--
-- * 'Justified' is a warning suppression with a justification (good)
--
-- * 'Unjustified' is a warning suppression without a justification (bad)
--
-- * 'Code' is any other line (uninteresting)
data Line = Justified {rule :: T.Text, reason :: T.Text}
          | Unjustified {rule :: T.Text}
          | Code deriving (Show, Eq, Data, Typeable)

isJustified :: Line -> Bool
isJustified (Justified _ _) = True
isJustified _ = False


-- |Tell us which lines are interesting
-- True if a 'Justified' or 'Unjustifed' 'Line', False if a code line
isSuppression :: Line -> Bool
isSuppression Code = False 
isSuppression _ = True

-- |Return all the suppresssions in the text
getSuppressions :: T.Text -> ([Line],[Line])
getSuppressions content = case parseOnly javaParser content of
                           Right result -> partition (not.isJustified) $ filter isSuppression result
                           _ -> ([],[])

-- |Parse all lines
javaParser :: Parser [Line]
javaParser = many1 javaLineParser

-- |Parse a single java line
javaLineParser :: Parser Line
javaLineParser = suppressionParser <|> skipToEOL *> pure Code

-- |Try to parse a line as a suppression, either 'Justified' or 'Unjustified'
suppressionParser :: Parser Line
suppressionParser = do
  rule <- ruleParser
  option (Unjustified rule) $ fmap (Justified rule) reasonParser <* skipSpace

-- |Try to parse a rule
ruleParser :: Parser T.Text
ruleParser = annotationParser *> char '(' *> skipSpace *>
             quotedString <* skipSpace <* char ')'

-- |Try to parse the annotations we are interested in
annotationParser :: Parser ()
annotationParser = skipString "@SuppressWarnings" <|> skipString "@SuppressFBWarnings"

-- |Try to parse a reason (i.e. a comment)
reasonParser :: Parser T.Text
reasonParser = skipString "//" *> Data.Attoparsec.Text.takeWhile (/= '\n') <?> "reasonParser"

-- |Try to parse a string in quotes returning the string without quotes
quotedString :: Parser T.Text
quotedString = char '"' *> Data.Attoparsec.Text.takeWhile (/= '"') <* char '"' <?> "quotedString"

-- |Skip a specific string plus any whitespace before and after
skipString :: T.Text -> Parser ()
skipString str = skipSpace *> string str *> skipSpace <?> ("skipString " ++ T.unpack str)
 
-- |Skip all chars upto and including the end of line
skipToEOL :: Parser ()
skipToEOL = skipWhile (/= '\n') *> endOfLine
