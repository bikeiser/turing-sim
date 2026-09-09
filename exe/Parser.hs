{-# LANGUAGE LambdaCase #-}
module Parser (ParseError, parseString) where

import Turing

import Control.Applicative (optional)
import Control.Monad (void)
import Data.Char (isDigit)
import Text.Parsec
import Text.Parsec.String

parseString :: String -> Either ParseError KTuringMachineDesc
parseString = parse machine ""

machine :: Parser KTuringMachineDesc
machine = do
  k <- Control.Applicative.optional num >>= \case Just n -> return n; Nothing -> return 1
  voidSymbol "M"
  eq
  lparan
  q <- turingStates <* comma
  inputA <- alphabet <* comma
  tape <- alphabet <* comma
  voidSymbol "d" <* comma
  start <- turingState <* comma
  blankC <- tapeChar <* comma
  accept <- turingStates
  rparan
  voidSymbol "d"
  eq
  d <- turingTransitions k
  eof

  return $
    KTuringMachineDesc
      { k = k,
        kStates = q,
        kInputAlphabet = inputA,
        kTapeAlphabet = tape,
        kTransitions = d,
        kStartState = start,
        kBlank = blankC,
        kAcceptStates = accept
      }

turingStates :: Parser [Turing.State]
turingStates = between lcurl rcurl $ turingState `sepBy` comma

turingState :: Parser Turing.State
turingState = between quote quote stateName
  where
    stateName :: Parser String
    stateName = many1 (alphaNum <|> char '_')

alphabet :: Parser [TapeChar]
alphabet = between lcurl rcurl $ tapeChar `sepBy` comma

tapeChar :: Parser TapeChar
tapeChar = between tick tick $ noneOf "'"

turingTransitions :: Int -> Parser [KTransition]
turingTransitions k = between lbrack rbrack $ turingTransition k `sepBy` comma
  where
    turingTransition :: Int -> Parser KTransition
    turingTransition k = do
      lparan
      inputState <- turingState
      comma
      inputChars <- kListOrJustOne k tapeChar
      rparan
      arrow
      lparan
      resultState <- turingState
      comma
      resultChars <- kListOrJustOne k tapeChar 
      comma
      resultDirs <- kListOrJustOne k dir 
      rparan
      return ((inputState, inputChars), (resultState, resultChars, resultDirs))

kListOrJustOne :: Int -> Parser a -> Parser [a]
kListOrJustOne k p = if k == 1 
    then (: []) <$> p 
    else reverse <$> between lparan rparan (flip (:) <$> count (k - 1) (p <* comma) <*> p)

ws :: Parser ()
ws = skipMany (void (oneOf [' ', '\t', '\n']))

lexeme :: Parser a -> Parser a
lexeme p = do
  v <- p
  ws
  return v

symbol :: String -> Parser String
symbol = try . lexeme . string

voidSymbol :: String -> Parser ()
voidSymbol = void . symbol

num :: Parser Int
num = read <$> many1 (satisfy isDigit)

arrow :: Parser ()
arrow = voidSymbol "->"

dir :: Parser Direction
dir = Parser.left <|> Parser.right <|> Parser.stay

left :: Parser Direction
left = lexeme (void $ char 'L') >> return TMLeft

right :: Parser Direction
right = lexeme (void $ char 'R') >> return TMRight

stay :: Parser Direction
stay = lexeme (void $ char 'S') >> return TMStay

lparan :: Parser ()
lparan = lexeme $ void $ char '('

rparan :: Parser ()
rparan = lexeme $ void $ char ')'

lbrack :: Parser ()
lbrack = lexeme $ void $ char '['

rbrack :: Parser ()
rbrack = lexeme $ void $ char ']'

lcurl :: Parser ()
lcurl = lexeme $ void $ char '{'

rcurl :: Parser ()
rcurl = lexeme $ void $ char '}'

quote :: Parser ()
quote = lexeme $ void $ char '"'

tick :: Parser ()
tick = lexeme $ void $ char '\''

eq :: Parser ()
eq = lexeme $ void $ char '='

comma :: Parser ()
comma = lexeme $ void $ char ','

