module Parser (ParseError, parseString) where

import Turing

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

parseString :: String -> Either ParseError TuringMachineDesc
parseString = parse machine ""

machine :: Parser TuringMachineDesc
machine = do
    voidSymbol "M"
    eq
    lparan
    q<-turingStates <* comma
    inputA<-alphabet <* comma
    tape<-alphabet <* comma
    voidSymbol "d" <* comma
    start<-turingState <* comma
    blankC<-tapeChar <* comma
    accept<-turingStates
    rparan
    voidSymbol "d"
    eq
    d<-turingTransitions
    eof

    return $ TuringMachineDesc {
        states=q,
        inputAlphabet=inputA,
        tapeAlphabet=tape,
        transitions=d,
        startState=start,
        blank=blankC,
        acceptStates=accept
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

turingTransitions :: Parser [Transition]
turingTransitions = between lbrack rbrack $ turingTransition `sepBy` comma
  where
    turingTransition :: Parser Transition
    turingTransition = do
      lparan
      inputState <- turingState
      comma
      inputChar <- tapeChar
      rparan
      arrow
      lparan
      resultState <- turingState
      comma
      resultChar <- tapeChar
      comma
      resultDir <- direction
      rparan
      return ((inputState, inputChar), (resultState, resultChar, resultDir))
    direction :: Parser Direction
    direction = Parser.left <|> Parser.right

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

arrow :: Parser ()
arrow = voidSymbol "->"

left :: Parser Direction
left = lexeme (void $ char 'L') >> return TMLeft

right :: Parser Direction
right = lexeme (void $ char 'R') >> return TMRight

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

