module Turing where

data TuringResult = Execution
  { accept :: Bool,
    finalTape :: Tape,
    finalTrace :: [TMConfiguration],
    steps :: Integer
  } deriving Show

run :: TuringMachineDesc -> String -> TuringResult
run m str =
  Execution
    { accept = wasAccept,
      finalTape = finalTape,
      finalTrace = init finalTrace,
      steps = stepsCount
    }
  where
    wasAccept = case last finalTrace of Accept {} -> True; Reject {} -> False; Running {} -> error ""
    stepsCount = case last finalTrace of
      Accept _ i -> i
      Reject _ i -> i
      Running {} -> error ""
    finalTape = pruneTape $ case last finalTrace of
      Accept t _ -> t
      Reject t _ -> t
      Running {} -> error ""

    pruneTape :: Tape -> Tape
    pruneTape t = reverse $ dropBlanks $ reverse $ dropBlanks t
    
    dropBlanks :: Tape -> Tape
    dropBlanks = dropWhile (\c -> c == blank m)

    finalTrace :: [TMConfiguration]
    finalTrace = it [] initConf

    it :: [TMConfiguration] -> TMConfiguration -> [TMConfiguration]
    it cs a@Accept {} = a:cs
    it cs r@Reject {} = r:cs
    it cs conf = conf : it cs (step m conf)

    initConf = initConfiguration str

    initConfiguration [] =
      Running
        { left = [],
          tapeHead = (startState m, blank m),
          right = [],
          count = 0
        }
    initConfiguration (c : cs) =
      if all (`elem` inputAlphabet m) (c : cs)
        then
          Running
            { left = [],
              tapeHead = (startState m, c),
              right = cs,
              count = 0
            }
        else error "invalid input!"

data TuringMachineDesc = TuringMachineDesc
  { states :: [State],
    inputAlphabet :: Alphabet,
    tapeAlphabet :: Alphabet,
    transitions :: [Transition],
    startState :: State,
    blank :: TapeChar,
    acceptStates :: [State]
  } deriving Show

type State = String

type Alphabet = [TapeChar]

type TapeHead = (State, TapeChar)

type Transition = (TapeHead, (State, TapeChar, Direction))

data Direction = TMLeft | TMRight deriving Show

type TapeChar = Char

type Tape = [TapeChar]

data TMConfiguration
  = Running
      { left :: Tape,
        tapeHead :: TapeHead,
        right :: Tape,
        count :: Integer
      }
  | Accept Tape Integer
  | Reject Tape Integer

instance Show TMConfiguration where
  show (Accept _ _) = "accept!"
  show (Reject _ _) = "reject!"
  show Running {left = tmleft, tapeHead = (s, c), right = tmright} =
    reverse tmleft
      ++ (c : tmright)
      ++ "\n"
      ++ replicate (length tmleft) ' '
      ++ s

data TuringMachineDescError
    = InvalidStates
    | InvalidAlhabets
    | InvalidTransitions
    deriving (Show, Eq)

wellFormed :: TuringMachineDesc -> Maybe TuringMachineDescError
wellFormed m
    | not statesOk       = Just InvalidStates
    | not alphabetsOk    = Just InvalidAlhabets
    | not transitionsOk  = Just InvalidTransitions
    | otherwise          = Nothing
  where
    statesOk = startState m `elem` states m && all (`elem` states m) (acceptStates m)
    alphabetsOk = all (`elem` tapeAlphabet m) (inputAlphabet m)
    transitionsOk =
      all
        ( \((inputState, inputChar), (outState, outChar, _)) ->
            inputState `elem` states m
              && inputChar `elem` tapeAlphabet m
              && outChar `elem` tapeAlphabet m
              && outState `elem` states m
        )
        (transitions m)

tapeHeadPutChar :: TapeHead -> TapeChar -> TapeHead
tapeHeadPutChar (state, _) c = (state, c)

headChar :: TapeHead -> TapeChar
headChar (_, c) = c

currentState :: TapeHead -> State
currentState (s, _) = s

moveLeft :: TuringMachineDesc -> TMConfiguration -> TMConfiguration
moveLeft _ a@Accept {} = a
moveLeft _ r@Reject {} = r
moveLeft m conf@(Running {}) =
  let (l, ls) = case left conf of
        [] -> (blank m, [])
        (l' : ls') -> (l', ls')
   in Running
        { left = ls,
          tapeHead = tapeHeadPutChar (tapeHead conf) l,
          right = headChar (tapeHead conf) : right conf,
          count = count conf
        }

moveRight :: TuringMachineDesc -> TMConfiguration -> TMConfiguration
moveRight _ a@Accept {} = a
moveRight _ r@Reject {} = r
moveRight m conf@(Running {}) =
  let (r, rs) = case right conf of
        [] -> (blank m, [])
        (r' : rs') -> (r', rs')
   in Running
        { left = headChar (tapeHead conf) : left conf,
          tapeHead = tapeHeadPutChar (tapeHead conf) r,
          right = rs,
          count = count conf
        }

step :: TuringMachineDesc -> TMConfiguration -> TMConfiguration
step _ a@Accept {} = a
step _ r@Reject {} = r
step m conf@Running {} =
  if currentState (tapeHead conf) `elem` acceptStates m
    then uncurry Accept dump
    else case lookup (tapeHead conf) (transitions m) of
      Just (newState, writeChar, dir) ->
        let updatedConf = update (newState, writeChar)
         in case dir of
              TMLeft -> moveLeft m updatedConf
              TMRight -> moveRight m updatedConf
      Nothing -> uncurry Reject dump
  where
    update (state, char) =
      Running
        { left = left conf,
          tapeHead = (state, char),
          right = right conf,
          count = 1 + count conf
        }

    dump :: (Tape, Integer)
    dump =
      let (_, c) = tapeHead conf
       in if c == blank m
            then (reverse ls ++ rs, count conf)
            else (reverse ls ++ c : rs, count conf)
      where
        ls = dropWhile (== blank m) $ left conf
        rs = dropWhile (== blank m) $ right conf

equal01 :: TuringMachineDesc
equal01 =
  TuringMachineDesc
    { states = ["0", "1", "2", "3", "4"],
      inputAlphabet = "01",
      tapeAlphabet = "01XYB",
      transitions = ts,
      startState = "0",
      blank = 'B',
      acceptStates = ["4"]
    }
  where
    ts :: [Transition]
    ts =
      [ (("0", '0'), ("1", 'X', TMRight)),
        (("0", 'Y'), ("3", 'Y', TMRight)),
        (("1", '0'), ("1", '0', TMRight)),
        (("1", '1'), ("2", 'Y', TMLeft)),
        (("1", 'Y'), ("1", 'Y', TMRight)),
        (("2", '0'), ("2", '0', TMLeft)),
        (("2", 'X'), ("0", 'X', TMRight)),
        (("2", 'Y'), ("2", 'Y', TMLeft)),
        (("3", 'Y'), ("3", 'Y', TMRight)),
        (("3", 'B'), ("4", 'B', TMRight))
      ]

