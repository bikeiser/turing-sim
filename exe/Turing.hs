{-# LANGUAGE LambdaCase #-}
module Turing where

import Data.List (intercalate)

data TuringResult = Execution
  { accept :: Bool,
    finalTape :: Tape,
    finalTrace :: [TMConfiguration],
    steps :: Integer
  } deriving Show

kPrintTrace :: [[TMConfiguration]] -> IO ()
kPrintTrace confs = putStrLn $ prLst' $ map (\x -> prLst x ++ "\n") confs
  where
    prLst xs = intercalate "\n" $ map show xs
    prLst' = intercalate "\n"

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

data KTuringResult = KExecution
  { kAccept :: Bool,
    kFinalTape :: Tape,
    kFinalTrace :: [[TMConfiguration]],
    kSteps :: Integer
  } deriving Show

krun :: KTuringMachineDesc -> String -> KTuringResult
krun m str =
  KExecution
    { kAccept = wasAccept,
      kFinalTape = finalTape,
      kFinalTrace = init finalTrace,
      kSteps = stepsCount
    }
  where
    -- type KTransition = ((State, [TapeChar]), (State, [TapeChar], [Direction]))
    k = length (snd . fst . head $ kTransitions m)
    wasAccept = hasAccept $ last finalTrace
    stepsCount = case head . last $ finalTrace of
      Accept _ i -> i
      Reject _ i -> i
      Running {} -> error ""
    finalTape = pruneTape $ case head . last $ finalTrace of
      Accept t _ -> t
      Reject t _ -> t
      Running {} -> error ""

    pruneTape :: Tape -> Tape
    pruneTape t = reverse $ dropBlanks $ reverse $ dropBlanks t
    
    dropBlanks :: Tape -> Tape
    dropBlanks = dropWhile (\c -> c == kBlank m)

    finalTrace :: [[TMConfiguration]]
    finalTrace = it [] initConf

    it :: [[TMConfiguration]] -> [TMConfiguration] -> [[TMConfiguration]]
    it cs confs 
        | hasAccept confs = confs:cs
        | hasReject confs = confs:cs
        | otherwise = confs : it cs (kstep m confs)

    initConf = initConfiguration str

    emptyConf :: TMConfiguration
    emptyConf = Running
        { left = [],
          tapeHead = (kStartState m, kBlank m),
          right = [],
          count = 0
        }

    initConfiguration :: [TapeChar] -> [TMConfiguration]
    initConfiguration [] =
      Running
        { left = [],
          tapeHead = (kStartState m, kBlank m),
          right = [],
          count = 0
        } : replicate (k-1) emptyConf
    initConfiguration (c : cs) =
      if all (`elem` kInputAlphabet m) (c : cs)
        then
          Running
            { left = [],
              tapeHead = (kStartState m, c),
              right = cs,
              count = 0
            } : replicate (k-1) emptyConf
        else error "invalid input!"

data KTuringMachineDesc = KTuringMachineDesc
  { kStates :: [State],
    kInputAlphabet :: Alphabet,
    kTapeAlphabet :: Alphabet,
    kTransitions :: [KTransition],
    kStartState :: State,
    kBlank :: TapeChar,
    kAcceptStates :: [State]
  }
  deriving (Show)

type KTransition = ((State, [TapeChar]), (State, [TapeChar], [Direction]))

hasAccept :: [TMConfiguration] -> Bool
hasAccept = any (\case Accept {} -> True; _ -> False)

hasReject :: [TMConfiguration] -> Bool
hasReject = any (\case Reject {} -> True; _ -> False)

kstep :: KTuringMachineDesc -> [TMConfiguration] -> [TMConfiguration]
kstep m confs
  | shouldAccept m confs = map (uncurry Accept . dump (kBlank m)) confs
  | otherwise =
      let (q, ts) =
            foldl
              ( \(qAcc, tsAcc) (q', t) ->
                  if qAcc == q'
                    then (qAcc, tsAcc ++ [t])
                    else error $ "invalid " ++ show k ++ "-tape TM"
              )
              (let (qAcc, _) = tapeHead . head $ confs in qAcc, [])
              (map tapeHead confs)
       in let transition = filter (\((inQ, inTs), _) -> inQ == q && ts == inTs) $ kTransitions m
           in case transition of
                [(_, o)] -> map (istep o) [0 .. k - 1]
                [] -> map (uncurry Reject . dump (kBlank m)) confs
                _ ->
                  error $ "invalid " ++ show k ++ "-tape TM " ++ show transition
  where
    k = length confs

    istep (outQ, outTs, outDirs) i = move (outDirs !! i) $ update (outQ, outTs !! i) (confs !! i)

    move dir conf = case dir of
      TMLeft -> moveLeft (kBlank m) conf
      TMRight -> moveRight (kBlank m) conf
      TMStay -> conf

update :: (State, TapeChar) -> TMConfiguration -> TMConfiguration
update (state, char) conf =
  Running
    { left = left conf,
      tapeHead = (state, char),
      right = right conf,
      count = 1 + count conf
    }

shouldAccept :: KTuringMachineDesc -> [TMConfiguration] -> Bool
shouldAccept m = any (\conf -> let (q, _) = tapeHead conf in q `elem` kAcceptStates m)

dump :: TapeChar -> TMConfiguration -> (Tape, Integer)
dump blank conf =
  let (_, c) = tapeHead conf
   in if c == blank
        then (reverse ls ++ rs, count conf)
        else (reverse ls ++ c : rs, count conf)
  where
    ls = dropWhile (== blank) $ left conf
    rs = dropWhile (== blank) $ right conf

step :: TuringMachineDesc -> TMConfiguration -> TMConfiguration
step _ a@Accept {} = a
step _ r@Reject {} = r
step m conf@Running {} =
  if currentState (tapeHead conf) `elem` acceptStates m
    then uncurry Accept (dump (blank m) conf)
    else case lookup (tapeHead conf) (transitions m) of
      Just (newState, writeChar, dir) ->
        let updatedConf = update (newState, writeChar) conf
         in case dir of
              TMLeft -> moveLeft (blank m) updatedConf
              TMRight -> moveRight (blank m) updatedConf
              TMStay -> updatedConf
      Nothing -> uncurry Reject (dump (blank m) conf)

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

data Direction = TMLeft | TMRight | TMStay deriving Show

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

moveLeft :: TapeChar -> TMConfiguration -> TMConfiguration
moveLeft _ a@Accept {} = a
moveLeft _ r@Reject {} = r
moveLeft blank conf@(Running {}) =
  let (l, ls) = case left conf of
        [] -> (blank, [])
        (l' : ls') -> (l', ls')
   in Running
        { left = ls,
          tapeHead = tapeHeadPutChar (tapeHead conf) l,
          right = headChar (tapeHead conf) : right conf,
          count = count conf
        }

moveRight :: TapeChar -> TMConfiguration -> TMConfiguration
moveRight _ a@Accept {} = a
moveRight _ r@Reject {} = r
moveRight blank conf@(Running {}) =
  let (r, rs) = case right conf of
        [] -> (blank, [])
        (r' : rs') -> (r', rs')
   in Running
        { left = headChar (tapeHead conf) : left conf,
          tapeHead = tapeHeadPutChar (tapeHead conf) r,
          right = rs,
          count = count conf
        }

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


kTapeEqual01 :: KTuringMachineDesc
kTapeEqual01 =
  KTuringMachineDesc
    { kStates = ["0", "1", "2"],
      kInputAlphabet = "01",
      kTapeAlphabet = "01XYB",
      kTransitions = ts,
      kStartState = "0",
      kBlank = 'B',
      kAcceptStates = ["2"]
    }
  where
    ts :: [KTransition]
    ts =
      [ 
        (("0", ['0', 'B']), ("0", ['B', '0'], [TMRight, TMRight])), -- copy 0s
        (("0", ['1', 'B']), ("1", ['1', 'B'], [TMStay, TMLeft])), -- until 1 is encountered
        (("1", ['1', '0']), ("1", ['B', 'B'], [TMRight, TMLeft])), -- tape1: 111, tape2: 000
        (("1", ['B', 'B']), ("2", ['B', 'B'], [TMStay, TMStay])) 
      ]

