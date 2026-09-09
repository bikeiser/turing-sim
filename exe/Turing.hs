{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Turing where

import Data.List (intercalate)

type Tape = [TapeChar]

type TapeChar = Char

data TuringMachineDescError
  = InvalidStates
  | InvalidAlhabets
  | InvalidTransitions
  deriving (Show, Eq)

data TuringResult = TuringResult
  { accept :: Bool,
    finalTape :: Tape,
    finalTrace :: [Tape],
    steps :: Int
  }
  deriving (Show)

class TuringMachine m where
    type Configuration m

    blankChar :: m -> TapeChar

    wellFormed :: m -> Maybe TuringMachineDescError
    initConfiguration :: m -> String -> Configuration m
    step :: m -> Configuration m -> Configuration m
    halted :: m -> Configuration m -> Bool
    accepted :: m -> Configuration m -> Bool
    tapeOf :: m -> Configuration m -> Tape

run :: (TuringMachine m) => m -> String -> TuringResult
run m input =
  let cs = execute m (initConfiguration m input)
      c = last cs
   in TuringResult
        { accept = accepted m c,
          finalTape = tapeOf m . last $ cs,
          finalTrace = map (tapeOf m) $ init cs,
          steps = length cs - 1
        }

execute :: (TuringMachine m) => m -> Configuration m -> [Configuration m]
execute tm conf = execute' tm conf []
  where
    execute' :: (TuringMachine m) => m -> Configuration m -> [Configuration m] -> [Configuration m]
    execute' tm' x xs =
      if halted tm' x
        then reverse (x : xs)
        else x : execute' tm' (step tm' x) xs

type State = String

type Alphabet = [TapeChar]

type TapeHead = (State, TapeChar)

type Transition = (TapeHead, (State, TapeChar, Direction))

data Direction = TMLeft | TMRight | TMStay deriving (Show)

data TMConfiguration
  = Running
      { left :: Tape,
        tapeHead :: TapeHead,
        right :: Tape
      }
  | Accept Tape
  | Reject Tape

instance Show TMConfiguration where
  show (Accept _) = "accept!"
  show (Reject _) = "reject!"
  show Running {left = tmleft, tapeHead = (s, c), right = tmright} =
    reverse tmleft
      ++ (c : tmright)
      ++ "\n"
      ++ replicate (length tmleft) ' '
      ++ s

data TuringMachineDesc = TuringMachineDesc
  { states :: [State],
    inputAlphabet :: Alphabet,
    tapeAlphabet :: Alphabet,
    transitions :: [Transition],
    startState :: State,
    blank :: TapeChar,
    acceptStates :: [State]
  }
  deriving (Show)

data KTuringMachineDesc = KTuringMachineDesc
  { 
    k :: Int,
    kStates :: [State],
    kInputAlphabet :: Alphabet,
    kTapeAlphabet :: Alphabet,
    kTransitions :: [KTransition],
    kStartState :: State,
    kBlank :: TapeChar,
    kAcceptStates :: [State]
  }
  deriving (Show)

type KTransition = ((State, [TapeChar]), (State, [TapeChar], [Direction]))

instance TuringMachine KTuringMachineDesc where
    type Configuration KTuringMachineDesc = [TMConfiguration]

    blankChar = kBlank

    wellFormed _ = Nothing

    halted _ = any (\case {Running {} -> False; _ -> True})

    accepted _ = any (\case {Accept {} -> True; _ -> False})

    tapeOf m confs = intercalate "\n" $ map (\conf -> toTape conf (kBlank m)) confs

    initConfiguration m input =
      case input of
        [] ->
          Running
            { left = [],
              tapeHead = (kStartState m, kBlank m),
              right = []
            }
            : replicate (k m - 1) emptyConf
        c : cs ->
          if all (`elem` kInputAlphabet m) (c : cs)
            then
              Running
                { left = [],
                  tapeHead = (kStartState m, c),
                  right = cs
                }
                : replicate (k m - 1) emptyConf
            else error "invalid input!"
      where
        emptyConf :: TMConfiguration
        emptyConf =
          Running
            { left = [],
              tapeHead = (kStartState m, kBlank m),
              right = []
            }

    step m confs
      | shouldAccept m confs = map (\conf -> Accept $ toTape conf (kBlank m)) confs
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
                    [] -> map (\conf -> Reject $ toTape conf (kBlank m)) confs
                    _ ->
                      error $ "invalid " ++ show k ++ "-tape TM " ++ show transition
      where
        k = length confs

        istep (outQ, outTs, outDirs) i = move (outDirs !! i) $ update (outQ, outTs !! i) (confs !! i)

        move dir conf = case dir of
          TMLeft -> moveLeft (kBlank m) conf
          TMRight -> moveRight (kBlank m) conf
          TMStay -> conf


shouldAccept :: KTuringMachineDesc -> [TMConfiguration] -> Bool
shouldAccept m = any (\conf -> let (q, _) = tapeHead conf in q `elem` kAcceptStates m)

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
          right = headChar (tapeHead conf) : right conf
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
          right = rs
        }

tapeHeadPutChar :: TapeHead -> TapeChar -> TapeHead
tapeHeadPutChar (state, _) c = (state, c)

headChar :: TapeHead -> TapeChar
headChar (_, c) = c

update :: (State, TapeChar) -> TMConfiguration -> TMConfiguration
update (state, char) conf =
  Running
    { left = left conf,
      tapeHead = (state, char),
      right = right conf
    }

currentState :: TapeHead -> State
currentState (s, _) = s

instance TuringMachine TuringMachineDesc where
    type Configuration TuringMachineDesc = TMConfiguration

    blankChar = blank

    initConfiguration m [] =
      Running
        { left = [],
          tapeHead = (startState m, blank m),
          right = []
        }
    initConfiguration m (c : cs) =
      if all (`elem` inputAlphabet m) (c : cs)
        then
          Running
            { left = [],
              tapeHead = (startState m, c),
              right = cs
            }
        else error "invalid input!"

    accepted _ Reject {} = False
    accepted _ Accept {} = True
    accepted _ Running {} = True

    step m conf = case conf of
      a@Accept {} -> a
      r@Reject {} -> r
      Running {} ->
        if currentState (tapeHead conf) `elem` acceptStates m
          then Accept (toTape conf (blank m))
          else case lookup (tapeHead conf) (transitions m) of
            Just (newState, writeChar, dir) ->
              let updatedConf = update (newState, writeChar) conf
               in case dir of
                    TMLeft -> moveLeft (blank m) updatedConf
                    TMRight -> moveRight (blank m) updatedConf
                    TMStay -> updatedConf
            Nothing -> Reject (toTape conf (blank m))

    halted _ Accept {} = True
    halted _ Reject {} = True
    halted _ Running {} = False

    tapeOf m conf = toTape conf (blank m)

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

toTape :: TMConfiguration -> TapeChar -> Tape
toTape conf blank = case conf of 
      (Accept c) -> pruneTape c
      (Reject c) -> pruneTape c
      Running {left = tmleft, tapeHead = (s, c), right = tmright} ->
        reverse tmleft
          ++ (c : tmright)
          ++ "\n"
          ++ replicate (length tmleft) ' '
          ++ s
      where
        pruneTape :: Tape -> Tape
        pruneTape t = reverse $ dropBlanks $ reverse $ dropBlanks t
          where
            dropBlanks :: Tape -> Tape
            dropBlanks = dropWhile (== blank)

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
    { k = 2,
      kStates = ["0", "1", "2"],
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
      [ (("0", ['0', 'B']), ("0", ['B', '0'], [TMRight, TMRight])), -- copy 0s
        (("0", ['1', 'B']), ("1", ['1', 'B'], [TMStay, TMLeft])), -- until 1 is encountered
        (("1", ['1', '0']), ("1", ['B', 'B'], [TMRight, TMLeft])), -- tape1: 111, tape2: 000
        (("1", ['B', 'B']), ("2", ['B', 'B'], [TMStay, TMStay]))
      ]
