-- Liam Wynn, 9/2/2020, Emilian
import Data.Char
import System.IO ( isEOF )

data State = State String deriving (Show, Eq)
data Symbol = Symbol Char | Epsilon deriving (Show, Eq)
data Alphabet = Alphabet [Symbol] deriving (Show)

--data Transition = Transition Symbol State [State]
data Transition = Transition {
      symbol :: Symbol
    , state  :: State
    , moveTo :: [State]
    } deriving (Show)

-- Definition of a Nondeterministic Finite Automata
data NDFA = NDFA {
      start :: State
    , transitions :: [Transition]
    , finalStates :: [State]
    } deriving (Show)

-- Definition of a regular expression
data Regex =   Singleton Symbol -- Denotes a single symbol, or empty
             | Union Regex Regex -- Denotes r | s
             | Concat Regex Regex -- Denotes rs
             | Star Regex -- Denotes r*
             deriving (Show)

data Token = SymUnion
               | SymStar 
               | OpenParen 
               | CloseParen
               | Letter Char
               | Done
               | Error
               deriving (Show, Eq)

ndfaFromRegex :: Regex -> NDFA
ndfaFromRegex regex = fst (ndfaFromRegexStep regex 0)

-- The integer is a global counter for the number of states.
-- We can use it to generate unique names for other states.
-- We want to return the modified counter value so we can
-- keep track of it for subsequent state constructions
ndfaFromRegexStep :: Regex -> Integer -> (NDFA, Integer)
ndfaFromRegexStep (Singleton symbol) counter =
  let start = State (show (counter + 1))
      end = State (show (counter + 2))
      result = NDFA start [(Transition symbol start [end])] [end]
  in (result, counter + 2)

ndfaFromRegexStep (Union r s) counter =
      -- Construct the sub-ndfa for r
  let resultr = ndfaFromRegexStep r counter
      -- Construct the sub-ndfa for s
      results = ndfaFromRegexStep s (snd resultr)
      -- Construct the new states we will add
      counter' = (snd results) + 1
      startState = State (show counter')
      end = State (show (counter' + 1))
      -- Now construct the transitions we will need for final ndfa
      -- Get the final states from each ndfa. Note there is only one.
      finalr = head (finalStates (fst resultr))
      finals = head (finalStates (fst results))
      transEnd1 = Transition Epsilon finalr [end]
      transEnd2 = Transition Epsilon finals [end]
      -- Now construct the new start transitions
      transStart1 = Transition Epsilon startState [(start (fst resultr))]
      transStart2 = Transition Epsilon startState [(start (fst results))]
      -- For convenience, we will grab the transitions in each sub ndfa
      transr = transitions (fst resultr)
      transs = transitions (fst results)
      -- Now build the resulting NDFA
      result = NDFA startState ([transStart1, transStart2, transEnd1, transEnd2] ++ transr ++ transs) [end]
  in (result, counter' + 1)

ndfaFromRegexStep (Concat r s) counter =
  let resultr = ndfaFromRegexStep r counter
      results = ndfaFromRegexStep s (snd resultr)
      -- Now we add an epsilon transition from the final state of r to start of s
      connect = Transition Epsilon ((head.finalStates.fst) resultr) [((start.fst) results)]
      -- Now we can build our NDFA
      result = NDFA ((start.fst) resultr) ([connect] ++ ((transitions.fst) resultr) ++ ((transitions.fst) results)) ((finalStates.fst) results)
  in (result, (snd results))

ndfaFromRegexStep (Star r) counter = 
  let resultr = ndfaFromRegexStep r counter
      counter' = snd resultr
      startState = State (show (counter' + 1))
      endState = State (show (counter' + 2))
      startTrans = Transition Epsilon startState [endState, ((start.fst) resultr)]
      endTrans = Transition Epsilon ((head.finalStates.fst) resultr) [endState]
      loopTrans = Transition Epsilon ((head.finalStates.fst) resultr) [((start.fst) resultr)]
      result = NDFA startState ([startTrans, endTrans, loopTrans] ++ ((transitions.fst) resultr)) [endState]
  in (result, counter' + 2)

constructState :: String -> State
constructState name = State name

constructAlphabet :: [Char] -> Alphabet
constructAlphabet chars = Alphabet (map Symbol chars)

-- From a given a set of states, we want to find every possible state you
-- can reach from Epsilon transitions alone.
eClosure :: [State] -> [Transition] -> [State]
eClosure states transitions = eDFS states transitions states

eDFS :: [State] -> [Transition] -> [State] -> [State]
eDFS [] _ reached = reached
eDFS (top : rest)  transitions reached =
      -- All transitions that goes from top
  let statesFromTop = searchByState transitions top
      -- All transitons in statesFromTop where transition is epsilon
      epsilonStateTrans = searchBySymbol statesFromTop Epsilon
      -- Take all states you can reach, combine them into a single
      -- list, and remove all that are in reached
      epsilonStates = filter (\s -> notElem s reached) (foldl (++) [] (map moveTo epsilonStateTrans))
  in eDFS (rest ++ epsilonStates) transitions (reached ++ epsilonStates)

move :: [State] -> Symbol -> [Transition] -> [State]
move [] _ _ = []
move (s : ss) a transitions =
  let moveResult = moveStep s a transitions
  in moveResult ++ (move ss a transitions)

moveStep :: State -> Symbol -> [Transition] -> [State]
moveStep s a transitions =
  -- Get all transitions that start from s. From that, get all transitions
  -- that move on a
  let relevantTransitions = searchBySymbol (searchByState transitions s) a
  -- Get the states you reach and combine into a single list
  in foldl (++) [] (map moveTo relevantTransitions)

searchByState :: [Transition] -> State -> [Transition]
searchByState ndfa keyState = filter (\tr -> (state tr) == keyState) ndfa

searchBySymbol :: [Transition] -> Symbol -> [Transition]
searchBySymbol ndfa keySymbol = filter (\tr -> (symbol tr) == keySymbol) ndfa

-- Given a set of states, we determine if any are an acceptance state
acceptance :: [State] -> [State] -> Bool
acceptance currStates finalStates =
  -- For each state in currState, determine if the state in in finalStates.
  -- this creates a list of boolean values
  let mapOfCurrsInFinal = foldr (\curr -> \searched -> searched ++ [(elem curr finalStates)]) [] currStates
  -- then apply or to that list of boolean values.
  in or mapOfCurrsInFinal

verify :: [Symbol] -> NDFA -> Bool
verify str automata =
  let currStates = eClosure [(start automata)] (transitions automata)
  in verifyStep str currStates automata

verifyStep :: [Symbol] -> [State] -> NDFA -> Bool
verifyStep [] currStates automata = acceptance currStates (finalStates automata)
verifyStep (x : xs) currStates automata =
  let currStates' = eClosure (move currStates x (transitions automata)) (transitions automata)
  in verifyStep xs currStates' automata

testNdfa :: NDFA
testNdfa =
  -- This ndfa is from the book "Compilers: Principles, Techniques, and Tools"
  -- on page 120
  let s0 = State "0"
      s1 = State "1"
      s2 = State "2"
      s3 = State "3"
      s4 = State "4"
      s5 = State "5"
      s6 = State "6"
      s7 = State "7"
      s8 = State "8"
      s9 = State "9"
      s10 = State "10"
  -- Now define the transitons
      s0_to_s1 = Transition Epsilon s0 [s1]
      s0_to_s7 = Transition Epsilon s0 [s7]
      s1_to_s2 = Transition Epsilon s1 [s2]
      s1_to_s4 = Transition Epsilon s1 [s4]
      s2_to_s3 = Transition (Symbol 'a') s2 [s3]
      s3_to_s6 = Transition Epsilon s3 [s6]
      s4_to_s5 = Transition (Symbol 'b') s4 [s5]
      s5_to_s6 = Transition Epsilon s5 [s6]
      s6_to_s1 = Transition Epsilon s6 [s1]
      s6_to_s7 = Transition Epsilon s6 [s7]
      s7_to_s8 = Transition (Symbol 'a') s7 [s8]
      s8_to_s9 = Transition (Symbol 'b') s8 [s9]
      s9_to_s10 = Transition (Symbol 'b') s9 [s10]
      transitions = [s0_to_s1, s0_to_s7, s1_to_s2, s1_to_s4,
                     s2_to_s3, s3_to_s6, s4_to_s5, s5_to_s6,
                     s6_to_s1, s6_to_s7, s7_to_s8, s8_to_s9,
                     s9_to_s10]
      start = s0
      accepting = [s10]
    in NDFA start transitions accepting

-- Reads the next input character from a given buffer
--lexan :: [Char] -> ([Char], Token)
--lexan [] = ([], Done)
--lexan ('|' : xs) = (xs, SymUnion)
--lexan ('*' : xs) = (xs, SymStar)
--lexan ('(' : xs) = (xs, OpenParen)
--lexan (')' : xs) = (xs, CloseParen)
--lexan (x : xs)
--  | isSpace x == True = lexan xs
--  | isAlphaNum x == True = (xs, Letter x)
--  | otherwise = ([], Error)

lexan :: IO Token
lexan = getChar >>= (\c -> constructToken c)
constructToken :: Char -> IO Token
constructToken '|' = return SymUnion
constructToken '*' = return SymStar
constructToken '(' = return OpenParen
constructToken ')' = return CloseParen
constructToken x
  | isSpace x == True = lexan
  | isAlphaNum x == True = return (Letter x)
  | otherwise = return Error

match :: Token -> Token -> IO Token
match token lookahead
  | token == lookahead = lexan
  | otherwise = return Error

main = do
  --let lingo = Union (Singleton (Symbol 'a')) (Singleton Epsilon)
  let lingo = Star (Singleton (Symbol 'a'))
  let automata = ndfaFromRegex lingo
 -- let automata = NDFA (State "0") [(Transition Epsilon (State "0") [(State "1"), (State "3")]),
 --                                  (Transition (Symbol 'a') (State "1") [(State "2")]),
 --                                  (Transition Epsilon (State "3") [(State "4")]),
 --                                  (Transition Epsilon (State "2") [(State "f")]),
 --                                  (Transition Epsilon (State "4") [(State "f")])]
 --                     [(State "f")]
  putStrLn (show automata)
  putStrLn (show (verify [(Symbol 'a')] automata))
  putStrLn (show (verify [(Symbol 'a'), (Symbol 'b')] automata))
  putStrLn (show (verify [(Symbol 'a'), (Symbol 'a')] automata))
  putStrLn (show (verify [Epsilon] automata))
