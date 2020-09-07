-- Liam Wynn, 9/2/2020, Emilian

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

move :: State -> Symbol -> [Transition] -> [State]
move s a transitions =
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

main = do
  putStrLn (show testNdfa)
