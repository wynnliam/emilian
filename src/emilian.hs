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

constructState :: String -> State
constructState name = State name

constructAlphabet :: [Char] -> Alphabet
constructAlphabet chars = Alphabet (map Symbol chars)

move :: Transition -> Symbol -> State -> [State]
move (Transition a s r) b t
  | a == b && s == t = r
  | otherwise = []

-- From a given a set of states, we want to find every possible state you
-- can reach from Epsilon transitions alone.
eClosure :: [State] -> [Transition] -> [State]
eClosure states ndfa = states ++ (eDFS states ndfa [])

eDFS :: [State] -> [Transition] -> [State] -> [State]
eDFS [] _ reached = reached
eDFS (top : rest)  ndfa reached =
      -- All transitions that goes from top
  let statesFromTop = searchByState ndfa top
      -- All transitons in statesFromTop where transition is epsilon
      epsilonStateTrans = searchBySymbol statesFromTop Epsilon
      epsilonStates = filter (\s -> (elem s reached) == False) (foldl (++) [] (map moveTo epsilonStateTrans))
  in eDFS rest ndfa (reached ++ epsilonStates)
      

searchByState :: [Transition] -> State -> [Transition]
searchByState ndfa keyState = filter (\tr -> (state tr) == keyState) ndfa

searchBySymbol :: [Transition] -> Symbol -> [Transition]
searchBySymbol ndfa keySymbol = filter (\tr -> (symbol tr) == keySymbol) ndfa

main = do
  -- Defines the NFDA from page 116 of "Compilers: Principles, Tools, and Techniques"
  -- First, define the alphabet. For now, this is a symbolic step, since we
  -- never actually use this variable
  let alpha = constructAlphabet ['a', 'b']

  let q0 = constructState "0"
  let q1 = constructState "1"
  let q2 = constructState "2"
  let q3 = constructState "3"
  let q4 = constructState "4"

  -- Now, we define our transitions
  let q0_q1 = Transition Epsilon q0 [q1]
  let q0_q3 = Transition Epsilon q0 [q3]
  let q1_q2 = Transition (Symbol 'a') q1 [q2]
  let q2_q2 = Transition (Symbol 'a') q2 [q2]
  let q3_q4 = Transition (Symbol 'b') q3 [q4]
  let q4_q4 = Transition (Symbol 'b') q4 [q4]
  -- For ease of use, let's put these in a list
  let transitions = [q0_q1, q0_q3, q1_q2, q2_q2, q3_q4, q4_q4]

  putStrLn (show (eClosure [q0] transitions))
  --putStrLn (show (searchByState transitions q0))
  --putStrLn (show (searchBySymbol transitions (Symbol 'a')))
