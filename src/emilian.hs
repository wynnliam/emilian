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
eClosure states ndfa = eDFS states ndfa states

eDFS :: [State] -> [Transition] -> [State] -> [State]
eDFS [] _ reached = reached
eDFS (top : rest)  ndfa reached =
      -- All transitions that goes from top
  let statesFromTop = searchByState ndfa top
      -- All transitons in statesFromTop where transition is epsilon
      epsilonStateTrans = searchBySymbol statesFromTop Epsilon
      epsilonStates = filter (\s -> notElem s reached) (foldl (++) [] (map moveTo epsilonStateTrans))
  in eDFS (rest ++ epsilonStates) ndfa (reached ++ epsilonStates)
      

searchByState :: [Transition] -> State -> [Transition]
searchByState ndfa keyState = filter (\tr -> (state tr) == keyState) ndfa

searchBySymbol :: [Transition] -> Symbol -> [Transition]
searchBySymbol ndfa keySymbol = filter (\tr -> (symbol tr) == keySymbol) ndfa

testNdfa =
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
  in [s0_to_s1, s0_to_s7, s1_to_s2, s1_to_s4,
      s2_to_s3, s3_to_s6, s4_to_s5, s5_to_s6,
      s6_to_s1, s6_to_s7, s7_to_s8, s8_to_s9,
      s9_to_s10]

main = do
  -- Defines the NDFA on page 120 of "Compilers: Principles, Tools, and Techniques"
  let s0 = State "0"
  let s1 = State "1"
  let s2 = State "2"
  let s3 = State "3"
  let s4 = State "4"
  let s5 = State "5"
  let s6 = State "6"
  let s7 = State "7"
  let s8 = State "8"
  let s9 = State "9"
  let s10 = State "10"

  -- Now define the transitons
  let s0_to_s1 = Transition Epsilon s0 [s1]
  let s0_to_s7 = Transition Epsilon s0 [s7]
  let s1_to_s2 = Transition Epsilon s1 [s2]
  let s1_to_s4 = Transition Epsilon s1 [s4]
  let s2_to_s3 = Transition (Symbol 'a') s2 [s3]
  let s3_to_s6 = Transition Epsilon s3 [s6]
  let s4_to_s5 = Transition (Symbol 'b') s4 [s5]
  let s5_to_s6 = Transition Epsilon s5 [s6]
  let s6_to_s1 = Transition Epsilon s6 [s1]
  let s6_to_s7 = Transition Epsilon s6 [s7]
  let s7_to_s8 = Transition (Symbol 'a') s7 [s8]
  let s8_to_s9 = Transition (Symbol 'b') s8 [s9]
  let s9_to_s10 = Transition (Symbol 'b') s9 [s10]

  let ndfa = [s0_to_s1, s0_to_s7, s1_to_s2, s1_to_s4,
              s2_to_s3, s3_to_s6, s4_to_s5, s5_to_s6,
              s6_to_s1, s6_to_s7, s7_to_s8, s8_to_s9,
              s9_to_s10]

  putStrLn (show (eClosure [s5] ndfa))
