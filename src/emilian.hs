data State = State String deriving (Show, Eq)
data Symbol = Symbol Char deriving (Show, Eq)
data Alphabet = Alphabet [Symbol] deriving (Show)

data Transition = Transition Symbol State [State]

constructState :: String -> State
constructState name = State name

constructAlphabet :: [Char] -> Alphabet
constructAlphabet chars = Alphabet (map Symbol chars)

move :: Transition -> Symbol -> State -> [State]
move (Transition a s r) b t
  | a == b && s == t = r
  | otherwise = []

main = do
  let q0 = constructState "q0"
  let q1 = constructState "q1"
  let q2 = constructState "q2"
  let s = Symbol 'a'
  let s2 = Symbol 'b'
  let t = Transition s q0 [q1, q2]

  putStrLn (show (move t s q0))
  putStrLn (show (move t s2 q0))
  putStrLn (show (move t s q1))
  putStrLn (show (move t s2 q1))
