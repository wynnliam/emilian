data State = State String deriving (Show)
data Symbol = Symbol Char deriving (Show)
data Alphabet = Alphabet [Symbol] deriving (Show)

constructState :: String -> State
constructState name = State name

constructAlphabet :: [Char] -> Alphabet
constructAlphabet chars = Alphabet (map Symbol chars)

main = let alpha = constructAlphabet ['a', 'b', 'c'] in
  putStrLn (show alpha)
