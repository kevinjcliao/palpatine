module Parse

import Election

%access public export

parseChopped : List String -> Maybe (List Cand)
parseChopped [] = Just []
parseChopped (x :: xs) = do
    cand <- candidate x
    rest <- parseChopped xs
    pure (cand :: rest)

parseBallot : String -> Maybe Ballot
parseBallot str = do
    let noFirstChar = drop 1 $ unpack str;
    noLastChar <- init' noFirstChar;
    let cutString = pack noLastChar;
    let pieces = split (==',') cutString;
    result <- parseChopped pieces
    pure (result, 1)

splitToStringBallots : String -> List String
splitToStringBallots = split (== '\n')

parseBallots : String -> List Ballot
parseBallots str = mapMaybe parseBallot (splitToStringBallots str)