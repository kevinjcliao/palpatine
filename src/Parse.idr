module Parse

import Election
import Ballot

%access public export

total
parseChopped : List String -> Maybe (List Cand)
parseChopped [] = Just []
parseChopped (x :: xs) = do
    cand <- candidate x
    rest <- parseChopped xs
    pure (cand :: rest)

total
parseBallot : String -> Maybe Ballot
parseBallot str = do
    let noFirstChar = drop 1 $ unpack str;
    noLastChar <- init' noFirstChar;
    let cutString = pack noLastChar;
    let pieces = split (==',') cutString;
    result <- parseChopped pieces
    pure (result, 1)

total
splitToStringBallots : String -> List String
splitToStringBallots = split (== '\n')

total
parseBallots : String -> List Ballot
parseBallots str = mapMaybe parseBallot (splitToStringBallots str)