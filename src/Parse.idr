module Parse

import Election
import Ballot
import Data.Vect

%access public export

data Ev : Type -> Type where
   ExVect : Vect n a -> Ev a

Candidate : Type
Candidate = String

Candidates : Nat -> Type
Candidates n = Vect n Candidate

total
parseChopped : List String -> Maybe (List Cand)
parseChopped [] = Just []
parseChopped (x :: xs) = do
    cand <- candidate x
    rest <- parseChopped xs
    pure (cand :: rest)

total
parseList : String -> Maybe $ List String
parseList str = do
    let noFirstChar = drop 1 $ unpack str;
    noLastChar <- init' noFirstChar;
    let cutString = pack noLastChar;
    let pieces = split (==',') cutString
    pure pieces

-- total
-- parseBallot : String -> Maybe Ballot
-- parseBallot str = do
--     let pieces = parseList str
--     result <- parseChopped pieces
--     pure (result, 1)

total
splitToStringBallots : String -> List String
splitToStringBallots = split (== '\n')

-- total
-- parseBallots : String -> List Ballot
-- parseBallots str = mapMaybe parseBallot (splitToStringBallots str)

||| This expects to fail if there are more candidates than 
-- partial
-- parseCands : (n : Nat) -> List Candidate -> Ev String
-- parseCands Z     _              = Nil
-- -- parseCands (S n) []             = Nil
-- parseCands (S n) (cand :: rest) = cand :: parseCands n rest

toVec1 : List a -> Ev a
toVec1 []     = ExVect Nil
toVec1 (x :: xs) = case toVec1 xs of ExVect xs' => ExVect (x :: xs')

||| This is a use of dependent types. 
partial
readFirstLine : String -> Maybe (Ev String)
readFirstLine input = do
    let splitted = split (== ':') input
    strCand <- head' splitted
    strNum <- last' splitted
    listCand <- parseList strCand
    pure $ toVec1 listCand



-- First line of file not included. Everything else. 
readBallots : List String -> List Ballot
readBallots str = map initBallot listOfPrefs where
    listOfPrefs : List $ List String
    listOfPrefs = mapMaybe parseList str
    initBallot : List String -> Ballot
    initBallot str = (str, 1)

parseInput : String -> Maybe (Ev String, List Ballot)
parseInput str = do
    let lines = splitToStringBallots str
    firstLine <- head' lines
    let rest = drop 1 lines
    candidates <- readFirstLine firstLine
    let ballots = readBallots rest
    pure (candidates, ballots)