module Parse

import Election
import Ballot
import Data.Vect

%access public export

total
parseList : String -> Maybe $ List String
parseList str = do
    let noFirstChar = drop 1 $ unpack str;
    noLastChar <- init' noFirstChar;
    let cutString = pack noLastChar;
    let pieces = split (==',') cutString
    pure pieces

total
splitToStringBallots : String -> List String
splitToStringBallots = split (== '\n')

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