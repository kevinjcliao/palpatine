module Parse

import Candidates
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

total
toVec1 : List a -> Ev a
toVec1 []     = ExVect Nil
toVec1 (x :: xs) = case toVec1 xs of ExVect xs' => ExVect (x :: xs')

||| This is a use of dependent types. 
total
readFirstLine : String -> Maybe (Ev Candidate)
readFirstLine input = do
    let splitted = split (== ':') input
    strCand <- head' splitted
    strNum <- last' splitted
    listCand <- parseList strCand
    pure $ toVec1 listCand

parseBallot : Candidates n -> List Candidate  -> Ballot
parseBallot cands strs = (prefs, 1) where
    getCandAsNat : Candidate -> Maybe Nat
    getCandAsNat cand = case elemIndex cand cands of
        Just ind => Just $ cast ind
        Nothing  => Nothing
    prefs : List Nat
    prefs = mapMaybe getCandAsNat strs

-- First line of file not included. Everything else. 
total
readBallots : List Candidate -> Ev Candidate -> List Ballot
readBallots str (ExVect cands) = map (parseBallot cands) listOfPrefs where
    listOfPrefs : List $ List String
    listOfPrefs = mapMaybe parseList str

total
parseInput : String -> Maybe (Ev Candidate, List Ballot)
parseInput str = do
    let lines = splitToStringBallots str
    firstLine <- head' lines
    let rest = drop 1 lines
    candidates <- readFirstLine firstLine
    let ballots = readBallots rest candidates
    pure (candidates, ballots)