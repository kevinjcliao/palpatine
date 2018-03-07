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

-- toVec : List a -> (n ** Vect n a)
toVec2 : List a -> (n ** Vect n a)
toVec2 []      = (Z ** Nil)
toVec2 (x :: xs) = case toVec2 xs of
    (ns ** rest) => ((S ns) ** (x :: rest)) 

total
toVec : List a -> Ev a
toVec []     = ExVect Nil
toVec (x :: xs) = case toVec xs of ExVect xs' => ExVect (x :: xs')

||| This is a use of dependent types. 
total
readFirstLine : String -> Maybe $ Ev Candidate
readFirstLine input = do
    let splitted = split (== ':') input
    strCand <- head' splitted
    strNum <- last' splitted
    listCand <- parseList strCand
    pure $ toVec listCand

-- How do I prove this???? 
parseBallot : Candidates n -> List Candidate  -> Ballot2 n
parseBallot {n} cands strs = (prefs, 1) where
    getCandAsFin : Candidate -> Maybe $ Fin n
    getCandAsFin cand = elemIndex cand cands
    prefs : List $ Fin n
    prefs = mapMaybe getCandAsFin strs

total
readBallots : String -> Candidates n -> List $ Ballot2 n
readBallots input cands = map (parseBallot cands) listOfPrefs where
    lines : List String
    lines = drop 1 $ splitToStringBallots input
    listOfPrefs : List $ List String
    listOfPrefs = mapMaybe parseList lines

total
getCandidates : String -> Maybe $ Ev Candidate
getCandidates input = do
    let lines = splitToStringBallots input
    firstLine <- head' lines
    candidates <- readFirstLine firstLine
    pure candidates