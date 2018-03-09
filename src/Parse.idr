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
toVec : List a -> ExVect a
toVec []      = (Z ** Nil)
toVec (x :: xs) = case toVec xs of
    (ns ** rest) => ((S ns) ** (x :: rest))

||| This is a use of dependent types. 
total
readFirstLine : List String -> Maybe $ ExVect Candidate
readFirstLine input = do
    firstLine <- head' input
    let splitted = split (== ':') firstLine
    strCand <- head' splitted
    strNum <- last' splitted
    listCand <- parseList strCand
    pure $ toVec listCand

parseBallot : Candidates x -> List Candidate  -> Ballot x
parseBallot {x} cands strs = (prefs, 1) where
    getCandAsFin : Candidate -> Maybe $ Fin x
    getCandAsFin cand = elemIndex cand cands
    prefs : List $ Fin x
    prefs = mapMaybe getCandAsFin strs

total
readBallots : String -> (y ** Vect y Candidate) -> List $ Ballot y
readBallots input (_ ** cands) = map (parseBallot cands) listOfPrefs where
    lines : List String
    lines = drop 1 $ splitToStringBallots input
    listOfPrefs : List $ List String
    listOfPrefs = mapMaybe parseList lines

total
getCandidates : String -> ExVect Candidate
getCandidates input = do
    let lines = splitToStringBallots input
    let candidates = readFirstLine lines
    case candidates of
        Just exvec => exvec
        Nothing    => (Z ** Nil)