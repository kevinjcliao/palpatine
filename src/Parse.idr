module Parse

import Candidates
import Ballot
import Data.Vect
import Data.SortedMap
import Data.String

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
splitToLines : String -> List String
splitToLines = split (== '\n')

-- toVec : List a -> (n ** Vect n a)
toVec : List a -> ExVect a
toVec []      = (Z ** Nil)
toVec (x :: xs) = case toVec xs of
    (ns ** rest) => ((S ns) ** (x :: rest))

||| This is a use of dependent types. 
total
readFirstLine : String -> Maybe (ExVect Candidate, Nat)
readFirstLine input = do
    let lines = splitToLines input
    firstLine <- head' lines
    let splitted =split (== ':') firstLine
    strCand <- head' splitted
    strNum <- last' splitted
    listCand <- parseList strCand
    let cands = map (\x => MkCandidate x 0) listCand
    seats <- parsePositive strNum
    pure $ ((toVec cands), cast seats)

total
parseBallot : Candidates x -> List CandidateName -> Ballot x
parseBallot {x} cands strs = MkBallot [] prefs 1 where
    getCandAsFin : CandidateName -> Maybe $ Fin x
    getCandAsFin cand = findIndex (\x => candName x == cand) cands
    prefs : List $ Fin x
    prefs = mapMaybe getCandAsFin strs

total
readBallots : String -> Candidates y -> Ballots y
readBallots input cands = map (parseBallot cands) listOfPrefs where
    lines : List String
    lines = drop 1 $ splitToLines input
    listOfPrefs : List $ List String
    listOfPrefs = mapMaybe parseList lines