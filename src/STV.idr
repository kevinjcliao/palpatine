module STV

import Candidates
import Ballot
import VoteCount
import Data.Vect

%access public export

||| Calculates the Droop Quota, as defined by the Australian Electoral Commission
||| http://www.aec.gov.au/Voting/counting/senate_count.htm
total
droopQuota : Int -> Int -> Int
droopQuota intBallots intSeats = 
    flooredFirstDiv + 1
    where
        numBallots : Double
        numBallots = cast intBallots
        numSeats : Double
        numSeats = cast intSeats
        flooredFirstDiv : Int
        flooredFirstDiv = cast $ numBallots / (numSeats + 1)

||| firstCount runs through the ballots for the first time. It then
||| inserts the value into the VoteCount SortedMap. 
||| addVote : Fin n -> Candidates n -> VoteValue -> VoteCount -> VoteCount
total
firstCount : Candidates n -> List (Ballot n) -> VoteCount -> VoteCount
firstCount cands [] vc = vc
firstCount cands (bal :: rest) vc = 
    firstCount cands rest newVc where
    newVc : VoteCount
    newVc = case bal of
        ((cand :: _), val) => addVote cand cands val vc
        ([], _)            => vc

total
getElectedCands : Candidates n -> VoteCount -> Int -> (p ** Candidates p)
getElectedCands cands vc dq = filter isOverQuota cands where
    isOverQuota : Candidate -> Bool
    isOverQuota cand = case getVoteVal cand vc of 
        Just voteVal => voteVal >= cast dq
        Nothing => False

electCandidate : Candidates (S n) -> 
    List (Ballot (S n)) ->
    VoteCount ->
    (Candidate, Candidates n, List (Ballot n), VoteCount)
electCandidate _ _ _ = ?electCandidateHole

isEwin : (n : Nat) -> (electedCands : Candidates x) -> Maybe (Candidates n)
isEwin Z (x :: xs)     = Nothing
isEwin (S n) Nil       = Nothing
isEwin Z Nil           = Just Nil
isEwin (S n) (x :: xs) = case isEwin n xs of
    Nothing  => Nothing
    Just vec => Just (x :: vec)

total
getLowestIndex : Vect (S n) VoteValue -> (VoteValue, Fin (S n))
getLowestIndex (x :: Nil) = (x, FZ)
getLowestIndex (x :: xs@(_ :: _))  = case getLowestIndex xs of
    (lowestVal, lowestIndex) => 
        if lowestVal < x
            then
                (lowestVal, FS lowestIndex)
            else
                (x, FZ)

||| Maps through the HashMap and chooses the least popular candidate
||| to eliminate. Returns the candidate eliminated, and the new VoteCount.
total 
chooseToEliminate : VoteCount 
                -> Candidates (S n) 
                -> (Candidate, VoteCount, Candidates n)
chooseToEliminate {n} vc cands = (lowestCand, newVc, newCandidates) where
    getVal : Candidate -> VoteValue
    getVal cand = case getVoteVal cand vc of
        Just val => val
        Nothing  => -1
    candVoteVals : Vect (S n) VoteValue
    candVoteVals = map getVal cands
    lowestCandIndex : Fin (S n)
    lowestCandIndex = case getLowestIndex candVoteVals of
        (_, i) => i
    lowestCand : Candidate
    lowestCand = case getLowestIndex candVoteVals of
        (lowestVal, lowestIndex) => getCand lowestIndex cands
    newVc : VoteCount
    newVc = deleteCandidate lowestCand vc
    newCandidates : Candidates n
    newCandidates = removeCand lowestCandIndex cands

||| Running an STV election involves taking in the candidates, the seats, the
||| ballots and producing a list of candidates to take that seat. 
stv : Candidates (x + seats)
    -> List $ Ballot (x + seats) 
    -> (seats : Nat) 
    -> (Candidates seats, Candidates (x))
stv = ?stv