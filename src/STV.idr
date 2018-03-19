module STV

import Candidates
import Ballot
import VoteCount
import Data.Vect

%access public export

-- As defined by the Australian Electoral Commission
-- http://www.aec.gov.au/Voting/counting/senate_count.htm
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

firstCount : Int -> Candidates n -> List (Ballot n) -> VoteCount -> VoteCount
firstCount dq cands bals vc = 

electCandidate : Candidates (S n) -> 
    List (Ballot (S n)) ->
    VoteCount ->
    (Candidate, Candidates n, List (Ballot n), VoteCount)
electCandidate _ _ _ = ?electCandidateHole

countBallots : Candidates n -> 
    List (Ballot n) -> 
    VoteCount ->
    (seats : Nat) -> 
    Candidates seats
countBallots _     _       _  Z     = Nil
countBallots cands ballots vc (S n) = case electCandidate cands ballots vc of
    (elected, newCands, newBalls, vc) => (elected :: countBallots newCands newBalls, vc)