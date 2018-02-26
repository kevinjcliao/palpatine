{- VoteCount describes a data structure where the votes are stored. -}

module VoteCount

import Election
import Ballot

%access public export

VoteCount : Type
VoteCount = List (Cand, Double)

-- Adds a value to the vote count of a candidate.    
total
addValue : VoteCount -> Cand -> Double -> VoteCount
addValue []                        _    _      = []
addValue (head@(cand1, origVal) :: rest) cand2 newVal = 
    if cand1 == cand2 then
        ((cand1, origVal + newVal) :: rest)
    else
        (head :: addValue rest cand2 newVal)

total
getVoteForCandidate : VoteCount -> Cand -> Maybe Double
getVoteForCandidate [] _                        = Nothing
getVoteForCandidate ((cand1, val) :: rest) cand2   =
    if cand1 == cand2 then
        Just val
    else
        getVoteForCandidate rest cand2

total
addVote : VoteCount -> Ballot -> Maybe VoteCount
addVote vc b = do
    let voteVal = ballotValue b
    cand <- nextCand b
    pure $ addValue vc cand voteVal