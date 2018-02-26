{- VoteCount describes a data structure where the votes are stored. -}

module VoteCount

import Election
import Ballot

%access public export

VoteCount : Type
VoteCount = List (Cand, Double)

-- Adds a value to the vote count of a candidate. 
addValue : VoteCount -> Cand -> Double -> VoteCount
addValue = 

addVote : VoteCount -> Ballot -> (VoteCount, Ballot)
addVote vc b = (newVc, newB) where
    voteVal = ballotValue b
    newVc   = 