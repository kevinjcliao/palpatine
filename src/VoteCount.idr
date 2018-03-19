{- VoteCount describes a data structure where the votes are stored. -}

module VoteCount

import Candidates
import Ballot
import Data.SortedMap
import Data.Vect

%access public export

VoteCount : Type
VoteCount = SortedMap Candidate VoteValue

ElectedCandidates : Type
ElectedCandidates = ExVect Candidate

total
initVoteCount : Candidates n -> VoteCount
initVoteCount cands = helper cands empty where
    helper : Candidates n -> VoteCount -> VoteCount
    helper Nil vc = vc
    helper (x :: xs) vc = helper xs vc2 where
        vc2 : VoteCount
        vc2 = insert x 0 vc


total
getVoteVal : Candidate -> VoteCount -> Maybe VoteValue
getVoteVal = lookup


total
addVoteVal : Candidate -> VoteValue -> VoteCount -> VoteCount
addVoteVal cand newVoteVal vc = insert cand val vc where
    oldVal : VoteValue
    oldVal = case getVoteVal cand vc of
        Just fromVc => fromVc
        Nothing     => 0
    val : VoteValue
    val = oldVal + newVoteVal

-- addVoteVal indexes 
total
addVote : Fin n -> Candidates n -> VoteValue -> VoteCount -> VoteCount
addVote candAsFin cands val vc = addVoteVal cand val vc where
    cand : Candidate
    cand = index candAsFin cands

total
decVoteVal : Candidate -> VoteValue -> VoteCount -> VoteCount
decVoteVal can val vc = addVoteVal can (-1 * val) vc