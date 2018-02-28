{- VoteCount describes a data structure where the votes are stored. -}

module VoteCount

import Election
import Ballot
import Data.SortedMap
import Data.Vect

%access public export

VoteCount : Type
VoteCount = SortedMap Candidate VoteValue

initVoteCount : Candidates n -> VoteCount -> VoteCount
initVoteCount Nil vc = vc
initVoteCount (x :: xs) vc = initVoteCount xs vc2 where
    vc2 : VoteCount
    vc2 = insert x 0 vc

private
addValue : Candidate -> VoteValue -> VoteCount -> VoteCount
addValue = insert

getVoteVal : Candidate -> VoteCount -> Maybe VoteValue
getVoteVal = lookup

addVoteVal : Candidate -> VoteValue -> VoteCount -> VoteCount
addVoteVal can val vc = vc2 where
    origVal : VoteValue
    origVal = case getVoteVal can vc of
        Just v  => v
        Nothing => 0
    newVal : VoteValue
    newVal = origVal + val
    vc2 : VoteCount
    vc2 = addValue can newVal vc

decVoteVal : Candidate -> VoteValue -> VoteCount -> VoteCount
decVoteVal can val vc = addVoteVal can (-1 * val) vc