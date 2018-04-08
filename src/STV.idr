module STV

import Candidates
import Ballot
import Election
import Data.Vect
import Data.SortedMap

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

total
transferValue : Int -> VoteValue -> VoteValue
transferValue dq votes = surplus / votes where
    surplus : VoteValue
    surplus = votes - cast dq

||| After a candidate was just eliminated from remaining, this iterates through
||| the ballots and reindexes them according to the new candidate indices rather
||| than the old one. 
reindexBallots : Ballots (S r) -> Candidates (S r) -> Candidates r -> Ballots r
reindexBallots {r} ballots oldCands newCands = 
    map reindexBallot ballots where
        reindexCand : Fin $ S r -> Maybe $ Fin r
        reindexCand oldPref = findIndex (\x => candName x == cn) newCands where
            cn : CandidateName
            cn = candName $ index oldPref oldCands
        reindexBallot : Ballot (S r) -> Ballot r
        reindexBallot (prefs, voteval) = 
            (mapMaybe reindexCand prefs, voteval)
        

count : Election r j -> Election r j
count {r} election@(dq, seats, ballots, cands, results) = ?count where
    countBallot : Ballot r -> Candidates r -> Candidates r
    countBallot ballot cands = case nextCand ballot of
        Just topPrefIndex => addVoteVal topPrefIndex cands $ ballotValue ballot
        Nothing           => cands
    countBallots : Ballots r -> Candidates r -> Candidates r
    countBallots Nil cands       = cands
    countBallots (x :: xs) cands = countBallots xs $ countBallot x cands

-- electCandidates should map through the elected candidates and elect each one. 
||| You can use Typed Holes as error messages and that's really stupid! 
total
electOne : Election (S r) j -> Election r (S j)
electOne {r} {j} election@(dq, seats, ballots, cands, results) = 
    (dq, seats, newBallots, newCands, newResults) where
        highestCandIndex : Fin $ S r
        highestCandIndex = case getHighestIndex cands of
            (i, _) => i
        highestCandValue : VoteValue
        highestCandValue = candValue $ index highestCandIndex cands
        result : Judged
        result = elect $ getCand highestCandIndex cands
        newResults : Results (S j)
        newResults = (result :: results)
        newCands : Candidates r
        newCands = removeCand highestCandIndex cands
        newBallotVal : VoteValue
        newBallotVal = transferValue dq highestCandValue
        ballotsWithNewValueAndRest : Ballots (S r)
        ballotsWithNewValueAndRest = 
            map (changeBallotIfIsCand highestCandIndex newBallotVal) ballots
        newBallots : Ballots r
        newBallots = reindexBallots ballotsWithNewValueAndRest cands newCands

total
elimOne : Election (S r) j -> Election r (S j)
elimOne {r} {j} election@(dq, seats, ballots, cands, results) = 
    (dq, seats, newBallots, newCands, newResults) where
        lowestCandIndex : Fin $ S r
        lowestCandIndex = case getLowestIndex cands of
            (i, _) => i
        result : Judged
        result = dontElect $ getCand lowestCandIndex cands
        newResults : Results (S j)
        newResults = (result :: results)
        newCands : Candidates r
        newCands = removeCand lowestCandIndex cands
        newBallots : Ballots r
        newBallots = reindexBallots ballots cands newCands

weCanElect : Int -> Candidates (S n) -> Bool
weCanElect dq cands = maxCandValue > (cast dq) where
    maxCandValue : VoteValue
    maxCandValue = case getHighestIndex cands of
        (_, vv) => vv

notElectedHead : Election (S r) j -> Election r (S j)
notElectedHead election@(dq, seats, ballots, (x :: xs), results) = 
    makeElection dq seats ?ballots xs ((dontElect x) :: results)

total
processOne : Election (S r) j -> Election r (S j)
processOne election@(_, Z, _, _, _)          = notElectedHead election
processOne election@(dq, (S n), _, cands, _) = 
    if weCanElect dq cands
        then electOne election
        else elimOne election

-- ||| Running an STV election involves taking in the candidates, the seats, the
-- ||| ballots and producing a list of candidates to take the seats. 
-- ||| Returns a tuple of elected candidates and unelected candidates.
stv : Election r Z -> Election Z r
stv e@(_, _, _, Nil, _) = e