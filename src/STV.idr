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
total
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
        
||| Does a full count of the ballots by taking the top preference and the vote
||| value and putting it into the candidates votes for the given candidate. 
total
count : Election r j -> Election r j
count {r} election@(dq, seats, ballots, cands, results) = 
    updateRemaining newCands election where
        countBallot : Ballot r -> Candidates r -> Candidates r
        countBallot ballot cands = case nextCand ballot of
            Just topPrefIndex => 
                addVoteVal topPrefIndex cands $ ballotValue ballot
            Nothing           => cands
        countBallots : Ballots r -> Candidates r -> Candidates r
        countBallots Nil cands       = cands
        countBallots (x :: xs) cands = countBallots xs $ countBallot x cands
        newCands : Candidates r
        newCands = countBallots ballots cands

||| electOne elects a candidate. It takes in the highest candidate index and
||| makes a judgment on that candidate as the new elected candidate. It then
||| redistributes the preferences according to the new transfer value. 
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
        -- This creates ballots with newBallotVal (if they preferenced highest
        -- cand first). It then removes the head of all the ballots. 
        ballotsWithNewValueAndRest : Ballots (S r)
        ballotsWithNewValueAndRest = 
            map (changeBallotIfIsCand highestCandIndex newBallotVal) ballots
        newBallots : Ballots r
        newBallots = reindexBallots ballotsWithNewValueAndRest cands newCands

||| elimOne eliminates a candidate. It chooses the lowest valued candidate
||| and then makes a judgment that the candidate is eliminated. It then
||| reindexes the ballots without changing the vote value of the ballots. 
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

total
weCanElect : Int -> Candidates (S n) -> Bool
weCanElect dq cands = maxCandValue > (cast dq) where
    maxCandValue : VoteValue
    maxCandValue = case getHighestIndex cands of
        (_, vv) => vv

total
notElectedHead : Election (S r) j -> Election r (S j)
notElectedHead election@(dq, seats, ballots, (x :: xs), results) = 
    makeElection dq seats ?ballots xs ((dontElect x) :: results)

total
processOne : Election (S r) j -> Election r (S j)
processOne election@(_, Z, _, _, _)          = notElectedHead election
processOne election@(dq, (S n), _, _, _) = case count election of
    counted@(_, _, _, cands, _) => if weCanElect dq cands
        then electOne counted
        else elimOne counted

--         Type mismatch between
--         Election 0 (n + S j) (Type of stv (processOne e))
-- and
--         (Int,
--          Nat,
--          List (List (Fin 0), Double),
--          Vect 0 Candidate,
--          Vect (S (plus n j)) Judged) (Expected type)

-- Specifically:
--         Type mismatch between
--                 plus n (S j)
--         and
--                 S (plus n j)


        -- total plusSuccRightSucc : (left : Nat) -> (right : Nat) ->
        --     S (left + right) = left + (S right)
        --   plusSuccRightSucc Z right        = Refl
        --   plusSuccRightSucc (S left) right =
        --     let inductiveHypothesis = plusSuccRightSucc left right in
        --   rewrite inductiveHypothesis in Refl

||| Running an STV election involves taking in the candidates, the seats, the
||| ballots and producing a list of candidates to take the seats. 
||| Returns a tuple of elected candidates and unelected candidates.
total
stv : Election r j -> Election Z (r + j)
stv         e@(_, _, _, Nil, _)      = e
stv {r=(S n)} {j} e@(_, _, _, (_ :: _), _) = 
    rewrite plusSuccRightSucc n j in stv $ processOne e