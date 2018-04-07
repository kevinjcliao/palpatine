module STV

import Candidates
import Ballot
import Election
import Data.Vect
import Data.SortedMap

%access public export

||| Calculates the Droop Quota, as defined by the Australian Electoral Commission
||| http://www.aec.gov.au/Voting/counting/senate_count.htm
-- total
-- droopQuota : Int -> Int -> Int
-- droopQuota intBallots intSeats = 
--     flooredFirstDiv + 1
--     where
--         numBallots : Double
--         numBallots = cast intBallots
--         numSeats : Double
--         numSeats = cast intSeats
--         flooredFirstDiv : Int
--         flooredFirstDiv = cast $ numBallots / (numSeats + 1)

-- ||| firstCount runs through the ballots for the first time. It then
-- ||| inserts the value into the VoteCount SortedMap. 
-- ||| addVote : Fin n -> Candidates n -> VoteValue -> VoteCount -> VoteCount
-- total
-- firstCount : Candidates n -> List (Ballot n) -> VoteCount -> VoteCount
-- firstCount cands [] vc = vc
-- firstCount cands (bal :: rest) vc = 
--     firstCount cands rest newVc where
--     newVc : VoteCount
--     newVc = case bal of
--         ((cand :: _), val) => addVote cand cands val vc
--         ([], _)            => vc




-- ||| isEwin checks to make sure that the number of elected candidates is the
-- ||| same as the number of seats available. What happens when there are less
-- ||| candidates than there are seats available? 
-- isEwin : (n : Nat) -> (electedCands : Candidates x) -> Bool
-- -- Case 1: Less seats than there are candidates?? What do we do in this case...
-- isEwin Z (x :: xs)     = True
-- -- Case 2: Less candidates than there are seats available: False.
-- isEwin (S n) Nil       = False
-- -- Case 3: No seats left and same number of candidates: True.
-- isEwin Z Nil           = True
-- isEwin (S n) (x :: xs) = isEwin n xs

-- elimCandFromBallot : Fin n -> Ballot n -> Ballot n
-- elimCandFromBallot index b@(cands, v) = (filter (/= index) cands, v)

-- ||| Only should be called from eliminate. 
-- total
-- elimCandFromBallots : List $ Ballot n -> Fin n -> List $ Ballot n
-- elimCandFromBallots ballots index = map (elimCandFromBallot index) ballots

-- ||| Maps through the HashMap and chooses the least popular candidate
-- ||| to eliminate. Returns the new ballots with that candidate
-- ||| eliminated. NOTE: Does not eliminate the candidate from
-- ||| remaining. We do this to preserve the relationship where
-- ||| elected + remaining is always the same number. 
-- total 
-- eliminate : VoteCount 
--           -> Candidates (S n)
--           -> List $ Ballot (S n)
--           -> (Candidate, VoteCount, List $ Ballot $ S n)
-- eliminate {n} vc cands ballots = 
--     (lowestCand, newVc, newBallots) where
--     voteVals : Vect (S n) VoteValue
--     voteVals = candVoteVals cands vc
--     lowestCandIndex : Fin (S n)
--     lowestCandIndex = case getLowestIndex voteVals of
--         (_, i) => i
--     lowestCand : Candidate
--     lowestCand = case getLowestIndex voteVals of
--         (lowestVal, lowestIndex) => getCand lowestIndex cands
--     newVc : VoteCount
--     newVc = deleteCandidate lowestCand vc
--     newBallots : List $ Ballot (S n)
--     newBallots = elimCandFromBallots ballots lowestCandIndex


-- ||| revalueBallot takes a ballot and a new vote value and
-- ||| creates a new one. 
-- revalueBallot : (value : VoteValue)
--               -> (cand : Candidate)
--               -> (cands : Candidates n)
--               -> (ballot : Ballot n)
--               -> Ballot n
-- revalueBallot val cand cands bal@(Nil, _)      = bal
-- revalueBallot val cand cands bal@((x :: _), _) = 
--     if (index x cands) == cand
--         then newBallotVal bal val
--         else bal

-- ||| Revalues the ballots that were responsible for electing
-- ||| the candidate that was just elected. 
-- revalue : (elected : Candidate)
--         -> (droopQuota : Int)
--         -> (candScore : VoteValue)
--         -> (cands : Candidates n)
--         -> (oldBallots : List $ Ballot n)
--         -> List $ Ballot n
-- revalue cand dq score cands ballots = 
--     map (revalueBallot newVal cand cands) ballots where
--         surplus : VoteValue
--         surplus = score - (cast dq)
--         newVal : VoteValue
--         newVal = surplus / score

-- total   
-- reindexPref : (former : Candidates (S n))
--             -> (new : Candidates n)
--             -> (prev : Fin (S n))
--             -> Maybe $ Fin n
-- reindexPref former new oldPref = findIndex (==candidate) new where
--     candidate : Candidate
--     candidate = index oldPref former

-- ||| Reindexes a ballot according to the new candidates after one
-- ||| has been elected or eliminated. 
-- total
-- redoBallot : (elected : Candidate)
--             -> (former : Candidates (S n))
--             -> (new : Candidates n)
--             -> (oldBallot : Ballot (S n))
--             -> Ballot n
-- redoBallot cand former new (oldPrefs, val) = 
--     ( mapMaybe (reindexPref former new) oldPrefs
--     , val
--     )


-- ||| Maps through the ballots, selecting th
-- total
-- redoBallots : (elected : Candidate)
--             -> (former : Candidates (S n))
--             -> (new : Candidates n)
--             -> (oldBallots : List $ Ballot (S n))
--             -> List $ Ballot n
-- redoBallots elected former new oldBallots = 
--     map (redoBallot elected former new) oldBallots

-- -- TODO: Transfer the surplus!!! 
-- electCandidate : (remaining : Candidates (S n))
--                -> (elected : Candidates p)
--                -> (candIndex : Fin (S n))
--                -> (candValue : VoteValue)
--                -> List (Ballot (S n)) 
--                -> VoteCount
--                -> (dq : Int)
--                -> (Candidates n, Candidates (S p), List (Ballot n), VoteCount)
-- electCandidate {n} {p} remaining elected cand vv ballots vc dq = 
--     (newCands, electedCands, newBallots, newVc) where
--         electedCand : Candidate
--         electedCand = index cand remaining
--         newCands : Candidates n
--         newCands = removeCand cand remaining
--         ballotsWithNewValue : List $ Ballot $ S n
--         ballotsWithNewValue = revalue electedCand dq vv remaining ballots
--         removeBallotHead : Ballot r -> Ballot r
--         removeBallotHead b@([], v)     = ([], v)
--         removeBallotHead b@(x :: xs, v) = (xs, v)
--         ballotsWithoutHead : List $ Ballot $ S n
--         ballotsWithoutHead = map removeBallotHead ballotsWithNewValue
--         newBallots : List (Ballot n)
--         newBallots = redoBallots electedCand remaining newCands ballotsWithoutHead
--         electedCands : Candidates (S p)
--         electedCands = (electedCand :: elected)
--         newVc : VoteCount
--         newVc = deleteCandidate electedCand vc

-- canElect : VoteCount -> Candidates (S n) -> Maybe $ Fin (S n)
-- canElect = ?canElect

-- electHighestCand : (cands : Candidates (S n))
--                  -> (elected : Candidates e)
--                  -> (ballots : List $ Ballot (S n))
--                  -> (vc : VoteCount)
--                  -> (dq : Int)
--                  -> (Candidates (S e), Candidates n)
-- electHighestCand {n} {e} cands elected ballots vc dq = (newElected, newRemaining) where
--     toElect : (Fin $ S n, VoteValue)
--     toElect = highestCandIndex vc cands ballots
--     toElectIndex : Fin $ S n
--     toElectIndex = case toElect of (i, _) => i
--     toElectValue : VoteValue
--     toElectValue = case toElect of (_, v) => v
--     newStuff : (Candidates n, Candidates (S e), List (Ballot n), VoteCount)
--     newStuff = electCandidate cands elected toElectIndex toElectValue ballots vc dq
--     newElected : Candidates (S e)
--     newElected = case newStuff of
--         (_, elected, _, _) => elected
--     newRemaining : Candidates n
--     newRemaining = case newStuff of
--         (remaining, _, _, _) => remaining


-- electCandidates should map through the elected candidates and elect each one. 
||| You can use Typed Holes as error messages and that's really stupid! 
total
electOne : Election (S r) j -> Election r (S j)
electOne = ?electOneHole

||| TODO: Reindex ballots. 
total
elimOne : Election (S r) j -> Election r (S j)
elimOne {r} {j} election@(dq, _, _, cands, results) = ?elimOneHole where
    lowestCandIndex : Fin $ S r
    lowestCandIndex = case getLowestIndex cands of
        (i, _) => i
    result : Judged
    result = dontElect $ getCand lowestCandIndex cands
    newResults : Results (S j)
    newResults = (result :: results)
    newCands : Candidates r
    newCands = removeCand lowestCandIndex cands

weCanElect : Int -> Candidates (S n) -> Bool
weCanElect dq cands = maxCandValue > (cast dq) where
    maxCandValue : VoteValue
    maxCandValue = case getHighestIndex cands of
        (_, vv) => vv

notElectedHead : Election (S r) j -> Election r (S j)
notElectedHead election@(dq, seats, ballots, (x :: xs), results) = 
    makeElection dq seats ?ballots xs ((dontElect x) :: results)

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