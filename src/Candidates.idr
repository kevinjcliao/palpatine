module Candidates

import Data.Vect

%access public export

VoteValue : Type
VoteValue = Double

ExVect : Type -> Type
ExVect t = (n ** Vect n t)

CandidateName : Type
CandidateName = String

record Candidate where
    constructor MkCandidate
    candName : CandidateName
    candValue : VoteValue

Candidates : Nat -> Type
Candidates n = Vect n Candidate

data Judgment = Elected | NotElected

implementation Eq Judgment where
    (==) Elected Elected = True
    (==) NotElected NotElected = True
    (==) _ _ = False



record Judged where
    constructor MkJudgment
    candName : CandidateName
    judgment : Judgment

Results : Nat -> Type
Results n = Vect n Judged

getCand : Fin n -> Candidates n -> Candidate
getCand = index

getCandVal : Fin n -> Candidates n -> VoteValue
getCandVal i cands = candValue $ getCand i cands

getCandName : Fin n -> Candidates n -> CandidateName
getCandName i cands = candName $ getCand i cands

addVoteVal : Fin n -> Candidates n -> VoteValue -> Candidates n
addVoteVal i cands newVal = replaceAt i newCand cands where
    oldCand : Candidate
    oldCand = index i cands
    newVoteVal : VoteValue
    newVoteVal = newVal + candValue oldCand
    newCand : Candidate
    newCand = record { candValue = newVoteVal } oldCand

decVoteVal : Fin n -> Candidates n -> VoteValue -> Candidates n
decVoteVal i cands vv = addVoteVal i cands (-1 * vv)

removeCand : Fin (S n) -> Candidates (S n) -> Candidates n
removeCand = deleteAt

dontElect : Candidate -> Judged
dontElect cand = MkJudgment (candName cand) NotElected

-- Set this to be the election data being parsed.
-- TODO: Parse this as a command line argument! 
votes : String
votes = "small_election.txt"

-- Set this to the number of candidates being elected.
-- TODO: This should be parsed from the file. 
seats : Int
seats = 2

total
getElectedCands : Results n -> (p ** Results p)
getElectedCands res = filter isElected res where
    isElected : Judged -> Bool
    isElected j = Elected == (judgment j)