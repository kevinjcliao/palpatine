module Ballot

import Candidates
import Data.Fin
import Data.Vect

%access public export

||| Convert a Fin to an Integer
finToInt : Fin n -> Int
finToInt FZ     = 0
finToInt (FS k) = 1 + finToInt k

Preferences : Nat -> Type
Preferences n = List $ Fin n

record Ballot (n : Nat) where
    constructor MkBallot
    votedFor : List CandidateName
    prefs : Preferences n
    value : VoteValue

replacePrefs : Preferences n -> Ballot n -> Ballot n
replacePrefs new ballot = record { prefs = new } ballot

replaceValue : VoteValue -> Ballot n -> Ballot n
replaceValue new ballot = record { value = new } ballot

replaceVotedFor : List CandidateName -> Ballot n -> Ballot n
replaceVotedFor new ballot = record { votedFor = new } ballot

total
getPrefs : Ballot n -> Preferences n
getPrefs = prefs

total
ballotValue : Ballot n -> VoteValue
ballotValue = value

makeBallotShowable : Ballot n -> (List CandidateName, List Int, VoteValue)
makeBallotShowable ballot = 
    ( votedFor ballot
    , map finToInt (prefs ballot)
    , value ballot
    )

Ballots : Nat -> Type
Ballots n = List $ Ballot n

length : Ballots n -> Int
length Nil = 0
length (x :: xs) = 1 + length xs

makeBallotsShowable : Ballots n 
                    -> List $ (List CandidateName, List Int, VoteValue)
makeBallotsShowable = map makeBallotShowable

total
nextCand : Ballot n -> Maybe $ Fin n
nextCand ballot         = case getPrefs ballot of
    [] => Nothing
    (cand :: _) => Just cand

total
restCand : Ballot n -> Ballot n
restCand ballot         = case getPrefs ballot of
    []          => ballot
    (_ :: rest) => replacePrefs rest ballot

ballotDidElectCandidate : CandidateName -> Ballot n -> Bool
ballotDidElectCandidate name ballot = elem name (votedFor ballot)

addToVotedFor : Candidates n -> Ballot n -> Ballot n
addToVotedFor cands ballot = case nextCand ballot of
    Just topPrefIndex => let name = candName (index topPrefIndex cands) in 
        record { 
            votedFor = name :: (votedFor ballot) 
        } (restCand ballot)
    Nothing => ballot

total
changeBallotIfIsCand : CandidateName -> VoteValue -> Ballot n -> Ballot n
changeBallotIfIsCand cand vv ballot = if ballotDidElectCandidate cand ballot
    then replaceValue vv ballot
    else ballot