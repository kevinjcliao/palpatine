module Ballot

import Candidates
import Data.Fin

%access public export

||| Convert a Fin to an Integer
finToInt : Fin n -> Int
finToInt FZ     = 0
finToInt (FS k) = 1 + finToInt k

Preferences : Nat -> Type
Preferences n = List $ Fin n

record Ballot (n : Nat) where
    constructor MkBallot
    elected : List CandidateName
    prefs : Preferences n
    value : VoteValue

replacePrefs : Preferences n -> Ballot n -> Ballot n
replacePrefs new ballot = record { prefs = new } ballot

replaceValue : VoteValue -> Ballot n -> Ballot n
replaceValue new ballot = record { value = new } ballot

replaceElected : List CandidateName -> Ballot n -> Ballot n
replaceElected new ballot = record { elected = new } ballot

total
getPrefs : Ballot n -> Preferences n
getPrefs = prefs

total
ballotValue : Ballot n -> VoteValue
ballotValue = value

makeBallotShowable : Ballot n -> (List CandidateName, List Int, VoteValue)
makeBallotShowable ballot = 
    ( elected ballot
    , map finToInt (prefs ballot)
    , value ballot
    )

Ballots : Nat -> Type
Ballots n = List $ Ballot n

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

total
changeBallotIfIsCand : Fin n -> VoteValue -> Ballot n -> Ballot n
changeBallotIfIsCand cand vv ballot = case nextCand ballot of
    Just next => if cand == next 
        then restCand $ replaceValue vv ballot
        else restCand ballot
    Nothing   => restCand ballot