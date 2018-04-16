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

record Ballot2 (n : Nat) where
    constructor MkBallot
    elected : List CandidateName
    prefs : Preferences n
    value : VoteValue

replacePrefs : Preferences n -> Ballot2 n -> Ballot2 n
replacePrefs new ballot = record { prefs = new } ballot

replaceValue : VoteValue -> Ballot2 n -> Ballot2 n
replaceValue new ballot = record { value = new } ballot

replaceElected : List CandidateName -> Ballot2 n -> Ballot2 n
replaceElected new ballot = record { elected = new } ballot

-- New ballot type has a list of fins and
-- a double as a pair. 
total
Ballot : Nat -> Type
Ballot n = (Preferences n, VoteValue)

total
getPrefs : Ballot n -> Preferences n
getPrefs (prefs, _) = prefs

total
ballotValue : Ballot n -> VoteValue
ballotValue (_, val) = val

makeBallotShowable : Ballot n -> (List Int, VoteValue)
makeBallotShowable (prefs, vv) = (map finToInt prefs, vv)

Ballots : Nat -> Type
Ballots n = List $ Ballot n

makeBallotsShowable : Ballots n -> List $ (List Int, VoteValue)
makeBallotsShowable = map makeBallotShowable

total
newBallotVal : Ballot n -> Double -> Ballot n
newBallotVal (prefs, _) newVal = (prefs, newVal)

total
nextCand : Ballot n -> Maybe $ Fin n
nextCand ([], _)          = Nothing
nextCand ((cand :: _), _) = Just cand

total
restCand : Ballot n -> Ballot n
restCand ([], v)          = ([], v)
restCand ((_ :: rest), v) = (rest, v)

total
changeBallotIfIsCand : Fin n -> VoteValue -> Ballot n -> Ballot n
changeBallotIfIsCand cand vv ballot@(_, v) = case nextCand ballot of
    Just next => if cand == next 
        then restCand $ newBallotVal ballot vv 
        else restCand ballot
    Nothing   => restCand ballot