module Ballot

import Candidates
import Data.Fin

%access public export

-- New ballot type has a list of fins and
-- a double as a pair. 
total
Ballot : Nat -> Type
Ballot n = (List (Fin n), VoteValue)

Ballots : Nat -> Type
Ballots n = List $ Ballot n

total
ballotValue : Ballot n -> Double
ballotValue (_, val) = val

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
        else ballot
    Nothing   => ballot