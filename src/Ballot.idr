module Ballot

import Candidates
import Data.Fin

%access public export

VoteValue : Type
VoteValue = Double

-- New ballot type has a list of fins and
-- a double as a pair. 
total
Ballot : Nat -> Type
Ballot n = (List (Fin n), VoteValue)

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
restCand : Ballot n -> Maybe $ List $ Fin n
restCand ([], _)          = Nothing
restCand ((_ :: rest), _) = Just rest