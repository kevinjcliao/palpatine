module Ballot

import Candidates
import Data.Fin

%access public export

VoteValue : Type
VoteValue = Double

total
Ballot : Type
Ballot = (List Nat, VoteValue)

total
Ballot2 : Nat -> Type
Ballot2 n = (List (Fin n), VoteValue)

total
ballotValue : Ballot -> Double
ballotValue (_, val) = val

total
nextCand : Ballot -> Maybe Nat
nextCand ([], _)          = Nothing
nextCand ((cand :: _), _) = Just cand

total
restCand : Ballot -> Maybe $ List Nat
restCand ([], _)          = Nothing
restCand ((_ :: rest), _) = Just rest