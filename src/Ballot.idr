module Ballot

import Election

%access public export

VoteValue : Type
VoteValue = Double

total
Ballot : Type
Ballot = (List String, VoteValue)

total
ballotValue : Ballot -> Double
ballotValue (_, val) = val

total
nextCand : Ballot -> Maybe String
nextCand ([], _)          = Nothing
nextCand ((cand :: _), _) = Just cand

total
restCand : Ballot -> Maybe $ List String
restCand ([], _)          = Nothing
restCand ((_ :: rest), _) = Just rest