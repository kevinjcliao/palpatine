module Ballot

import Candidates
import Data.Fin

%access public export

VoteValue : Type
VoteValue = Double

total
Ballot : Type
Ballot = (List Nat, VoteValue)

-- New ballot type has a list of fins and
-- a double as a pair. 
total
Ballot2 : Nat -> Type
Ballot2 n = (List (Fin n), VoteValue)

-- implementation Show (List (Ballot2 n)) where
--     show n = "hello world"

--- implementation Show (Ballot2 n) where
--    show (ls, val) = "(" ++ (show ls) ++ ", " ++ (show val) ++ ")"

total
ballotValue : Ballot2 n -> Double
ballotValue (_, val) = val

total
nextCand : Ballot2 n -> Maybe $ Fin n
nextCand ([], _)          = Nothing
nextCand ((cand :: _), _) = Just cand

total
restCand : Ballot2 n -> Maybe $ List $ Fin n
restCand ([], _)          = Nothing
restCand ((_ :: rest), _) = Just rest