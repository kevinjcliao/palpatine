module Candidates

import Data.Vect
import Ballot

%access public export

ExVect : Type -> Type
ExVect t = (n ** Vect n t)

record Candidate where
    constructor MkCandidate
    candName : Candidate
    candValue : VoteValue

Candidates : Nat -> Type
Candidates n = Vect n Candidate

data Judgment = Elected | NotElected

record Judged where
    constructor MkJudgment
    candName : Candidate
    judgment : Judgment

Results : Nat -> Type
Results n = Vect n Judged


getCand : Fin n -> Candidates n -> Candidate
getCand = candName index

getCandVal : Fin n -> Candidates n -> VoteValue
getCandVal = candValue index

addVoteVal : Fin n -> Candidates n -> VoteValue -> VoteValue

removeCand : Fin (S n) -> Candidates (S n) -> Candidates n
removeCand = deleteAt

-- Set this to be the election data being parsed.
-- TODO: Parse this as a command line argument! 
votes : String
votes = "small_election.txt"

-- Set this to the number of candidates being elected.
-- TODO: This should be parsed from the file. 
seats : Int
seats = 2
