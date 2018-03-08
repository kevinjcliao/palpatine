module Candidates

import Data.Vect

%access public export

ExVect : Type -> Type
ExVect t = (n ** Vect n t)
 
Candidate : Type
Candidate = String

Candidates : Nat -> Type
Candidates n = Vect n Candidate

-- Set this to be the election data being parsed.
votes : String
votes = "small_election.txt"

-- Set this to the number of candidates being elected.
seats : Int
seats = 2
