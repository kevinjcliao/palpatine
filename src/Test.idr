module Test

import STV
import Candidates
import Ballot
import Election
import Data.Fin
import Data.Vect

%access public export

-- Sample ballots: 
-- These ballots are what it would look like if parsing
-- for the small_election.txt succeeded. We will run an election on
-- this small data set. 
sampleCandidates : Candidates 3
sampleCandidates = 
    [ MkCandidate "A" 0
    , MkCandidate "B" 0
    , MkCandidate "C" 0
    ]

ballot1 : Ballot 3
ballot1 = ([0,2], 1)

ballot2 : Ballot 3
ballot2 = ([0,1,2], 1)

ballot3 : Ballot 3
ballot3 = ([0,2,1], 1)

ballot4 : Ballot 3
ballot4 = ([1,0], 1)

ballot5 : Ballot 3
ballot5 = ([2,1,0], 1)

sampleBallots : List $ Ballot 3
sampleBallots = [ballot1, ballot2, ballot3, ballot4, ballot5]

-- Set this to the number of candidates being elected.
-- TODO: This should be parsed from the file. 
seats : Nat
seats = 2

sampleElection : Election 3 0
sampleElection = makeElection
    (droopQuota 5 (cast seats))
    seats
    sampleBallots
    sampleCandidates
    emptyResults

assertEq : Eq a => (given : a) -> (expected : a) -> IO Unit
assertEq g e = if g == e
               then putStrLn "Test Passed"
               else putStrLn "Test Failed"

failTest : IO Unit
failTest = assertEq 0 1

spec : IO ()
spec = assertEq 1 1

testDroopQuota : IO ()
testDroopQuota = assertEq (droopQuota 4376143 6) 625164


