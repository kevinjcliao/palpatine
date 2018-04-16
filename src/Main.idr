module Main

import Candidates
-- import Parse
import STV
import Ballot
import Data.Vect
import Data.SortedMap
import Election


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

total
main : IO ()
main = do
    -- Right file <- readFile votes
    --     | Left err => printLn err
    -- -- Parsing is broken so we'll simulate parsing working
    -- -- for now. 
    let cands = sampleCandidates
    let ballots = sampleBallots
    let dq = droopQuota 5 (cast seats)
    let election = makeElection 
        dq
        seats
        ballots
        cands
        emptyResults
    printLn $ "Droop Quota is: " ++ (show dq)
    printLn $ "first: " ++ (show cands)
    let count1 = processOne election
    printLn $ show $ makeBallotsShowable $ getBallots count1
    printLn $ getRemaining count1
    let count2 = processOne count1
    printLn $ getRemaining count2
    let count3 = processOne count2
    printLn $ getRemaining count3
    -- printLn $ getElectedCands cands count1 dq
    case stv election of
        e@(_,_,_,_,results) => printLn results