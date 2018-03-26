module Main

import Candidates
import Parse
import STV
import Ballot
import VoteCount
import Data.Vect
import Data.SortedMap


-- Sample ballots: 
-- These ballots are what it would look like if parsing
-- for the small_election.txt succeeded. We will run an election on
-- this small data set. 
sampleCandidates : Candidates 3
sampleCandidates = ["A", "B", "C"]

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

total
main : IO ()
main = do
    Right file <- readFile votes
        | Left err => printLn err
    -- Parsing is broken so we'll simulate parsing working
    -- for now. 
    let cands = sampleCandidates
    let ballots = sampleBallots
    let initialCount = initVoteCount cands
    let count1 = firstCount cands ballots initialCount
    let dq = droopQuota 5 seats
    printLn $ getElectedCands cands count1 dq
    case chooseToEliminate count1 cands of
        (eliminated, vc2, _) => printLn eliminated