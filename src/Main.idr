module Main

import Candidates
import Parse
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
ballot1 = MkBallot [] [0,2] 1

ballot2 : Ballot 3
ballot2 = MkBallot [] [0,1,2] 1

ballot3 : Ballot 3
ballot3 = MkBallot [] [0,2,1] 1

ballot4 : Ballot 3
ballot4 = MkBallot [] [1,0]  1

ballot5 : Ballot 3
ballot5 = MkBallot [] [2,1,0] 1

sampleBallots : List $ Ballot 3
sampleBallots = [ballot1, ballot2, ballot3, ballot4, ballot5]

-- Set this to the number of candidates being elected.
-- TODO: This should be parsed from the file. 
seats : Nat
seats = 2

total
main : IO ()
main = do
    Right file <- readFile votes
        | Left err => printLn err
    case readFirstLine file of
        Just ((p ** cands), seats) => let ballots = readBallots file cands in
            case stv   
                ( makeElection 
                  (droopQuota (length ballots) (cast seats))
                  seats
                  ballots
                  cands
                  emptyResults
                ) of
            e@(_,_,_,_,results) => printLn results
        Nothing => printLn "Parse error."
    -- let cands = sampleCandidates
    -- let ballots = sampleBallots
    -- let dq = droopQuota 5 (cast seats)
    -- let election = makeElection 
    --     dq
    --     seats
    --     ballots
    --     cands
    --     emptyResults
    -- printLn $ "Droop Quota is: " ++ (show dq)
    -- printLn "Beginning initial count 0"
    -- let count0 = count election
    -- printLn "After the first count, the ballots are: "
    -- printLn $ show $ makeBallotsShowable $ getBallots count0
    -- printLn "The candidates are: "
    -- printLn $ show $ getRemaining count0
    -- let count1 = processOne election
    -- printLn "========================"
    -- printLn "FIRST PROCESS HAS BEEN RUN"
    -- printLn "========================"
    -- let ballot1 = show $ makeBallotsShowable $ getBallots count1
    -- printLn $ "ballots after first election: " ++ ballot1
    -- printLn "Remaining: "
    -- printLn $ show $ getRemaining count1
    -- printLn "Results: "
    -- printLn $ show $ getResults count1
    -- let count2 = processOne count1
    -- printLn "========================="
    -- printLn "SECOND PROCESS HAS BEEN RUN"
    -- printLn "========================="
    -- let ballot2 = show $ makeBallotsShowable $ getBallots count2
    -- printLn $ "ballots after SECOND election: " ++ ballot2
    -- printLn "Remaining: "
    -- printLn $ show $ getRemaining count2
    -- printLn "Results: "
    -- printLn $ show $ getResults count2
    -- let count3 = processOne count2
    -- printLn "=========================="
    -- printLn "THIRD PROCESS HAS BEEN RUN"
    -- printLn "=========================="
    -- let ballot3 = show $ makeBallotsShowable $ getBallots count3
    -- printLn $ "ballots after THIRD election: " ++ ballot3
    -- printLn "Remaining: "
    -- printLn $ show $ getRemaining count3
    -- printLn "Results: "
    -- printLn $ show $ getResults count3
    -- -- printLn $ getElectedCands cands count1 dq