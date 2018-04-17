module Main

import Candidates
import Parse
import STV
import Ballot
import Data.Vect
import Data.List
import Data.SortedMap
import Election
import Prelude.File


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

-- -- Set this to the number of candidates being elected.
-- -- TODO: This should be parsed from the file. 
-- seats : Nat
-- seats = 2
total
runElection : String -> IO ()
runElection fileName = do
    Right str <- readFile fileName
    | Left err => putStrLn "ERROR: ReadFile Failed."
    putStrLn "Printing file."
    let lines = split (=='\n') str
    putStrLn "Readfile succeeded."
    case readFirstLine str of
        Just ((p ** cands), seats) => do 
            putStrLn "Beginning readBallots."
            putStrLn $ "Electing: " ++ (show seats) ++ "seats."
            let ballots = readBallots str cands
            putStrLn "Readballots complete."
            let dq = (droopQuota (length ballots) (cast seats))
            putStrLn $ "The Droop Quota is: " ++ (show dq)
            putStrLn "The Ballots are: "
            putStrLn $ show $ makeBallotsShowable ballots
            case stv
                ( makeElection 
                  dq
                  seats
                  ballots
                  cands
                  emptyResults
                ) of
                e@(_,_,_,_,results) => do
                    putStrLn "Done running the election. The results are:"
                    putStrLn $ show results
        Nothing => putStrLn "Parse error."

partial
main : IO ()
main = do
    args <- getArgs
    case args of
        [_, fileName] => do
            putStrLn $ "Palpatine has been invoked on: " ++ fileName
            runElection fileName
            putStrLn "Done running election."
        _ => do 
            putStrLn "ERROR: File name not given. Running default small_election"
            runElection "sample_data/small_election.txt"
