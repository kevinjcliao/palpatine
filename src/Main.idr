module Main

import Election
import Parse
import STV
import Ballot
import VoteCount


total
main : IO ()
main = do
    Right file <- readFile votes
        | Left err => printLn err
    let ballots = parseBallots file
    let numBallots = length ballots
    let quota = droopQuota (cast numBallots) seats
    printLn $ "The quota to get a seat is: " ++ (show quota)
    let newCounter = initiateCount candAll
    let firstPrefs = countFirstPrefs ballots newCounter
    printLn $ show $ firstPrefs