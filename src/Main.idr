module Main

import Election
import Parse
import STV
import Ballot
import VoteCount
import Data.Vect


main : IO ()
main = do
    Right file <- readFile votes
        | Left err => printLn err
    printLn $ show $ parseList "[1,2,3]"
    case readFirstLine "[1,2,3]:3" of
        Nothing          => printLn "Parse error."
        Just $ ExVect ls => printLn $ show ls

    -- let ballots = parseBallots file
    -- let numBallots = length ballots
    -- let quota = droopQuota (cast numBallots) seats
    -- printLn $ "The quota to get a seat is: " ++ (show quota)
    -- let newCounter = initiateCount candAll
    -- let firstPrefs = countFirstPrefs ballots newCounter
    -- printLn $ show $ firstPrefs