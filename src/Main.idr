module Main

import Candidates
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
    case parseInput file of
        Just (ExVect x, ls) => printLn $ "The candidates are: " ++ show x ++ " and the ballots are: " ++ show ls
        Nothing => printLn "Parse failed."