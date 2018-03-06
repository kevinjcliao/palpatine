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
        Just (ExVect x) => printLn $ "The ballots are: " ++ (show $ readBallots file x) ++ " and the candidates are: " ++ (show x)
        Nothing => printLn "Parse failed."