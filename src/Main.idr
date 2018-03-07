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
    case getCandidates file of
        Just (ExVect x) => printLn "Hi!"
        Nothing => printLn "Parse failed."