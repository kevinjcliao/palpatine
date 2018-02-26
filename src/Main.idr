module Main

import Election
import Parse
import STV
import Ballot


total
main : IO ()
main = do
    Right file <- readFile votes
        | Left err => printLn err
    let ballots = parseBallots file
    printLn "Done!"