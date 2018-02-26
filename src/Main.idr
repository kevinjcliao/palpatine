module Main

import Election
import Parse

main : IO ()
main = do
    Right file <- readFile "small_election.txt"
        | Left err => printLn err
    let ballots = parseBallots file
    printLn "Done!"