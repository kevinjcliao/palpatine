module Main

import Election
import Parse
import STV


main : IO ()
main = do
    Right file <- readFile votes
        | Left err => printLn err
    let ballots = parseBallots file
    printLn "Done!"