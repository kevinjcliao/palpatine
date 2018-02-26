module STV

import Election

%access public export

-- As defined by the Australian Electoral Commission
-- http://www.aec.gov.au/Voting/counting/senate_count.htm
droopQuota : Int -> Int -> Int
droopQuota intBallots intSeats = 
    flooredFirstDiv + 1
    where
        numBallots : Double
        numBallots = cast intBallots
        numSeats : Double
        numSeats = cast intSeats
        flooredFirstDiv : Int
        flooredFirstDiv = cast $ numBallots / (numSeats + 1)

-- Vote count is stored as a list of tuples [(Cand, Score)]
-- Why not a Map? Idris only has support for a SortedMap
-- Type where the key must implement Ord. To my knowledge,
-- deriving Ord as an instance is not supported. 
-- Initiate count takes the list of candidates and generates
-- a list of tuples with [(Cand, 0)]
initiateCount : List Cand -> List (Cand, Double)
initiateCount = map toTuple where
    toTuple : Cand -> (Cand, Double)
    toTuple cand = (cand, 0)

