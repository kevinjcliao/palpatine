module STV

import Candidates
import Ballot
import VoteCount

%access public export

-- As defined by the Australian Electoral Commission
-- http://www.aec.gov.au/Voting/counting/senate_count.htm
-- total
-- droopQuota : Int -> Int -> Int
-- droopQuota intBallots intSeats = 
--     flooredFirstDiv + 1
--     where
--         numBallots : Double
--         numBallots = cast intBallots
--         numSeats : Double
--         numSeats = cast intSeats
--         flooredFirstDiv : Int
--         flooredFirstDiv = cast $ numBallots / (numSeats + 1)

-- -- To count first prefs, you need: 
-- -- List of ballots. List of candidates. 
-- total
-- countFirstPrefs : List Ballot -> VoteCount -> VoteCount
-- countFirstPrefs ballots vc = newCount where
--     count : List Ballot -> VoteCount -> VoteCount
--     count [] vc = vc
--     count (ballot :: rest) vc = case addVote vc ballot of
--         Just newVc => count rest newVc
--         Nothing => vc
--     newCount = count ballots vc