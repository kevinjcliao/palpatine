module Test

import STV
import Candidates
import VoteCount
import Ballot

%access public export

assertEq : Eq a => (given : a) -> (expected : a) -> IO Unit
assertEq g e = if g == e
               then putStrLn "Test Passed"
               else putStrLn "Test Failed"

failTest : IO Unit
failTest = assertEq 0 1

testVoteCount : VoteCount
testVoteCount = [(A, 0), (B, 0), (C, 0)]

spec : IO ()
spec = assertEq 1 1

testDroopQuota : IO ()
testDroopQuota = assertEq (droopQuota 4376143 6) 625164

testInitiateCount : IO ()
testInitiateCount = assertEq (show $ initiateCount [A,B,C]) (show [(A,0), (B,0), (C,0)])

testGetVoteForCandidate : IO ()
testGetVoteForCandidate = case (getVoteForCandidate testVoteCount A) of
    Just val => assertEq val 0
    Nothing  => failTest

sampleBallot1 : Ballot
sampleBallot1 = ([C], 0.5)

-- Increment the score of candidate C by 1
testAddVote : IO ()
testAddVote = case addVote testVoteCount sampleBallot1 of
    Just vc => case getVoteForCandidate vc C of
        Just val => assertEq val 0.5
        Nothing  => failTest
    Nothing => failTest
