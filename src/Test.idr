module Test

import STV
import Election

%access public export

assertEq : Eq a => (given : a) -> (expected : a) -> IO Unit
assertEq g e = if g == e
               then putStrLn "Test Passed"
               else putStrLn "Test Failed"

spec : IO ()
spec = assertEq 1 1

testDroopQuota : IO ()
testDroopQuota = assertEq (droopQuota 4376143 6) 625164

testInitiateCount : IO ()
testInitiateCount = assertEq (show $ initiateCount [A,B,C]) (show [(A,0), (B,0), (C,0)])