module Test

import Election
import STV

%access public export

assertEq : Eq a => (given : a) -> (expected : a) -> IO Unit
assertEq g e = if g == e
               then putStrLn "Test Passed"
               else putStrLn "Test Failed"

spec : IO ()
spec = assertEq 1 1

testDroopQuota : IO ()
testDroopQuota = assertEq (droopQuota 4376143 6) 625164