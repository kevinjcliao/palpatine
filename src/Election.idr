module Election

import Candidates
import Ballot
import Data.Vect
import Data.Fin

%access public export

Election : Nat -> Nat -> Type
Election r j = (Int, Nat, Ballots r, Candidates r, Results j)

makeElection : Int 
            -> Nat 
            -> Ballots r 
            -> Candidates r 
            -> Results j 
            -> Election r j
makeElection i n b r j = (i, n, b, r, j)

getDq : Election r j -> Int
getDq e@(i, _, _, _, _) = i

getSeats : Election r j -> Nat
getSeats e@(_, n, _, _, _) = n

getBallots : Election r j -> Ballots r
getBallots e@(_, _, b, _, _) = b

getRemaining : Election r j -> Candidates r
getRemaining e@(_, _, _, r, _) = r

getResults : Election r j -> Results j
getResults e@(_, _, _, _, r) = r

updateSeats : Nat -> Election r j -> Election r j
updateSeats n2 e@(i, n, b, c, r) = (i, n2, b, c, r)

updateBallots : Ballots r -> Election r j -> Election r j
updateBallots b2 e@(i, n, b, c, r) = (i, n, b2, c, r)

updateRemaining : Candidates r -> Election r j -> Election r j
updateRemaining c2 e@(i, n, b, c, r) = (i, n, b, c2, r)

updateResults : Results j-> Election r j -> Election r j
updateResults r2 e@(i, n, b, c, r) = (i, n, b, c, r2)