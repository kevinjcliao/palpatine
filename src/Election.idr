module Election

%access public export

-- Set this to be the election data being parsed.
votes : String
votes = "small_election.txt"

-- Set this to the number of candidates being elected.
seats : Int
seats = 2

data Cand =
    A
    | B
    | C

instance Show Cand where
    show A = "A"
    show B = "B"
    show C = "C"

cand_all : List Cand
cand_all = [A,B,C]

Ballot : Type
Ballot = (List Cand, Double)

candidate : String -> Maybe Cand
candidate "A" = Just A
candidate "B" = Just B
candidate "C" = Just C
candidate error = Nothing