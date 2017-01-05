module Utils 
(
Constraint(Constraint),
getTooNearC,
getTooNearP,
getMachineC,
getMachineP,
Solution(Solution),
getAssignment,
getPenalty,
splitOn,
splitLeft,
remove,
extract,
allTrue,
insert,
replace,
delete,
blank2dInt,
blankInt,
blank2dBool,
blankBool,
taskLetter,
taskNumber
)
where

{-
Here is something I used to trace my solver, it is exciting to the ultimate max:

solve constraint best assignments remaining | trace ("Best: " ++ show best ++ "; " ++ show assignments) False = undefined

or:

function <args> | trace (<string>) False = undefined

Amazing!
-}

data Constraint = Constraint [[Bool]] [[Bool]] [[Int]] [[Int]] deriving (Show)

getTooNearC (Constraint c_tooNear _ _ _) = c_tooNear
getTooNearP (Constraint _ _ p_tooNear _) = p_tooNear
getMachineC (Constraint _ c_machine _ _) = c_machine
getMachineP (Constraint _ _ _ p_machine) = p_machine

data Solution = Solution [Int] Int deriving (Show)

getAssignment (Solution solution _) = solution
getPenalty  (Solution _ penalty ) = penalty

-- Split string on character. -------------------------------------------------
splitOn char [] = []
splitOn char str =
  result:(splitOn char remaining )
  where (result, remaining) = splitLeft str char

splitLeft (x:xs) char
    | x == char = ([],xs)
    | xs == [] = (x:[],[])
    | otherwise = (x:result, remaining)
  where (result, remaining) = splitLeft xs char


-- Removes element at index from the list -------------------------------------
remove (x:xs) 0     = xs
remove (x:[]) index = x:[]
remove (x:xs) index = x:(remove xs (index - 1))

-- Extracts element at index from the list ------------------------------------
extract list element = (list !! element, remove list element)

-- Determines whether a whole list is true ------------------------------------
allTrue :: [Bool] -> Bool
allTrue (x:[]) = x
allTrue (True:xs) = allTrue xs
allTrue (False:xs) = False

-- Inserts an element at an index in a list -----------------------------------
insert element 0 list = element:list
insert element index (x:xs) = x:(insert element (index - 1) xs)

-- Replaces an element at an index in a list ----------------------------------
replace element 0 (x:xs) = element:xs
replace element index (x:xs) = x:(replace element (index - 1) xs)

-- Deletes an element equal to item in a list ---------------------------------
delete item (x:xs)
              | x == item = xs
              | xs == [] = x:[]
              | otherwise = x:(delete item xs)

-- Blank --
blank2dBool sizeX 0    init = []
blank2dBool sizeX sizeY init = (blankBool sizeX init):(blank2dBool sizeX (sizeY - 1) init)

blankBool :: Int -> Bool -> [Bool]
blankBool 0 init = []
blankBool size init = init:(blankBool (size - 1) init)

blank2dInt sizeX 0     init = []
blank2dInt sizeX sizeY init = (blankInt sizeX init):(blank2dInt sizeX (sizeY - 1) init)

blankInt :: Int -> Int -> [Int]
blankInt 0 init = []
blankInt size init = init:(blankInt (size - 1) init)

-- Translate task numbers --
taskNumber 'A' = 0
taskNumber 'B' = 1
taskNumber 'C' = 2
taskNumber 'D' = 3
taskNumber 'E' = 4
taskNumber 'F' = 5
taskNumber 'G' = 6
taskNumber 'H' = 7
taskNumber c = -1

taskLetter 0 = 'A'
taskLetter 1 = 'B'
taskLetter 2 = 'C'
taskLetter 3 = 'D'
taskLetter 4 = 'E'
taskLetter 5 = 'F'
taskLetter 6 = 'G'
taskLetter 7 = 'H'
taskLetter c = '\0'
