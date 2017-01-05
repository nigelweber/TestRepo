module Solver
(
  solver,
  solve,
  branch,
  assign,
  setup,
  valid,
  validAssignments,
  penalty
)
where

import Data.List (foldl)
import Utils
import Debug.Trace
-------------------------------------------------------------------------------
-- The solver ! 
-------------------------------------------------------------------------------


solver :: (Constraint, [(Int, Int)], [Char]) -> (Solution, [Char])
solver (constraint, partials, []) =
  if error == []
    then sanitize (solve constraint (Solution [] max) init remaining)
    else ((Solution [] 0), error)
  where (init, remaining, error) = setup constraint partials [] []
        max           = 99999999

solver (constraint, partials, error) = ((Solution [] 0), error)

sanitize :: Solution -> (Solution, [Char])
sanitize solution = 
  if getAssignment solution == []
    then ((Solution [] 0), "No valid solution possible!")
    else (solution, [])

solve :: Constraint -> Solution -> [Int] -> [Int] -> Solution
solve constraint best assignments [] = 
  if (valid constraint assignments) && (penalty < (getPenalty best))
    then Solution assignments penalty
    else best
  where penalty = penalties constraint assignments

solve constraint best assignments remaining =
  if valid constraint assignments && (penalties constraint assignments) < (getPenalty best)
    then branch constraint best assignments (map (extract remaining) [0..max])
    else best
  where max = (length remaining) - 1

branch :: Constraint -> Solution -> [Int] -> [(Int, [Int])] -> Solution
branch constraint best assignments ((task, nextRemaining):[]) =
  solve constraint best (assign assignments task) nextRemaining

branch constraint best assignments ((task, nextRemaining):remaining) =
  branch constraint (solve constraint best (assign assignments task) nextRemaining) assignments remaining

  -- Assign a task to the first empty machine ---------------------------------
assign :: [Int] -> Int-> [Int]
assign assignments task = replace task (emptyMachine assignments) assignments

  -- Find the first empty machine ---------------------------------------------
emptyMachine :: [Int] -> Int
emptyMachine assignments = emptyMachineLoop assignments 0

emptyMachineLoop :: [Int] -> Int-> Int
emptyMachineLoop ((-1):xs) index = index
emptyMachineLoop (x:[]) index = (-1)
emptyMachineLoop (x:xs) index = emptyMachineLoop xs (index + 1)
  
-------------------------------------------------------------------------------
-- Set up initial state 
-------------------------------------------------------------------------------
setup :: Constraint -> [(Int, Int)] -> [Int] -> [Int] -> ([Int],[Int],[Char])
setup constraint partials [] [] =
  setup constraint partials (blankInt 8 (-1)) [0..7]

setup constraint [] assignments remaining = 
  (assignments, remaining, [])

setup constraint ((machine, task):pairs) assignments remaining =
  if assignments !! machine == (-1) && valid constraint next
    then setup constraint pairs next (delete task remaining)
    else ([], [0..7], "No valid solution possible!")
  where next = replace task machine assignments
        
-------------------------------------------------------------------------------
-- Constraint functions
-------------------------------------------------------------------------------
valid :: Constraint -> [Int] -> Bool
valid constraint assignments = 
  allTrue (map (validAssignments constraint assignments) [0..7])

validAssignments :: Constraint -> [Int] -> Int-> Bool
validAssignments constraint assignments machine =
  (validMachine constraint assignments machine)
  && (validTooNear constraint assignments machine)

  -- Check against the invalid machine constraints ----------------------------
validMachine :: Constraint -> [Int] -> Int-> Bool
validMachine constraint assignments 7 =
  isValidMachine constraint 7 (assignments !! 7    )

validMachine constraint assignments index =
  isValidMachine constraint index (assignments !! index)
  && validMachine constraint assignments (succ index) 

isValidMachine :: Constraint -> Int-> Int-> Bool
isValidMachine constraint machine (-1) = True
isValidMachine constraint machine task = not $ ((getMachineC constraint)!! machine) !! task

  -- Check against the too-near task constraints ------------------------------
validTooNear :: Constraint -> [Int] -> Int-> Bool
validTooNear constraint assignments 7 = 
  isValidTooNear constraint (assignments !! 7    ) (assignments !! 0           )

validTooNear constraint assignments index = 
  isValidTooNear constraint (assignments !! index) (assignments !! (succ index))
  && validTooNear constraint assignments (succ index)

isValidTooNear :: Constraint -> Int-> Int-> Bool
isValidTooNear constraint (-1) task' = True
isValidTooNear constraint task (-1)  = True
isValidTooNear constraint task task' = not $ ((getTooNearC constraint) !! task) !! task'


-------------------------------------------------------------------------------
-- Penalty functions 
-------------------------------------------------------------------------------
penalties :: Constraint -> [Int] -> Int
penalties constraint assignments = 
  foldl (+) 0 (map (penalty constraint assignments) [0..7])

penalty :: Constraint -> [Int] -> Int-> Int
penalty constraint assignments machine = 
  (+) (tooNearPenalty constraint assignments machine)
      (machinePenalty constraint assignments machine)

  -- Get the machine penalty --------------------------------------------------
machinePenalty :: Constraint -> [Int] -> Int-> Int
machinePenalty constraint assignments machine = 
  if task >= 0
    then ((getMachineP constraint) !! machine) !! task
    else 0
  where task = assignments !! machine

  -- Get the too-near penalty -------------------------------------------------
tooNearPenalty :: Constraint -> [Int] -> Int -> Int
tooNearPenalty constraint assignments 7 = 
  superRadFunction constraint (assignments !! 7) (assignments !! 0)

tooNearPenalty constraint assignments machine = 
  if machine < 7
    then superRadFunction constraint (assignments !! machine) (assignments !! (succ machine))
    else 0

superRadFunction :: Constraint -> Int -> Int -> Int
superRadFunction constraint (-1) task' = 0
superRadFunction constraint task (-1)  = 0
superRadFunction constraint task task' = ((getTooNearP constraint) !! task) !! task'

