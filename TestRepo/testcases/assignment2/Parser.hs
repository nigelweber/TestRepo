module Parser (
parser,
parse,
parseForcedPartials,
parseForbiddenMachine,
parseLineForbidden,
firstInteger,
firstCharacter,
thirdInteger,
secondCharacter,
splitComma,
parseTooNearPenalties,
parseLineTooNearPenalties
)
where 
import Data.Char
import Utils
import Debug.Trace

-- output type: ([tooNearPen] (2D list of ints),[machinePen] (2D list in ints),[tooNear] (2D list of bool),[forbidden] (2D list of bool),[forced] (list of (machine,task pairs (example: 1,a)),[optionalErrorMessage])--

parser a = parse a (Constraint [] [] [] [], [(-2,-2)], "")

parse :: [String] -> (Constraint, [(Int, Int)], String) -> (Constraint, [(Int, Int)], String)
parse []      (con, partials, err)
    | partials == [(-2,-2)] || partials == [(-1,-1)] = (con, [], err_parsing)
    | getMachineC con == [] = (con, [], err_parsing)
    | getMachineP con == [] = (con, [], err_parsing)
    | getTooNearC con == [] = (con, [], err_parsing)
    | getTooNearP con == [] = (con, [], err_parsing)
    | otherwise = (con, partials, err)

parse strList (con, partials, err)
    | err     /= [] = (con, partials, err)
    | line    == "Name:" = parseInnerName rem (con, [(-1,-1)], err)
    | line    == "forced partial assignment:" = parseInnerPartial rem (con, partials, err) -- overwrite default
    | line    == "forbidden machine:" = parseInnerMachineC rem (con, partials, err)
    | line    == "too-near tasks:" = parseInnerTooNearC rem (con, partials, err)
    | line    == "machine penalties:" = parseInnerMachineP rem (con, partials, err)
    | line    == "too-near penalities" = parseInnerTooNearP rem (con, partials, err)
    | line    == [] = parse rem (con, partials, err)
    | otherwise     = (con, [], err_parsing)
    where line = rtrim ( head strList )
          rem  = tail strList

parseInnerName strList (con, partials, err) 
    | (length strList) < 2 = (con, [], err_parsing)
    | otherwise            = parse (tail (tail strList)) (con, partials, err)

parseInnerPartial strList (con, partials, err)
    | partials == [(-2,-2)] = (con, [], err_parsing)
    | err     /= [] = (con, [], err)
    | strList == [] = (con, [], err_parsing)
    | otherwise     = parse rem (con, partials', err')
    where (rem, partials', err') = parseForcedPartials (strList, [], [])
    
parseInnerMachineC strList (con, partials, err)
    | err     /= [] = (con, [], err)
    | strList == [] = (con, [], err_parsing)
    | otherwise     = parse rem (con', partials, err')
    where (rem, bools, err') = parseForbiddenMachine (strList, blank2dBool 8 8 False, [])
          con' = Constraint (getTooNearC con) bools (getTooNearP con) (getMachineP con)

parseInnerTooNearC strList (con, partials, err)
    | err     /= [] = (con, [], err)
    | strList == [] = (con, [], err_parsing)
    | otherwise     = parse rem (con', partials, err')
    where (rem, bools, err') = parseTooNearTasks (strList, blank2dBool 8 8 False, [])
          con' = Constraint bools (getMachineC con) (getTooNearP con) (getMachineP con)

parseInnerMachineP strList (con, partials, err)
    | err     /= [] = (con, [], err)
    | strList == [] = (con, [], err_parsing)
    | otherwise     = parse rem (con', partials, err')
    where (rem, ints, err') = parseMachinePenalties (strList, blank2dInt 8 8 0, [])
          con' = Constraint (getTooNearC con) (getMachineC con) (getTooNearP con) ints

parseInnerTooNearP strList (con, partials, err)
    | err     /= [] = (con, [], err)
    | strList == [] = (con', partials, err) -- special since it's last
    | otherwise     = parse rem (con', partials, err')
    where (rem, ints, err') = parseTooNearPenalties (strList, blank2dInt 8 8 0, [])
          con' = Constraint (getTooNearC con) (getMachineC con) ints (getMachineP con)

parseForcedPartials ::  ([String], [(Int, Int)], String) -> ([String], [(Int, Int)], String)
parseForcedPartials (a, b, c)
    |c      /= []         = (a,b,c)
    |head a == []		      = ((tail a),b,c)
    |not (isValidTuple (head a) 2) = (a,b,err_parsing)
    |first < 0 || first > 7 = (a,b,err_machine_task)
    |second < 0 || second > 7 = (a,b,err_machine_task)
    |pairIsIn pair b = (a, b, "partial assignment error")
    |otherwise = parseForcedPartials(tail a, pair:b, c)
    where
      first = firstInteger (head a)
      second = taskNumber $ secondCharacter (head a)
      pair = (first, second)

pairIsIn (a,b) [] = False
pairIsIn (a,b) ((a',b'):xs) 
    | a == a' || b == b' = True
    | xs == [] = False
    | otherwise = pairIsIn (a,b) xs
  

parseForbiddenMachine :: ([String], [[Bool]], String) -> ([String], [[Bool]], String)
parseForbiddenMachine (a,b,c)
    |c /= "" = (a,b,c)
    |a == [] = ([], b, err_parsing)
    |word == "\n" = (rem, b, c)
    |word == "" = (rem, b, c)
    -- |word == " " = (rem, b, c)
    |not (isValidTuple word 2) = (a,b,err_parsing)
    |otherwise = parseLineForbidden (a,b,c)
    where word = rtrim $ head a
          rem  = tail a

parseLineForbidden :: ([String], [[Bool]], String) -> ([String], [[Bool]], String)
parseLineForbidden (strList,table,err) 
    |err /= ""    = (strList,table,err)
    |word == "\n" = (rem,table,err)
    |word == ""   = (rem,table,err)
    |first `notElem` [0..7]      = (strList,table,err_machine_task)
    |second `notElem` ['A'..'H'] = (strList,table,err_machine_task)
    |otherwise = parseForbiddenMachine (rem, insertBool table first (taskNumber second), err)
    where word   = head strList
          rem    = tail strList
          first  = firstInteger word
          second = secondCharacter word

insertBool bools machine task = replace (replace True task (bools !! machine)) machine bools

parseTooNearTasks :: ([String], [[Bool]], String) -> ([String], [[Bool]], String)
parseTooNearTasks (a,b,c)
    |c /= [] = (a,b,c)
    |head a == "\n" = (tail a,b,c)
    |head a == "" = (tail a, b, c)
    |not (isValidTuple (head a) 2) = (a,b,err_parsing)
    |otherwise = parseLineTooNearTasks (a,b,c)


parseLineTooNearTasks :: ([String], [[Bool]], String) -> ([String], [[Bool]], String)
parseLineTooNearTasks (a,b,c) 
    |c /= "" = (a,b,c)
    |head a == "" = (a,b,c)
    |first `notElem` ['A'..'H'] = (a,b,err_machine_task)
    |second `notElem` ['A'..'H'] = (a,b,err_machine_task)
    |otherwise = parseTooNearTasks (tail a, insertBool b (taskNumber first) (taskNumber second), c)
    where first  = firstCharacter (head a)
          second = secondCharacter (head a)
	
	
parseMachinePenalties :: ([String], [[Int]], String) -> ([String], [[Int]], String)
parseMachinePenalties (("":xs),b,c) = (xs, b, err_machinePenalty)
parseMachinePenalties (a,b,c) = parseMachineReturn(parseMachineHelper (a,b,c,0))
{-machine penalties:
i i i i i i i i
j j j j j j j j-}
parseMachineHelper :: ([String], [[Int]], String, Int) -> ([String], [[Int]], String, Int)
parseMachineHelper (a,b,c,d)
    |d > 7 && (rtrim (head a)) /= [] = (a,b,err_machinePenalty,d)
    |d > 7 =	(a,b,c,d)
    |(rtrim (head a)) == "" = (a,b,err_machinePenalty,d)   
    |not (areValidPenalties (head a)) = (a,b,err_penalty,d)
    |length (splitOn ' ' (head a)) /= 8 = (a,b,err_machinePenalty,d)
    |otherwise = parseMachineHelper (tail a, replace (map read $ words (head a) :: [Int]) d b, c, d+1)

parseMachineReturn :: ([String], [[Int]], String, Int) -> ([String], [[Int]], String)
parseMachineReturn (a,b,c,d) = (a,b,c)
	
	
parseTooNearPenalties :: ([String], [[Int]], String) -> ([String], [[Int]], String)
parseTooNearPenalties (strList,ints,err)
    |err /= ""     = (strList,ints,err)
    |strList == [] = (strList,ints,err)
    |word    == "" = (strList,ints,err)
    |not (isValidTuple word 3) = (strList,ints,err_parsing)
    |otherwise = parseLineTooNearPenalties (strList,ints,err)
    where word = head strList

parseLineTooNearPenalties :: ([String], [[Int]], String) -> ([String], [[Int]], String)
parseLineTooNearPenalties (strList,ints,err)
    |task   `notElem` [0..7] = (strList,ints,err_task)
    |task'  `notElem` [0..7] = (strList,ints,err_task)
    |penalty < 0 = (strList, ints, err_penalty)
    |otherwise = parseTooNearPenalties (tail strList, ints', err)
    where 
      word   = head strList
      penalty= thirdInteger word
      task   = taskNumber $ firstCharacter word
      task'  = taskNumber $ secondCharacter word
      ints'  = replace (replace penalty task' (ints !! task)) task ints
	
err_machinePenalty = "machine penalty error"
err_tooNearPenalty = "toonear penalty error"
err_task           = "invalid task"
err_penalty        = "invalid penalty"
err_machine_task   = "invalid machine/task"
err_parsing        = "Error while parsing input file"

--------------------------------------------------------------------------
--String functions to get the input we want

-- Check if a string matches the format "(..,..)" with no spaces.
areValidPenalties :: String -> Bool
areValidPenalties [] = True
areValidPenalties (x:xs)
  | xs /= [] && x == ' ' && (head xs) == ' ' = False
  | (isDigit x) || x == ' ' = areValidPenalties xs
  | otherwise = False

isValidTuple :: String -> Int -> Bool
isValidTuple str len =
  (hasBrackets rstr) && (noSpaces rstr) && length (splitComma str) == len
  where rstr = rtrim str

noSpaces [] = True
noSpaces (' ':xs) = False
noSpaces (x:xs) = noSpaces xs

hasBrackets [] = False
hasBrackets str = (head str) == '(' && (last str) == ')'

rtrim str
  | str == []       = []
  | last str == ' ' = rtrim (init str)
  | otherwise       = str

--Removes first and last element of [char]
removeBrackets xs = tail (init xs)  

--Splits [char] separated by comma to elements
splitComma xs = splitOn ',' (removeBrackets xs)

--Splits [char] separated by space to elements
splitSpace xs = splitOn ' ' xs -- DOES NOT WORK! Ended up using (map read $ words) instead.

--gets the Integer found at the first element
firstInteger str
  | not (allTrue (map isDigit word)) = -1
  | otherwise = (read word :: Int) - 1
  where split = splitComma str
        word  = split !! 0

--gets the Integer found at the third element
thirdInteger a
  | length split /= 3 = -1
  | not (allTrue (map isDigit word)) = -1
  | otherwise = read ((splitComma a) !! 2) :: Int
  where split = splitComma a
        word =  split !! 2

--gets the Char found at the first element
firstCharacter a
  | (length word) /= 1 = '\0'
  | otherwise = toUpper $ word !! 0
  where split = splitComma a
        word  = split !! 0

--gets the Char found at the second element
secondCharacter a
  | (length word) /= 1 = '\0'
  | otherwise = toUpper $ word !! 0
  where split = splitComma a
        word  = split !! 1

