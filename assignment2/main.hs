

import System.IO
import System.Environment
import Data.List

import Solver
import Parser
import Printer
import Utils

main = do

  args <- getArgs
  if (length args) == 2
    then
      do
        let readFile=args !! 0
        handle<- openFile readFile ReadMode
        contents <- hGetContents handle
        --print (solver(parser(lines contents)))
        printer(solver(parser(lines contents)))
    else
      putStrLn "Usage: <input file> <output file>"
