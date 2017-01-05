module Printer
(printer)
where


import System.Environment
import System.IO
import Utils


printer :: (Solution,String)->IO()
printer (solution, []) = do
  args<-getArgs
  {-
  let writeFile = args !! 1
  handle<-openFile writeFile WriteMode
  let prntStatement = ("Solution " ++ (init (unwords(appendSpace (map taskLetter (getAssignment solution)))))++ "; Quality: " ++ show( getPenalty solution))
  hPutStrLn handle (prntStatement)
  -}
  --let prntStatement = ("Solution " ++ (init (unwords(appendSpace (map taskLetter (getAssignment solution)))))++ "; Quality: " ++ show( getPenalty solution))
  let prntStatement = ("Solution " ++ unwords(map (\c -> [taskLetter c]) (getAssignment solution))++"; Quality: " ++ show (getPenalty solution))
  writeFile (args !! 1) prntStatement

  
printer (solution, error) = do
  args<-getArgs
  {-
  let writeFile = args !! 1
  handle<-openFile writeFile WriteMode
  --hPrint handle error
  hPutStrLn handle (error)
  -}
  writeFile (args !! 1) error
  
  


appendSpace :: String->[String]
appendSpace x = map(\s -> [s] ++ " ")  x
