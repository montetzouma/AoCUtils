module AoCUtils.SolutionPrinter 
  ( printSolution)
where

printSolution :: Int -> IO () -> IO ()
printSolution day solver = do
  putStrLn $ mconcat ["------- DAY ", show day, " -------"]
  solver
  putStrLn "\n"