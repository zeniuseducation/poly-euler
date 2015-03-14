import Data.List
import System.Environment (getArgs)

interactWith fun inFile outFile = do
  input <- readFile inFile
  writeFile outFile (fun input)

main = mainWith myFunction
  where mainWith func = do
          args <- getArgs
          case args of
            [input,output] -> interactWith func input output
            _ -> putStrLn "Error guoblok!!"
        myFunction = reverse
