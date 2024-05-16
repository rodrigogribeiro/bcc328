import System.Environment (getArgs)
import System.Exit (exitSuccess)

import SVM.Interp
import SVM.StackParser



main :: IO () 
main = do 
  args <- getArgs 
  fileName <- processArgs args 
  content <- readFile fileName 
  case parseCode content of 
    Left err -> do 
      putStrLn err 
      exitSuccess
    Right instrs -> do 
      res <- runVM instrs 
      case res of 
        Left err -> do 
          print err 
          exitSuccess
        Right _ -> return ()
 
processArgs :: [String] -> IO FilePath  
processArgs [file] = return file 
processArgs _ 
  = do  
      let err1 = "Invalid Arguments"
      let err2 = "SVM - Simple Virtual Machine"
      let err3 = "Usage:\n svm <file-name>"
      let msg  = unwords [err1, err2, err3]
      putStrLn msg 
      exitSuccess


