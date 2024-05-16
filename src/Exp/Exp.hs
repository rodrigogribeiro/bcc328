
import Exp.Frontend.Parser.Parser 
import Exp.Interpreter.ExpInterpreter
import Exp.Syntax.Syntax  
import Exp.Backend.C.ExpCCodegen 
import Exp.Backend.SVM.ExpSVMCodegen 

import SVM.Instr

import System.Environment
import System.FilePath

-- main function 

main :: IO ()
main = do 
  opts <- parseOptions 
  either putStrLn runWithOptions opts 

-- running the compiler / interpreter 

runWithOptions :: Option -> IO ()
runWithOptions opts 
  = do 
      content <- readFile (file opts)
      let tree = expParser content 
      case flag opts of
        Interp -> print $ expInterp tree  
        SVMCompiler -> compileToSVM tree (file opts) 
        CCompiler -> compileToC tree (file opts)

-- calling the code generators 

compileToSVM :: Exp -> FilePath -> IO ()
compileToSVM e path 
  = do 
       let code = svmExpCodegen e
           noExt = dropExtension path 
           newfile = noExt <.> ".svm"
       writeFile newfile (pprintCode code)

compileToC :: Exp -> FilePath -> IO () 
compileToC e path 
  = do 
       let noExt = dropExtension path 
           newfile = noExt <.> ".c"
       writeFile newfile (cExpCodegen e)


-- command line options infrastructure 

data Option 
  = Option {
      flag :: Flag 
    , file :: FilePath 
    }

data Flag 
  = Interp 
  | SVMCompiler
  | CCompiler

parseOptions :: IO (Either String Option)
parseOptions 
  = do 
      args <- getArgs 
      case args of 
        [fl, fn] -> buildOption fl fn 
        _ -> errorMessage 

buildOption :: String -> String -> IO (Either String Option)
buildOption theFlag theFile 
  = case theFlag of 
      "--eval" -> return $ Right $ Option Interp theFile 
      "--svm" -> return $ Right $ Option SVMCompiler theFile 
      "--C" -> return $ Right $ Option CCompiler theFile 
      _ -> errorMessage 

errorMessage :: IO (Either String Option)
errorMessage 
  = return $ Left $ unlines [ "Invalid arguments!"
                            , "Usage: exp <flag> <file>"
                            , "Flags:"
                            , "--eval: run expression interpreter"
                            , "--svm: compile to SVM bytecode"
                            , "--C: compile to C"
                            ]
