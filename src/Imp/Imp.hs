
import Imp.Interpreter.ImpInterpreter
import Imp.Frontend.Parser.Parser 
import Imp.Frontend.Semantics.StatementTypeChecker
import Imp.Syntax.Syntax

import Control.Monad 
import System.Environment
import Imp.Backend.SVM.StatementCodegen (compileProgram)
import Imp.Backend.C.StatementCCodegen
import System.FilePath
import SVM.Instr (pprintCode)

-- main function for Imp 

main :: IO ()
main = do 
  opts <- parseOptions 
  either putStrLn runWithOptions opts 

-- running according to options 

runWithOptions :: Option -> IO ()
runWithOptions opts 
  = do 
      content <- readFile (file opts)
      let tree = impParser content 
      case flag opts of 
        Typed -> 
          case tcProgram tree of 
            Left err -> print err 
            Right _  -> void $ interpProgram tree 
        Untyped -> void $ interpProgram tree 
        SVMCompiler -> compileToSVM tree (file opts)
        CCompiler -> compileToC tree (file opts)

-- compiling to SVM code 

compileToSVM :: Program -> FilePath -> IO () 
compileToSVM prog path 
  =  case tcProgram prog of 
        Left err -> print err 
        Right _ -> do 
          code <- compileProgram prog 
          let noExt = dropExtension path  
              outPath = noExt <.> ".svm"
          writeFile outPath (pprintCode code)   

-- compiling to C code

compileToC :: Program -> FilePath -> IO ()
compileToC prog path
  =  case tcProgram prog of 
        Left err -> print err 
        Right _ -> do 
          let code = programCodegen prog 
              noExt = dropExtension path  
              outPath = noExt <.> ".c"
          writeFile outPath code   

-- data type for command line options

data Option 
  = Option {
      flag :: Flag 
    , file :: FilePath
    } deriving Show 

-- flag which determine, what should be executed.

data Flag 
  = Typed 
  | Untyped
  | SVMCompiler
  | CCompiler
  deriving Show 

-- error message, when parameters are passed wrong

errorMessage :: IO (Either String Option)
errorMessage 
  = return $ Left str 
    where 
      str = unlines [ "Invalid parameter usage!"
                    , "Imp - compiler"
                    , "Usage:"
                    , "imp <flag> <file>"
                    , "<flag> : type of execution"
                    , " --typed: typed interpreter"
                    , " --untyped: untyped interpreter"
                    , " --svm: compile to SVM bytecode"
                    , " --C: compile to C code"
                    ]

buildOption :: String -> String -> IO (Either String Option)
buildOption fg fl 
  = case fg of 
      "--type" -> return $ Right (Option Typed fl)
      "--untyped" -> return $ Right (Option Untyped fl)
      "--svm" -> return $ Right (Option SVMCompiler fl)
      "--C" -> return $ Right (Option CCompiler fl)
      _ -> errorMessage  

parseOptions :: IO (Either String Option)
parseOptions 
  = do 
      args <- getArgs 
      case args of 
        [fg, fl] -> buildOption fg fl 
        _ -> errorMessage

