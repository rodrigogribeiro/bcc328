
import Imp.Interpreter.ImpInterpreter
import qualified Imp.Frontend.Parser.LALR.Parser as L
import qualified Imp.Frontend.Parser.Recursive.Parser as R
import qualified Imp.Frontend.Parser.PEG.Parser as P 
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
      let tree = parseWithOptions opts content 
      case backEnd opts of 
        Left Typed ->
          case tree of 
            Left err -> print err 
            Right ast -> 
              case tcProgram ast of 
                Left err -> print err 
                Right _  -> void $ interpProgram ast 
        Left Untyped -> 
          case tree of 
            Left err -> print err  
            Right ast -> void $ interpProgram ast 
        Right SVMCompiler -> compileToSVM tree (file opts)
        Right CCompiler -> compileToC tree (file opts)

parseWithOptions :: Option -> String -> Either String Program
parseWithOptions opts content
  = case parserType opts of
      LALR -> L.impParser content  
      Recursive -> R.impParser content  
      PEG -> P.impParser content  

-- compiling to SVM code 

compileToSVM :: Either String Program -> FilePath -> IO () 
compileToSVM (Left err) _ = putStrLn err
compileToSVM (Right prog) path 
  =  case tcProgram prog of 
        Left err -> print err 
        Right _ -> do 
          code <- compileProgram prog 
          let noExt = dropExtension path  
              outPath = noExt <.> ".svm"
          writeFile outPath (pprintCode code)   

-- compiling to C code

compileToC :: Either String Program -> FilePath -> IO ()
compileToC (Left err) _ = putStrLn err
compileToC (Right prog) path
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
      parserType :: ParserType   
    , backEnd :: Either Interpreter CodeGen 
    , file :: FilePath
    } deriving Show 

emptyOption :: Option 
emptyOption = Option LALR (Left Untyped) ""

-- flag which determine, what should be executed.

data ParserType 
  = LALR 
  | Recursive 
  | PEG 
  deriving Show 

data Interpreter 
  = Typed 
  | Untyped
  deriving Show 

data CodeGen 
  = SVMCompiler
  | CCompiler
  deriving Show 

-- error message, when parameters are passed wrong

errorMessage :: String
errorMessage 
  = unlines ("Invalid parameter usage!" : helpMessage)


helpMessage :: [String]
helpMessage = [ "Imp - compiler"
              , "Usage:"
              , "imp <flags> <file>"
              , "<flag> : parser / execution options"
              , "* Parser options:"
              , "--lalr: use lalr parser"
              , "--recursive: use recursive descendent parser"
              , "--peg: use PEG based parser"
              , "* Interpreter options:"
              , " --typed: typed interpreter"
              , " --untyped: untyped interpreter"
              , "* Code generator options:"
              , " --svm: compile to SVM bytecode"
              , " --C: compile to C code"
              , "* Help message"
              , " --help: prints this message"
              ]

buildOption :: String -> Either String Option -> Either String Option
buildOption _ (Left err) = Left err
buildOption fg (Right opt) 
  = case fg of 
      "--lalr:" -> Right (opt{parserType = LALR})
      "--recursive:" -> Right (opt{parserType = Recursive})
      "--peg:" -> Right (opt{parserType = PEG})
      "--typed" -> Right (opt{backEnd = Left Typed})
      "--untyped" -> Right (opt{backEnd = Left Untyped})
      "--svm" -> Right (opt{backEnd = Right SVMCompiler})
      "--C" -> Right (opt{backEnd = Right CCompiler})
      "--help" -> Left $ unlines helpMessage
      _ -> Left errorMessage  

parseOptions :: IO (Either String Option)
parseOptions 
  = do 
      args <- getArgs 
      pure $ foldr buildOption (Right emptyOption) args

