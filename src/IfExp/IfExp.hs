import IfExp.Interpreter.Interpreter
import IfExp.Frontend.Parser.Parser 

import System.Environment

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
      let tree = ifExpParser content
      case interp tree of 
        TypeError -> putStrLn "The program has a type error"
        Ok v -> print v 

-- data type for command line options

data Option 
  = Option {
     file :: FilePath
    } deriving Show 

-- error message, when parameters are passed wrong

errorMessage :: IO (Either String Option)
errorMessage 
  = return $ Left str 
    where 
      str = unlines [ "Invalid parameter usage!"
                    , "IfExp - Interpreter"
                    , "Usage:"
                    , "ifexp <file>"
                   ]

buildOption :: String -> IO (Either String Option)
buildOption fg 
  = pure $ Right $ Option fg 

parseOptions :: IO (Either String Option)
parseOptions 
  = do 
      args <- getArgs 
      case args of 
        [fg] -> buildOption fg
        _ -> errorMessage

