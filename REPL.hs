

module REPL where
import System.IO

import Core
import Parser


main = do greet
          repl stdin mempty

greet :: IO ()
greet = putStrLn "Morpheus REPL environment"

quit :: IO ()
quit = putStrLn "Goodbye."

showHelp = putStr help
    where help = unlines [ "-- Morpheus REPL help --"
                         , "REPL environment commands are preceded with a ':'"
                         , "Any line not beginning with a colon is executed as Morpheus code."
                         , "- Commands"
                         , " :?  - Show this help page"
                         , " :q  - quit the Morpheus REPL"
                         , " :clear  - reset to an empty environment (clear the stack)"
                         , ""
                         ]

prompt = ">"

repl :: Handle -> MState -> IO ()
repl hdl s = do putStr prompt
                hFlush stdout
                input <- hGetLine hdl
                case input of
                  (':':cmd) -> replCmd cmd hdl s
                  _ -> replRun $ parse input
    where replRun (Left err) = putStrLn (unlines err) >> repl hdl s
          replRun (Right prgm) = let (ret, newState) = runEnv prgm s
                                 in do exec newState
                                       print ret
                                       putStrLn $ "Stack: " ++ showStack (stack newState)
                                       repl hdl $ setAction newState (return ())
                                       -- clear action so we don't do it again next loop

replCmd :: String -> Handle -> MState -> IO ()
replCmd "q" _ _ = quit
replCmd "?" hdl s = showHelp >> repl hdl s
replCmd "clear" hdl _ = repl hdl mempty

exec :: MState -> IO ()
exec = action

