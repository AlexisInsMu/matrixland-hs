module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputChar, outputStrLn, runInputT, InputT )
import System.Console.ANSI ( clearScreen, setCursorPosition )
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runInputT defaultSettings loop
    where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputChar "Press a key (q to quit): "
      case minput of
        Just 'q' -> outputStrLn "Exiting..."
        Just c   -> do
          liftIO clearScreen
          liftIO $ setCursorPosition 0 0
          outputStrLn ("Key pressed: " ++ [c])
          loop
        Nothing  -> loop
        
