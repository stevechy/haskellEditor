module HaskellEditor.ShellExecution 

where

import qualified Shelly
import qualified Data.Text
import qualified Data.String
import Control.Concurrent.STM
import Graphics.UI.Gtk

import qualified HaskellEditor.Types

runCabal ::  TextViewClass self => HaskellEditor.Types.EditorWindow -> self -> (String, [String]) -> IO ()
runCabal editor consoleOut (cabalCommand, cabalArgs) = do
    rootPath <- atomically $ readTVar $ HaskellEditor.Types._rootPath editor
    putStrLn $ show rootPath
    case rootPath of 
       Just rootFilePath -> do 
           results <- Shelly.shelly $ Shelly.errExit False $ Shelly.chdir (Shelly.fromText $ Data.String.fromString rootFilePath) $ do
               runResults <- Shelly.run (Shelly.fromText $ Data.String.fromString $ cabalCommand) (map Data.String.fromString cabalArgs)
               stdErrResults <- Shelly.lastStderr
               return $ Data.Text.concat [runResults, stdErrResults]
           textBuf <- textViewGetBuffer consoleOut

           let resultString = Data.Text.unpack results
           textBufferSetText textBuf resultString
           return ()
       Nothing -> return ()
    return ()
