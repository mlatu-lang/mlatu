module Main where

import Arguments (Arguments, parseArguments)
import Arguments qualified
import Interact qualified
import Mlatu (compile, compilePrelude, runMlatu)
import Mlatu.Dictionary (Dictionary)
import Mlatu.Interpret (interpret)
import Mlatu.Monad (M)
import Mlatu.Name (GeneralName (..), Qualified (..))
import Mlatu.Vocabulary qualified as Vocabulary
import Relude
import Report (reportAll)
import System.Directory (makeAbsolute)
import System.IO (hPutStrLn, hSetEncoding, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- parseArguments
  case Arguments.inputPaths arguments of
    [] -> case Arguments.compileMode arguments of
      Arguments.CheckMode -> do
        hPutStrLn stderr "Cannot run interactively in check mode."
        exitFailure
      Arguments.InterpretMode -> Interact.run (Arguments.prelude arguments)
    (_ : _) -> runBatch arguments

runBatch :: Arguments -> IO ()
runBatch arguments = do
  paths <- forM (Arguments.inputPaths arguments) makeAbsolute
  ((runDictionary `handleResult`) . compile mainPermissions Nothing paths . Just)
    `handleResult` compilePrelude (Arguments.prelude arguments) mainPermissions Nothing
  where
    mainPermissions =
      [ QualifiedName $ Qualified Vocabulary.global "IO",
        QualifiedName $ Qualified Vocabulary.global "Fail"
      ]
    runDictionary :: Dictionary -> IO ()
    runDictionary program =
      case Arguments.compileMode arguments of
        Arguments.CheckMode -> pass
        Arguments.InterpretMode ->
          void $ interpret program Nothing [] stdin stdout stderr []

handleResult :: (Dictionary -> IO ()) -> M Dictionary -> IO ()
handleResult dictionaryHandler dictionaryProducer = do
  do
    e <- runMlatu dictionaryProducer
    bifor_
      e
      ( reportAll >=> const exitFailure
      )
      dictionaryHandler
