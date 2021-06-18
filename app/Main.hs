module Main where

import Arguments qualified
import Mlatu (compilePrelude, compileWithPrelude, fragment, fragmentFromSource, runMlatu)
import Mlatu.Back.Print qualified as Erlang
import Mlatu.Base.Name (GeneralName (..))
import Mlatu.Base.Vocabulary
import Mlatu.Informer (warnCheckpoint)
import Mlatu.Middle.Dictionary qualified as Dictionary
import Mlatu.Pretty (printFragment)
import Options.Applicative (execParser, header, helper, info)
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderIO)
import Report (reportAll)
import System.Directory (makeAbsolute, removeFile)
import System.IO (hSetEncoding, utf8)
import System.Process.Typed (runProcess_)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  arguments <- execParser opts
  case arguments of
    Arguments.FormatFiles files -> formatFiles files
    Arguments.CheckFiles files -> checkFiles files
    Arguments.RunFiles bench files -> runFiles bench files
    Arguments.CompileFiles files -> compileFiles files
    Arguments.Script input -> scriptInput input
  where
    opts =
      info (Arguments.options <**> helper) (header "The Mlatu programming language")

mainPermissions :: [GeneralName]
mainPermissions =
  [ QualifiedName $ Global "io",
    QualifiedName $ Global "fail",
    QualifiedName $ Global "otp"
  ]

handleCompilation _ (Nothing, reports) = reportAll reports >> exitFailure
handleCompilation f (Just dict, reports) = reportAll reports >> f dict

formatFiles :: [FilePath] -> IO ()
formatFiles =
  traverse_
    ( makeAbsolute
        >=> ( \path ->
                readFileBS path
                  >>= (runMlatu . fragmentFromSource mainPermissions Nothing 0 path . decodeUtf8)
                  >>= handleCompilation
                    ( \fragment ->
                        withFile
                          path
                          WriteMode
                          (`renderIO` layoutSmart defaultLayoutOptions (printFragment fragment))
                          >> exitSuccess
                    )
            )
    )

checkFiles :: [FilePath] -> IO ()
checkFiles relativePaths =
  forM relativePaths makeAbsolute
    >>= ( \paths ->
            runMlatu
              ( do
                  dict <- compileWithPrelude mainPermissions Nothing paths
                  warnCheckpoint
                  pure dict
              )
              >>= handleCompilation (const exitSuccess)
        )

base :: IO () -> [FilePath] -> IO ()
base after relativePaths =
  forM relativePaths makeAbsolute
    >>= (runMlatu . compileWithPrelude mainPermissions Nothing)
    >>= handleCompilation
      ( \program ->
          Erlang.generate program Nothing >>= \contents ->
            writeFileText "mlatu.erl" contents
              >> runProcess_ "erlc -W0 mlatu.erl"
              >> removeFile "mlatu.erl"
              >> after
      )

runFiles :: Bool -> [FilePath] -> IO ()
runFiles bench = base $ runProcess_ (if bench then "time escript mlatu.beam" else "escript mlatu.beam") >> removeFile "mlatu.beam" >> exitSuccess

compileFiles :: [FilePath] -> IO ()
compileFiles = base $ putStrLn "Produced the BEAM bytecode file `mlatu.beam`.\nExecute it by running `escript mlatu.beam`" >> exitSuccess

benchFiles :: [FilePath] -> IO ()
benchFiles = base $ runProcess_ "time escript mlatu.beam" >> removeFile "mlatu.beam" >> exitSuccess

scriptInput :: Text -> IO ()
scriptInput input = do
  let modifiedInput = input <> " println"
  result <- runMlatu $ do
    commonDictionary <- Mlatu.compilePrelude mainPermissions Nothing
    parsed <- Mlatu.fragmentFromSource mainPermissions Nothing 1 "input" modifiedInput
    Mlatu.fragment parsed commonDictionary
  handleCompilation
    ( \program ->
        Erlang.generate program Nothing >>= \contents ->
          writeFileText "mlatu.erl" contents
            >> runProcess_ "escript mlatu.erl"
            -- >> removeFile "mlatu.erl"
    )
    result
