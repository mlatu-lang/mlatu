module Report
  ( reportAll,
  )
where

import Mlatu.Report (Level (..), Report (..), human)
import Relude
import System.IO (hPutStrLn)

reportAll :: [Report] -> IO ()
reportAll reports = do
  unless (null errors) $ traverse_ (hPutStrLn stderr) $ "Errors: " : errors
  unless (null warnings) $ traverse_ (hPutStrLn stderr) $ "Warnings: " : warnings
  unless (null infos) $ traverse_ (hPutStrLn stderr) $ "Infos: " : infos
  where
    errors = ordNub $ show . human <$> filter (\(Report level _) -> level == Error) reports
    warnings = ordNub $ show . human <$> filter (\(Report level _) -> level == Warn) reports
    infos = ordNub $ show . human <$> filter (\(Report level _) -> level == Info) reports
