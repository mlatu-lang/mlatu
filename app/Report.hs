module Report
  ( reportAll,
  )
where

import Mlatu.Report (Level (..), Report (..), human)
import Relude
import System.IO (hPutStrLn)

reportAll :: [Report] -> IO ()
reportAll reports = do
  unless (null errors) $ mapM_ (hPutStrLn stderr) $ "Errors: " : errors
  unless (null warnings) $ mapM_ (hPutStrLn stderr) $ "Warnings: " : warnings
  unless (null infos) $ mapM_ (hPutStrLn stderr) $ "Infos: " : infos
  where
    errors = ordNub $ map (show . human) $ filter (\(Report level _) -> level == Error) reports
    warnings = ordNub $ map (show . human) $ filter (\(Report level _) -> level == Warn) reports
    infos = ordNub $ map (show . human) $ filter (\(Report level _) -> level == Info) reports
