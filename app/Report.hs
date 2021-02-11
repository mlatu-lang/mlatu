module Report
  ( reportAll,
  )
where

import Mlatu.Report (Report)
import Mlatu.Report qualified as Report
import Relude
import System.IO (hPutStrLn)
import Text.PrettyPrint qualified as Pretty

reportAll :: [Report] -> IO ()
reportAll = mapM_ $ hPutStrLn stderr . Pretty.render . Report.human
