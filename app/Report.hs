module Report
  ( reportAll,
  )
where

import Mlatu.Informer (Level (..), Report (..))
import Mlatu.Origin (Origin, beginColumn, beginLine, endColumn, endLine, name)
import Mlatu.Pretty (printOrigin)
import Optics
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Directory (makeAbsolute)
import System.IO (hPrint, hPutStrLn)

reportAll :: [Report] -> IO ()
reportAll reports = do
  traverse_ printReport (ordNub ((\(Report _ o d) -> (o, show d)) <$> reports))
  where
    printReport (o, doc) = do
      hPrint stderr (printOrigin o)
      let (bl, bc) = (view beginLine o, view beginColumn o)
      let (el, ec) = (view endLine o, view endColumn o)
      when (bl == el) $ do
        absolute <- makeAbsolute (toString (view name o))
        contents <- decodeUtf8 <$> readFileBS absolute
        let rightLine = case lines contents !!? (bl - 1) of
              Just line -> line
              Nothing -> Unsafe.head (lines contents)
        hPutStrLn stderr (toString rightLine)
        hPutStrLn stderr (replicate (bc - 1) ' ' <> replicate (ec - bc) '^')
      hPutStrLn stderr doc
