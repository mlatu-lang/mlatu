module Mlatu.Ice (ice) where

import Relude
import Prelude (errorWithoutStackTrace)

ice :: String -> String -> a
ice fnName msg = errorWithoutStackTrace $ "Internal Compiler Error: Mlatu has failed internally.\nPlease submit a bug report at https://github.com/brightly-salty/mlatu/issues/new\nError in " <> fnName <> " with message \"" <> msg <> "\""
