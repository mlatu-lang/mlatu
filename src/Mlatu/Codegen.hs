module Mlatu.Codegen (generateWasm) where

import Mlatu.Codegen.Binary qualified as Binary
import Mlatu.Codegen.Structure (Module (..))
import Mlatu.Dictionary (Dictionary (..))
import Relude
import Data.ByteString.Builder (Builder)

generateWasm :: Dictionary -> Builder
generateWasm = Binary.module_ . dictToModule

dictToModule :: Dictionary -> Module
dictToModule = undefined