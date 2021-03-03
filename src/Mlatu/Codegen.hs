module Mlatu.Codegen (generateWasm) where

import Mlatu.Codegen.Binary qualified as Binary
import Mlatu.Codegen.Structure (Module (..))
import Mlatu.Dictionary (Dictionary (..))
import Relude

generateWasm :: Dictionary -> ByteString
generateWasm = Binary.module . dictToModule

dictToModule :: Dictionary -> Module
dictToModule = undefined