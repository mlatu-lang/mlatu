module Mlatu.Codegen (generateWasm) where

import Mlatu.Codegen.Structure (Module (..))
import Mlatu.Dictionary (Dictionary (..))
import Relude

generateWasm :: Dictionary -> ByteString
generateWasm = moduleToBS . dictToModule

dictToModule :: Dictionary -> Module
dictToModule = undefined

moduleToBS :: Module -> ByteString
moduleToBS = undefined
