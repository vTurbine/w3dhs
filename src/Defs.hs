{-
  Common project definitions
-}

module Defs where

-- | Variant of build
--
data BuildVariant = BuildBeta       -- beta test
                  | BuildJapan      -- japanese
                  | BuildJapanDemo  -- japanese demo
                  | BuildUpload     -- looks like a demo of GoodTimes
                  | BuildSpear      -- Spear of the destiny (SOD) release
                  | BuildSpearDemo  -- SOD demo
                  | BuildGoodTimes  -- normal WOLF3D release
                  deriving (Show)


-- | Select game resource extension in accordance to the build variant
--
gameBinExt :: BuildVariant -> String
gameBinExt BuildJapanDemo = ".WJ1"
gameBinExt BuildJapan     = ".WJ6"
gameBinExt BuildUpload    = ".WL1"
gameBinExt BuildBeta      = ".WL3"
gameBinExt BuildGoodTimes = ".WL6"
gameBinExt BuildSpearDemo = ".SDM"
gameBinExt BuildSpear     = ".SOD"
