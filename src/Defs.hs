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


