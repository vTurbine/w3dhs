module SoundManager
    ( Sdmode(..)
    , Smmode(..)
    , Sdsmode(..)
    )
    where

-- | Sound mode
data Sdmode     = Sdm_Off
                | Sdm_PC
                | Sdm_AdLib
                deriving (Enum, Show)

-- | Music mode
data Smmode     = Smm_Off
                | Smm_AdLib
                deriving (Enum, Show)

-- | Digi mode
data Sdsmode    = Sds_Off
                | Sds_PC
                | Sds_SoundSource
                | Sds_SoundBlaster
                deriving (Enum, Show)

startCpMusic = undefined
