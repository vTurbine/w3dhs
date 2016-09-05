module Settings where


-- |The width of the screen in pxs
scrWidth :: Int
scrWidth = 320

-- |The height of the screen in pxs
scrHeight :: Int
scrHeight = 200

-- |Screen's depth
scrBpp :: Int
scrBpp = 8      -- we use indexed mode with predefined palette.

-- |Path to the game sources
gameSrcPath :: FilePath
gameSrcPath = "wolf3d/WOLFSRC/" -- @todo Change in accordance to the cmdline

-- |Path to the game data
gameBinPath :: FilePath
gameBinPath = "data/" -- @todo Change in accordance to the cmdline

-- |The extension of the game's resources
gameBinExt :: String
gameBinExt = ".WL1" -- @todo Change in accordance to selected mode
