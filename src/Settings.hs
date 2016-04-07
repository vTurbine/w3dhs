module Settings where


-- |The width of the screen in pxs
scrWidth :: Int
scrWidth = 320

-- |The height of the screen in pxs
scrHeight :: Int
scrHeight = 200

-- |Screen's depth
scrBpp :: Int
scrBpp = 3 * 6 -- we have only 6 bits per px color component in VGA mode.

-- |Path to the game sources
gameSrcPath :: FilePath
gameSrcPath = "WOLF3D-SRC\\"

-- |Path to the game data
gameBinPath :: FilePath
gameBinPath = "WOLF3D\\"
